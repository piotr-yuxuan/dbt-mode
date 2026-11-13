;;; dbt-lsp-test.el --- dbt-lsp.el with a scripted dbt lsp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; These tests run the client through lsp-mode, as a user's commands do,
;; against tests/bin/fake_dbt_lsp.py, which tests/bin/dbt runs for `dbt
;; lsp'.  Each test gives the server its answers, which follow the shape
;; that dbt lsp 2.0.9 gives them, and reads what the client sent in the
;; server's log.  They need python3, and no dbt.

;;; Code:

(require 'dbt-mode-test-helpers)
;; lsp-mode loads its own modules, such as lsp-lens, by their autoloads.
(load "lsp-mode-autoloads" nil t)
(require 'dbt-lsp)
(require 'json)

(defconst dbt-lsp-test-dbt (expand-file-name "bin/dbt" dbt-test-directory)
  "The stand-in for dbt, whose `dbt lsp' is the scripted server.")

(defvar dbt-lsp-test-log nil
  "The log of the scripted server of the running test.")

(defvar dbt-lsp-test-messages-start nil
  "Where the messages of the running test start in the *Messages* buffer.")

(defmacro dbt-lsp-test-with-server (root answers &rest body)
  "Run BODY in models/marts/orders.sql of a fresh project ROOT, with dbt lsp.
The server is the scripted one, and stops afterwards.  ANSWERS,
evaluated with ROOT bound, is an alist from a method, or from
\"workspace/executeCommand COMMAND\", to its answer, as `json-encode'
writes it.  The buffers of rows and lineage are killed afterwards."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((answers-file (make-symbol "answers-file")))
    `(dbt-test-with-project ,root
       (let* ((,answers-file (make-temp-file "dbt-lsp-answers" nil ".json" (json-encode (or ,answers (make-hash-table)))))
              (dbt-lsp-test-log (make-temp-file "dbt-lsp-log"))
              (dbt-lsp-test-messages-start (with-current-buffer (messages-buffer) (point-max)))
              (process-environment (append (list (concat "DBT_MODE_FAKE_ANSWERS=" ,answers-file)
                                                 (concat "DBT_MODE_FAKE_LOG=" dbt-lsp-test-log))
                                           process-environment))
              (dbt-executable dbt-lsp-test-dbt)
              (lsp--session nil)
              (lsp-session-file (make-temp-name (expand-file-name "lsp-session" temporary-file-directory)))
              (lsp-enable-file-watchers nil)
              (lsp-restart 'ignore)
              (lsp-keep-workspace-alive nil)
              (lsp-auto-guess-root nil)
              (lsp-client-packages nil))
         (unwind-protect
             (progn
               (dbt-test-visit ,root "models/marts/orders.sql")
               (dbt-lsp)
               (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))))
               ,@body)
           (dolist (workspace (lsp--session-workspaces (lsp-session)))
             (lsp-workspace-shutdown workspace))
           (dolist (buffer (buffer-list))
             (when (string-match-p "\\`\\*dbt \\(?:rows\\|lineage\\): " (buffer-name buffer))
               (kill-buffer buffer)))
           (delete-file ,answers-file)
           (delete-file dbt-lsp-test-log))))))

(defun dbt-lsp-test-received ()
  "Return what the scripted server received: its arguments, then each message."
  (with-temp-buffer
    (insert-file-contents dbt-lsp-test-log)
    (mapcar (lambda (line) (json-parse-string line :object-type 'plist :null-object nil))
            (split-string (buffer-string) "\n" t))))

(defun dbt-lsp-test-methods ()
  "Return the methods that the scripted server received, in order.
A command is shown as \"workspace/executeCommand COMMAND\"."
  (delq nil (mapcar (lambda (message)
                      (let ((method (plist-get message :method)))
                        (if (equal method "workspace/executeCommand")
                            (concat method " " (plist-get (plist-get message :params) :command))
                          method)))
                    (dbt-lsp-test-received))))

(defun dbt-lsp-test-message (regexp)
  "Wait until a message of the running test matches REGEXP, and return it."
  (dbt-test-wait (lambda ()
                   (with-current-buffer (messages-buffer)
                     (save-excursion
                       (goto-char dbt-lsp-test-messages-start)
                       (and (re-search-forward (concat "^.*" regexp ".*$") nil t)
                            (match-string 0)))))))

(defun dbt-lsp-test-rows (name)
  "Return the rows of the buffer of results NAME, once it shows them.
The buffer is killed, so that the next preview makes a new one."
  (let ((buffer (dbt-test-wait (lambda () (get-buffer (format "*dbt rows: %s*" name))))))
    (prog1 (with-current-buffer buffer
             (mapcar (lambda (entry) (append (cadr entry) nil)) tabulated-list-entries))
      (kill-buffer buffer))))

(defun dbt-lsp-test-file-text (root file)
  "Return the text of FILE, relative to ROOT, as it is on disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file root))
    (buffer-string)))

(ert-deftest dbt-lsp-runs-the-server-on-the-project ()
  "The server runs on the root of the project, with the options of dbt-lsp."
  (let ((dbt-lsp-enable-linter t)
        (dbt-lsp-arguments '("--profiles-dir" "/profiles")))
    (dbt-lsp-test-with-server root nil
      (should (equal (plist-get (car (dbt-lsp-test-received)) :argv)
                     (vector "lsp" "--project-dir" (directory-file-name root)
                             "--lint-enabled" "true" "--profiles-dir" "/profiles"))))))

(ert-deftest dbt-lsp-hides-formatting-from-lsp-mode ()
  "lsp-mode does not format by itself, as the server formats the file on disk."
  (dbt-lsp-test-with-server root nil
    (should (lsp-feature? "textDocument/hover"))
    (should-not (lsp-feature? "textDocument/formatting"))
    (should-not (lsp-feature? "textDocument/rangeFormatting"))))

(ert-deftest dbt-lsp-takes-the-notifications-of-the-server ()
  "The notifications of the server's own raise no warning."
  (let ((start (with-current-buffer (get-buffer-create "*Warnings*") (point-max)))
        (warning-minimum-log-level :warning))
    (dbt-lsp-test-with-server root nil
      ;; The server answers in order, so its notifications came first.
      (lsp-request "textDocument/hover" (lsp--text-document-position-params))
      (should-not (string-match-p "Unknown notification"
                                  (with-current-buffer "*Warnings*" (buffer-substring start (point-max))))))))

(ert-deftest dbt-lsp-show-shows-null-true-numbers-and-strings ()
  "The rows of a model show null as NULL, true, numbers and strings as such.
lsp-mode reads false as it reads null, so false shows as NULL too."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show"
         :columns ["n" "f" "t" "z" "s" "x"]
         :data [(:n nil :f :json-false :t t :z 0 :s "text" :x 1.5)
                (:n "a" :f "b" :t "c" :z "d" :s "e" :x "f")]
         :error nil))
    (let ((dbt-preview-limit 7))
      (dbt-lsp-show))
    (should (equal (dbt-lsp-test-rows "orders")
                   '(("NULL" "NULL" "true" "0" "text" "1.5") ("a" "b" "c" "d" "e" "f"))))
    (let ((params (plist-get (car (last (dbt-lsp-test-received))) :params)))
      (should (equal (plist-get params :command) "dbt.show"))
      (should (equal (aref (plist-get params :arguments) 0)
                     (list :uri (lsp--buffer-uri) :limit 7))))))

(ert-deftest dbt-lsp-show-shows-no-rows ()
  "A query of no rows shows its columns and no row."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show" :columns ["a"] :data [] :error nil))
    (dbt-lsp-show "select 1 as a where false")
    (should (equal (dbt-lsp-test-rows "query") nil))))

(ert-deftest dbt-lsp-compile-file-shows-the-compiled-sql ()
  "C-c C-v shows, read-only, the file of compiled SQL that the server names."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))
         :error nil))
    (make-directory (expand-file-name "target/compiled/shop/models/marts/" root) t)
    (with-temp-file (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)
      (insert "select 1 as compiled\n"))
    (dbt-lsp-compile-file)
    (with-current-buffer (dbt-test-wait (lambda () (get-file-buffer (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))))
      (should buffer-read-only)
      (should (equal (buffer-string) "select 1 as compiled\n"))
      (should (get-buffer-window (current-buffer) t)))))

(ert-deftest dbt-lsp-preview-cte-shows-the-rows-of-a-lens ()
  "The code lens of a CTE shows the rows of that CTE of the compiled SQL."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)))
        ("workspace/executeCommand dbt.show" :columns ["a"] :data [(:a "1")]))
    (make-directory (expand-file-name "target/compiled/shop/models/marts/" root) t)
    (with-temp-file (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)
      (insert "with customers as (\n    select 1 as a\n)\nselect * from customers\n"))
    (lsp--execute-command
     (json-parse-string (json-encode (list :command "dbt.previewCte"
                                           :arguments (vector buffer-file-name '(:name "customers"))))
                        :null-object nil :false-object :json-false))
    (should (equal (dbt-lsp-test-rows "customers") '(("1"))))
    (should (string-match-p "select 1 as a"
                            (plist-get (aref (plist-get (plist-get (car (last (dbt-lsp-test-received))) :params)
                                                        :arguments)
                                             0)
                                       :inline)))))

(ert-deftest dbt-lsp-lineage-shows-the-nodes-of-the-server ()
  "C-c C-g shows the nodes that the server lists, files in their packages."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes"
         :nodes [(:unique_id "seed.shop.raw_orders" :name "raw_orders" :resource_type "seed"
                             :package_name "shop" :original_file_path "seeds/raw_orders.csv"
                             :depends_on (:nodes []))
                 (:unique_id "model.shop.stg_orders" :name "stg_orders" :resource_type "model"
                             :package_name "shop" :original_file_path "models/staging/stg_orders.sql"
                             :depends_on (:nodes ["seed.shop.raw_orders"]))
                 (:unique_id "model.shop_utils.util" :name "util" :resource_type "model"
                             :package_name "shop_utils" :original_file_path "models/util.sql"
                             :depends_on (:nodes ["model.shop.stg_orders"]))]
         :error nil))
    (dbt-lsp-lineage "stg_orders")
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt lineage: stg_orders*")))
      (should (equal (mapcar (lambda (entry) (append (cadr entry) nil)) tabulated-list-entries)
                     '(("upstream" "1" "seed" "raw_orders" "seeds/raw_orders.csv")
                       ("downstream" "1" "model" "util" "dbt_packages/shop_utils/models/util.sql")))))
    (should (equal (aref (plist-get (plist-get (car (last (dbt-lsp-test-received))) :params) :arguments) 0)
                   "+stg_orders+"))))

(ert-deftest dbt-lsp-rename-model-refuses-what-dbt-would-not-take ()
  "Renaming a model refuses a name that is not one, or the name of its neighbour."
  (dbt-lsp-test-with-server root nil
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (should-error (dbt-lsp-rename-model "2fast") :type 'user-error)
      (should-error (dbt-lsp-rename-model "stg-clients") :type 'user-error)
      (should-error (dbt-lsp-rename-model "stg_orders") :type 'user-error)
      (should (equal buffer-file-name (expand-file-name "models/staging/stg_customers.sql" root))))
    (should-not (member "workspace/willRenameFiles" (dbt-lsp-test-methods)))))

(ert-deftest dbt-lsp-rename-model-renames-the-file ()
  "Renaming a model renames its file, which lsp-mode tells the server first."
  (dbt-lsp-test-with-server root nil
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (dbt-lsp-rename-model "stg_clients")
      (should (equal buffer-file-name (expand-file-name "models/staging/stg_clients.sql" root)))
      (should-not (file-exists-p (expand-file-name "models/staging/stg_customers.sql" root))))
    (should (member "workspace/willRenameFiles" (dbt-lsp-test-methods)))))

(ert-deftest dbt-lsp-format-buffer-saves-then-formats ()
  "Formatting saves the buffer, then applies the edits of the server."
  (dbt-lsp-test-with-server root
      '(("textDocument/formatting"
         . [(:range (:start (:line 0 :character 0) :end (:line 0 :character 0)) :newText "-- formatted\n")]))
    (goto-char (point-min))
    (insert "-- unsaved\n")
    (dbt-lsp-format-buffer)
    (should (string-prefix-p "-- unsaved\n" (dbt-lsp-test-file-text root "models/marts/orders.sql")))
    (should (string-prefix-p "-- formatted\n-- unsaved\n" (buffer-string)))
    (should (member "textDocument/didSave"
                    (member "textDocument/didChange" (dbt-lsp-test-methods))))
    (should (member "textDocument/formatting"
                    (member "textDocument/didSave" (dbt-lsp-test-methods))))))

;; What the server answers is checked before it is read.

(ert-deftest dbt-lsp-names-the-command-of-an-answer-it-cannot-read ()
  "An answer of an unexpected shape is reported with its command, and changes nothing."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show" :columns 3 :data [])
        ("workspace/executeCommand dbt.compileFile" :file_uri 7)
        ("workspace/executeCommand dbt.listNodes" . ["model.shop.orders"])
        ("textDocument/formatting" . [(:range 3 :newText "x")]))
    (dbt-lsp-show)
    (dbt-lsp-test-message "dbt lsp: dbt\\.show answered what dbt-mode cannot read: {\"columns\":3")
    (dbt-lsp-compile-file)
    (dbt-lsp-test-message "dbt lsp: dbt\\.compileFile answered what dbt-mode cannot read")
    (dbt-lsp-lineage "orders")
    (dbt-lsp-test-message "dbt lsp: dbt\\.listNodes answered what dbt-mode cannot read")
    (let ((text (buffer-string)))
      (should (string-match-p "\\`dbt lsp: textDocument/formatting answered what dbt-mode cannot read"
                              (error-message-string (should-error (dbt-lsp-format-buffer)))))
      (should (equal (buffer-string) text)))
    (should-not (get-buffer "*dbt rows: orders*"))
    (should-not (get-buffer "*dbt lineage: orders*"))))

(ert-deftest dbt-lsp-names-the-node-it-cannot-read ()
  "A node of an unexpected shape among those of the lineage is reported."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes"
         :nodes [(:unique_id "model.shop.orders" :name "orders" :resource_type "model"
                             :package_name "shop" :original_file_path "models/marts/orders.sql"
                             :depends_on (:nodes [3]))]))
    (dbt-lsp-lineage "orders")
    (dbt-lsp-test-message "dbt lsp: dbt\\.listNodes answered what dbt-mode cannot read")
    (should-not (get-buffer "*dbt lineage: orders*"))))

(ert-deftest dbt-lsp-says-when-the-server-answers-nothing-or-fails ()
  "A command that the server answers with null, or with an error, says so."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes" :error "No nodes found" :nodes []))
    (dbt-lsp-show)
    (dbt-lsp-test-message "dbt lsp: dbt\\.show answered nothing")
    (dbt-lsp-lineage "orders")
    (dbt-lsp-test-message "dbt lsp: dbt\\.listNodes failed: No nodes found")
    (should-not (get-buffer "*dbt rows: orders*"))))

(ert-deftest dbt-lsp-names-the-lens-it-cannot-read ()
  "A code lens of a CTE whose arguments are not a file and a CTE is reported."
  (dbt-lsp-test-with-server root nil
    (should (string-match-p "\\`dbt lsp: dbt\\.previewCte answered what dbt-mode cannot read"
                            (error-message-string
                             (should-error
                              (lsp--execute-command
                               (json-parse-string (json-encode (list :command "dbt.previewCte"
                                                                     :arguments (vector buffer-file-name)))
                                                  :null-object nil :false-object :json-false))))))))

(provide 'dbt-lsp-test)
;;; dbt-lsp-test.el ends here
