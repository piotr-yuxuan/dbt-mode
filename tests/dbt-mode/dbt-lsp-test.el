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
(require 'cl-lib)
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

(ert-deftest dbt-lsp-loads-without-the-commands-of-dbt ()
  "The client of dbt lsp takes the preview limit from the project model.
So it does not load dbt-cli, with transient and compile."
  (with-temp-buffer
    (should (eq 0 (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                "-Q" "--batch" "-L" (file-name-directory (locate-library "dbt-lsp"))
                                "--eval" "(load \"lsp-mode-autoloads\" nil t)" "-l" "dbt-lsp"
                                "--eval" "(princ (list (featurep 'dbt-cli) dbt-preview-limit))")))
    (should (equal (buffer-string) "(nil 50)"))))

(ert-deftest dbt-lsp-runs-the-server-on-the-project ()
  "The server runs on the root of the project, with the options of dbt-lsp."
  (let ((dbt-lsp-enable-linter t)
        (dbt-lsp-arguments '("--profiles-dir" "/profiles")))
    (dbt-lsp-test-with-server root nil
      (should (equal (plist-get (car (dbt-lsp-test-received)) :argv)
                     (vector "lsp" "--project-dir" (directory-file-name root)
                             "--lint-enabled" "true" "--profiles-dir" "/profiles"))))))

(ert-deftest dbt-lsp-runs-a-nested-project-on-itself ()
  "A project nested in another, as the integration tests of a package, has its server."
  (dbt-lsp-test-with-server root nil
    (let ((nested (expand-file-name "integration_tests/" root)))
      (make-directory (expand-file-name "models" nested) t)
      (with-temp-file (expand-file-name "dbt_project.yml" nested)
        (insert "name: 'shop_tests'\nprofile: 'shop'\n"))
      (with-temp-file (expand-file-name "packages.yml" nested)
        (insert "packages:\n  - local: ../\n"))
      (with-temp-file (expand-file-name "models/uses_shop.sql" nested)
        (insert "select * from {{ ref('orders') }}\n"))
      (with-current-buffer (dbt-test-visit nested "models/uses_shop.sql")
        (dbt-lsp)
        (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))))
        (should (equal (cadr (member "--project-dir" (process-command (lsp--workspace-cmd-proc (dbt-lsp-workspace)))))
                       (directory-file-name nested)))
        (should (equal (lsp--workspace-root (dbt-lsp-workspace)) (lsp-f-canonical nested)))))))

(ert-deftest dbt-lsp-runs-a-server-for-each-project-of-a-repository ()
  "Two dbt projects of a repository get a server each, though lsp-mode guesses roots."
  (dbt-lsp-test-with-server root nil
    (let ((default-directory root))
      (should (eq 0 (call-process "git" nil nil nil "init" "-q"))))
    (copy-directory dbt-test-fixture (expand-file-name "other/" root) nil nil t)
    (let ((first (dbt-lsp-workspace))
          (lsp-auto-guess-root t))
      (with-current-buffer (dbt-test-visit root "other/models/marts/orders.sql")
        (dbt-lsp)
        (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))))
        (should-not (eq (dbt-lsp-workspace) first))
        (should (equal (cadr (member "--project-dir" (process-command (lsp--workspace-cmd-proc (dbt-lsp-workspace)))))
                       (expand-file-name "other" root)))))))

(defmacro dbt-lsp-test-with-dbt (dbt script &rest body)
  "Run BODY with DBT bound to a fresh executable that runs the shell SCRIPT.
SCRIPT runs with the variable LOG naming the file calls in its directory."
  (declare (indent 2) (debug (symbolp form body)))
  `(let* ((,dbt (expand-file-name "dbt" (make-temp-file "dbt-lsp-probe" t))))
     (unwind-protect
         (progn
           (with-temp-file ,dbt
             (insert "#!/bin/sh\nLOG=" (shell-quote-argument (expand-file-name "calls" (file-name-directory ,dbt)))
                     "\n" ,script "\n"))
           (set-file-modes ,dbt #o755)
           ,@body)
       (delete-directory (file-name-directory ,dbt) t))))

(ert-deftest dbt-lsp-asks-dbt-once-whether-it-runs-a-server ()
  "dbt is asked once whether it has `dbt lsp', even when it has not."
  (dbt-lsp-test-with-dbt dbt "echo \"$*\" >> \"$LOG\"; exit 2"
    (let ((dbt-executable dbt))
      (should-not (dbt-lsp-server-p))
      (should-not (dbt-lsp-server-p)))
    (should (equal (dbt-test-file-text (file-name-directory dbt) "calls") "lsp --help\n"))))

(ert-deftest dbt-lsp-gives-up-on-a-dbt-that-does-not-answer ()
  "A dbt that does not say in time whether it has `dbt lsp' runs no server."
  (dbt-lsp-test-with-dbt dbt "exec sleep 20"
    (let ((dbt-executable dbt)
          (dbt-lsp--probe-seconds 1)
          (start (float-time)))
      (should-not (dbt-lsp-server-p))
      (should (< (- (float-time) start) 5)))))

(ert-deftest dbt-lsp-adds-a-folder-only-for-a-server ()
  "The root of the project becomes a folder of the lsp session only if dbt runs a server.
The session persists its folders, so it would keep one it cannot use."
  (dbt-test-with-project root
    (let ((lsp--session nil)
          (lsp-session-file (make-temp-name (expand-file-name "lsp-session" temporary-file-directory))))
      (dbt-test-visit root "models/marts/orders.sql")
      (let ((dbt-executable (expand-file-name "no-dbt" root)))
        (dbt-lsp-add-project-folder))
      (should-not (lsp-session-folders (lsp-session)))
      (let ((dbt-executable dbt-lsp-test-dbt))
        (dbt-lsp-add-project-folder))
      (should (equal (lsp-session-folders (lsp-session)) (list (lsp-f-canonical root)))))))

(ert-deftest dbt-lsp-hides-formatting-from-lsp-mode ()
  "lsp-mode does not format by itself, as the server formats the file on disk.
Its one code action, which fixes and formats, is hidden too."
  (dbt-lsp-test-with-server root nil
    (should (lsp-feature? "textDocument/hover"))
    (should-not (lsp-feature? "textDocument/formatting"))
    (should-not (lsp-feature? "textDocument/rangeFormatting"))
    (should-not (lsp-feature? "textDocument/codeAction"))))

(ert-deftest dbt-lsp-takes-the-notifications-of-the-server ()
  "The notifications of the server's own raise no warning."
  (let ((start (with-current-buffer (get-buffer-create "*Warnings*") (point-max)))
        (warning-minimum-log-level :warning))
    (dbt-lsp-test-with-server root nil
      ;; The server answers in order, so its notifications came first.
      (lsp-request "textDocument/hover" (lsp--text-document-position-params))
      (should-not (string-match-p "Unknown notification"
                                  (with-current-buffer "*Warnings*" (buffer-substring start (point-max))))))))

(ert-deftest dbt-lsp-says-in-the-mode-line-when-the-server-compiles ()
  "While the server compiles, as it notifies, the mode line says so; once it stops, no more.
A start, background or not, says so; a completion ends it.  The
notifications come before the answer to a later request."
  (dbt-lsp-test-with-server root '(("$notifications" . ["dbt/lspCompileStart"]))
    (dbt-test-wait (lambda () dbt-mode-compiling))
    ;; In batch, `format-mode-line' formats nothing: what it would show,
    ;; kept within the spinner that lsp-mode adds around it.
    (should (string-search "(dbt-mode-compiling \" compiling\")" (format "%S" mode-line-process))))
  (dbt-lsp-test-with-server root '(("$notifications" . ["dbt/lspCompileStart" "dbt/lspCompileComplete"]))
    (lsp-request "textDocument/hover" (lsp--text-document-position-params))
    (should-not dbt-mode-compiling))
  (dbt-lsp-test-with-server root '(("$notifications" . ["dbt/lspCompileComplete" "dbt/lspBackgroundCompileStart"]))
    (lsp-request "textDocument/hover" (lsp--text-document-position-params))
    (should dbt-mode-compiling)
    (lsp-workspace-shutdown (dbt-lsp-workspace))
    (dbt-test-wait (lambda () (not dbt-mode-compiling)))))

(ert-deftest dbt-lsp-turns-off-lsp-mode-s-formatting-on-save ()
  "Set for other languages, lsp-mode's formatting and fixes on save log no error here."
  (let ((lsp-format-buffer-on-save t)
        (lsp-fix-all-on-save t))
    (dbt-lsp-test-with-server root nil
      (should-not lsp-format-buffer-on-save)
      (should-not lsp-fix-all-on-save)
      (insert " ")
      (save-buffer)
      (should-not (with-current-buffer (messages-buffer)
                    (string-search "before-save" (buffer-substring dbt-lsp-test-messages-start (point-max))))))))

(ert-deftest dbt-lsp-leaves-xref-to-dbt-mode-where-it-resolves ()
  "With the server, M-. and M-? on a ref or a snapshot are dbt-mode's; elsewhere, the server's.
Choosing the server reads nothing of the project."
  (dbt-lsp-test-with-server root nil
    (dbt-test-goto "ref('stg_orders')" 6)
    (should (eq (xref-find-backend) 'dbt))
    (dbt-project-forget (dbt-project-root))
    (dbt-test-goto "customers.first_name")
    (should (eq (xref-find-backend) 'xref-lsp))
    (should-not (seq-some (lambda (key) (equal (car key) root)) (hash-table-keys dbt-project--cache)))
    ;; dbt lsp alone: a source is dbt-mode's.
    (goto-char (point-max))
    (insert "\n{{ source('raw', 'raw_orders') }}")
    (dbt-test-goto "raw_orders')")
    (should (eq (xref-find-backend) 'dbt))
    (with-current-buffer (dbt-test-visit root "snapshots/orders_snapshot.sql")
      (dbt-lsp)
      (dbt-test-goto "orders_snapshot %}")
      (should (eq (xref-find-backend) 'dbt)))))

(ert-deftest dbt-lsp-leaves-sources-to-the-proxy-when-it-says-so ()
  "With dbt-lsp-plus listing sources for a request, that request on a source is the server's.
M-? goes to the server when references lists source, M-. when definition
does; the rest stays dbt-mode's.  dbt-lsp-plus runs the server, given
after --, when `dbt-lsp-proxy' is on, and dbt as found on `exec-path'."
  (let* ((bin (make-temp-file "dbt-lsp-plus" t))
         (exec-path (cons bin exec-path))
         (dbt-lsp-proxy t))
    (write-region "#!/bin/sh\nshift\nexec \"$@\"\n" nil (expand-file-name "dbt-lsp-plus" bin))
    (set-file-modes (expand-file-name "dbt-lsp-plus" bin) #o755)
    (unwind-protect
        (progn
          (dbt-lsp-test-with-server root
              `(("$experimental" . (:dbtLspPlus (:version 1 :references ["source"] :rename ["source"])))
                ("textDocument/references"
                 . [(:uri ,(lsp--path-to-uri (expand-file-name "models/marts/customers.sql" root))
                          :range (:start (:line 0 :character 0) :end (:line 0 :character 1)))]))
            (should (equal (seq-take (process-command (lsp--workspace-cmd-proc (dbt-lsp-workspace))) 3)
                           (list "dbt-lsp-plus" "--" dbt-lsp-test-dbt)))
            (goto-char (point-max))
            (insert "\n{{ source('raw', 'raw_orders') }}\n{{ source('raw', 'raw_customers') }}")
            (save-buffer)
            (dbt-test-goto "raw_orders')")
            (should (eq (xref-find-backend) 'dbt))
            (let* (shown
                   (xref-show-xrefs-function (lambda (fetcher _alist) (setq shown (funcall fetcher))))
                   (xref-show-definitions-function xref-show-xrefs-function))
              (save-window-excursion
                (switch-to-buffer (current-buffer))
                (execute-kbd-macro (kbd "M-?"))
                (should (equal (mapcar (lambda (item) (file-relative-name (xref-location-group (xref-item-location item)) root))
                                       shown)
                               '("models/marts/customers.sql")))
                (execute-kbd-macro (kbd "M-."))
                (should (equal (mapcar (lambda (item) (file-relative-name (xref-location-group (xref-item-location item)) root))
                                       shown)
                               '("models/staging/_sources.yml")))
                ;; A source typed at the prompt is not the one at point: dbt-mode's.
                (setq shown nil)
                (xref-find-references "source:raw/raw_customers")
                (should (equal (mapcar (lambda (item) (file-relative-name (xref-location-group (xref-item-location item)) root))
                                       shown)
                               '("models/marts/orders.sql")))
                ;; A ref stays dbt-mode's.
                (dbt-test-goto "ref('stg_orders')" 6)
                (execute-kbd-macro (kbd "M-?"))))
            (should (= 1 (seq-count (lambda (method) (equal method "textDocument/references")) (dbt-lsp-test-methods))))
            (should-not (member "textDocument/definition" (dbt-lsp-test-methods))))
          (dbt-lsp-test-with-server root
              `(("$experimental" . (:dbtLspPlus (:version 1 :definition ["source"])))
                ("textDocument/definition"
                 . [(:uri ,(lsp--path-to-uri (expand-file-name "models/staging/_sources.yml" root))
                          :range (:start (:line 7 :character 8) :end (:line 7 :character 18)))]))
            (goto-char (point-max))
            (insert "\n{{ source('raw', 'raw_orders') }}")
            (dbt-test-goto "raw_orders')")
            (save-window-excursion
              (switch-to-buffer (current-buffer))
              (execute-kbd-macro (kbd "M-.")))
            (should (member "textDocument/definition" (dbt-lsp-test-methods)))
            ;; dbt, as found on exec-path, which dbt-lsp-plus does not search.
            (let ((dbt-executable (file-name-nondirectory dbt-lsp-test-dbt))
                  (exec-path (cons (file-name-directory dbt-lsp-test-dbt) exec-path)))
              (should (equal (seq-take (dbt-lsp--command) 3) (list "dbt-lsp-plus" "--" dbt-lsp-test-dbt))))))
      (delete-directory bin t))))

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

(ert-deftest dbt-lsp-show-shows-each-row-on-one-line ()
  "A nested value shows as JSON, and a newline in a string as \\n."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show"
         :columns ["s" "l" "nl"]
         :data [(:s (:a "1" :b nil) :l ["1" "2"] :nl "line1\nline2")]
         :error nil))
    (dbt-lsp-show)
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: orders*")))
      (should (equal (count-lines (point-min) (point-max)) 1)))
    (should (equal (dbt-lsp-test-rows "orders")
                   '(("{\"a\":\"1\",\"b\":null}" "[\"1\",\"2\"]" "line1\\nline2"))))))

(ert-deftest dbt-lsp-show-says-when-there-is-nothing-to-show ()
  "When the server answers no columns, as for a macro, no table shows."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show" :columns nil :data nil :error nil))
    (dbt-lsp-show)
    (dbt-lsp-test-message "dbt lsp: dbt\\.show has no rows to show for orders")
    (should-not (get-buffer "*dbt rows: orders*"))))

(ert-deftest dbt-lsp-asks-again-for-rows-from-the-model-s-file ()
  "g in the rows asks the server again, once the model's buffer is gone too;
for a CTE, the model is compiled again first."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.show" :columns ["n"] :data [(:n 1)] :error nil)
        ("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))
         :error nil))
    (let ((compiled (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)))
      (make-directory (file-name-directory compiled) t)
      (copy-file (expand-file-name "models/marts/orders.sql" root) compiled))
    ;; Another buffer of the project keeps the server running.
    (with-current-buffer (dbt-test-visit root "models/staging/stg_orders.sql")
      (dbt-lsp))
    (dbt-lsp-show)
    (dbt-lsp-preview-cte "customers")
    (let ((orders (current-buffer)))
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: orders/customers*")))
        (let ((compiles (seq-count (lambda (method) (equal method "workspace/executeCommand dbt.compileFile"))
                                   (dbt-lsp-test-methods))))
          (revert-buffer)
          (dbt-test-wait (lambda () (> (seq-count (lambda (method) (equal method "workspace/executeCommand dbt.compileFile"))
                                                  (dbt-lsp-test-methods))
                                       compiles)))))
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: orders*")))
        (kill-buffer orders)
        (let ((asked (length (dbt-lsp-test-methods))))
          (revert-buffer)
          (dbt-test-wait (lambda () (> (length (dbt-lsp-test-methods)) asked))))))))

(ert-deftest dbt-lsp-compiles-again-on-save-while-the-compiled-sql-shows ()
  "With the compiled SQL beside it, saving the model has the server compile it again."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))
         :error nil))
    (let ((compiled (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))
          (compiles (lambda () (seq-count (lambda (method) (equal method "workspace/executeCommand dbt.compileFile"))
                                          (dbt-lsp-test-methods)))))
      (make-directory (file-name-directory compiled) t)
      (write-region "select 1\n" nil compiled)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (dbt-visit-compiled)
        (dbt-test-wait (lambda () (get-file-buffer compiled)))
        (should dbt-compiled-follow-mode)
        (let ((before (funcall compiles)))
          (insert " ")
          (save-buffer)
          (dbt-test-wait (lambda () (> (funcall compiles) before))))))))

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

(ert-deftest dbt-lsp-compile-file-shows-what-changed-on-disk-without-asking ()
  "Compiling again shows the compiled SQL anew, with no question in between."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))))
    (let ((compiled (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))
          (questions nil))
      (make-directory (file-name-directory compiled) t)
      (with-temp-file compiled (insert "select 1 as a\n"))
      (dbt-lsp-compile-file)
      (dbt-test-wait (lambda () (get-file-buffer compiled)))
      (with-temp-file compiled (insert "select 2 as a\n"))
      (set-file-times compiled (time-add nil 10))
      ;; A question would come from the process filter, in the user's way.
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push prompt questions) t)))
        (dbt-lsp-compile-file)
        (dbt-test-wait (lambda () (with-current-buffer (get-file-buffer compiled)
                                    (equal (buffer-string) "select 2 as a\n")))))
      (should-not questions))))

(ert-deftest dbt-lsp-compiles-and-shows-the-buffer-saved ()
  "The server reads the file on disk, so the buffer is saved before it compiles or shows."
  (dbt-lsp-test-with-server root
      `(("workspace/executeCommand dbt.compileFile"
         :file_uri ,(lsp--path-to-uri (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)))
        ("workspace/executeCommand dbt.show" :columns ["a"] :data [(:a "1")]))
    (make-directory (expand-file-name "target/compiled/shop/models/marts/" root) t)
    (with-temp-file (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)
      (insert "select 1 as a\n"))
    (goto-char (point-max))
    (insert "-- compiled\n")
    (dbt-lsp-compile-file)
    (dbt-test-wait (lambda () (get-file-buffer (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))))
    (insert "-- shown\n")
    (dbt-lsp-show)
    (should (equal (dbt-lsp-test-rows "orders") '(("1"))))
    (should-not (buffer-modified-p))
    (should (string-suffix-p "-- compiled\n-- shown\n" (dbt-test-file-text root "models/marts/orders.sql")))
    (should (member "workspace/executeCommand dbt.show"
                    (member "textDocument/didSave"
                            (member "workspace/executeCommand dbt.compileFile"
                                    (member "textDocument/didSave" (dbt-lsp-test-methods))))))))

(ert-deftest dbt-lsp-previews-a-file-that-is-no-model ()
  "With the server, C-c C-p shows the rows of any file it shows, such as a singular test."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show" :columns ["n"] :data [(:n "1")] :error nil))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "tests/assert_positive_amount.sql"))
      (dbt-lsp)
      (execute-kbd-macro (kbd "C-c C-p"))
      (should (equal (dbt-lsp-test-rows "assert_positive_amount") '(("1")))))))

(ert-deftest dbt-lsp-diffs-the-rows-of-a-model-with-a-state ()
  "With the server, the data diff's query goes to dbt.show, as a preview's does."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.show" :columns ["dbt_diff" "n"] :data [(:dbt_diff "only in target dev" :n "1")]
         :error nil))
    (let ((state (expand-file-name "prod-state/" root)))
      (make-directory state)
      (write-region "{\"nodes\": {\"model.shop.orders\": {\"relation_name\": \"prod.orders\"}}}"
                    nil (expand-file-name "manifest.json" state))
      (dbt-data-diff state))
    (should (equal (dbt-lsp-test-rows "orders/diff") '(("only in target dev" "1"))))
    (should (string-search "dbt_diff_there as (select * from prod.orders)"
                           (plist-get (aref (plist-get (plist-get (car (last (dbt-lsp-test-received))) :params)
                                                       :arguments)
                                            0)
                                      :inline)))))

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
    (should (equal (dbt-lsp-test-rows "orders/customers") '(("1"))))
    (should (string-match-p "select 1 as a"
                            (plist-get (aref (plist-get (plist-get (car (last (dbt-lsp-test-received))) :params)
                                                        :arguments)
                                             0)
                                       :inline)))))

(ert-deftest dbt-lsp-lineage-shows-the-nodes-of-the-server ()
  "C-c C-g shows the nodes that the server lists, files in their packages.
A node of a package that dbt-mode does not know has no file."
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
                             :depends_on (:nodes ["model.shop.stg_orders"]))
                 ;; As those of dbt_internal_packages: dbt-mode does not
                 ;; know where its package is.
                 (:unique_id "model.dbt_adapters.internal" :name "internal" :resource_type "model"
                             :package_name "dbt_adapters" :original_file_path "models/internal.sql"
                             :depends_on (:nodes ["model.shop_utils.util"]))]
         :error nil))
    (dbt-lsp-lineage "stg_orders" "model.shop.stg_orders")
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt lineage: stg_orders*")))
      (should (equal (dbt-test-lineage-rows)
                     '(("upstream" "1" "seed" "raw_orders" "seeds/raw_orders.csv")
                       ("downstream" "1" "model" "util" "dbt_packages/shop_utils/models/util.sql")
                       ("downstream" "2" "model" "internal" "")))))
    (should (equal (aref (plist-get (plist-get (car (last (dbt-lsp-test-received))) :params) :arguments) 0)
                   "+stg_orders+"))))

(ert-deftest dbt-lsp-rename-model-refuses-what-dbt-would-not-take ()
  "Renaming a model refuses a name that is not one, or that another resource has.
dbt names the models, seeds and snapshots of a package apart, wherever
their files are."
  (dbt-lsp-test-with-server root nil
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (should-error (dbt-lsp-rename-model "2fast") :type 'user-error)
      (should-error (dbt-lsp-rename-model "stg-clients") :type 'user-error)
      (should-error (dbt-lsp-rename-model "stg_orders") :type 'user-error)
      (should-error (dbt-lsp-rename-model "orders") :type 'user-error)
      (should-error (dbt-lsp-rename-model "raw_orders") :type 'user-error)
      (should-error (dbt-lsp-rename-model "orders_snapshot") :type 'user-error)
      (should (equal buffer-file-name (expand-file-name "models/staging/stg_customers.sql" root))))
    (should-not (member "workspace/willRenameFiles" (dbt-lsp-test-methods)))))

(ert-deftest dbt-lsp-rename-model-renames-the-file ()
  "Renaming a model renames its file, which lsp-mode tells the server first.
The new name is then taken, before any buffer is saved."
  (dbt-lsp-test-with-server root nil
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (dbt-lsp-rename-model "stg_clients")
      (should (equal buffer-file-name (expand-file-name "models/staging/stg_clients.sql" root)))
      (should-not (file-exists-p (expand-file-name "models/staging/stg_customers.sql" root))))
    (should (member "workspace/willRenameFiles" (dbt-lsp-test-methods)))
    (should-error (dbt-lsp-rename-model "stg_clients") :type 'user-error)
    (should (equal buffer-file-name (expand-file-name "models/marts/orders.sql" root)))))

(ert-deftest dbt-lsp-rename-model-lists-the-uses-the-server-leaves ()
  "The uses that still name a renamed model are listed; a version is not renamed.
The server edits the ref of orders.sql and that of a CTE named as the
model, in their buffers; the uses it leaves are listed wherever they
are, in a visited buffer or a file, the renamed file included."
  (dbt-lsp-test-with-server root
      `(("workspace/willRenameFiles"
         . (:changes ((,(lsp--path-to-uri (expand-file-name "models/marts/orders.sql" root))
                       . [(:range (:start (:line 11 :character 26) :end (:line 11 :character 36))
                                  :newText "stg_sales")])
                      (,(lsp--path-to-uri (expand-file-name "models/marts/recent.sql" root))
                       . [(:range (:start (:line 0 :character 42) :end (:line 0 :character 52))
                                  :newText "stg_sales")])))))
    (write-region "with stg_orders as (select * from {{ ref('stg_orders') }})\nselect * from stg_orders\n" nil
                  (expand-file-name "models/marts/recent.sql" root))
    (write-region "-- formerly {{ ref('stg_orders') }}\n" nil
                  (expand-file-name "models/staging/stg_orders.sql" root) t)
    (dbt-project-forget root)
    (dbt-test-visit root "analyses/order_counts.sql")
    (with-current-buffer (dbt-test-visit root "models/staging/stg_orders.sql")
      (dbt-lsp)
      (let* (shown
             (xref-show-xrefs-function (lambda (fetcher _alist) (setq shown (funcall fetcher)))))
        (dbt-lsp-rename-model "stg_sales")
        (should (equal (sort (mapcar (lambda (item)
                                       (let ((location (xref-item-location item)))
                                         (format "%s:%d" (file-relative-name (xref-location-group location) root)
                                                 (xref-location-line location))))
                                     shown))
                       '("analyses/order_counts.sql:1" "models/staging/_staging.yml:4"
                         "models/staging/stg_sales.sql:18" "snapshots/orders_snapshot.sql:3"
                         "tests/assert_positive_amount.sql:1")))
        (dbt-lsp-test-message "5 uses still name stg_orders, which dbt lsp leaves")))
    (dolist (file '("dim_v1" "dim_v2"))
      (write-region "select 1" nil (expand-file-name (format "models/marts/%s.sql" file) root)))
    (write-region "models:\n  - name: dim\n    versions:\n      - v: 1\n      - v: 2\n" nil
                  (expand-file-name "models/marts/_dim.yml" root))
    (dbt-project-forget root)
    (with-current-buffer (dbt-test-visit root "models/marts/dim_v2.sql")
      (dbt-lsp)
      (should-error (dbt-lsp-rename-model "dim_clients") :type 'user-error)
      (should (file-exists-p (expand-file-name "models/marts/dim_v2.sql" root))))))

(ert-deftest dbt-lsp-formats-on-c-c-c-q-and-after-each-save ()
  "With the server, C-c C-q saves, then formats; on save, the formatted buffer is saved too.
In `dbt-format-on-save-mode', a save has the server format the file,
and the buffer is saved again; a failure is a message, and the save
stands."
  (dbt-lsp-test-with-server root
      '(("textDocument/formatting"
         . [(:range (:start (:line 0 :character 0) :end (:line 0 :character 0)) :newText "-- formatted\n")]))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (insert "-- edited\n")
      (execute-kbd-macro (kbd "C-c C-q"))
      (should (string-prefix-p "-- edited\n" (dbt-test-file-text root "models/marts/orders.sql")))
      (should (string-prefix-p "-- formatted\n-- edited\n" (buffer-string)))
      (dbt-format-on-save-mode 1)
      (execute-kbd-macro (kbd "C-x C-s"))
      (should-not (buffer-modified-p))
      (should (string-prefix-p "-- formatted\n-- formatted\n-- edited\n"
                               (dbt-test-file-text root "models/marts/orders.sql")))
      ;; C-c C-q formats once, its save included.
      (goto-char (point-min))
      (insert "-- again\n")
      (execute-kbd-macro (kbd "C-c C-q"))
      (should (string-prefix-p "-- formatted\n-- again\n" (buffer-string)))))
  (dbt-lsp-test-with-server root '(("textDocument/formatting" . "not edits"))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (dbt-format-on-save-mode 1)
      (goto-char (point-min))
      (insert "-- edited\n")
      (execute-kbd-macro (kbd "C-x C-s"))
      (should-not (buffer-modified-p))
      (should (string-prefix-p "-- edited\n" (dbt-test-file-text root "models/marts/orders.sql")))
      (dbt-lsp-test-message "dbt could not format orders\\.sql"))))

(ert-deftest dbt-lsp-format-buffer-saves-then-formats ()
  "Formatting saves the buffer, then applies the edits of the server."
  (dbt-lsp-test-with-server root
      '(("textDocument/formatting"
         . [(:range (:start (:line 0 :character 0) :end (:line 0 :character 0)) :newText "-- formatted\n")]))
    (goto-char (point-min))
    (insert "-- unsaved\n")
    (dbt-lsp-format-buffer)
    (should (string-prefix-p "-- unsaved\n" (dbt-test-file-text root "models/marts/orders.sql")))
    (should (string-prefix-p "-- formatted\n-- unsaved\n" (buffer-string)))
    (should (member "textDocument/didSave"
                    (member "textDocument/didChange" (dbt-lsp-test-methods))))
    (should (member "textDocument/formatting"
                    (member "textDocument/didSave" (dbt-lsp-test-methods))))))

(ert-deftest dbt-lsp-fix-all-saves-then-fixes ()
  "Fixing the lint issues saves the buffer, then applies the edits of the server."
  (dbt-lsp-test-with-server root
      `(("textDocument/codeAction"
         . [(:title "Fix all auto-fixable lint issues and format document" :kind "source.fixAll"
                    :edit (:changes ((,(lsp--path-to-uri (expand-file-name "models/marts/orders.sql" root))
                                      . [(:range (:start (:line 0 :character 0) :end (:line 0 :character 0))
                                                 :newText "-- fixed\n")]))))]))
    (goto-char (point-min))
    (insert "-- unsaved\n")
    (dbt-lsp-fix-all)
    (should (string-prefix-p "-- unsaved\n" (dbt-test-file-text root "models/marts/orders.sql")))
    (should (string-prefix-p "-- fixed\n-- unsaved\n" (buffer-string)))
    (should (member "textDocument/codeAction"
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
    (dbt-lsp-lineage "orders" "model.shop.orders")
    (dbt-lsp-test-message "dbt lsp: dbt\\.listNodes answered what dbt-mode cannot read")
    (let ((text (buffer-string)))
      (should (string-match-p "\\`dbt lsp: textDocument/formatting answered what dbt-mode cannot read"
                              (error-message-string (should-error (dbt-lsp-format-buffer)))))
      (should (equal (buffer-string) text)))
    (should-not (get-buffer "*dbt rows: orders*"))
    (should-not (get-buffer "*dbt lineage: orders*"))))

(ert-deftest dbt-lsp-lineage-asks-again-without-the-model-buffer ()
  "g in the lineage asks the server again, once the model's buffer is gone too."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes"
         :nodes [(:unique_id "model.shop.orders" :name "orders" :resource_type "model"
                             :package_name "shop" :original_file_path "models/marts/orders.sql"
                             :depends_on (:nodes []))]
         :error nil))
    ;; Another buffer of the project keeps the server running.
    (with-current-buffer (dbt-test-visit root "models/staging/stg_orders.sql")
      (dbt-lsp))
    (dbt-lsp-lineage "orders" "model.shop.orders")
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt lineage: orders*")))
      (kill-buffer (get-file-buffer (expand-file-name "models/marts/orders.sql" root)))
      (let ((asked (length (dbt-lsp-test-methods))))
        (revert-buffer)
        (dbt-test-wait (lambda () (> (length (dbt-lsp-test-methods)) asked)))))))

(ert-deftest dbt-lsp-names-the-node-it-cannot-read ()
  "A node of an unexpected shape among those of the lineage is reported."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes"
         :nodes [(:unique_id "model.shop.orders" :name "orders" :resource_type "model"
                             :package_name "shop" :original_file_path "models/marts/orders.sql"
                             :depends_on (:nodes [3]))]))
    (dbt-lsp-lineage "orders" "model.shop.orders")
    (dbt-lsp-test-message "dbt lsp: dbt\\.listNodes answered what dbt-mode cannot read")
    (should-not (get-buffer "*dbt lineage: orders*"))))

(ert-deftest dbt-lsp-says-when-the-server-answers-nothing-or-fails ()
  "A command that the server answers with null, or with an error, says so."
  (dbt-lsp-test-with-server root
      '(("workspace/executeCommand dbt.listNodes" :error "No nodes found" :nodes []))
    (dbt-lsp-show)
    (dbt-lsp-test-message "dbt lsp: dbt\\.show answered nothing")
    (dbt-lsp-lineage "orders" "model.shop.orders")
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
