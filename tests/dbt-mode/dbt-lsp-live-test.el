;;; dbt-lsp-live-test.el --- dbt-mode with lsp-mode and the real dbt lsp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; These tests run the real server, `dbt lsp' of dbt 2, on a copy of the
;; fixture, through lsp-mode, as a user's commands do.  They need the
;; environment variable DBT_MODE_TEST_DBT to name a dbt 2 executable, and
;; skip otherwise.  What they expect comes from the server's own answers
;; to the same questions, observed on dbt 2.0.9: no dbt account, so no
;; column-level feature.

;;; Code:

(require 'dbt-mode-test-helpers)
;; lsp-mode loads its own modules, such as lsp-lens, by their autoloads.
(load "lsp-mode-autoloads" nil t)
(require 'dbt-lsp)

(defconst dbt-test-live-dbt (getenv "DBT_MODE_TEST_DBT")
  "The dbt 2 executable of the live tests, or nil.")

(defmacro dbt-test-with-server (root &rest body)
  "Run BODY in models/marts/orders.sql of a fresh project ROOT, with dbt lsp.
The server runs with the profile of the fixture and stops afterwards."
  (declare (indent 1) (debug (symbolp body)))
  `(progn
     (unless dbt-test-live-dbt (ert-skip "no dbt 2 executable"))
     (dbt-test-with-project ,root
       (let ((dbt-executable dbt-test-live-dbt)
             (dbt-lsp-arguments (list "--profiles-dir" ,root "--no-manage-state"))
             (lsp-session-file (expand-file-name "lsp-session" temporary-file-directory))
             (lsp-enable-file-watchers nil)
             (lsp-restart 'ignore)
             (lsp-keep-workspace-alive nil)
             (lsp-auto-guess-root nil)
             (lsp-client-packages nil)
             (warning-minimum-log-level :error))
         (unwind-protect
             (progn
               (dbt-test-visit ,root "models/marts/orders.sql")
               (dbt-lsp)
               (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))))
               ,@body)
           (dolist (workspace (lsp--session-workspaces (lsp-session)))
             (lsp-workspace-shutdown workspace)))))))

(defun dbt-test-lsp-definition-files ()
  "Return the files where lsp-mode's xref finds the definition at point."
  (mapcar (lambda (item) (xref-location-group (xref-item-location item)))
          (dbt-test-wait (lambda () (xref-backend-definitions 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp))))))

(ert-deftest dbt-lsp-live-navigates-a-project ()
  "M-. on a ref and on macros, M-?, hover and completion come from the server."
  (dbt-test-with-server root
    (should (equal (cadr (member "--project-dir" (process-command (lsp--workspace-cmd-proc (dbt-lsp-workspace)))))
                   (directory-file-name root)))
    (dbt-test-goto "ref('stg_orders')" 6)
    (should (equal (dbt-test-lsp-definition-files) (list (expand-file-name "models/staging/stg_orders.sql" root))))
    (dbt-test-goto "is_positive" 2)
    (should (equal (dbt-test-lsp-definition-files)
                   (list (expand-file-name "dbt_packages/shop_utils/macros/is_positive.sql" root))))
    (with-current-buffer (dbt-test-visit root "models/staging/stg_orders.sql")
      (dbt-lsp)
      (dbt-test-goto "cents_to_dollars" 2)
      (should (equal (dbt-test-lsp-definition-files) (list (expand-file-name "macros/cents_to_dollars.sql" root)))))
    (dbt-test-goto "ref('stg_orders')" 6)
    (should (member (expand-file-name "models/marts/orders.sql" root)
                    (mapcar (lambda (item) (xref-location-group (xref-item-location item)))
                            (xref-backend-references 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp)))))
    (should (string-match-p "Parent Models[^z-a]*raw_orders"
                            (lsp-get (lsp-get (lsp-request "textDocument/hover" (lsp--text-document-position-params))
                                              :contents)
                                     :value)))
    (goto-char (point-max))
    (insert "\n{{ ref('stg_")
    (let ((completion (lsp-completion-at-point)))
      (should (member "stg_customers" (all-completions "stg_" (nth 2 completion)))))
    (should-not (lsp-feature? "textDocument/formatting"))
    (should-not (with-current-buffer (get-buffer-create "*Warnings*")
                  (string-match-p "Unknown notification" (buffer-string))))))

(defun dbt-test-build (root)
  "Build the project at ROOT with the dbt of the live tests, so that it has rows."
  (let ((default-directory root))
    (should (eq 0 (call-process dbt-test-live-dbt nil nil nil "build" "--profiles-dir" root
                                "--exclude" "customers")))))

(defun dbt-test-rows (name)
  "Return the rows of the buffer of results NAME, once it shows them.
The buffer is killed, so that the next preview makes a new one."
  (let ((buffer (dbt-test-wait (lambda () (get-buffer (format "*dbt rows: %s*" name))))))
    (prog1 (with-current-buffer buffer
             (mapcar (lambda (entry) (append (cadr entry) nil)) tabulated-list-entries))
      (kill-buffer buffer))))

(ert-deftest dbt-lsp-live-compiles-and-previews ()
  "C-c C-v shows the compiled SQL; C-c C-p the rows of a model, a region, a CTE."
  (dbt-test-with-server root
    (dbt-test-build root)
    (dbt-visit-compiled)
    (with-current-buffer (dbt-test-wait (lambda () (get-file-buffer (expand-file-name "target/compiled/shop/models/marts/orders.sql" root))))
      (should buffer-read-only)
      (should (string-match-p "from \"[^\"]+\"\\.\"main\"\\.\"stg_orders\"" (buffer-string)))
      (should-not (string-match-p "{{" (buffer-string))))
    (dbt-preview)
    (should (equal (length (dbt-test-rows "orders")) 3))
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (dbt-preview (point-min) (point-max))
      (should (equal (sort (mapcar #'cadr (dbt-test-rows "query")) #'string<) '("Ada" "Alan"))))
    ;; The code lens of a CTE runs what the server asks the client to.
    (let ((lens (seq-find (lambda (lens) (equal (lsp-get (lsp-get lens :command) :command) "dbt.previewCte"))
                          (lsp-request "textDocument/codeLens" (list :textDocument (lsp--text-document-identifier))))))
      (should lens)
      (lsp--execute-command (lsp-get lens :command))
      (let ((name (lsp-get (nth 1 (append (lsp-get (lsp-get lens :command) :arguments) nil)) :name)))
        (should (dbt-test-rows name))))
    (dbt-test-goto "customers.first_name")
    (dbt-preview nil nil t)
    (should (equal (length (dbt-test-rows "final")) 3))))

(ert-deftest dbt-lsp-live-shows-lineage ()
  "C-c C-g shows the upstream and downstream that the server computes."
  (dbt-test-with-server root
    (dbt-lineage "stg_orders")
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt lineage: stg_orders*")))
      (let ((rows (mapcar (lambda (entry) (append (cadr entry) nil)) tabulated-list-entries)))
        (should (member '("upstream" "1" "seed" "raw_orders" "seeds/raw_orders.csv") rows))
        (should (member '("downstream" "1" "model" "orders" "models/marts/orders.sql") rows))
        (should (member '("downstream" "2" "model" "customers" "models/marts/customers.sql") rows))))))

(ert-deftest dbt-lsp-live-renames-a-model ()
  "Renaming a model renames its file and every ref and YAML entry of it."
  (dbt-test-with-server root
    (with-current-buffer (dbt-test-visit root "models/staging/stg_customers.sql")
      (dbt-lsp)
      (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))))
      (dbt-lsp-rename-model "stg_clients")
      (should (equal buffer-file-name (expand-file-name "models/staging/stg_clients.sql" root)))
      (should-not (file-exists-p (expand-file-name "models/staging/stg_customers.sql" root))))
    (save-some-buffers t)
    (dolist (file '("models/marts/orders.sql" "models/marts/customers.sql"))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file root))
        (should (string-match-p "ref('stg_clients')" (buffer-string)))
        (should-not (string-match-p "stg_customers" (buffer-string)))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "models/staging/_staging.yml" root))
      (should (string-match-p "- name: stg_clients" (buffer-string))))))

(ert-deftest dbt-lsp-live-formats-and-reports-errors ()
  "Formatting saves first, and an unknown ref is a diagnostic of its line."
  (dbt-test-with-server root
    (goto-char (point-max))
    (insert "\n-- unsaved\n")
    (dbt-lsp-format-buffer)
    (should-not (buffer-modified-p))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "models/marts/orders.sql" root))
      (should (string-match-p "-- unsaved" (buffer-string))))
    (dbt-test-goto "ref('stg_orders')")
    (replace-match "ref('nope')")
    (save-buffer)
    (let ((file (lsp--fix-path-casing buffer-file-name)))
      (dbt-test-wait (lambda ()
                       (seq-find (lambda (diagnostic) (string-match-p "'nope' not found" (lsp-get diagnostic :message)))
                                 (gethash file (lsp-diagnostics t))))
                     60))))

(provide 'dbt-lsp-live-test)
;;; dbt-lsp-live-test.el ends here
