;;; dbt-lsp-alone-live-test.el --- dbt-mode with dbt-lsp-plus alone, on a real dbt-oss  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; These tests run the real dbt-lsp-plus alone, as it runs for a dbt 2
;; that lacks dbt lsp, such as dbt-oss, on a copy of the fixture, through
;; lsp-mode, as a user's commands do.  They need DBT_MODE_TEST_DBT to name
;; such a dbt, and DBT_MODE_TEST_PROXY the executable of dbt-lsp-plus,
;; and skip otherwise.  The proxy answers from the files and from its own
;; `dbt parse', run offline; dbt-mode asks dbt itself for the rest.

;;; Code:

(require 'dbt-mode-test-helpers)
;; lsp-mode loads its own modules, such as lsp-lens, by their autoloads.
(load "lsp-mode-autoloads" nil t)
(require 'dbt-lsp)
(require 'dbt-node)
(require 'dbt-cli)

(defconst dbt-test-alone-dbt (getenv "DBT_MODE_TEST_DBT")
  "The dbt 2 executable of the live tests, or nil.")

(defconst dbt-test-alone-proxy (let ((proxy (getenv "DBT_MODE_TEST_PROXY")))
                                 (and proxy (not (string-empty-p proxy)) (expand-file-name proxy)))
  "The executable of dbt-lsp-plus, or nil; an empty value is none.")

(defmacro dbt-test-with-proxy-alone (root &rest body)
  "Run BODY in models/marts/orders.sql of a fresh project ROOT, with dbt-lsp-plus alone.
dbt-mode runs dbt-lsp-plus by that name, from `exec-path': a directory
of its own holds a link to `dbt-test-alone-proxy'.  The server stops
afterwards, and the buffers of rows, lineage and nodes are killed."
  (declare (indent 1) (debug (symbolp body)))
  `(progn
     (unless dbt-test-alone-dbt (ert-skip "no dbt 2 executable"))
     (unless dbt-test-alone-proxy (ert-skip "no dbt-lsp-plus"))
     ;; The reasons that the Makefile allows: dbt Core or a dbt that does
     ;; not answer lacks no dbt lsp as dbt-oss does.
     (pcase (let ((dbt-executable dbt-test-alone-dbt)) (dbt-project-command-answer "lsp"))
       ('lacks nil)
       ('yes (ert-skip "this dbt runs dbt lsp"))
       (_ (ert-skip "no dbt lsp in this dbt")))
     (let ((bin (make-temp-file "dbt-lsp-plus" t)))
       (make-symbolic-link dbt-test-alone-proxy (expand-file-name "dbt-lsp-plus" bin))
       (unwind-protect
           (dbt-test-with-project ,root
             (let ((dbt-executable dbt-test-alone-dbt)
                   (dbt-lsp-proxy t)
                   (exec-path (cons bin exec-path))
                   (dbt-lsp-arguments (list "--profiles-dir" ,root))
                   (process-environment (cons (concat "DBT_PROFILES_DIR=" ,root) process-environment))
                   (lsp-session-file (expand-file-name "lsp-session" temporary-file-directory))
                   (lsp-enable-file-watchers nil)
                   (lsp-restart 'ignore)
                   (lsp-keep-workspace-alive nil)
                   (lsp-auto-guess-root nil)
                   (lsp-client-packages nil))
               (unwind-protect
                   (save-window-excursion
                     (switch-to-buffer (dbt-test-visit ,root "models/marts/orders.sql"))
                     (dbt-lsp)
                     (dbt-test-wait (lambda () (eq 'initialized (lsp--workspace-status (dbt-lsp-workspace)))) 120)
                     ,@body)
                 (dolist (workspace (lsp--session-workspaces (lsp-session)))
                   (lsp-workspace-shutdown workspace))
                 (dolist (buffer (buffer-list))
                   (when (string-match-p "\\`\\*dbt \\(?:rows\\|lineage\\|node\\): " (buffer-name buffer))
                     (kill-buffer buffer))))))
         (delete-directory bin t)))))

(defun dbt-test-alone-parsed ()
  "Wait until dbt-lsp-plus has parsed the project, as nodeInfo answers then."
  (dbt-test-wait (lambda ()
                   (hash-table-p (ignore-errors
                                   (lsp-request "workspace/executeCommand"
                                                (list :command "dbt-lsp-plus.nodeInfo"
                                                      :arguments ["model.shop.stg_orders"])))))
                 240))

(ert-deftest dbt-lsp-alone-live-runs-the-proxy-alone ()
  "With dbt-oss and dbt-lsp-plus, dbt-mode starts the proxy, which runs alone and answers from its parse.
C-c C-o shows its node; C-c C-g asks its call hierarchy, a level at a
time; C-c C-q says that dbt-oss cannot format."
  (dbt-test-with-proxy-alone root
    (should (equal (lsp-get (lsp-get (lsp--workspace-server-capabilities (dbt-lsp-workspace)) :executeCommandProvider)
                            :commands)
                   ["dbt-lsp-plus.nodeInfo"]))
    (dbt-test-alone-parsed)
    (dbt-test-goto "ref('stg_orders')" 6)
    (execute-kbd-macro (kbd "C-c C-o"))
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt node: model.shop.stg_orders*")))
      (should (string-search "from dbt-lsp-plus" (buffer-string))))
    ;; C-c C-o selects the node's buffer.
    (switch-to-buffer (get-file-buffer (expand-file-name "models/marts/orders.sql" root)))
    (execute-kbd-macro (kbd "C-c C-g RET"))
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt lineage: orders*")))
      (dbt-test-wait (lambda () (assoc "upstream" (dbt-test-lineage-rows))))
      (should (equal header-line-format " Lineage of orders, to depth 2"))
      (should (member '("upstream" "1" "model" "stg_orders" "models/staging/stg_orders.sql") (dbt-test-lineage-rows))))
    (switch-to-buffer (get-file-buffer (expand-file-name "models/marts/orders.sql" root)))
    (should (string-prefix-p "This dbt cannot format"
                             (cadr (should-error (execute-kbd-macro (kbd "C-c C-q")) :type 'user-error))))))

(ert-deftest dbt-lsp-alone-live-shows-a-failed-test-of-a-build ()
  "With dbt-lsp-plus alone, a build that fails a test shows as a diagnostic at the test's YAML entry.
As plan 009's second criterion asks: the proxy reads the run's results
alone too.  Then C-c C-p on a model that the build made asks dbt show."
  (dbt-test-with-proxy-alone root
    (dbt-test-alone-parsed)
    ;; order_id 1 twice: the test unique of stg_orders fails.
    (write-region "1,3,2018-01-05,placed,500\n" nil (expand-file-name "seeds/raw_orders.csv" root) 'append)
    (dbt-cli-run "build")
    (dbt-test-wait (lambda () (let ((buffer (get-buffer "*dbt shop: build*")))
                                (and buffer (not (get-buffer-process buffer)))))
                   300)
    (should (equal (dbt-test-wait (lambda ()
                                    (let (found)
                                      (maphash (lambda (file diagnostics)
                                                 (when (string-suffix-p "/_staging.yml" file)
                                                   (dolist (diagnostic diagnostics)
                                                     (push (lsp-get (lsp-get (lsp-get diagnostic :range) :start) :line)
                                                           found))))
                                               (lsp-diagnostics t))
                                      found))
                                  60)
                   ;; The line of `- unique', from 0.
                   '(8)))
    (switch-to-buffer (dbt-test-visit root "models/staging/stg_orders.sql"))
    (execute-kbd-macro (kbd "C-c C-p"))
    (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: stg_orders*")) 120)
      (should (dbt-test-wait (lambda () tabulated-list-entries) 120)))))

(provide 'dbt-lsp-alone-live-test)
;;; dbt-lsp-alone-live-test.el ends here
