;;; dbt-cli-test.el --- Tests of the commands of dbt-mode that run dbt  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The arguments that dbt-dispatch gives dbt, and the errors of dbt's
;; output, with a stand-in for dbt, tests/bin/dbt.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-cli)
(require 'dbt-mode)

(ert-deftest dbt-cli-arguments-add-upstream-and-downstream ()
  "The switches of dbt-dispatch become a + before or after the selection."
  (should (equal (dbt-cli-arguments '("--select=orders" "--upstream" "--downstream" "--full-refresh"
                                      "--target=dev"))
                 '("--select" "+orders+" "--full-refresh" "--target" "dev")))
  (should (equal (dbt-cli-arguments '("--select=orders")) '("--select" "orders"))))

(ert-deftest dbt-cli-dispatch-gives-each-command-the-options-it-takes ()
  "Each command of dbt-dispatch gets only those of its options that dbt takes.
The options each command takes are those that `dbt COMMAND OPTION
--help' of dbt 2.0.9 accepts; --target and --fail-fast go to every
command, the selection only to the commands that select resources."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (transient-values '((dbt-dispatch "--upstream" "--exclude=customers" "--target=prod"
                                            "--full-refresh" "--fail-fast" "--limit=3")))
          (selecting '("select +orders" "exclude customers" "target prod" "fail-fast"))
          (other '("target prod" "fail-fast")))
      (dolist (case `(("build" ,@selecting "full-refresh" "limit 3")
                      ("run" ,@selecting "full-refresh" "limit 3")
                      ("compile" ,@selecting "full-refresh" "limit 3")
                      ("seed" ,@selecting "full-refresh")
                      ("test" ,@selecting "limit 3")
                      ("show" ,@selecting "limit 3")
                      ("list" ,@selecting "limit 3")
                      ("snapshot" ,@selecting)
                      ("format" ,@selecting)
                      ("lint" ,@selecting)
                      ("parse" ,@other)
                      ("deps" ,@other)
                      ("debug" ,@other)
                      ("retry" ,@other)
                      ("docs generate" ,@other)))
        (with-current-buffer (dbt-cli-dispatch (car case))
          (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
          (goto-char (point-min))
          (should (re-search-forward (format "^arguments: %s\\(.*\\)$" (car case)) nil t))
          (should (equal (cons (car case) (sort (split-string (match-string 1) " --" t)))
                         (cons (car case) (sort (cdr case))))))))))

(ert-deftest dbt-cli-runs-at-the-root-and-links-errors ()
  "dbt runs at the root of the project, and its errors link to their files."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (dbt-preview-limit 7))
      (dbt-cli-show "orders" nil)
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt show: orders*")))
        (should (equal default-directory root))
        (should (string-match-p "^arguments: show --quiet --output json --limit 7 --select orders$"
                                (buffer-string)))
        (goto-char (point-min))
        (compilation-next-error 1)
        (let ((location (compilation--message->loc (get-text-property (point) 'compilation-message))))
          (should (equal (caar (compilation--loc->file-struct location)) "models/marts/orders.sql"))
          (should (equal (compilation--loc->line location) 12))
          (should (equal (compilation--loc->col location) 19)))
        (kill-buffer)))))

(ert-deftest dbt-cli-show-shows-the-rows-in-the-grid ()
  "dbt show's JSON fills the grid, whatever dbt writes on stderr.
In the grid, + runs dbt show again with twice the limit."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (process-environment (cons "FAKE_DBT_SUCCEED=1" process-environment))
          (dbt-preview-limit 7))
      (dbt-cli-show "a_cte" "select 1 as one")
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: a_cte*")))
        (should (equal (append (cadr (car tabulated-list-entries)) nil)
                       '("show --quiet --output json --limit 7 --inline select 1 as one" "NULL")))
        (dbt-rows-more)
        (dbt-test-wait (lambda () (string-search "--limit 14 " (aref (cadr (car tabulated-list-entries)) 0))))
        (kill-buffer)))))

(ert-deftest dbt-cli-asks-again-for-a-cte-as-it-is-now ()
  "g in the rows of a CTE runs its query as the model's file now writes it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (process-environment (cons "FAKE_DBT_SUCCEED=1" process-environment)))
      (dbt-cli-preview-cte "customers")
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: customers*")))
        (should (string-search "ref('stg_customers')" (aref (cadr (car tabulated-list-entries)) 0)))
        (with-current-buffer (get-file-buffer (expand-file-name "models/marts/orders.sql" root))
          (dbt-test-goto "ref('stg_customers')")
          (replace-match "ref('raw_customers')")
          (save-buffer)
          (kill-buffer))
        (revert-buffer)
        (dbt-test-wait (lambda () (string-search "ref('raw_customers')" (aref (cadr (car tabulated-list-entries)) 0))))
        (kill-buffer)))))

(defun dbt-test-compilation-messages ()
  "Return the messages of the current compilation buffer.
Each is a list (FILE LINE COLUMN TYPE), where TYPE is 2 for an error and
1 for a warning."
  (compilation--ensure-parse (point-max))
  (let ((position (point-min))
        messages)
    (while (setq position (next-single-property-change position 'compilation-message))
      (when-let* ((message (get-text-property position 'compilation-message))
                  (location (compilation--message->loc message)))
        (push (list (caar (compilation--loc->file-struct location)) (compilation--loc->line location)
                    (compilation--loc->col location) (compilation--message->type message))
              messages)))
    (nreverse messages)))

(ert-deftest dbt-cli-links-a-warning-without-a-line-to-its-file ()
  "A warning whose location has no line links to its file, not to the next error.
The output is that of dbt 2.0.9 on a project with a properties file
that names no model and a ref to no model."
  (with-temp-buffer
    (insert "=================== Errors and Warnings ====================\n"
            "[warning] [NoNodeForYamlKey (dbt1089)]: Unused schema.yml entry for model 'does_not_exist'\n"
            "  --> models/staging/_extra.yml\n"
            "[error] [DependencyNotFound (dbt1048)]: Ref 'nope' not found in project. "
            "Searched for 'shop.nope, nope'\n"
            "  --> models/marts/customers.sql:4:6\n"
            "\n"
            "==================== Execution Summary =====================\n")
    (dbt-compilation-mode)
    (should (equal (dbt-test-compilation-messages)
                   '(("models/staging/_extra.yml" nil nil 1) ("models/marts/customers.sql" 4 6 2))))))

(ert-deftest dbt-cli-links-an-error-of-the-database-to-the-sql-that-ran ()
  "An error of the database, which has no location, links to the SQL that ran.
The output is that of dbt 2.0.9 on a model that selects a missing column."
  (with-temp-buffer
    (insert "=================== Errors and Warnings ====================\n"
            "[error] [DbDriverFailed (dbt1308)]: Database Error in model badcol "
            "(target/run/shop/models/marts/badcol.sql)\n"
            "  Binder Error: Referenced column \"nope\" not found in FROM clause!\n"
            "\n"
            "==================== Execution Summary =====================\n")
    (dbt-compilation-mode)
    (should (equal (dbt-test-compilation-messages) '(("target/run/shop/models/marts/badcol.sql" nil nil 2))))))

(ert-deftest dbt-cli-links-the-errors-of-dbt-in-a-colour-terminal ()
  "With TERM naming a colour terminal, dbt's JSON reads and its output links.
Emacs started from a terminal passes its TERM on to dbt, which then
colours its output and draws progress bars: none of them may reach the
rows or the buffer, and the error must still link to its file."
  (unless (getenv "DBT_MODE_TEST_DBT") (ert-skip "no dbt 2 executable"))
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/customers.sql")
    (let ((dbt-executable (getenv "DBT_MODE_TEST_DBT"))
          (process-environment (cons "TERM=xterm-256color" process-environment)))
      (dbt-cli-show "query" "select 42 as the_answer, null as nothing")
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt rows: query*")) 60)
        (should (equal (mapcar #'car tabulated-list-format) '("the_answer" "nothing")))
        (should (equal (append (cadr (car tabulated-list-entries)) nil) '("42" "NULL")))
        (kill-buffer))
      (dbt-test-goto "ref('orders')")
      (replace-match "ref('nope')")
      (save-buffer)
      (with-current-buffer (dbt-cli-run "parse")
        (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))) 60)
        (should (string-search "Ref 'nope' not found" (buffer-string)))
        (should-not (string-search "\e" (buffer-string)))
        (should (member '("models/marts/customers.sql" 4 6 2) (dbt-test-compilation-messages)))))))

(ert-deftest dbt-cli-saves-the-project-before-running-dbt ()
  "dbt reads files, so the project's modified buffers are saved first.
A buffer outside the project is left alone."
  (dbt-test-with-project root
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (compilation-ask-about-save nil)
          (outside (make-temp-file "outside" nil ".sql" "select 1")))
      (unwind-protect
          (let ((other (find-file-noselect outside)))
            (with-current-buffer other (insert "-- unsaved\n"))
            (dbt-test-visit root "models/staging/stg_customers.sql")
            (goto-char (point-max))
            (insert "-- unsaved\n")
            (with-current-buffer (dbt-cli-run "build")
              (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer))))))
            (should-not (buffer-modified-p (get-file-buffer (expand-file-name "models/staging/stg_customers.sql" root))))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "models/staging/stg_customers.sql" root))
              (should (string-suffix-p "-- unsaved\n" (buffer-string))))
            (should (buffer-modified-p other))
            (with-current-buffer other (set-buffer-modified-p nil) (kill-buffer)))
        (delete-file outside)))))

(provide 'dbt-cli-test)
;;; dbt-cli-test.el ends here
