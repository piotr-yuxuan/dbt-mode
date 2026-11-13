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
      (with-current-buffer (dbt-cli-show nil "orders")
        (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
        (should (equal default-directory root))
        (should (string-match-p "^arguments: show --select orders --limit 7$" (buffer-string)))
        (goto-char (point-min))
        (compilation-next-error 1)
        (let ((location (compilation--message->loc (get-text-property (point) 'compilation-message))))
          (should (equal (caar (compilation--loc->file-struct location)) "models/marts/orders.sql"))
          (should (equal (compilation--loc->line location) 12))
          (should (equal (compilation--loc->col location) 19)))))))

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

(ert-deftest dbt-cli-links-the-errors-of-dbt-in-a-colour-terminal ()
  "With TERM naming a colour terminal, the output of dbt is plain and links.
Emacs started from a terminal passes its TERM on to dbt, which then
colours its output and draws progress bars: none of them may reach the
buffer, and the error must still link to its file."
  (unless (getenv "DBT_MODE_TEST_DBT") (ert-skip "no dbt 2 executable"))
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/customers.sql")
    (let ((dbt-executable (getenv "DBT_MODE_TEST_DBT"))
          (process-environment (cons "TERM=xterm-256color" process-environment)))
      ;; dbt show writes the header of its table in bold even with no colours.
      (with-current-buffer (dbt-cli-show "select 1 as the_answer" nil)
        (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))) 60)
        (should (string-match-p "^│ the_answer │$" (buffer-string)))
        (should-not (string-search "\e" (buffer-string))))
      (dbt-test-goto "ref('orders')")
      (replace-match "ref('nope')")
      (save-buffer)
      (with-current-buffer (dbt-cli-run "parse")
        (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))) 60)
        (should (string-search "Ref 'nope' not found" (buffer-string)))
        (should-not (string-search "\e" (buffer-string)))
        (should (member '("models/marts/customers.sql" 4 6 2) (dbt-test-compilation-messages)))))))

(provide 'dbt-cli-test)
;;; dbt-cli-test.el ends here
