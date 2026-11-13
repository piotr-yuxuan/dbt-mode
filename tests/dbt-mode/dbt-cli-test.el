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

(provide 'dbt-cli-test)
;;; dbt-cli-test.el ends here
