;;; dbt-babel-test.el --- Tests of the Org Babel blocks of dbt  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What C-c C-c on a dbt block of an Org file gives: rows, compiled SQL,
;; the target it runs on and the question a protected target asks.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-babel)
(require 'org)

(defun dbt-test-babel (root header body)
  "Run a dbt block of HEADER and BODY in an Org file of ROOT; return its result.
dbt is the stand-in, which succeeds."
  (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
        (process-environment (cons "FAKE_DBT_SUCCEED=1" process-environment))
        (org-confirm-babel-evaluate nil))
    (with-temp-buffer
      (setq default-directory root)
      (org-mode)
      (insert (format "#+begin_src dbt %s\n%s\n#+end_src\n" header body))
      (goto-char (point-min))
      (org-babel-execute-src-block nil nil '((:results . "value"))))))

(ert-deftest dbt-babel-gives-the-rows-of-a-block ()
  "A block gives dbt show's rows as a table with its header, NULL as such.
The limit, the target and the variables of the block reach dbt."
  (dbt-test-with-project root
    (let ((table (dbt-test-babel root ":limit 3 :target dev :var x=1" "select {{ var('x') }} as x")))
      (should (equal (nth 0 table) '("arguments" "nothing")))
      (should (eq (nth 1 table) 'hline))
      (should (equal (cadr (nth 2 table)) "NULL"))
      (should (equal (car (nth 2 table))
                     "show --quiet --output json --limit 3 --inline select {{ var('x') }} as x --target dev --vars {\"x\":1}")))))

(ert-deftest dbt-babel-gives-the-compiled-sql-of-a-block ()
  "With :compile yes, a block gives its compiled SQL."
  (dbt-test-with-project root
    (should (equal (dbt-test-babel root ":compile yes" "select 1")
                   "compile --log-format json --inline select 1"))))

(ert-deftest dbt-babel-asks-before-a-protected-target ()
  "On a protected target, a block runs only once the user says yes.
The default target is the profile's, unless DBT_TARGET says otherwise."
  (dbt-test-with-project root
    (should (equal (dbt-project-default-target root) "dev"))
    (let ((process-environment (cons "DBT_TARGET=prod" process-environment))
          (asked nil))
      (should (equal (dbt-project-default-target root) "prod"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil)))
        (should-error (dbt-test-babel root "" "select 1") :type 'user-error))
      (should (string-match-p "protected target prod" asked)))
    (let ((dbt-protected-targets '("dev")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
        (should (dbt-test-babel root "" "select 1"))))))

(ert-deftest dbt-babel-shows-what-dbt-says-when-it-fails ()
  "A block that dbt refuses shows dbt's errors in Babel's error buffer."
  (dbt-test-with-project root
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
          (org-confirm-babel-evaluate nil))
      (with-temp-buffer
        (setq default-directory root)
        (org-mode)
        (insert "#+begin_src dbt\nselect nope\n#+end_src\n")
        (goto-char (point-min))
        (should-error (org-babel-execute-src-block) :type 'user-error))
      (with-current-buffer org-babel-error-buffer-name
        (should (string-search "Babel evaluation exited with code 1" (buffer-string)))
        (kill-buffer)))))

(ert-deftest dbt-babel-runs-blocks-with-dbt-2 ()
  "With the real dbt, a block gives its rows, and with :compile yes its SQL."
  (unless (getenv "DBT_MODE_TEST_DBT") (ert-skip "no dbt 2 executable"))
  (dbt-test-with-project root
    (let ((dbt-executable (getenv "DBT_MODE_TEST_DBT"))
          (org-confirm-babel-evaluate nil))
      (with-temp-buffer
        (setq default-directory root)
        (org-mode)
        (insert "#+begin_src dbt :var n=42\nselect {{ var('n') }} as the_answer, null as nothing\n#+end_src\n"
                "#+begin_src dbt :compile yes\nselect * from {{ ref('orders') }}\n#+end_src\n")
        (goto-char (point-min))
        (should (equal (org-babel-execute-src-block nil nil '((:results . "value")))
                       '(("the_answer" "nothing") hline ("42" "NULL"))))
        (org-babel-next-src-block)
        (should (string-match-p "from \"[^\"]+\"\\.\"main\"\\.\"orders\""
                                (org-babel-execute-src-block nil nil '((:results . "value")))))))))

(provide 'dbt-babel-test)
;;; dbt-babel-test.el ends here
