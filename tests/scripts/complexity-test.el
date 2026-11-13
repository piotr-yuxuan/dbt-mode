;;; complexity-test.el --- Tests of the complexity check  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What scripts/complexity.el counts.  The expectations come from its
;; commentary: 1 plus the decision points, each binding of a *-let form
;; being one.

;;; Code:

(require 'dbt-mode-test-helpers)
(load (expand-file-name "../scripts/complexity.el" dbt-test-directory) nil t)

(defun dbt-test-complexity (definition)
  "Return the complexity of DEFINITION, a form such as a `defun'."
  (1+ (repo-complexity--points (cdr (car (repo-complexity--definitions definition))))))

(ert-deftest complexity-counts-each-binding-of-a-let-form ()
  "Each binding of when-let*, if-let*, and-let* and while-let may stop the form."
  (should (= (dbt-test-complexity '(defun f (x) (when-let* ((a x) (b a) ((> b 1))) b))) 4))
  (should (= (dbt-test-complexity '(defun f (x) (if-let* ((a x)) a 0))) 2))
  (should (= (dbt-test-complexity '(defun f (x) (and-let* ((a x) (b a))))) 3))
  (should (= (dbt-test-complexity '(defun f (x) (while-let ((a (pop x)) (b (car a))) (ignore b)))) 3))
  (should (= (dbt-test-complexity '(defun f (x) (when-let ((a x) (b a)) b))) 3)))

(ert-deftest complexity-counts-the-old-single-binding-once ()
  "The old form (if-let (SYMBOL VALUE) ...) is one binding."
  (should (= (dbt-test-complexity '(defun f (x) (if-let (a x) a 0))) 2)))

(provide 'complexity-test)
;;; complexity-test.el ends here
