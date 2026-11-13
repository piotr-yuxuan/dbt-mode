;;; dbt-rows-test.el --- Tests of the grid of rows  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user sees and does in the grid of a query's rows, whoever ran
;; the query: the cells, the order, the filter, the copies, more rows.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-rows)

(defmacro dbt-rows-test-with-grid (rows &rest body)
  "Run BODY in a grid of ROWS, columns name, n and note, asked with limit 5.
The grid is in the selected window, where keys go.  `reruns' holds the
limits that the grid asked again with, latest first."
  (declare (indent 1) (debug (form body)))
  `(let ((reruns nil))
     (save-window-excursion
       (switch-to-buffer (dbt-rows-show "test" '("name" "n" "note") ,rows 5
                                        (lambda (limit) (push limit reruns))))
       (unwind-protect (progn ,@body)
         (kill-buffer)))))

(defun dbt-rows-test-shown ()
  "Return the cells of the rows that the grid shows, in their order."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (push (append (tabulated-list-get-entry) nil) rows)
        (forward-line 1))
      (nreverse rows))))

(defconst dbt-rows-test-rows
  '(("Ada" "10" "line 1\nline 2") ("Alan" "9" nil) ("Grace" nil "a|b") ("Edsger" "-1.5" "x,y \"q\""))
  "Rows with numbers, NULL, a newline, a bar, a comma and quotes.")

(ert-deftest dbt-rows-shows-each-row-on-one-line ()
  "Each row is one line; NULL shows as NULL in its own face, a newline as \\n."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (should (equal (buffer-name) "*dbt rows: test*"))
    (should (equal (count-lines (point-min) (point-max)) 4))
    (should (equal (car (dbt-rows-test-shown)) '("Ada" "10" "line 1\\nline 2")))
    (goto-char (point-min))
    (search-forward "NULL")
    (should (eq (get-text-property (match-beginning 0) 'face) 'dbt-rows-null))))

(ert-deftest dbt-rows-sorts-numbers-as-numbers ()
  "S on a column of numbers sorts them as numbers, NULL last, first in reverse."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (goto-char (point-min))
    (search-forward "10")
    (execute-kbd-macro (kbd "S"))
    (should (equal (mapcar #'cadr (dbt-rows-test-shown)) '("-1.5" "9" "10" "NULL")))
    (execute-kbd-macro (kbd "S"))
    (should (equal (mapcar #'cadr (dbt-rows-test-shown)) '("NULL" "10" "9" "-1.5")))
    (goto-char (point-min))
    (search-forward "Ada")
    (execute-kbd-macro (kbd "S"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Ada" "Alan" "Edsger" "Grace")))))

(ert-deftest dbt-rows-filters-by-a-regexp ()
  "/ keeps the rows with a cell that matches; C-u / looks in one column."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (execute-kbd-macro (kbd "/ \\`A RET"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Ada" "Alan")))
    (should (equal mode-line-process " 2 of 4 rows"))
    (execute-kbd-macro (kbd "/ NULL RET"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Alan" "Grace")))
    (execute-kbd-macro (kbd "C-u / note RET NULL RET"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Alan")))
    (execute-kbd-macro (kbd "/ RET"))
    (should (equal (length (dbt-rows-test-shown)) 4))
    (should (equal mode-line-process " 4 of 4 rows"))))

(ert-deftest dbt-rows-copies-csv-markdown-and-org ()
  "w copies the rows shown, in their order, as CSV, Markdown or Org."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (execute-kbd-macro (kbd "w CSV RET"))
    (should (equal (current-kill 0)
                   (concat "name,n,note\n" "Ada,10,\"line 1\nline 2\"\n" "Alan,9,\n" "Grace,,a|b\n"
                           "Edsger,-1.5,\"x,y \"\"q\"\"\"\n")))
    (should-not (text-properties-at 0 (current-kill 0)))
    (execute-kbd-macro (kbd "/ \\`[AG] RET w Markdown RET"))
    (should (equal (current-kill 0)
                   (concat "| name | n | note |\n" "|---|---|---|\n" "| Ada | 10 | line 1\\nline 2 |\n"
                           "| Alan | 9 | NULL |\n" "| Grace | NULL | a\\|b |\n")))
    (goto-char (point-min))
    (search-forward "10")
    (execute-kbd-macro (kbd "S w Org RET"))
    (should (equal (current-kill 0)
                   (concat "| name | n | note |\n" "|---+---+---|\n" "| Alan | 9 | NULL |\n"
                           "| Ada | 10 | line 1\\nline 2 |\n" "| Grace | NULL | a\\vert{}b |\n")))))

(ert-deftest dbt-rows-asks-again-for-more-rows ()
  "+ asks with twice the limit, or a numeric prefix's; g with the same.
The mode line says when the rows reached the limit."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (execute-kbd-macro (kbd "+"))
    (execute-kbd-macro (kbd "C-u 1 2 +"))
    (execute-kbd-macro (kbd "g"))
    (should (equal reruns '(5 12 10))))
  (dbt-rows-test-with-grid (make-list 5 '("a" "1" "b"))
    (should (equal mode-line-process " 5 of 5 rows, limit reached"))))

(ert-deftest dbt-rows-keeps-its-filter-and-order-when-shown-again ()
  "Rows shown again keep the filter and the order, unless their column is gone."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (goto-char (point-min))
    (search-forward "10")
    (execute-kbd-macro (kbd "S / \\`A RET"))
    (dbt-rows-show "test" '("name" "n" "note") (cons '("Ann" "1" nil) dbt-rows-test-rows) 5 #'ignore)
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Ann" "Alan" "Ada")))
    (dbt-rows-show "test" '("name") '(("Bob") ("Al")) 5 #'ignore)
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Al")))))

(ert-deftest dbt-rows-shows-the-whole-value-of-a-cell ()
  "RET on a cell shows its whole value, newlines included, and NULL as such."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (goto-char (point-min))
    (search-forward "line 1")
    (goto-char (match-beginning 0))
    (let ((column (current-column))
          (messages (with-current-buffer (messages-buffer) (point-max-marker))))
      (execute-kbd-macro (kbd "RET"))
      (forward-line 1)
      (move-to-column column)
      (execute-kbd-macro (kbd "RET"))
      (should (equal (with-current-buffer (messages-buffer) (buffer-substring messages (point-max)))
                     "note: line 1\nline 2\nnote: NULL\n")))))

(provide 'dbt-rows-test)
;;; dbt-rows-test.el ends here
