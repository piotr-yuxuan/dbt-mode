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

(ert-deftest dbt-rows-reads-the-json-of-dbt-show ()
  "dbt show's JSON gives its columns in order, and each value's raw text.
NULL is nil; a value that is not a string is its JSON."
  (should (equal (dbt-rows-read-json
                  (concat "[{\"z\":null,\"t\":true,\"f\":false,\"n\":1.5,\"s\":\"a\\nb\","
                          "\"o\":{\"k\":null},\"l\":[1,\"x\"]},{\"z\":1}]"))
                 '(("z" "t" "f" "n" "s" "o" "l")
                   (nil "true" "false" "1.5" "a\nb" "{\"k\":null}" "[1,\"x\"]")
                   ("1" nil nil nil nil nil nil))))
  (should (equal (dbt-rows-read-json "[]") '(nil)))
  (dolist (json '("" "not json" "{\"a\":1}" "[1,2]" "[{\"a\":1}] trailing"))
    (should-not (dbt-rows-read-json json))))

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
  "RET on a cell shows its whole value, and NULL as such.
A value of several lines, or wider than the frame, shows in a buffer of
its own, which the echo area would cut."
  (dbt-rows-test-with-grid (cons (list "Long" "1" (make-string (* 2 (frame-width)) ?x)) dbt-rows-test-rows)
    (goto-char (point-min))
    (search-forward "line 1")
    (goto-char (match-beginning 0))
    (let ((column (current-column))
          (messages (with-current-buffer (messages-buffer) (point-max-marker))))
      (execute-kbd-macro (kbd "RET"))
      (should (equal (with-current-buffer "*dbt value*" (buffer-string)) "line 1\nline 2"))
      (forward-line 1)
      (move-to-column column)
      (execute-kbd-macro (kbd "RET"))
      (should (equal (with-current-buffer (messages-buffer) (buffer-substring messages (point-max)))
                     "note: NULL\n"))
      (goto-char (point-min))
      (move-to-column column)
      (execute-kbd-macro (kbd "RET"))
      (should (equal (with-current-buffer "*dbt value*" (buffer-string)) (make-string (* 2 (frame-width)) ?x)))))
  ;; A header line would read the % of a column's name as a construct.
  (save-window-excursion
    (switch-to-buffer (dbt-rows-show "test" '("growth %b") (list (list (make-string 200 ?1))) 5 #'ignore))
    (goto-char (point-min))
    (execute-kbd-macro (kbd "RET"))
    (should (equal (with-current-buffer "*dbt value*" header-line-format) " growth %%b, row 1"))
    (kill-buffer)))

(ert-deftest dbt-rows-reads-what-dbt-show-prints-around-its-rows ()
  "Lines before the rows, a column name twice, and text beyond ASCII read as dbt means them.
dbt prints \"Waiting for lease\" before its JSON when another dbt holds
the project; a join may give two columns of one name, the second named
as dbt lsp names it; a nested value keeps its characters, null included,
whoever read the JSON."
  (should (equal (dbt-rows-read-json "Waiting for lease on 'dbt_packages' directory\n[{\"a\":1}]\n")
                 '(("a") ("1"))))
  (should (equal (dbt-rows-read-json "[{\"id\":3,\"id\":1,\"name\":\"Ada\"},{\"id\":4,\"id\":2,\"name\":\"Alan\"}]")
                 '(("id" "id_1" "name") ("3" "1" "Ada") ("4" "2" "Alan"))))
  ;; As dbt lsp 2.0.9 names select 1 as id, 2 as id, 3 as id_1, 4 as id.
  (should (equal (car (dbt-rows-read-json "[{\"id\":1,\"id\":2,\"id_1\":3,\"id\":4}]"))
                 '("id" "id_1" "id_1_1" "id_2")))
  (should (equal (dbt-rows-read-json "[{\"o\":{\"name\":\"Zoë\",\"n\":null}}]")
                 '(("o") ("{\"name\":\"Zoë\",\"n\":null}"))))
  (let ((value (make-hash-table :test #'equal)))
    (puthash "name" "Zoë" value)
    (puthash "n" nil value)
    (should (equal (dbt-rows-cell value nil) "{\"name\":\"Zoë\",\"n\":null}"))
    (should-not (dbt-rows-cell nil nil))))

(ert-deftest dbt-rows-filters-with-a-regexp-and-a-column-it-has ()
  "An invalid regexp is refused, the filter as it was; a filter whose column a new query lacks goes.
As in `occur', a regexp in lower case ignores case, and an upper-case
letter makes it matter."
  (dbt-rows-test-with-grid dbt-rows-test-rows
    (execute-kbd-macro (kbd "/ ada RET"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Ada")))
    (execute-kbd-macro (kbd "/ ADA RET"))
    (should-not (dbt-rows-test-shown))
    (should-error (execute-kbd-macro (kbd "/ [ RET")) :type 'user-error)
    (should-not (dbt-rows-test-shown))
    (execute-kbd-macro (kbd "C-u / name RET A RET"))
    (should (equal (mapcar #'car (dbt-rows-test-shown)) '("Ada" "Alan")))
    (dbt-rows-show "test" '("n" "note") '(("1" "x") ("2" "y")) 5 #'ignore)
    (should (equal (dbt-rows-test-shown) '(("1" "x") ("2" "y"))))))

(ert-deftest dbt-rows-sizes-and-prints-columns-as-they-show ()
  "A column is as wide as its text shows, wide characters included; widening it prints its rows again."
  (dbt-rows-test-with-grid '(("日本語" "1" "x"))
    (should (equal (nth 1 (aref tabulated-list-format 0)) 6))
    (goto-char (point-min))
    (let ((width (- (line-end-position) (line-beginning-position))))
      (execute-kbd-macro (kbd "}"))
      (should (= (- (line-end-position) (line-beginning-position)) (1+ width))))))

(provide 'dbt-rows-test)
;;; dbt-rows-test.el ends here
