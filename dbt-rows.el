;;; dbt-rows.el --- The rows of a query, in a grid  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The grid where the rows of a dbt query show, whoever ran it: each row
;; on one line, NULL in its own face, a column of numbers sorted as
;; numbers.  `/' filters the rows, `w' copies them as CSV, Markdown or
;; Org, `+' asks for more of them and `RET' shows a cell's whole value.

;;; Code:

(require 'seq)
(require 'tabulated-list)

(defface dbt-rows-null '((t :inherit shadow))
  "Face of NULL in the rows of a query."
  :group 'dbt)

(defvar-local dbt-rows--columns nil
  "The names of the columns of this buffer's rows.")

(defvar-local dbt-rows--rows nil
  "The rows of this buffer, a vector of vectors of cells.
A cell is a string, the text of its value, or nil for NULL.")

(defvar-local dbt-rows--entries nil
  "The entries of every row of this buffer, as `tabulated-list-entries'.
Each entry's id is the index of its row in `dbt-rows--rows'.")

(defvar-local dbt-rows--filter nil
  "The filter of the rows shown, (COLUMN . REGEXP), or nil.
A row shows when the text of a cell matches REGEXP, in COLUMN only
unless COLUMN is nil.")

(defvar-local dbt-rows--limit nil
  "The number of rows that this buffer's query asked for.")

(defvar-local dbt-rows--rerun nil
  "The function of a limit that runs this buffer's query again.")

(defvar-local dbt-rows--lines nil
  "The line printed for each row, and the format they were printed for.
A pair (FORMAT . LINES): LINES is a vector indexed by the id of a row,
of its line as printed for FORMAT, the `tabulated-list-format' of then.
Keeping them makes a sort five times as fast, and nearly doubles the
memory of a large grid: 1.2 GB rather than 0.6 GB at 10,000 rows by 50
columns.")

(defun dbt-rows--print-entry (id cells)
  "Insert the line of the row ID, whose CELLS are those of its entry.
For `tabulated-list-printer': the line is printed once for each
`tabulated-list-format', which a new query or a change of width replaces,
then inserted again as it was."
  (unless (eq (car dbt-rows--lines) tabulated-list-format)
    (setq dbt-rows--lines (cons tabulated-list-format (make-vector (length dbt-rows--rows) nil))))
  (if-let* ((line (aref (cdr dbt-rows--lines) id)))
      (insert line)
    (let ((start (point)))
      (tabulated-list-print-entry id cells)
      (aset (cdr dbt-rows--lines) id (buffer-substring start (point))))))

(defun dbt-rows--print ()
  "Show the rows that the filter keeps, and say how many in the mode line."
  (let* ((regexp (cdr dbt-rows--filter))
         (index (and (car dbt-rows--filter) (seq-position dbt-rows--columns (car dbt-rows--filter))))
         ;; As in `occur', an upper-case letter makes the case matter.
         (case-fold-search (if (and regexp case-fold-search search-upper-case)
                               (isearch-no-upper-case-p regexp t)
                             case-fold-search)))
    (setq tabulated-list-entries
          (seq-filter (lambda (entry)
                        (or (not regexp)
                            (seq-some (lambda (text) (string-match-p regexp text))
                                      (if index (list (aref (cadr entry) index)) (cadr entry)))))
                      dbt-rows--entries)
          mode-line-process (format " %d of %d rows%s" (length tabulated-list-entries) (length dbt-rows--entries)
                                    (if (< (length dbt-rows--entries) dbt-rows--limit) "" ", limit reached")))
    (tabulated-list-print t)))

(defun dbt-rows-cell (value null)
  "Return the text of a cell holding the JSON VALUE, or nil for null.
A string is its own text; any other value is its JSON.  NULL is what
JSON null reads as in VALUE: :null for `json-parse-string', nil for
lsp-mode."
  (cond ((eq value null) nil)
        ((stringp value) value)
        ;; json-serialize gives UTF-8 bytes.
        (t (decode-coding-string (json-serialize value :null-object null) 'utf-8))))

(defun dbt-rows-read-json (json)
  "Return the rows of JSON, as `dbt show --output json' prints them.
JSON is an array of objects, one per row, whose keys are the columns, in
the same order in every row, as dbt prints them; a row's missing last
keys are NULL.  The lines before the array, such as dbt's \"Waiting for
lease\", are skipped.  The rows are (COLUMNS . ROWS), as `dbt-rows-show'
takes them, each cell as `dbt-rows-cell' reads it.  A name that repeats,
as the columns of a join may, is renamed as dbt lsp renames it, until no
other column has it: id, id, id_1 and id are id, id_1, id_1_1 and id_2.
An array of no row has no columns either: (nil).  Return nil when JSON
is not such an array."
  ;; json.el, which lsp-mode loads, redefines some errors of
  ;; `json-parse-string' as errors of json-error only.
  (let ((rows (ignore-error json-error
                (json-parse-string (substring json (or (string-match "^\\[" json) 0)) :object-type 'alist))))
    (when (and (vectorp rows) (seq-every-p #'listp rows))
      (let ((columns (and (not (seq-empty-p rows)) (mapcar #'car (aref rows 0))))
            (taken (make-hash-table :test #'equal)))
        (cons (mapcar (lambda (column)
                        (let* ((name (symbol-name column))
                               (renamed name)
                               (count 0))
                          (while (gethash renamed taken)
                            (setq count (1+ count)
                                  renamed (format "%s_%d" name count)))
                          (puthash renamed t taken)
                          renamed))
                      columns)
              (mapcar (lambda (row)
                        (mapcar (lambda (value) (dbt-rows-cell value :null))
                                (seq-take (append (mapcar #'cdr row) (make-list (length columns) :null))
                                          (length columns))))
                      rows))))))

(defun dbt-rows-show (title columns rows limit rerun)
  "Show ROWS of COLUMNS in the buffer *dbt rows: TITLE*, and return it.
COLUMNS lists the names of the columns, and ROWS the rows, each a list
of cells: a string, the text of the value, or nil for NULL.  LIMIT
is the number of rows asked for, and RERUN a function of a limit that
asks for the rows again.  A buffer that shows rows already keeps its
filter and its order."
  (with-current-buffer (get-buffer-create (format "*dbt rows: %s*" title))
    (unless (derived-mode-p 'dbt-rows-mode)
      (dbt-rows-mode))
    (setq dbt-rows--columns columns
          dbt-rows--rows (vconcat (mapcar #'vconcat rows))
          dbt-rows--entries (seq-map-indexed
                             (lambda (row index)
                               (list index (vconcat (mapcar (lambda (cell)
                                                              (if cell
                                                                  (string-replace "\n" "\\n" cell)
                                                                (propertize "NULL" 'face 'dbt-rows-null)))
                                                            row))))
                             rows)
          dbt-rows--limit limit
          dbt-rows--rerun rerun
          tabulated-list-format
          (vconcat (seq-map-indexed
                    (lambda (column index)
                      (let ((cells (delq nil (mapcar (lambda (row) (aref row index)) dbt-rows--rows))))
                        (list column
                              (min 40 (apply #'max (string-width column)
                                             (mapcar (lambda (entry) (string-width (aref (cadr entry) index)))
                                                     dbt-rows--entries)))
                              ;; A column of numbers sorts as numbers; NULL
                              ;; sorts last, or first in reverse.
                              (let ((numbers (and cells (seq-every-p
                                                         (lambda (cell)
                                                           (string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]*\\)?\\(?:[eE][-+]?[0-9]+\\)?\\'"
                                                                           cell))
                                                         cells)))
                                    (rows dbt-rows--rows))
                                (lambda (a b)
                                  (let ((x (aref (aref rows (car a)) index))
                                        (y (aref (aref rows (car b)) index)))
                                    (cond ((null y) (and x t))
                                          ((null x) nil)
                                          (numbers (< (string-to-number x) (string-to-number y)))
                                          (t (string< x y)))))))))
                    columns)))
    (unless (member (car tabulated-list-sort-key) columns)
      (setq tabulated-list-sort-key nil))
    (unless (member (car dbt-rows--filter) (cons nil columns))
      (setq dbt-rows--filter nil))
    (tabulated-list-init-header)
    (dbt-rows--print)
    (display-buffer (current-buffer))
    (current-buffer)))

(defun dbt-rows-filter (regexp &optional column)
  "Show only the rows with a cell whose text matches REGEXP.
With a prefix argument, read COLUMN: the cell must be in that column.
An empty REGEXP shows every row.  NULL matches as its text, NULL.  As
in `occur', an upper-case letter in REGEXP makes the case matter."
  (interactive
   (let ((column (and current-prefix-arg (completing-read "Column: " dbt-rows--columns nil t))))
     (list (read-regexp (if column (format "Rows whose %s matches" column) "Rows with a cell that matches"))
           column)))
  (condition-case err
      (string-match-p regexp "")
    (invalid-regexp (user-error "Not a regexp, %s: %s" (cadr err) regexp)))
  (setq dbt-rows--filter (and (not (string-empty-p regexp)) (cons column regexp)))
  (dbt-rows--print))

(defun dbt-rows-org-text (text)
  "Return TEXT on one line of an Org table: a newline as \\n, | as \\vert{}.
TEXT nil, a NULL cell, is NULL."
  (if text
      (string-replace "|" "\\vert{}" (string-replace "\n" "\\n" text))
    "NULL"))

(defun dbt-rows-copy (format)
  "Copy the rows shown, in their order, as FORMAT, with their header.
FORMAT is \"CSV\", \"Markdown\" or \"Org\".  The CSV holds the text
of each cell, NULL as an empty field, quoted as RFC 4180 says; the
tables show each row on one line, as the grid does."
  (interactive (list (completing-read "Copy as: " '("CSV" "Markdown" "Org") nil t)))
  (let* ((csv (equal format "CSV"))
         (escape (pcase format
                   ("CSV" (lambda (cell)
                            (cond ((null cell) "")
                                  ((string-match-p "[\",\n\r]" cell)
                                   (concat "\"" (string-replace "\"" "\"\"" cell) "\""))
                                  (t cell))))
                   ("Markdown" (lambda (text) (string-replace "|" "\\|" text)))
                   (_ #'dbt-rows-org-text)))
         (line (lambda (cells)
                 (if csv
                     (mapconcat escape cells ",")
                   (concat "| " (mapconcat escape cells " | ") " |")))))
    (kill-new (substring-no-properties
               (mapconcat (lambda (text) (concat text "\n"))
                          `(,(funcall line dbt-rows--columns)
                            ;; The rule under the header of a table.
                            ,@(unless csv
                                (list (concat "|" (mapconcat (lambda (_) "---") dbt-rows--columns
                                                             (if (equal format "Org") "+" "|"))
                                              "|")))
                            ,@(mapcar (lambda (entry)
                                        (funcall line (append (if csv (aref dbt-rows--rows (car entry)) (cadr entry))
                                                              nil)))
                                      tabulated-list-entries)))))
    (message "Copied %d rows as %s" (length tabulated-list-entries) format)))

(defun dbt-rows-more (&optional limit)
  "Ask for the rows again, at most LIMIT of them: twice as many by default.
Interactively, LIMIT is the numeric prefix argument.  dbt has no offset,
so the query runs again with the larger limit."
  (interactive "P")
  (funcall dbt-rows--rerun (if limit (prefix-numeric-value limit) (* 2 dbt-rows--limit))))

(defun dbt-rows-show-value ()
  "Show the whole value of the cell at point.
A value of several lines, or wider than the frame, shows in the buffer
*dbt value*; any other in the echo area."
  (interactive)
  (let* ((id (or (tabulated-list-get-id) (user-error "No row here")))
         (column (or (get-text-property (point) 'tabulated-list-column-name) (user-error "No cell here")))
         (value (or (aref (aref dbt-rows--rows id) (seq-position dbt-rows--columns column)) "NULL")))
    (if (and (not (string-search "\n" value)) (< (string-width value) (frame-width)))
        (message "%s: %s" column value)
      (with-current-buffer (get-buffer-create "*dbt value*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert value))
        (special-mode)
        ;; A header line reads % as a construct of its own.
        (setq header-line-format (string-replace "%" "%%" (format " %s, row %d" column (1+ id))))
        (display-buffer (current-buffer))))))

(defvar-keymap dbt-rows-mode-map
  :doc "Keymap of `dbt-rows-mode'."
  :parent tabulated-list-mode-map
  "/" #'dbt-rows-filter
  "w" #'dbt-rows-copy
  "+" #'dbt-rows-more
  "RET" #'dbt-rows-show-value)

(define-derived-mode dbt-rows-mode tabulated-list-mode "dbt rows"
  "Major mode of the rows that a dbt query returned.
\\<dbt-rows-mode-map>\\[revert-buffer] runs the query again, \\[dbt-rows-more] asks for more rows.

\\{dbt-rows-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (funcall dbt-rows--rerun dbt-rows--limit))
              tabulated-list-printer #'dbt-rows--print-entry))

(provide 'dbt-rows)
;;; dbt-rows.el ends here
