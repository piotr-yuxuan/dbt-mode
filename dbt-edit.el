;;; dbt-edit.el --- Type and move through Jinja and CTEs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The editing commands of dbt-mode:
;;
;; - typing `{' then `{', `%' or `#' gives `{{ | }}', `{% | %}' or
;;   `{# | #}', point in the middle, when nothing but blanks, a closing
;;   bracket or a comma follow (`dbt-edit-pair-jinja-opener', on
;;   `post-self-insert-hook'); a `-' or `+' typed then sticks to the
;;   opener, the closer typed out of habit replaces the inserted one, and
;;   DEL deletes the empty pair;
;; - `dbt-close-block' inserts the closing tag of the innermost Jinja block
;;   open at point, such as `{% endif %}';
;; - `dbt-jump-to-matching-tag' moves between the opening and the closing
;;   tags of a block;
;; - CTEs are the defuns of a model: C-M-a, C-M-e, C-M-h and C-x n d
;;   (`narrow-to-defun') work on them;
;; - `dbt-edit-expreg-regions' gives expreg the Jinja tag around point and
;;   its contents, the blocks and their contents, and the CTE and its body.

;;; Code:

(require 'dbt-mode)
(require 'seq)

(defcustom dbt-edit-pair-jinja t
  "Whether typing the second character of a Jinja opener inserts its closer."
  :type 'boolean
  :group 'dbt)

(defconst dbt-edit-closers '((?{ . "}}") (?% . "%}") (?# . "#}"))
  "Alist from the second character of a Jinja opener to its closer.")

(defconst dbt-edit-fresh-space
  (propertize " " 'dbt-edit-fresh t 'rear-nonsticky '(dbt-edit-fresh))
  "The space that a pair holds after its opener, until it gets a typed one.
A space or a sign typed after it is the first character of the tag: in
`{{ -1 }}' the space was typed, in `{{- x }}' it was not.  The mark moves
with the text, comes back with an undo, and goes to no typed character.")

(defun dbt-edit-pair-jinja-opener ()
  "Close the Jinja opener that the character just typed completes.
For `post-self-insert-hook': typing `{', `%' or `#' after `{' leaves
point in `{{ | }}', `{% | %}' or `{# | #}'.  Text after point is what
the opener wraps or comments out, so only blanks, a closing bracket or a
comma may follow.  A `}' or `}}' right after point, as `electric-pair-mode'
inserts, makes way for the closer; a closer after a space is that of a
tag being edited, and stays alone."
  (when-let* ((dbt-edit-pair-jinja)
              (closer (alist-get last-command-event dbt-edit-closers))
              ((eq (char-before (1- (point))) ?{))
              ((not (looking-at-p (concat "[ \t]+" (regexp-quote closer)))))
              ((looking-at-p "}?}?[ \t]*\\(?:[])},]\\|$\\)")))
    (when (looking-at "}}?")
      (delete-region (match-beginning 0) (match-end 0)))
    (insert dbt-edit-fresh-space)
    (save-excursion (insert " " closer))))

(defun dbt-edit-join-jinja-sign ()
  "Move a `-' or `+' typed into a fresh pair next to its opener: `{%- | %}'.
For `post-self-insert-hook': that is Jinja's whitespace control, which
the space of the pair would otherwise part from its delimiter."
  (when (and dbt-edit-pair-jinja
             (memq last-command-event '(?- ?+))
             (get-text-property (- (point) 2) 'dbt-edit-fresh)
             (looking-back "{[{%#] [-+]" (- (point) 4))
             (looking-at-p " [}%#]}"))
    (delete-char -2)
    (insert last-command-event dbt-edit-fresh-space)))

(defun dbt-edit-skip-jinja-closer ()
  "Let what is typed out of habit replace what the pairing inserted.
For `post-self-insert-hook': a space typed into a fresh pair, which
holds one already, goes; typing ` }}', ` %}', ` #}' or `-%}' before the
same closer removes the closer after point."
  (when dbt-edit-pair-jinja
    (cond ((and (eq last-command-event ?\s)
                (get-text-property (- (point) 2) 'dbt-edit-fresh)
                (looking-back "{[{%#][-+]?  " (- (point) 5))
                (looking-at-p "[ \t]+[}%#]}"))
           (delete-char -1)
           (remove-text-properties (1- (point)) (point) '(dbt-edit-fresh nil)))
          ((and (eq last-command-event ?})
                (looking-back "[-+ \t]\\([}%#]\\)}" (- (point) 3))
                (looking-at (concat "[ \t]+" (regexp-quote (match-string 1)) "}")))
           (delete-region (point) (match-end 0))))))

(defun dbt-edit-delete-backward (n)
  "Delete N characters backward, or the empty Jinja pair point is in.
Right after the opener of `{{  }}', `{%  %}' or `{#  #}', or of `{%-  %}'
and the like, as the pairing leaves them, DEL deletes the whole pair;
otherwise, or with an active region, this is `delete-backward-char'."
  (interactive "p")
  (if (and dbt-edit-pair-jinja
           (= n 1)
           (not (use-region-p))
           (get-text-property (1- (point)) 'dbt-edit-fresh)
           (looking-back "{\\([{%#]\\)[-+]? " (- (point) 4))
           (looking-at (concat " " (regexp-quote (alist-get (string-to-char (match-string 1)) dbt-edit-closers)))))
      (delete-region (match-end 0) (re-search-backward "{[{%#]" (- (point) 4)))
    (call-interactively #'delete-backward-char)))

(defconst dbt-edit-block-keywords
  '("if" "for" "macro" "call" "filter" "set" "raw" "docs" "snapshot" "test"
    "materialization" "block" "with")
  "The Jinja statements that open a block, which `end' and their name close.")

(defvar-local dbt-edit--scans (list (list 0))
  "The states that `dbt-edit-blocks' resumes its scan from, the last first.
Each is (POSITION CLOSED . OPEN), the blocks of the tags before
POSITION; the last is that of the buffer's start.  They hold for the
whole buffer, never for a narrowing of it.")

(defun dbt-edit--forget-scans (start &rest _)
  "Forget the states of `dbt-edit--scans' that a change at START may alter.
A tag before START and not closed yet may be closed by the change, up
to `dbt-mode-jinja-max-tag-length' further on.  For
`before-change-functions'."
  (let ((floor (- start dbt-mode-jinja-max-tag-length)))
    (while (and (cdr dbt-edit--scans) (>= (caar dbt-edit--scans) floor))
      (pop dbt-edit--scans))))

(defun dbt-edit-blocks (limit)
  "Return the Jinja blocks of the buffer before LIMIT, as (CLOSED . OPEN).
CLOSED lists the closed blocks, OPEN those still open at LIMIT,
innermost first.  A block is (KEYWORD OPEN-START OPEN-END CLOSE-START
CLOSE-END), the last two only when it is closed.  A `set' opens a block
only without `=', and a tag in a `{# #}' comment or not closed yet
counts for nothing; a tag that starts before LIMIT counts whole.  The
scan resumes from the last of `dbt-edit--scans' before LIMIT, unless the
buffer is narrowed, and keeps the states it reaches."
  (add-hook 'before-change-functions #'dbt-edit--forget-scans nil t)
  (save-excursion
    (let* ((scans (if (buffer-narrowed-p) (list (list 0)) dbt-edit--scans))
           (scans (member (seq-find (lambda (state) (<= (car state) limit)) scans) scans))
           (closed (cadar scans))
           (open (cddar scans)))
      (goto-char (caar scans))
      ;; After a tag that ends past LIMIT, no search may bound behind point.
      (while (re-search-forward "{%-?[ \t\n]*\\(end\\)?\\([a-z_]+\\)\\_>" (max limit (point)) t)
        ;; dbt-mode-jinja-bounds searches: read the match before it.
        (let* ((start (match-beginning 0))
               (closing (match-beginning 1))
               (keyword (match-string-no-properties 2))
               (end (and (not (nth 4 (save-excursion (syntax-ppss start))))
                         (cdr (dbt-mode-jinja-bounds (match-end 0))))))
          (when end
            (cond (closing
                   (when-let* ((block (assoc keyword open)))
                     ;; Not delq: the states kept share this list.
                     (setq open (remove block open))
                     (push (append block (list start end)) closed)))
                  ;; A `set' with `=' assigns a value, and opens no block.
                  ((member keyword (if (string-search "=" (buffer-substring-no-properties start end))
                                       (remove "set" dbt-edit-block-keywords)
                                     dbt-edit-block-keywords))
                   (push (list keyword start end) open)))
            (goto-char end))
          (push (cons (point) (cons closed open)) scans)))
      (unless (buffer-narrowed-p)
        (setq dbt-edit--scans scans))
      (cons closed open))))

(defcustom dbt-indent-offset 4
  "The columns by which a parenthesis or a Jinja block indents what it holds."
  :type 'natnum
  :group 'dbt)

(defconst dbt-edit-clause-regexp
  (concat "\\(?:"
          (regexp-opt '("select" "from" "where" "group" "having" "qualify" "order" "limit" "offset" "union"
                        "intersect" "except" "minus" "with" "window" "join" "lateral")
                      'symbols)
          ;; left(x, 3) is a column: a join's word starts a clause only
          ;; with join after it.
          "\\|\\_<\\(?:left\\|right\\|full\\|inner\\|cross\\|natural\\)\\_>[^(\n]*\\_<join\\_>\\)")
  "The keywords that start a clause of SQL, on a line of their own level.")

(defun dbt-edit--opener (position blocks)
  "Return the start of the parenthesis or Jinja block open at POSITION, or 0.
It is the one that opened last.  BLOCKS are Jinja blocks, closed or
open, as `dbt-edit-blocks' gives them for a position after POSITION."
  (let ((opener (or (nth 1 (save-excursion (syntax-ppss position))) 0)))
    (dolist (block blocks opener)
      (when (and (<= (nth 2 block) position (or (nth 3 block) position)) (> (nth 1 block) opener))
        (setq opener (nth 1 block))))))

(defun dbt-edit-indentation (blocks)
  "Return the column that the current line indents to, given BLOCKS.
BLOCKS are Jinja blocks, closed or open, as `dbt-edit-blocks' gives them
for a position after the line's start.
The nearest construct open at the line's start, a parenthesis or a Jinja
block, indents it by `dbt-indent-offset' from the line that opened it.
A line that starts by closing it, with `)', `]', `}' or a tag such as
`{% endif %}' or `{% else %}', is at that line's column.  At that base,
a line that starts with a clause keyword, such as select or from, with
a Jinja statement tag, or with a CTE, NAME as (, stays at the base; any
other is one step in
from the nearest line above at its level that starts a clause: the
columns of a select, the conditions of a where."
  (save-excursion
    (back-to-indentation)
    (let* ((opener (dbt-edit--opener (line-beginning-position) blocks))
           ;; At the top level, as if a line one step out had opened it.
           (column (if (= opener 0)
                       (- dbt-indent-offset)
                     (save-excursion (goto-char opener) (current-indentation))))
           found)
      (cond ((looking-at-p "\\(?:[])}]\\|{%-?[ \t\n]*\\(?:end[a-z_]*\\|else\\|elif\\)\\_>\\)") (max 0 column))
            ((looking-at-p (concat "{%\\|,?[ \t]*[A-Za-z_][A-Za-z0-9_]*[ \t]+as[ \t]*(\\|" dbt-edit-clause-regexp))
             (+ column dbt-indent-offset))
            (t
             ;; Up the clause lines of its level, past what a construct
             ;; closed since then holds, and past lines in a string or a
             ;; comment.
             (let ((bound (save-excursion (goto-char opener) (line-beginning-position))))
               (beginning-of-line)
               (while (and (not found) (re-search-backward (concat "^[ \t]*" dbt-edit-clause-regexp) bound t))
                 (let ((other (dbt-edit--opener (point) blocks)))
                   (cond ((> other opener) (goto-char other))
                         ((not (nth 8 (syntax-ppss (point))))
                          (setq found (current-indentation)))))))
             (+ (or found column) dbt-indent-offset))))))

(defun dbt-edit--indent (blocks)
  "Indent the current line as `dbt-edit-indentation' says, given BLOCKS.
A line that starts in a string or a comment keeps its indentation."
  (unless (nth 8 (save-excursion (syntax-ppss (line-beginning-position))))
    (indent-line-to (dbt-edit-indentation blocks))))

(defun dbt-edit-indent-line ()
  "Indent the current line as `dbt-edit-indentation' says.
A line that starts in a string or a comment keeps its indentation.
Point keeps its place in the text, or goes to the indentation from
within it.  For `indent-line-function'."
  (let* ((blocks (dbt-edit-blocks (line-beginning-position)))
         (blocks (append (car blocks) (cdr blocks))))
    (if (<= (current-column) (current-indentation))
        (dbt-edit--indent blocks)
      (save-excursion (dbt-edit--indent blocks)))))

(defun dbt-edit-indent-region (start end)
  "Indent the lines from START to END that hold text.
They indent as `dbt-edit-indent-line' indents them.  The Jinja blocks
are read once, as markers, which follow the changes of indentation.
For `indent-region-function'."
  (let* ((end (copy-marker end))
         (blocks (mapcar (lambda (block) (cons (car block) (mapcar #'copy-marker (cdr block))))
                         (let ((blocks (dbt-edit-blocks end))) (append (car blocks) (cdr blocks))))))
    (save-excursion
      (goto-char start)
      (unwind-protect
          (while (< (point) end)
            (unless (looking-at-p "[ \t]*$")
              (dbt-edit--indent blocks))
            (forward-line 1))
        (dolist (block blocks)
          (mapc (lambda (marker) (set-marker marker nil)) (cdr block)))
        (set-marker end nil)))))

(defun dbt-close-block ()
  "Insert the closing tag of the innermost Jinja block open at point.
On a blank line, the tag is indented as its opening tag."
  (interactive)
  (let ((block (or (car (cdr (dbt-edit-blocks (point)))) (user-error "No Jinja block is open here"))))
    (when (looking-back "^[ \t]*" (line-beginning-position))
      (indent-line-to (save-excursion (goto-char (nth 1 block)) (current-column))))
    (insert (format "{%% end%s %%}" (car block)))))

(defun dbt-jump-to-matching-tag ()
  "Move between the opening and the closing tags of the Jinja block at point."
  (interactive)
  (let* ((position (point))
         ;; A tag holds point from its `{' up to its end, which is after it.
         (opening (lambda (block) (and (<= (nth 1 block) position) (< position (nth 2 block)))))
         (block (or (seq-find (lambda (block) (or (funcall opening block)
                                                  (and (<= (nth 3 block) position) (< position (nth 4 block)))))
                              (car (dbt-edit-blocks (point-max))))
                    (user-error "Not on a tag of a closed Jinja block"))))
    (goto-char (if (funcall opening block) (nth 3 block) (nth 1 block)))))

(defun dbt-edit-beginning-of-cte (&optional arg)
  "Move to the start of the ARGth CTE before point; forward if ARG is negative.
For `beginning-of-defun-function': return non-nil when there is one,
and leave point at the start of its line."
  (let ((case-fold-search t)
        (arg (or arg 1)))
    (if (> arg 0)
        (re-search-backward (dbt-mode-cte-regexp) nil 'move arg)
      (end-of-line)
      (prog1 (re-search-forward (dbt-mode-cte-regexp) nil 'move (- arg))
        (beginning-of-line)))))

(defun dbt-edit-end-of-cte ()
  "Move after the parenthesis that closes the CTE starting at point.
For `end-of-defun-function'.  Before a CTE, stay, so that
`end-of-defun' moves on to it; after the last one, or in a model
without CTEs, move to the end of the buffer, as the final select runs
to it."
  (let ((case-fold-search t))
    (cond ((looking-at (dbt-mode-cte-regexp))
           (goto-char (1- (match-end 0)))
           (forward-sexp))
          ((not (save-excursion (re-search-forward (dbt-mode-cte-regexp) nil t)))
           (goto-char (point-max))))))

;;;###autoload
(defun dbt-edit-expreg-regions ()
  "Return the regions around point that expreg can select, in `dbt-mode'.
They are the Jinja tag and its contents, each Jinja block and its
contents, and the CTE and its body, each (NAME . (BEG . END)) as expreg
takes them.  For `expreg-functions'; nil in any other mode."
  (when (derived-mode-p 'dbt-mode)
    (let* ((position (point))
           (tag (dbt-mode-jinja-bounds))
           (cte (save-excursion
                  (end-of-line)
                  (and (dbt-edit-beginning-of-cte 1)
                       (let ((start (point)))
                         ;; A CTE whose parenthesis is not closed yet ends nowhere.
                         (and (ignore-error scan-error (dbt-edit-end-of-cte) t)
                              (<= start position (point))
                              (list start (point))))))))
      (append (and (cdr tag) `((dbt-tag . ,tag) (dbt-tag-contents ,(+ (car tag) 2) . ,(- (cdr tag) 2))))
              (and cte `((dbt-cte ,(nth 0 cte) . ,(nth 1 cte))
                         (dbt-cte-body ,(save-excursion
                                          (goto-char (nth 0 cte))
                                          (search-forward "(")
                                          (point))
                                       . ,(1- (nth 1 cte)))))
              (mapcan (lambda (block)
                        (and (<= (nth 1 block) position (nth 4 block))
                             `((dbt-block ,(nth 1 block) . ,(nth 4 block))
                               (dbt-block-contents ,(nth 2 block) . ,(nth 3 block)))))
                      (car (dbt-edit-blocks (point-max))))))))

(provide 'dbt-edit)
;;; dbt-edit.el ends here
