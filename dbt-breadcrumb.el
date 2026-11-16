;;; dbt-breadcrumb.el --- Where point is, in the header line  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-breadcrumb-mode' shows in the header line the directories from
;; the project's root to the file, the file, then the CTEs and the Jinja
;; blocks that hold point, as `models > marts > orders.sql > final > if
;; is_incremental()'.  It names them as dbt-lsp-plus's outline does, so
;; that a model reads the same with and without the proxy, whose symbols
;; lsp-mode's breadcrumbs draw: it steps aside when they show them.  With
;; dbt lsp alone, which announces none, lsp-mode still draws the
;; directories and the file, and this mode adds the rest.

;;; Code:

(require 'dbt-edit)
(require 'dbt-mode)
(require 'dbt-project)

(defvar-local dbt-breadcrumb--line nil
  "What the header line shows, as (PLACE . SYMBOLS), their `%' doubled.
PLACE names the directories and the file, SYMBOLS the CTEs and blocks
that hold point, each after \" > \".")

(defvar-local dbt-breadcrumb--timer nil
  "The idle timer that computes `dbt-breadcrumb--line', while it waits.")

(defvar-local dbt-breadcrumb--state nil
  "Point, the modification count and the file when the line was computed.")

(defvar lsp-headerline-breadcrumb-segments)
(declare-function dbt-lsp-symbols-p "dbt-lsp")

(defun dbt-breadcrumb--header-line ()
  "Return what the header line of `dbt-breadcrumb-mode' shows, at redisplay.
Nothing when lsp-mode's breadcrumbs show the document symbols that the
server announces; only the symbols when they show the file; otherwise
the whole line.  A function rather than code in the construct, which
nothing compiles."
  (let ((shown (and (bound-and-true-p lsp-headerline-breadcrumb-mode) lsp-headerline-breadcrumb-segments)))
    (cond ((and (memq 'symbols shown) (dbt-mode-server-p) (dbt-lsp-symbols-p)) nil)
          ;; lsp-mode's line, before this one, names the place.
          ((memq 'file shown) (cdr dbt-breadcrumb--line))
          (t (concat (car dbt-breadcrumb--line) (cdr dbt-breadcrumb--line))))))

(defconst dbt-breadcrumb--construct '(:eval (dbt-breadcrumb--header-line))
  "The construct of `header-line-format' of `dbt-breadcrumb-mode'.")

(defun dbt-breadcrumb--update (buffer)
  "Compute `dbt-breadcrumb--line' of BUFFER at its point, unless it was killed.
For the idle timer of `dbt-breadcrumb--schedule', and when the mode
starts.  The symbols are the Jinja blocks open at point, a definition
block named by the first identifier after its keyword and an `if',
`for' or `call' block by its statement, and the CTEs whose parenthesis
holds point, outermost first, in the whole buffer, narrowed or not.
On the first line of a CTE, point is in it: before its parenthesis, as
where `C-M-a' leaves it, and in a line that closes a CTE and opens the
next, `), final as (', which is the next one's."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq dbt-breadcrumb--timer nil
            dbt-breadcrumb--state (list (point) (buffer-chars-modified-tick) buffer-file-name))
      (save-excursion
        (save-restriction
          (widen)
          (let* ((case-fold-search t)
                 (position (point))
                 (file (expand-file-name (or (buffer-file-name (buffer-base-buffer)) (buffer-name))))
                 (root (dbt-project-root file))
                 (symbols nil))
            (dolist (block (cdr (dbt-edit-blocks position)))
              (let ((statement (replace-regexp-in-string
                                "[ \t\n\r]+" " "
                                (string-trim (buffer-substring-no-properties (+ (nth 1 block) 2) (- (nth 2 block) 2))
                                             "[-+]?[ \t\n\r]*" "[ \t\n\r]*[-+]?"))))
                ;; dbt-lsp-plus's outline leaves out raw, filter, block and with.
                (when (string-match (concat "\\`\\(?:\\(?:macro\\|test\\|snapshot\\|materialization\\|docs\\|set\\)"
                                            " \\(?1:[[:alpha:]_][[:alnum:]_]*\\)"
                                            "\\|\\(?1:\\(?:if\\|for\\|call\\)\\_>.*\\)\\)")
                                    statement)
                  (push (cons (nth 1 block) (match-string 1 statement)) symbols))))
            ;; A parenthesis in a comment or a string opens nothing for
            ;; `syntax-ppss'; the CTE's regexp matches at a line's start.
            (beginning-of-line)
            (dolist (paren (nth 9 (syntax-ppss (max position (or (re-search-forward (dbt-mode-cte-regexp) (line-end-position) t)
                                                                 position)))))
              (goto-char (1+ paren))
              (when (looking-back (dbt-mode-cte-regexp) (line-beginning-position))
                (push (cons (match-beginning 1) (match-string-no-properties 1)) symbols)))
            (setq dbt-breadcrumb--line
                  (cons (string-replace
                         "%" "%%"
                         (concat " " (string-join
                                      (append (and root (delete "." (split-string (file-relative-name (file-name-directory file) root)
                                                                                  "/" t)))
                                              (list (file-name-nondirectory file)))
                                      " > ")))
                        (string-replace "%" "%%" (mapconcat (lambda (symbol) (concat " > " (cdr symbol)))
                                                            (sort symbols #'car-less-than-car))))))))
      (force-mode-line-update))))

(defun dbt-breadcrumb--schedule ()
  "Have the header line computed once Emacs is idle, if point, text or file moved.
For `post-command-hook'.  Half a second, lsp-mode's `lsp-idle-delay'."
  (unless (or dbt-breadcrumb--timer
              (equal dbt-breadcrumb--state (list (point) (buffer-chars-modified-tick) buffer-file-name)))
    (setq dbt-breadcrumb--timer (run-with-idle-timer 0.5 nil #'dbt-breadcrumb--update (current-buffer)))))

;;;###autoload
(define-minor-mode dbt-breadcrumb-mode
  "Show in the header line where point is: the file, then its CTEs and blocks.
The header line names the directories from the project's root to the
file, the file, then the CTEs and the Jinja blocks that hold point, as
dbt-lsp-plus names them in its outline.  It follows point once Emacs is
idle.  When lsp-mode's breadcrumbs show the document symbols that the
server announces, as dbt-lsp-plus does, they replace this line; when
they show the file without them, as with dbt lsp alone, which
announces none, this mode adds the CTEs and blocks after them."
  :lighter nil
  (if dbt-breadcrumb-mode
      (progn
        ;; After lsp-mode's breadcrumbs, which it puts first.  A list whose
        ;; car is a string or a list holds constructs; any other value,
        ;; such as (:eval FORM) or a symbol, is one, which a list led by ""
        ;; holds, as (SYMBOL CONSTRUCT) would be a conditional.
        (setq header-line-format
              (append (if (and (consp header-line-format)
                               (or (stringp (car header-line-format)) (consp (car header-line-format))))
                          (remove dbt-breadcrumb--construct header-line-format)
                        (and header-line-format (list "" header-line-format)))
                      (list dbt-breadcrumb--construct)))
        (add-hook 'post-command-hook #'dbt-breadcrumb--schedule nil t)
        (dbt-breadcrumb--update (current-buffer)))
    (when (consp header-line-format)
      (setq header-line-format (remove dbt-breadcrumb--construct header-line-format)))
    (remove-hook 'post-command-hook #'dbt-breadcrumb--schedule t)
    (when dbt-breadcrumb--timer
      (cancel-timer dbt-breadcrumb--timer))
    (setq dbt-breadcrumb--timer nil
          dbt-breadcrumb--state nil)))

(provide 'dbt-breadcrumb)
;;; dbt-breadcrumb.el ends here
