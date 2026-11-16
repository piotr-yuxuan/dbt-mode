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
;; lsp-mode's breadcrumbs draw: it steps aside when the server announces
;; document symbols.  With dbt lsp alone, which announces none, lsp-mode
;; still draws the directories and the file, and this mode adds the rest.

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
  "Point and the buffer's modification count when the line was computed.")

(defconst dbt-breadcrumb--construct
  '(:eval (cond ((and (dbt-mode-server-p) (dbt-lsp-symbols-p)) nil)
                ;; lsp-mode's line, before this one, names the place.
                ((bound-and-true-p lsp-headerline-breadcrumb-mode) (cdr dbt-breadcrumb--line))
                (t (concat (car dbt-breadcrumb--line) (cdr dbt-breadcrumb--line)))))
  "The construct of `header-line-format' of `dbt-breadcrumb-mode'.
It shows nothing when the server announces document symbols, from
which lsp-mode draws its breadcrumbs, and only the symbols when
lsp-mode's breadcrumbs are on without them.")

(defun dbt-breadcrumb--update (buffer)
  "Compute `dbt-breadcrumb--line' of BUFFER at its point, unless it was killed.
For the idle timer of `dbt-breadcrumb--schedule'.  The symbols are the
Jinja blocks open at point, a definition block named by the first
identifier after its keyword and an `if', `for' or `call' block by its
statement, and the CTEs whose parenthesis holds point or is opened on
its line, outermost first."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq dbt-breadcrumb--timer nil
            dbt-breadcrumb--state (cons (point) (buffer-chars-modified-tick)))
      (let* ((case-fold-search t)
             (position (point))
             (root (dbt-project-root))
             (symbols nil))
        (save-excursion
          (dolist (block (cdr (dbt-edit-blocks position)))
            (let ((statement (replace-regexp-in-string
                              "[ \t\n\r]+" " "
                              (string-trim (buffer-substring-no-properties (+ (nth 1 block) 2) (- (nth 2 block) 2))
                                           "[-+]?[ \t\n\r]*" "[ \t\n\r]*[-+]?"))))
              ;; dbt-lsp-plus's outline leaves out raw, filter, block and with.
              (when (string-match (concat "\\`\\(?:\\(?:if\\|for\\|call\\)\\_>"
                                          "\\|\\(?:macro\\|test\\|snapshot\\|materialization\\|docs\\|set\\)"
                                          " \\([[:alpha:]_][[:alnum:]_]*\\)\\)")
                                  statement)
                (push (cons (nth 1 block) (or (match-string 1 statement) statement)) symbols))))
          ;; A parenthesis in a comment or a string opens nothing for `syntax-ppss'.
          (dolist (paren (seq-uniq (append (nth 9 (syntax-ppss position)) (nth 9 (syntax-ppss (line-end-position))))))
            (goto-char (1+ paren))
            (when (looking-back (dbt-mode-cte-regexp) (line-beginning-position))
              (push (cons (match-beginning 1) (match-string-no-properties 1)) symbols))))
        (setq dbt-breadcrumb--line
              (cons (string-replace
                     "%" "%%"
                     (concat " " (string-join
                                  (append (and root (delete "." (split-string (file-relative-name default-directory root) "/" t)))
                                          (list (file-name-nondirectory (or buffer-file-name (buffer-name)))))
                                  " > ")))
                    (string-replace "%" "%%" (mapconcat (lambda (symbol) (concat " > " (cdr symbol)))
                                                        (sort symbols #'car-less-than-car)))))
        (force-mode-line-update)))))

(defun dbt-breadcrumb--schedule ()
  "Have the header line computed once Emacs is idle, if point or the text moved.
For `post-command-hook'.  Half a second, lsp-mode's `lsp-idle-delay'."
  (unless (or dbt-breadcrumb--timer
              (equal dbt-breadcrumb--state (cons (point) (buffer-chars-modified-tick))))
    (setq dbt-breadcrumb--timer (run-with-idle-timer 0.5 nil #'dbt-breadcrumb--update (current-buffer)))))

;;;###autoload
(define-minor-mode dbt-breadcrumb-mode
  "Show in the header line where point is: the file, then its CTEs and blocks.
The header line names the directories from the project's root to the
file, the file, then the CTEs and the Jinja blocks that hold point, as
dbt-lsp-plus names them in its outline.  It follows point once Emacs is
idle.  When the server announces document symbols, as dbt-lsp-plus
does, lsp-mode's breadcrumbs show them instead; when lsp-mode's
breadcrumbs show the file without them, as with dbt lsp alone, this
mode adds the CTEs and blocks after them."
  :lighter nil
  (if dbt-breadcrumb-mode
      (progn
        (unless (listp header-line-format)
          (setq header-line-format (list header-line-format)))
        ;; After lsp-mode's breadcrumbs, which it puts first.
        (setq header-line-format (append (remove dbt-breadcrumb--construct header-line-format)
                                         (list dbt-breadcrumb--construct)))
        (add-hook 'post-command-hook #'dbt-breadcrumb--schedule nil t)
        (dbt-breadcrumb--schedule))
    (setq header-line-format (remove dbt-breadcrumb--construct header-line-format))
    (remove-hook 'post-command-hook #'dbt-breadcrumb--schedule t)
    (when dbt-breadcrumb--timer
      (cancel-timer dbt-breadcrumb--timer))
    (setq dbt-breadcrumb--timer nil
          dbt-breadcrumb--state nil)))

(provide 'dbt-breadcrumb)
;;; dbt-breadcrumb.el ends here
