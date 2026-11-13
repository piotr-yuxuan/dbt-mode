;;; dbt-compiled.el --- The compiled SQL beside its model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-visit-compiled' shows the compiled SQL of a model beside it, with
;; `dbt-compiled-show'.  While it shows, `dbt-compiled-follow-mode' keeps
;; point's line in it facing point's line in the model, CTE by CTE, and
;; saving the model compiles it again with dbt lsp.

;;; Code:

(require 'dbt-mode)

(declare-function dbt-lsp-compiled "dbt-lsp")
(declare-function dbt-edit-beginning-of-cte "dbt-edit")
(declare-function dbt-edit-end-of-cte "dbt-edit")

(defvar-local dbt-compiled--buffer nil
  "The buffer of the compiled SQL that the model faces, in the model's buffer.")

(defun dbt-compiled-with-buffer (callback)
  "Call CALLBACK in the buffer of the current model's compiled SQL, read-only.
With dbt lsp, once the server compiled the saved buffer; otherwise in
the file that the last `dbt compile' or run wrote, read again whenever
dbt writes it."
  (if (dbt-mode-server-p)
      (dbt-lsp-compiled callback)
    (with-current-buffer (find-file-noselect (or (dbt-mode-compiled-file "compiled")
                                                 (user-error "Not compiled yet: run dbt compile, or start dbt lsp")))
      (read-only-mode 1)
      (auto-revert-mode 1)
      (funcall callback))))

(defun dbt-compiled-position (compiled)
  "Return the position of the buffer COMPILED that faces point's line.
It is the same line of the same CTE, when COMPILED has a CTE of that
name, or else the line at the same share of the buffer."
  (let* ((case-fold-search t)
         (line (line-number-at-pos))
         ;; The CTE around point, as its name and point's line in it.
         (cte (save-excursion
                (end-of-line)
                (and (dbt-edit-beginning-of-cte 1)
                     (looking-at (dbt-mode-cte-regexp))
                     (let ((name (match-string-no-properties 1))
                           (start (line-number-at-pos)))
                       (and (ignore-error scan-error (dbt-edit-end-of-cte) t)
                            (<= line (line-number-at-pos))
                            (cons name (- line start)))))))
         (share (/ (float (1- line)) (max 1 (count-lines (point-min) (point-max))))))
    (with-current-buffer compiled
      (save-excursion
        (goto-char (point-min))
        (if (and cte (re-search-forward (dbt-mode-cte-regexp (car cte)) nil t))
            (progn (beginning-of-line)
                   (forward-line (cdr cte)))
          (forward-line (round (* share (count-lines (point-min) (point-max))))))
        (point)))))

(defun dbt-compiled--follow ()
  "Make the window of the compiled SQL face point's line in the model's window.
For `post-command-hook'.  Once no window shows the compiled SQL, turn
`dbt-compiled-follow-mode' off."
  (let ((window (and (buffer-live-p dbt-compiled--buffer) (get-buffer-window dbt-compiled--buffer)))
        (own (get-buffer-window)))
    (cond ((not window) (dbt-compiled-follow-mode -1))
          (own
           (let ((position (dbt-compiled-position dbt-compiled--buffer))
                 (above (count-lines (window-start own) (line-beginning-position))))
             (set-window-point window position)
             (set-window-start window (with-current-buffer dbt-compiled--buffer
                                        (save-excursion
                                          (goto-char position)
                                          (forward-line (- above))
                                          (point)))))))))

(defun dbt-compiled--recompile ()
  "Have dbt lsp compile the saved model again; for `after-save-hook'.
Without a server, the compiled SQL is read again as dbt writes it."
  (when (dbt-mode-server-p)
    (dbt-compiled-with-buffer #'ignore)))

(define-minor-mode dbt-compiled-follow-mode
  "Keep the compiled SQL beside the model facing point, and up to date.
While a window shows the compiled SQL that `dbt-visit-compiled' showed,
point's line in it faces point's line in the model: the same line of
the same CTE, or else the line at the same share of the buffer.  Saving
the model compiles it again with dbt lsp; without a server, the compiled
SQL is read again when dbt writes it.  The mode turns itself off once no
window shows the compiled SQL."
  :lighter " Follow"
  (if dbt-compiled-follow-mode
      (progn (add-hook 'post-command-hook #'dbt-compiled--follow nil t)
             (add-hook 'after-save-hook #'dbt-compiled--recompile nil t))
    (remove-hook 'post-command-hook #'dbt-compiled--follow t)
    (remove-hook 'after-save-hook #'dbt-compiled--recompile t)))

;;;###autoload
(defun dbt-compiled-show ()
  "Show the compiled SQL of the current model beside it, point left in the model.
`dbt-compiled-follow-mode' then keeps it facing point and up to date."
  (let ((model (current-buffer)))
    (dbt-compiled-with-buffer
     (lambda ()
       (let ((compiled (current-buffer)))
         (display-buffer compiled '((display-buffer-reuse-window display-buffer-in-direction)
                                    (direction . right)))
         (with-current-buffer model
           (setq dbt-compiled--buffer compiled)
           (dbt-compiled-follow-mode 1)
           (dbt-compiled--follow)))))))

(provide 'dbt-compiled)
;;; dbt-compiled.el ends here
