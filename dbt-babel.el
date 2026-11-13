;;; dbt-babel.el --- Org Babel blocks of dbt  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; C-c C-c on a block `#+begin_src dbt' runs it with `dbt show' in the
;; dbt project of the block's directory, Babel's `:dir', and gives its
;; rows as a table: at most `:limit' of them, `dbt-preview-limit' by
;; default, on the target `:target' or on dbt's default one.  With
;; `:compile yes', it gives the compiled SQL instead.  Each `:var' is a
;; variable of dbt's `--vars'.  On a target of `dbt-protected-targets',
;; the block asks first.  Org finds `org-babel-execute:dbt' through its
;; autoload: there is nothing to configure.

;;; Code:

(require 'dbt-project)
(require 'dbt-rows)
(require 'ob)

(defun dbt-babel--call (params arguments)
  "Run dbt with ARGUMENTS in the project of the block, and return its output.
Babel's PARAMS give the target, `:target', and the variables, `:var',
each a string or a number.  A protected target asks first, as
`dbt-project-confirm-target' says.  When dbt fails, its errors show in
Babel's error buffer."
  (let* ((root (or (dbt-project-root default-directory) (user-error "Not in a dbt project: give the block a :dir")))
         (default-directory root)
         (named (cdr (assq :target params)))
         (target (and named (format "%s" named)))
         (vars (org-babel--get-vars params))
         ;; What the block gets wrong goes before the question.
         (executable (if (seq-some (lambda (var) (consp (cdr var))) vars)
                         (user-error "A :var of a dbt block is a string or a number, which dbt's --vars takes")
                       (dbt-project-executable)))
         (label (dbt-project-confirm-target root target)))
    (let ((errors (make-temp-file "dbt-babel")))
      (unwind-protect
          (with-temp-buffer
            (let ((status (apply #'call-process executable nil (list t errors) nil
                                 (append arguments
                                         (and target (list "--target" target))
                                         (and vars (list "--vars" (json-serialize vars)))))))
              (unless (eql status 0)
                (org-babel-eval-error-notify status (with-temp-buffer
                                                      (insert-file-contents errors)
                                                      (buffer-string)))
                (user-error "The dbt %s of the block failed on %s" (car arguments) label))
              (message "Ran dbt %s on %s" (car arguments) label)
              (buffer-string)))
        (delete-file errors)))))

;;;###autoload
(defun org-babel-execute:dbt (body params)
  "Run the dbt block BODY with Babel's PARAMS: its rows, or its compiled SQL.
The rows come from `dbt show', a table with a header, NULL as such; the
compiled SQL, with `:compile yes', from the event CompiledNode of `dbt
compile --log-format json'; `:compile' is yes or no.  Each cell of the
table is on one line, as `dbt-rows-org-text' writes it.  For
`org-babel-execute-src-block'."
  (let ((compile (cdr (assq :compile params))))
    (unless (member compile '(nil "no" "yes"))
      (user-error "A dbt block's :compile is yes or no, not %s" compile))
    (if (equal compile "yes")
        (or (seq-some (lambda (line)
                        (let ((event (ignore-errors (json-parse-string line :object-type 'alist))))
                          (and (consp event)
                               (equal (alist-get 'name (alist-get 'info event)) "CompiledNode")
                               (alist-get 'compiled (alist-get 'data event)))))
                      (split-string (dbt-babel--call params (list "compile" "--no-write-json" "--log-format" "json" "--inline" body))
                                    "\n" t))
            (user-error "No compiled SQL in what dbt compile told"))
      (let ((rows (dbt-rows-read-json
                   (dbt-babel--call params (list "show" "--quiet" "--no-write-json" "--output" "json" "--limit"
                                                 (format "%s" (or (cdr (assq :limit params)) dbt-preview-limit))
                                                 "--inline" body)))))
        (cond ((null rows) (user-error "No rows that dbt-mode can read in what dbt show told"))
              ((null (car rows)) "No rows")
              (t (cons (mapcar #'dbt-rows-org-text (car rows))
                       (cons 'hline (mapcar (lambda (row)
                                              (mapcar #'dbt-rows-org-text row))
                                            (cdr rows))))))))))

(provide 'dbt-babel)
;;; dbt-babel.el ends here
