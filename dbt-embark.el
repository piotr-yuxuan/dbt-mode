;;; dbt-embark.el --- Actions on the resources of a dbt project, for embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-embark-target' gives embark the resource at point in `dbt-mode'
;; and `dbt-properties-mode': the call of `ref', `source', `doc' or a
;; macro under point, or the YAML entry.  Its targets, and the candidates
;; of `dbt-consult-resource' and `dbt-find-resource', are of the type
;; `dbt-identifier', on which `dbt-embark-identifier-map' acts: visit its
;; definitions, its YAML entry, build it, test it, show its lineage, copy
;; its relation.  Each action but the first works where
;; `xref-find-definitions' would go first.  The README says how to give
;; them to embark, which dbt-mode does not depend on.

;;; Code:

(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-cli)

(defun dbt-embark-target ()
  "Return the dbt resource at point as embark's target, or nil.
For `embark-target-finders': in `dbt-mode' and `dbt-properties-mode',
the call or the YAML entry under point, as (dbt-identifier IDENTIFIER
START . END)."
  (when-let* (((or (derived-mode-p 'dbt-mode) (bound-and-true-p dbt-properties-mode)))
              (target (dbt-mode-target-at-point))
              (bounds (bounds-of-thing-at-point 'symbol)))
    `(dbt-identifier ,(dbt-mode-identifier target) ,(car bounds) . ,(cdr bounds))))

(defun dbt-embark-read ()
  "Read the identifier of a resource, as `dbt-find-resource' does."
  (completing-read "dbt resource: " (xref-backend-identifier-completion-table 'dbt) nil t))

(defun dbt-embark--at (identifier action)
  "Call ACTION where the resource that IDENTIFIER names is defined.
That is the definition that `xref-find-definitions' would go to first
from the current buffer: ACTION runs in its file's buffer, with point
on its line."
  (let ((entity (or (car (dbt-mode-definitions (dbt-mode-parse-identifier identifier)))
                    (user-error "No definition of %s" identifier))))
    (with-current-buffer (find-file-noselect (plist-get entity :file))
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (plist-get entity :line)))
        (funcall action)))))

(defun dbt-embark-visit-properties (identifier)
  "Visit the YAML entry of the resource that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (dbt-embark--at identifier #'dbt-visit-properties))

(defun dbt-embark-build (identifier)
  "Build the resource that IDENTIFIER names: dbt build --select it."
  (interactive (list (dbt-embark-read)))
  (dbt-embark--at identifier (lambda () (dbt-cli-run "build" (list "--select" (dbt-mode-selection-at-point))))))

(defun dbt-embark-test (identifier)
  "Test the resource that IDENTIFIER names: dbt test --select it."
  (interactive (list (dbt-embark-read)))
  (dbt-embark--at identifier (lambda () (dbt-cli-run "test" (list "--select" (dbt-mode-selection-at-point))))))

(defun dbt-embark-lineage (identifier)
  "Show the lineage of the model, seed or snapshot that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (dbt-embark--at identifier
                  (lambda ()
                    (dbt-lineage (dbt-mode-resource-selector
                                  (or (dbt-mode-current-resource) (user-error "No model, seed or snapshot here")))))))

(defun dbt-embark-copy-relation (identifier)
  "Copy the relation, database.schema.alias, of the resource that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (dbt-embark--at identifier #'dbt-copy-relation))

(defvar-keymap dbt-embark-identifier-map
  :doc "The actions of embark on a dbt resource, a `dbt-identifier'."
  "o" #'dbt-find-resource
  "y" #'dbt-embark-visit-properties
  "b" #'dbt-embark-build
  "t" #'dbt-embark-test
  "l" #'dbt-embark-lineage
  "w" #'dbt-embark-copy-relation)

(provide 'dbt-embark)
;;; dbt-embark.el ends here
