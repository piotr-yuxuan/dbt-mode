;;; dbt-embark.el --- Actions on the resources of a dbt project, for embark  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-embark-target' gives embark the resource at point in `dbt-mode'
;; and `dbt-properties-mode': the call of `ref', `source', `doc' or a
;; macro under point, or the YAML entry.  Its targets, and the candidates
;; of `dbt-consult-resource', are of the type `dbt-identifier', on which
;; `dbt-embark-identifier-map' acts: visit its
;; definition or list them all, visit its YAML entry, build, run or test
;; it, show its lineage, copy its relation.  Each action but the first
;; two acts on the resource that `xref-find-definitions' would go to
;; first, from the current buffer's project, and refuses a kind of
;; resource it does not fit.  The README says how to give them to
;; embark, which dbt-mode does not depend on.

;;; Code:

(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-cli)

(defun dbt-embark-target ()
  "Return the dbt resource at point as embark's target, or nil.
For `embark-target-finders': in `dbt-mode' and `dbt-properties-mode',
the call or the YAML entry under point, as (dbt-identifier IDENTIFIER
START . END).  Nil while the region is active, so that embark acts on
the region first, as it does elsewhere."
  (when-let* (((and (not (use-region-p))
                    (or (derived-mode-p 'dbt-mode) (bound-and-true-p dbt-properties-mode))))
              (target (dbt-mode-target-at-point))
              (bounds (bounds-of-thing-at-point 'symbol)))
    `(dbt-identifier ,(dbt-mode-identifier target) ,(car bounds) . ,(cdr bounds))))

(defun dbt-embark-read ()
  "Read the identifier of a resource, as `dbt-find-resource' does."
  (completing-read "dbt resource: " (xref-backend-identifier-completion-table 'dbt) nil t))

(defun dbt-embark-entity (identifier kinds)
  "Return the resource that IDENTIFIER names, if it is of one of KINDS.
It is the definition that `xref-find-definitions' would go to first
from the current buffer.  A resource of none of KINDS is refused, before
any dbt runs."
  (let ((entity (or (car (dbt-mode-definitions (dbt-mode-parse-identifier identifier)))
                    (user-error "No definition of %s" identifier))))
    (unless (memq (plist-get entity :kind) kinds)
      (user-error "Not for a %s, only for a %s" (plist-get entity :kind) (string-join (mapcar #'symbol-name kinds) ", ")))
    entity))

(defconst dbt-embark-nodes '(model seed snapshot)
  "The kinds of resources that are nodes of dbt's graph, with a file of their own.")

(defun dbt-embark-visit (identifier)
  "Go where `xref-find-definitions' goes on a call of IDENTIFIER, from here.
That is the definition that dbt takes, or the list of namesakes."
  (interactive (list (dbt-embark-read)))
  (let ((xref-backend-functions (list (lambda () 'dbt))))
    (xref-find-definitions identifier)))

(defun dbt-embark-visit-properties (identifier)
  "Visit the YAML entry of the model, seed or snapshot that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (dbt-visit-properties (dbt-embark-entity identifier dbt-embark-nodes)))

(defun dbt-embark-build (identifier)
  "Build the resource that IDENTIFIER names: dbt build --select it."
  (interactive (list (dbt-embark-read)))
  (dbt-cli-run "build" (list "--select" (dbt-mode-entity-selection
                                         (dbt-embark-entity identifier (cons 'table dbt-embark-nodes))))))

(defun dbt-embark-run (identifier)
  "Run the model that IDENTIFIER names: dbt run --select it."
  (interactive (list (dbt-embark-read)))
  (dbt-cli-run "run" (list "--select" (dbt-mode-entity-selection (dbt-embark-entity identifier '(model))))))

(defun dbt-embark-test (identifier)
  "Test the resource that IDENTIFIER names: dbt test --select it."
  (interactive (list (dbt-embark-read)))
  (dbt-cli-run "test" (list "--select" (dbt-mode-entity-selection
                                        (dbt-embark-entity identifier (cons 'table dbt-embark-nodes))))))

(defun dbt-embark-lineage (identifier)
  "Show the lineage of the model, seed or snapshot that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (let ((entity (dbt-embark-entity identifier dbt-embark-nodes)))
    (dbt-lineage (dbt-mode-resource-selector entity) entity)))

(defun dbt-embark-copy-relation (identifier)
  "Copy the relation, database.schema.alias, of the resource that IDENTIFIER names."
  (interactive (list (dbt-embark-read)))
  (dbt-copy-relation (dbt-embark-entity identifier (cons 'table dbt-embark-nodes))))

(defvar-keymap dbt-embark-identifier-map
  :doc "The actions of embark on a dbt resource, a `dbt-identifier'.
The README gives it to embark with `embark-general-map', whose actions
it keeps but for w, which copies the relation here."
  "RET" #'dbt-embark-visit
  "d" #'dbt-find-resource
  "y" #'dbt-embark-visit-properties
  "b" #'dbt-embark-build
  "r" #'dbt-embark-run
  "t" #'dbt-embark-test
  "l" #'dbt-embark-lineage
  "w" #'dbt-embark-copy-relation)

(provide 'dbt-embark)
;;; dbt-embark.el ends here
