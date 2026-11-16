;;; dbt-consult.el --- The resources of a dbt project, read with consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-consult-resource' reads a resource of the project or of its
;; packages with consult, as `dbt-find-resource' names them: grouped by
;; kind, each with its package and file, and previewed at its definition
;; while it is the current candidate.  RET visits it.  Its candidates are
;; of the completion category `dbt-identifier', on which the actions of
;; dbt-embark.el act.  It needs consult, which dbt-mode does not depend
;; on.

;;; Code:

(require 'dbt-mode)
(require 'dbt-project)

(declare-function consult--read "ext:consult")
(declare-function consult--temporary-files "ext:consult")
(declare-function consult--jump-state "ext:consult")
(declare-function consult--marker-from-line-column "ext:consult")
(declare-function consult--lookup-member "ext:consult")

(defconst dbt-consult-groups
  '((model . "Models") (seed . "Seeds") (snapshot . "Snapshots") (analysis . "Analyses")
    (macro . "Macros") (test . "Tests") (doc . "Docs") (source . "Sources") (table . "Source tables"))
  "The kinds of resources that `dbt-consult-resource' reads, as they show.")

;;;###autoload
(defun dbt-consult-resource ()
  "Visit a resource of the project or of its packages, read with consult.
They are the resources that `dbt-find-resource' names, each under its
identifier, grouped by kind, with its package and file; consult
previews the definition of the current candidate."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "This command needs consult"))
  (let* ((root (or (dbt-project-root) (user-error "Not in a dbt project")))
         (project (dbt-project-name root))
         ;; Preview opens a file as consult opens a grep match, and visits
         ;; it for good on RET.
         (open (consult--temporary-files))
         (jump (consult--jump-state)))
    (consult--read
     (mapcan (pcase-lambda (`(,kind . ,_))
               (mapcar (lambda (entity)
                         (propertize (dbt-mode-identifier (dbt-mode-entity-target entity project))
                                     'dbt-consult-entity entity))
                       (dbt-project-entities root (list kind))))
             dbt-consult-groups)
     :prompt "dbt resource: "
     :category 'dbt-identifier
     :require-match t
     :sort nil
     :lookup #'consult--lookup-member
     :group (lambda (candidate transform)
              (if transform
                  candidate
                (alist-get (plist-get (get-text-property 0 'dbt-consult-entity candidate) :kind)
                           dbt-consult-groups)))
     :annotate (lambda (candidate)
                 (let ((entity (get-text-property 0 'dbt-consult-entity candidate)))
                   (format "  %s  %s" (plist-get entity :package) (file-relative-name (plist-get entity :file) root))))
     :state (lambda (action candidate)
              (unless candidate
                (funcall open))
              (funcall jump action
                       (when-let* ((entity (and candidate (get-text-property 0 'dbt-consult-entity candidate))))
                         (consult--marker-from-line-column
                          (funcall (if (eq action 'return) #'find-file-noselect open) (plist-get entity :file))
                          (plist-get entity :line) 0)))))))

(provide 'dbt-consult)
;;; dbt-consult.el ends here
