;;; dbt-node.el --- What dbt knows of a node, in a buffer of its own  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-describe' shows what dbt knows of the model, seed, snapshot or
;; source table at point, or of the current resource: its configuration,
;; columns, tests, what it depends on, what uses it and its description.
;; With dbt-lsp-plus, whose server lists the command
;; dbt-lsp-plus.nodeInfo, the node comes from the proxy's parse, with its
;; summary; otherwise from the user's target/manifest.json, as dbt last
;; wrote it.  What uses the node comes from the index, as for the
;; lineage.  RET visits a node, TAB folds a section.

;;; Code:

(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-lineage)
(require 'map)
(require 'outline)

(declare-function dbt-lsp-node-info "dbt-lsp")

(defvar-local dbt-node--root nil
  "The root of the project of this buffer's node.")

(defun dbt-node--section (title lines)
  "Insert the section TITLE and its LINES, unless there are none.
A line with the property `dbt-node-id', a unique id, is a node that RET
visits."
  (when lines
    (insert (propertize title 'face 'bold) "\n")
    (dolist (line lines)
      (insert (propertize (concat "  " line) 'dbt-node-id (get-text-property 0 'dbt-node-id line)) "\n"))))

(defun dbt-node--show (root id node from)
  "Show NODE, the node ID of ROOT as dbt gives it, which comes FROM somewhere.
NODE is a hash table of the manifest's node, which dbt-lsp-plus gives
with its `summary', its `relation', `disabled' when so, and `tests'.  A
value that is empty, null or false shows nothing."
  (let ((link (lambda (node-id)
                (propertize (format "%-9s %s" (car (split-string node-id "\\.")) (dbt-lineage-label node-id))
                            'dbt-node-id node-id)))
        (empty (lambda (value) (member value '(nil :false "" [])))))
    (with-current-buffer (get-buffer-create (format "*dbt node: %s*" (map-elt node "name")))
      (let ((inhibit-read-only t))
        (dbt-node-mode)
        (setq dbt-node--root root
              default-directory root)
        (insert (propertize (format "%s, a %s of %s" (map-elt node "name") (map-elt node "resource_type")
                                    (map-elt node "package_name"))
                            'face 'bold)
                "\n")
        (when-let* ((summary (map-elt node "summary")))
          (insert summary "\n"))
        (insert from "\n\n")
        (dbt-node--section
         "Configuration"
         (mapcar (pcase-lambda (`(,label . ,value))
                   (format "%-13s %s" label (if (vectorp value)
                                                (string-join (append value nil) ", ")
                                              (if (eq value t) "yes" value))))
                 (seq-remove (lambda (field) (funcall empty (cdr field)))
                             `(("relation" . ,(or (map-elt node "relation") (map-elt node "relation_name")))
                               ("materialized" . ,(map-nested-elt node '("config" "materialized")))
                               ("tags" . ,(map-nested-elt node '("config" "tags")))
                               ("group" . ,(map-nested-elt node '("config" "group")))
                               ("access" . ,(map-nested-elt node '("config" "access")))
                               ("contract" . ,(map-nested-elt node '("config" "contract" "enforced")))
                               ("version" . ,(map-elt node "version"))
                               ("disabled" . ,(or (map-elt node "disabled")
                                                  (eq (map-nested-elt node '("config" "enabled")) :false)))))))
        (dbt-node--section "Columns"
                           (map-apply (lambda (_name column)
                                        (mapconcat #'identity
                                                   (seq-remove empty (mapcar (lambda (key) (map-elt column key))
                                                                             '("name" "data_type" "description")))
                                                   "  "))
                                      (map-elt node "columns")))
        (dbt-node--section "Tests" (mapcar link (map-elt node "tests")))
        (dbt-node--section "Depends on" (mapcar link (map-nested-elt node '("depends_on" "nodes"))))
        (dbt-node--section "Used by"
                           (mapcar link (sort (mapcar (lambda (child) (plist-get child :id))
                                                      (seq-filter (lambda (child) (member id (plist-get child :parents)))
                                                                  (dbt-project-graph root)))
                                              #'string<)))
        (dbt-node--section "Description" (seq-remove empty (list (map-elt node "description"))))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun dbt-describe ()
  "Show what dbt knows of the model, seed, snapshot or source table at point.
On a `ref' or a `source', it is what the call names, else the current
resource: its configuration, columns, tests, what it depends on, what
uses it and its description, in a buffer of `dbt-node-mode'.  With
dbt-lsp-plus, whose server lists the command dbt-lsp-plus.nodeInfo, it
comes from the proxy's parse, with its summary; otherwise from
target/manifest.json, which `dbt parse', `dbt compile' and every run
write, saying when dbt wrote it."
  (interactive)
  (let* ((root (dbt-mode--root))
         (id (dbt-project-node-id (or (dbt-mode-resource-at-point) (user-error "No model or source here"))))
         (file (expand-file-name "manifest.json" (dbt-project-target root)))
         (from-manifest
          (lambda ()
            (unless (file-readable-p file)
              (user-error "No readable %s: run dbt parse first" (file-relative-name file root)))
            (dbt-node--show root id
                            (or (dbt-project-manifest-node file id)
                                (user-error "No node %s in %s: run dbt parse again" id (file-relative-name file root)))
                            (format "from %s, written at %s" (file-relative-name file root)
                                    (format-time-string "%F %R" (file-attribute-modification-time
                                                                 (file-attributes file))))))))
    (if (dbt-mode-server-p)
        (dbt-lsp-node-info id (lambda (node) (dbt-node--show root id node "from dbt-lsp-plus")) from-manifest)
      (funcall from-manifest))))

(defun dbt-node-visit ()
  "Visit the node on point's line, where the index has it."
  (interactive)
  (let* ((id (or (get-text-property (line-beginning-position) 'dbt-node-id) (user-error "No node here")))
         (node (or (seq-find (lambda (node) (equal (plist-get node :id) id)) (dbt-project-graph dbt-node--root))
                   (user-error "No file of %s in the index" id))))
    (find-file-other-window (plist-get node :file))
    (goto-char (point-min))
    (forward-line (1- (plist-get node :line)))))

(defvar-keymap dbt-node-mode-map
  :doc "Keymap of `dbt-node-mode'."
  :parent special-mode-map
  "RET" #'dbt-node-visit
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode dbt-node-mode special-mode "dbt node"
  "Major mode of the buffer of a dbt node, whose sections TAB folds.
\\{dbt-node-mode-map}"
  (setq-local outline-regexp "[^ \n]"
              outline-minor-mode-cycle t)
  (outline-minor-mode 1))

(provide 'dbt-node)
;;; dbt-node.el ends here
