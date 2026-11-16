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
A line with the property `dbt-node-id', a unique id, or `dbt-node-file',
a file, is what RET visits.  The lines of a value that has several are
indented as its first, so that only titles start a line."
  (when lines
    (insert (propertize title 'face 'bold) "\n")
    (dolist (line lines)
      (insert (propertize (concat "  " (replace-regexp-in-string "\n" "\n  " line))
                          'dbt-node-id (get-text-property 0 'dbt-node-id line)
                          'dbt-node-file (get-text-property 0 'dbt-node-file line))
              "\n"))))

(defun dbt-node--file (label directory relative)
  "Return the line LABEL of the file RELATIVE to DIRECTORY, which RET visits.
Nil when either is nil."
  (when (and directory relative)
    (propertize (format "%-13s %s" label relative) 'dbt-node-file (expand-file-name relative directory))))

(defun dbt-node--show (root id node from refresh)
  "Show NODE, the node ID of ROOT as dbt gives it, which comes FROM somewhere.
NODE is a hash table of the manifest's node, which dbt-lsp-plus gives
with its `summary', its `relation', `disabled' when so, and `tests'.  A
value that is empty, null or false shows nothing.  REFRESH, a function
of no argument, shows the node again, on \\[revert-buffer]."
  (let ((link (lambda (node-id)
                (propertize (format "%-9s %s" (car (split-string node-id "\\.")) (dbt-lineage-label node-id))
                            'dbt-node-id node-id)))
        (empty (lambda (value) (member value '(nil :false "" []))))
        (directory (nth 1 (assoc (map-elt node "package_name") (dbt-project-packages root))))
        (columns (map-values (map-elt node "columns"))))
    (with-current-buffer (get-buffer-create (format "*dbt node: %s*" id))
      (let ((inhibit-read-only t)
            (width (apply #'max 0 (mapcar (lambda (column) (length (map-elt column "name"))) columns))))
        (dbt-node-mode)
        (setq dbt-node--root root
              default-directory root)
        (setq-local revert-buffer-function (lambda (&rest _) (funcall refresh)))
        (erase-buffer)
        (insert (propertize (format "%s, a %s of %s" (dbt-lineage-label id) (map-elt node "resource_type")
                                    (map-elt node "package_name"))
                            'face 'bold)
                "\n")
        (when-let* ((summary (map-elt node "summary")))
          (insert summary "\n"))
        (insert from "\n\n")
        (dbt-node--section "Description" (seq-remove empty (list (map-elt node "description"))))
        (dbt-node--section "Files"
                           (delq nil (list (dbt-node--file "file" directory (map-elt node "original_file_path"))
                                           (dbt-node--file "properties" directory
                                                           (cadr (split-string (format "%s" (map-elt node "patch_path"))
                                                                               "://"))))))
        (dbt-node--section
         "Configuration"
         (mapcar (pcase-lambda (`(,label . ,value))
                   (format "%-13s %s" label (if (vectorp value)
                                                (string-join (append value nil) ", ")
                                              (if (eq value t) "yes" value))))
                 (seq-remove (lambda (field) (funcall empty (cdr field)))
                             `(("relation" . ,(if (map-contains-key node "relation")
                                                  (map-elt node "relation")
                                                (map-elt node "relation_name")))
                               ("owner" . ,(map-nested-elt node '("config" "meta" "owner")))
                               ("materialized" . ,(map-nested-elt node '("config" "materialized")))
                               ("unique key" . ,(map-nested-elt node '("config" "unique_key")))
                               ("strategy" . ,(or (map-nested-elt node '("config" "incremental_strategy"))
                                                  (map-nested-elt node '("config" "strategy"))))
                               ("schema change" . ,(map-nested-elt node '("config" "on_schema_change")))
                               ("updated at" . ,(map-nested-elt node '("config" "updated_at")))
                               ("tags" . ,(map-nested-elt node '("config" "tags")))
                               ("group" . ,(map-nested-elt node '("config" "group")))
                               ("access" . ,(map-nested-elt node '("config" "access")))
                               ("contract" . ,(map-nested-elt node '("config" "contract" "enforced")))
                               ("version" . ,(map-elt node "version"))
                               ("disabled" . ,(or (map-elt node "disabled")
                                                  (eq (map-nested-elt node '("config" "enabled")) :false)))))))
        (dbt-node--section "Columns"
                           (mapcar (lambda (column)
                                     (string-trim-right
                                      (format (format "%%-%ds  %%s" width) (map-elt column "name")
                                              (mapconcat #'identity
                                                         (seq-remove empty (list (map-elt column "data_type")
                                                                                 (map-elt column "description")))
                                                         "  "))))
                                   columns))
        (dbt-node--section "Tests"
                           (if (map-contains-key node "tests")
                               (mapcar (lambda (test)
                                         (replace-regexp-in-string "\\.[0-9a-f]\\{10\\}\\'" "" (dbt-lineage-label test)))
                                       (map-elt node "tests"))
                             (list "known to dbt-lsp-plus only: C-c C-t visits the YAML that holds them")))
        (dbt-node--section "Depends on" (mapcar link (map-nested-elt node '("depends_on" "nodes"))))
        (dbt-node--section "Used by, in the index"
                           (mapcar link (sort (mapcar (lambda (child) (plist-get child :id))
                                                      (seq-filter (lambda (child) (member id (plist-get child :parents)))
                                                                  (dbt-project-graph root)))
                                              #'string<)))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun dbt-node-describe (root id)
  "Show the node ID of ROOT in its buffer.
From dbt-lsp-plus when the current buffer's server lists the command
dbt-lsp-plus.nodeInfo, else from target/manifest.json, saying when dbt
wrote it.  \\[revert-buffer] in that buffer shows it again the same way."
  (let* ((file (expand-file-name "manifest.json" (dbt-project-target root)))
         (source (current-buffer))
         (refresh (lambda ()
                    (with-current-buffer (if (buffer-live-p source) source (current-buffer))
                      (dbt-node-describe root id))))
         (from-manifest
          (lambda ()
            (unless (file-readable-p file)
              (user-error "No readable %s: run dbt parse first" (file-relative-name file root)))
            (dbt-node--show root id
                            (or (dbt-project-manifest-node file id)
                                (user-error "No node %s in %s: run dbt parse again" id (file-relative-name file root)))
                            (format "from %s, written at %s" (file-relative-name file root)
                                    (format-time-string "%F %R" (file-attribute-modification-time
                                                                 (file-attributes file))))
                            refresh))))
    (if (dbt-mode-server-p)
        (dbt-lsp-node-info id (lambda (node) (dbt-node--show root id node "from dbt-lsp-plus" refresh)) from-manifest)
      (funcall from-manifest))))

;;;###autoload
(defun dbt-describe ()
  "Show what dbt knows of the model, seed, snapshot or source table at point.
On a `ref' or a `source', or the name of a YAML entry, it is what the
call or the entry names, else the current resource: its description,
its file and YAML entry, its configuration, columns, tests, what it
depends on and what uses it, in a buffer of `dbt-node-mode'.  With
dbt-lsp-plus, whose server lists the command dbt-lsp-plus.nodeInfo, it
comes from the proxy's parse, with its summary; otherwise from
target/manifest.json, which `dbt parse', `dbt compile' and every run
write, saying when dbt wrote it."
  (interactive)
  (dbt-node-describe (dbt-mode--root)
                     (dbt-project-node-id (or (dbt-mode-resource-at-point) (user-error "No model or source here")))))

(defun dbt-node-eldoc (callback &rest _)
  "Give CALLBACK the materialization and relation of the ref or source at point.
For `eldoc-documentation-functions', without a server, whose hover says
more.  They come from the project's manifest, once dbt-mode holds it as
dbt last wrote it: eldoc never reads one."
  (when-let* (((not (dbt-mode-server-p)))
              (root (dbt-project-root))
              (target (dbt-mode-target-at-point))
              ((memq (plist-get target :kind) '(ref source)))
              (entity (car (dbt-mode-definitions target)))
              (file (expand-file-name "manifest.json" (dbt-project-target root)))
              ((dbt-project-manifest-held-p file))
              (node (dbt-project-manifest-node file (dbt-project-node-id entity)))
              (said (delq nil (list (map-nested-elt node '("config" "materialized"))
                                    (map-elt node "relation_name")))))
    (funcall callback (string-join said " "))))

(defun dbt-node-visit ()
  "Visit the file or the node on point's line, a node where the index has it."
  (interactive)
  (let ((file (get-text-property (line-beginning-position) 'dbt-node-file))
        (id (get-text-property (line-beginning-position) 'dbt-node-id)))
    (if file
        (find-file-other-window file)
      (let ((node (or (seq-find (lambda (node) (equal (plist-get node :id) id))
                                (dbt-project-graph dbt-node--root))
                      (user-error (if id (format "No file of %s in the index" id) "No file or node here")))))
        (find-file-other-window (plist-get node :file))
        (goto-char (point-min))
        (forward-line (1- (plist-get node :line)))))))

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
