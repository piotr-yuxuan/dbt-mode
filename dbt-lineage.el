;;; dbt-lineage.el --- The upstream and downstream of a dbt resource  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A table of what a model depends on and of what depends on it, at
;; every depth, whether the nodes come from dbt lsp or from the files of
;; the project.  A node is a plist with :id, :name, :type, :package,
;; :file, :line and :parents, the identifiers of the nodes it depends on.
;; RET visits the file of the node at point; g asks for the lineage again.

;;; Code:

(require 'seq)
(require 'tabulated-list)

(defun dbt-lineage-depths (nodes focus direction)
  "Return the nodes of NODES upstream or downstream of FOCUS, with their depth.
FOCUS is the identifier of a node.  DIRECTION is `upstream', the nodes
FOCUS depends on, or `downstream', those that depend on it.  The value
is an alist from identifier to the length of the shortest path, FOCUS
excluded."
  (let ((next (make-hash-table :test #'equal))
        (depths (make-hash-table :test #'equal))
        (frontier (list focus))
        (depth 0)
        found)
    ;; NEXT maps an identifier to the next ones in DIRECTION, in the order
    ;; of NODES and of their parents.
    (dolist (node (reverse nodes))
      (dolist (parent (reverse (plist-get node :parents)))
        (if (eq direction 'upstream)
            (push parent (gethash (plist-get node :id) next))
          (push (plist-get node :id) (gethash parent next)))))
    (puthash focus 0 depths)
    (while frontier
      (setq depth (1+ depth))
      (let (layer)
        (dolist (id frontier)
          (dolist (other (gethash id next))
            (unless (gethash other depths)
              (puthash other depth depths)
              (push (cons other depth) found)
              (push other layer))))
        (setq frontier (nreverse layer))))
    (nreverse found)))

(defvar-local dbt-lineage--refresh nil
  "The function that shows the lineage of this buffer again.")

(defun dbt-lineage-visit ()
  "Visit the file of the node at point."
  (interactive)
  (let ((node (or (tabulated-list-get-id) (user-error "No node here"))))
    (unless (and (plist-get node :file) (file-exists-p (plist-get node :file)))
      (user-error "No file for %s" (plist-get node :name)))
    (find-file-other-window (plist-get node :file))
    (goto-char (point-min))
    (forward-line (1- (or (plist-get node :line) 1)))))

(defvar-keymap dbt-lineage-mode-map
  :doc "Keymap of `dbt-lineage-mode'."
  :parent tabulated-list-mode-map
  "RET" #'dbt-lineage-visit
  "o" #'dbt-lineage-visit)

(define-derived-mode dbt-lineage-mode tabulated-list-mode "dbt lineage"
  "Major mode of the lineage of a dbt resource.
\\{dbt-lineage-mode-map}"
  (setq tabulated-list-format [("Direction" 10 t) ("Depth" 5 t :right-align t) ("Type" 9 t)
                               ("Name" 32 t) ("File" 0 t)])
  (setq-local revert-buffer-function (lambda (&rest _) (funcall dbt-lineage--refresh))))

(defun dbt-lineage-show (root id nodes description refresh)
  "Show the lineage of the node ID of ROOT among NODES.
ID is a unique id, such as model.shop.orders or, for a version,
model.shop.orders.v2.  DESCRIPTION says where NODES come from, for the
header.  REFRESH is a function of no argument that asks for the lineage
again, from where the nodes came."
  (let ((focus (or (seq-find (lambda (node) (equal (plist-get node :id) id)) nodes)
                   (user-error "No node %s" id)))
        ;; The name in an id, with a version, or a source with a table.
        (label (lambda (id) (mapconcat #'identity (nthcdr 2 (split-string id "\\.")) ".")))
        (by-id (make-hash-table :test #'equal)))
    (dolist (node (reverse nodes))
      (puthash (plist-get node :id) node by-id))
    (with-current-buffer (get-buffer-create (format "*dbt lineage: %s*" (funcall label id)))
      (dbt-lineage-mode)
      (setq dbt-lineage--refresh refresh
            tabulated-list-entries
            (mapcan (lambda (direction)
                      (mapcar (pcase-lambda (`(,id . ,depth))
                                (let ((node (or (gethash id by-id) (list :id id :name id))))
                                  (list node (vector (symbol-name direction) (number-to-string depth)
                                                     (format "%s" (or (plist-get node :type) ""))
                                                     (funcall label id)
                                                     (if (plist-get node :file)
                                                         (file-relative-name (plist-get node :file) root)
                                                       "")))))
                              (seq-sort-by #'cdr #'< (dbt-lineage-depths nodes (plist-get focus :id) direction))))
                    '(upstream downstream)))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (display-buffer (current-buffer))
      (message "Lineage of %s %s, from %s" (plist-get focus :type) (funcall label id) description))))

(provide 'dbt-lineage)
;;; dbt-lineage.el ends here
