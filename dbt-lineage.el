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
  (let ((next (if (eq direction 'upstream)
                  (lambda (id) (plist-get (seq-find (lambda (node) (equal (plist-get node :id) id)) nodes)
                                          :parents))
                (lambda (id) (mapcar (lambda (node) (plist-get node :id))
                                     (seq-filter (lambda (node) (member id (plist-get node :parents)))
                                                 nodes)))))
        (frontier (list focus))
        (depth 0)
        (seen (list (cons focus 0))))
    (while frontier
      (setq depth (1+ depth)
            frontier (seq-remove (lambda (id) (assoc id seen))
                                 (seq-uniq (mapcan (lambda (id) (copy-sequence (funcall next id))) frontier))))
      (dolist (id frontier)
        (push (cons id depth) seen)))
    (cdr (nreverse seen))))

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

(defun dbt-lineage-show (root name nodes description &optional refresh)
  "Show the lineage of the resource NAME of ROOT among NODES.
DESCRIPTION says where NODES come from, for the header.  REFRESH is a
function of no argument that shows the lineage again; it defaults to
showing NODES again."
  (let ((focus (or (seq-find (lambda (node) (and (equal (plist-get node :name) name)
                                                 (memq (plist-get node :type) '(model seed snapshot))))
                             nodes)
                   (user-error "No model, seed or snapshot named %s" name))))
    (with-current-buffer (get-buffer-create (format "*dbt lineage: %s*" name))
      (dbt-lineage-mode)
      (setq dbt-lineage--refresh (or refresh (lambda () (dbt-lineage-show root name nodes description)))
            tabulated-list-entries
            (mapcan (lambda (direction)
                      (mapcar (pcase-lambda (`(,id . ,depth))
                                (let ((node (or (seq-find (lambda (node) (equal (plist-get node :id) id)) nodes)
                                                (list :id id :name id))))
                                  (list node (vector (symbol-name direction) (number-to-string depth)
                                                     (format "%s" (or (plist-get node :type) ""))
                                                     (plist-get node :name)
                                                     (if (plist-get node :file)
                                                         (file-relative-name (plist-get node :file) root)
                                                       "")))))
                              (seq-sort-by #'cdr #'< (dbt-lineage-depths nodes (plist-get focus :id) direction))))
                    '(upstream downstream)))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (display-buffer (current-buffer))
      (message "Lineage of %s %s, from %s" (plist-get focus :type) name description))))

(provide 'dbt-lineage)
;;; dbt-lineage.el ends here
