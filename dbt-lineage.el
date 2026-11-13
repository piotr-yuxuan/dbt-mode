;;; dbt-lineage.el --- The upstream and downstream of a dbt resource  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The tree of what a model depends on and of what depends on it, whether
;; the nodes come from dbt lsp or from the files of the project.  A node
;; is a plist with :id, :name, :type, :package, :file, :line and
;; :parents, the identifiers of the nodes it depends on.  Each node shows
;; once, under the node through which it is nearest.  TAB folds a node,
;; + and - change the depth shown, e writes the graph within that depth
;; as Graphviz dot, RET visits the file of the node at point, and g asks
;; for the lineage again.

;;; Code:

(require 'outline)
(require 'seq)

(defun dbt-lineage-reach (nodes focus direction)
  "Return the nodes of NODES upstream or downstream of FOCUS, as a tree.
FOCUS is the identifier of a node.  DIRECTION is `upstream', the nodes
FOCUS depends on, or `downstream', those that depend on it.  The value
is a list of (ID DEPTH PARENT), in the order of a breadth-first walk:
DEPTH is the length of the shortest path from FOCUS, and PARENT the
node through which the walk first reached ID, FOCUS at depth 1."
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
              (push (list other depth id) found)
              (push other layer))))
        (setq frontier (nreverse layer))))
    (nreverse found)))

(defun dbt-lineage-label (id)
  "Return the name in the unique ID: a model's with its version, a source's table."
  (mapconcat #'identity (nthcdr 2 (split-string id "\\.")) "."))

(defvar-local dbt-lineage--refresh nil
  "The function that shows the lineage of this buffer again.")

(defvar-local dbt-lineage--root nil
  "The root of the project of this buffer's lineage.")

(defvar-local dbt-lineage--nodes nil
  "The nodes of this buffer's lineage.")

(defvar-local dbt-lineage--focus nil
  "The identifier of the node whose lineage this buffer shows.")

(defvar-local dbt-lineage--depth nil
  "The deepest level that this buffer shows, or nil for every one.")

(defvar-local dbt-lineage--reach nil
  "The reach of this buffer's focus, as an alist from a direction.
Each is what `dbt-lineage-reach' returns, computed once for its nodes.")

(defun dbt-lineage--within (direction)
  "Return the reach of this buffer's focus in DIRECTION, within its depth."
  (seq-filter (lambda (entry) (or (null dbt-lineage--depth) (<= (nth 1 entry) dbt-lineage--depth)))
              (alist-get direction dbt-lineage--reach)))

(defun dbt-lineage--render ()
  "Write the tree of this buffer's lineage, within its depth."
  (let ((inhibit-read-only t)
        (by-id (make-hash-table :test #'equal)))
    (dolist (node (reverse dbt-lineage--nodes))
      (puthash (plist-get node :id) node by-id))
    (erase-buffer)
    (dolist (direction '(upstream downstream))
      (insert (propertize (symbol-name direction) 'face 'bold) "\n")
      (let ((children (make-hash-table :test #'equal))
            stack)
        (dolist (entry (reverse (dbt-lineage--within direction)))
          (push entry (gethash (nth 2 entry) children)))
        ;; Each node, then what it reaches, deeper: a chain of models may
        ;; be thousands of levels deep, too deep for a recursion.
        (setq stack (gethash dbt-lineage--focus children))
        (while stack
          (let* ((entry (pop stack))
                 (node (or (gethash (car entry) by-id) (list :id (car entry)))))
            (insert (propertize (concat (make-string (* 2 (nth 1 entry)) ?\s)
                                        (dbt-lineage-label (car entry))
                                        (propertize (format "  %s  %s" (or (plist-get node :type) "")
                                                            (if (plist-get node :file)
                                                                (file-relative-name (plist-get node :file)
                                                                                    dbt-lineage--root)
                                                              ""))
                                                    'face 'shadow))
                                'dbt-lineage-node node)
                    "\n")
            (setq stack (append (gethash (car entry) children) stack))))))
    (setq header-line-format (format " Lineage of %s, %s" (dbt-lineage-label dbt-lineage--focus)
                                     (if dbt-lineage--depth (format "to depth %d" dbt-lineage--depth) "at every depth")))
    (goto-char (point-min))))

(defun dbt-lineage--deepest ()
  "Return the depth of the deepest node of this buffer's lineage."
  (apply #'max 0 (mapcar #'cadr (apply #'append (mapcar #'cdr dbt-lineage--reach)))))

(defun dbt-lineage--show-depth (depth)
  "Show the lineage to DEPTH, nil for every depth, point kept on its node.
Point goes to the first line of that node while the tree shows it."
  (let ((at (get-text-property (line-beginning-position) 'dbt-lineage-node)))
    (setq dbt-lineage--depth depth)
    (dbt-lineage--render)
    (when-let* ((match (and at (text-property-search-forward 'dbt-lineage-node at t))))
      (goto-char (prop-match-beginning match)))))

(defun dbt-lineage-deeper ()
  "Show one level more of the lineage."
  (interactive)
  (dbt-lineage--show-depth (and dbt-lineage--depth (< (1+ dbt-lineage--depth) (dbt-lineage--deepest))
                                (1+ dbt-lineage--depth))))

(defun dbt-lineage-shallower ()
  "Show one level less of the lineage, down to the nearest nodes."
  (interactive)
  (dbt-lineage--show-depth (max 1 (1- (or dbt-lineage--depth (dbt-lineage--deepest))))))

(defun dbt-lineage-visit ()
  "Visit the file of the node on point's line."
  (interactive)
  (let ((node (or (get-text-property (line-beginning-position) 'dbt-lineage-node) (user-error "No node here"))))
    (unless (and (plist-get node :file) (file-exists-p (plist-get node :file)))
      (user-error "No file for %s" (dbt-lineage-label (plist-get node :id))))
    (find-file-other-window (plist-get node :file))
    (goto-char (point-min))
    (forward-line (1- (or (plist-get node :line) 1)))))

(defun dbt-lineage-export (file)
  "Write the graph of the lineage within the depth shown to FILE, as Graphviz dot.
Each node is named by its unique id and labelled by its name.  With the
executable dot, an SVG beside FILE follows, rendered while you go on;
the message says when it is there, or why not."
  (interactive (list (read-file-name "Graphviz file: " nil nil nil
                                     (format "%s.dot" (dbt-lineage-label dbt-lineage--focus)))))
  (let ((ids (make-hash-table :test #'equal))
        (nodes dbt-lineage--nodes)
        (svg (concat (file-name-sans-extension file) ".svg")))
    (dolist (id (cons dbt-lineage--focus (mapcar #'car (append (dbt-lineage--within 'upstream)
                                                               (dbt-lineage--within 'downstream)))))
      (puthash id t ids))
    (with-temp-file file
      (insert "digraph lineage {\n  rankdir=LR;\n")
      (maphash (lambda (id _) (insert (format "  %S [label=%S];\n" id (dbt-lineage-label id)))) ids)
      (dolist (node nodes)
        (when (gethash (plist-get node :id) ids)
          (dolist (parent (plist-get node :parents))
            (when (gethash parent ids)
              (insert (format "  %S -> %S;\n" parent (plist-get node :id)))))))
      (insert "}\n"))
    (if (not (executable-find "dot"))
        (message "Wrote %s; no dot to render it" file)
      (message "Wrote %s; dot renders %s..." file svg)
      (make-process :name "dot" :command (list "dot" "-Tsvg" file "-o" svg) :noquery t
                    :buffer (generate-new-buffer " *dot*")
                    :sentinel (lambda (process _event)
                                (unless (process-live-p process)
                                  (message (if (zerop (process-exit-status process)) "Wrote %s" "dot failed on %s: %s")
                                           (if (zerop (process-exit-status process)) svg file)
                                           (with-current-buffer (process-buffer process)
                                             (string-trim (buffer-string))))
                                  (kill-buffer (process-buffer process))))))))

(defvar-keymap dbt-lineage-mode-map
  :doc "Keymap of `dbt-lineage-mode'."
  :parent special-mode-map
  "RET" #'dbt-lineage-visit
  "o" #'dbt-lineage-visit
  "+" #'dbt-lineage-deeper
  "-" #'dbt-lineage-shallower
  "e" #'dbt-lineage-export
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode dbt-lineage-mode special-mode "dbt lineage"
  "Major mode of the lineage of a dbt resource, a tree that TAB folds.
\\{dbt-lineage-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (funcall dbt-lineage--refresh))
              outline-regexp "^ *[^ \n]"
              outline-level (lambda () (1+ (/ (current-indentation) 2)))
              outline-minor-mode-cycle t)
  (outline-minor-mode 1))

(defun dbt-lineage-show (root id nodes description refresh)
  "Show the lineage of the node ID of ROOT among NODES.
ID is a unique id, such as model.shop.orders or, for a version,
model.shop.orders.v2.  DESCRIPTION says where NODES come from, for the
message.  REFRESH is a function of no argument that asks for the lineage
again, from where the nodes came.  A buffer that shows the lineage of ID
already keeps its depth."
  (let ((focus (or (seq-find (lambda (node) (equal (plist-get node :id) id)) nodes)
                   (user-error "No node %s" id))))
    (with-current-buffer (get-buffer-create (format "*dbt lineage: %s*" (dbt-lineage-label id)))
      (unless (derived-mode-p 'dbt-lineage-mode)
        (dbt-lineage-mode))
      (setq dbt-lineage--refresh refresh
            dbt-lineage--root root
            dbt-lineage--nodes nodes
            dbt-lineage--focus id
            dbt-lineage--reach (mapcar (lambda (direction) (cons direction (dbt-lineage-reach nodes id direction)))
                                       '(upstream downstream)))
      (dbt-lineage--render)
      (display-buffer (current-buffer))
      (message "Lineage of %s %s, from %s" (plist-get focus :type) (dbt-lineage-label id) description))))

(provide 'dbt-lineage)
;;; dbt-lineage.el ends here
