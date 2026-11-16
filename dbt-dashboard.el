;;; dbt-dashboard.el --- The state of a dbt project, from its artifacts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-dashboard' shows what your own runs left in the target path,
;; read and never written: the nodes that failed, errored, warned or were
;; skipped in run_results.json, with dbt's message; the slowest of them;
;; the sources that sources.json says are stale, then where to see a
;; check that could not run, which dbt 2 leaves out of it.  Each section
;; says which command wrote its file, how many results it holds, and
;; when.  RET visits a node's
;; file, TAB folds a section, g reads the files again, and f runs `dbt
;; source freshness', then reads them again once it ends.

;;; Code:

(require 'dbt-project)
(require 'dbt-lineage)
(require 'outline)

(declare-function dbt-cli-run "dbt-cli")

(defconst dbt-dashboard-slowest 10
  "The number of the slowest nodes that the dashboard lists.")

(defvar-local dbt-dashboard--root nil
  "The root of the project of this dashboard.")

(defun dbt-dashboard--age (seconds)
  "Return SECONDS as an age a reader takes in at a glance, as \"3 days\"."
  (or (seq-some (pcase-lambda (`(,unit . ,name))
                  (and (>= seconds unit)
                       (format "%d %s%s" (/ seconds unit) name (if (< seconds (* 2 unit)) "" "s"))))
                '((86400 . "day") (3600 . "hour") (60 . "minute") (1 . "second")))
      "0 seconds"))

(defun dbt-dashboard--read (file)
  "Return the results of the artifact FILE and the command that wrote it.
The value is (RESULTS . COMMAND): RESULTS its objects of results, as
hash tables, COMMAND its args.which, or nil.  Nil when FILE cannot be
read or holds no array of results; an empty array gives (nil . COMMAND).
The artifacts are dbt's, read as they are."
  (let ((json (ignore-errors (with-temp-buffer
                               (insert-file-contents file)
                               (json-parse-buffer :null-object nil)))))
    (when (and (hash-table-p json) (vectorp (gethash "results" json)))
      (cons (seq-filter #'hash-table-p (gethash "results" json))
            (let ((args (gethash "args" json)))
              (and (hash-table-p args) (stringp (gethash "which" args)) (gethash "which" args)))))))

(defun dbt-dashboard--section (title file artifact lines)
  "Insert the section TITLE of FILE, and LINES.
ARTIFACT is FILE as `dbt-dashboard--read' read it.  Each of LINES is a
pair (TEXT . ID): ID, a unique id, is what RET visits, or nil for a line
that names no node.  The heading says how many results FILE holds and
how long ago dbt wrote it, none included, or else, without ARTIFACT,
that it holds no results that can be read."
  (insert (propertize title 'face 'bold)
          (if artifact
              (format ", from %s, %d result%s, written %s ago\n" (file-name-nondirectory file)
                      (length (car artifact)) (if (= (length (car artifact)) 1) "" "s")
                      (dbt-dashboard--age (max 0 (round (float-time (time-since (file-attribute-modification-time
                                                                                (file-attributes file))))))))
            (format ": no results in %s\n" (file-name-nondirectory file))))
  (pcase-dolist (`(,text . ,id) lines)
    (insert (propertize (concat "  " text) 'dbt-dashboard-node id) "\n"))
  (insert "\n"))

(defconst dbt-dashboard-severities '("error" "runtime error" "fail" "warn" "skipped")
  "The statuses of a node that the dashboard lists, the most severe first.")

(defun dbt-dashboard--render ()
  "Write the dashboard of this buffer's project from its artifacts."
  (let* ((inhibit-read-only t)
         (target (dbt-project-target dbt-dashboard--root))
         (runs-file (expand-file-name "run_results.json" target))
         (sources-file (expand-file-name "sources.json" target))
         (runs (dbt-dashboard--read runs-file))
         (sources (dbt-dashboard--read sources-file))
         (status (lambda (result) (downcase (format "%s" (gethash "status" result)))))
         (name (lambda (result) (dbt-lineage-label (format "%s" (gethash "unique_id" result)))))
         ;; dbt's message, to its cause: a database error says it second.
         (said (lambda (result key)
                 (string-join (seq-take (split-string (format "%s" (or (gethash key result) "")) "\n" t "[ \t]+") 2)
                              ": "))))
    (erase-buffer)
    (dbt-dashboard--section
     (format "Failed, warned or skipped in dbt %s" (or (cdr runs) "runs")) runs-file runs
     (mapcar (lambda (result)
               (cons (format "%-7s %s  %s%s" (funcall status result) (funcall name result)
                             (funcall said result "message")
                             (if (natnump (gethash "failures" result))
                                 (format ", %d failing rows" (gethash "failures" result))
                               ""))
                     (gethash "unique_id" result)))
             (seq-sort-by (lambda (result) (seq-position dbt-dashboard-severities (funcall status result))) #'<
                          (seq-filter (lambda (result) (member (funcall status result) dbt-dashboard-severities))
                                      (car runs)))))
    (dbt-dashboard--section
     "Slowest nodes" runs-file runs
     (mapcar (lambda (result)
               (cons (format "%7.2f s  %s  %s" (gethash "execution_time" result) (funcall name result)
                             (funcall status result))
                     (gethash "unique_id" result)))
             (seq-take (seq-sort-by (lambda (result) (gethash "execution_time" result)) #'>
                                    (seq-filter (lambda (result) (numberp (gethash "execution_time" result)))
                                                (car runs)))
                       dbt-dashboard-slowest)))
    (dbt-dashboard--section
     "Stale sources, of dbt source freshness" sources-file sources
     (append
      (mapcar (lambda (result)
                (cons (format "%-7s %s  %s" (funcall status result) (funcall name result)
                              (if (stringp (gethash "max_loaded_at" result))
                                  (format "loaded %s, %s ago" (gethash "max_loaded_at" result)
                                          (dbt-dashboard--age
                                           (round (let ((age (gethash "max_loaded_at_time_ago_in_s" result)))
                                                    (if (numberp age) (max 0 age) 0)))))
                                ;; A check that could not run has dbt's error
                                ;; instead, as dbt 1.x writes it.
                                (funcall said result (if (gethash "error" result) "error" "message"))))
                      (gethash "unique_id" result)))
              (seq-filter (lambda (result) (member (funcall status result) '("warn" "error" "runtime error")))
                          (car sources)))
      ;; dbt 2 writes no result for a check that could not run (probed
      ;; on dbt-oss 2.0.11 and the proprietary 2.0.9).
      (list (cons (propertize "dbt 2 leaves out a check that could not run: see dbt's output or logs/dbt.log." 'face 'shadow)
                  nil))))
    (goto-char (point-min))))

(defun dbt-dashboard-visit ()
  "Visit the file of the node on point's line, as the manifest gives it."
  (interactive)
  (let* ((id (or (get-text-property (line-beginning-position) 'dbt-dashboard-node) (user-error "No node here")))
         (node (dbt-project-manifest-node (expand-file-name "manifest.json" (dbt-project-target dbt-dashboard--root))
                                          id))
         (directory (nth 1 (assoc (and (hash-table-p node) (gethash "package_name" node))
                                  (dbt-project-packages dbt-dashboard--root))))
         (file (and directory (stringp (gethash "original_file_path" node))
                    (expand-file-name (gethash "original_file_path" node) directory))))
    (unless (and file (file-exists-p file))
      (user-error "No file of %s in the manifest" id))
    (find-file-other-window file)))

(defun dbt-dashboard-freshness ()
  "Run `dbt source freshness' on the project, then read the dashboard again.
It runs on the default target, which a protected target asks first, as
`dbt-project-confirm-target' says."
  (interactive)
  (require 'dbt-cli)
  (let ((dashboard (current-buffer))
        (label (dbt-project-confirm-target dbt-dashboard--root nil)))
    (with-current-buffer (dbt-cli-run "source freshness")
      (add-hook 'compilation-finish-functions
                (lambda (_buffer _status)
                  (when (buffer-live-p dashboard)
                    (with-current-buffer dashboard
                      (dbt-dashboard--render))))
                nil t))
    (message "dbt source freshness runs on %s" label)))

(defvar-keymap dbt-dashboard-mode-map
  :doc "Keymap of `dbt-dashboard-mode'."
  :parent special-mode-map
  "RET" #'dbt-dashboard-visit
  "f" #'dbt-dashboard-freshness
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode dbt-dashboard-mode special-mode "dbt dashboard"
  "Major mode of the dashboard of a dbt project, whose sections TAB folds.
\\{dbt-dashboard-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (dbt-dashboard--render))
              outline-regexp "[^ \n]"
              outline-minor-mode-cycle t)
  (outline-minor-mode 1))

;;;###autoload
(defun dbt-dashboard ()
  "Show the state of the current project, from the artifacts of your runs.
The nodes that failed, errored, warned or were skipped in
target/run_results.json, with dbt's message, the slowest nodes, and the
sources that target/sources.json says are stale, then where to see a
check that could not run, which dbt 2 leaves out of it; each section
says how many results its file holds, and when dbt wrote it.  See
`dbt-dashboard-mode'."
  (interactive)
  (let ((root (or (dbt-project-root) (user-error "Not in a dbt project"))))
    (with-current-buffer (get-buffer-create (format "*dbt dashboard: %s*" (dbt-project-name root)))
      (unless (derived-mode-p 'dbt-dashboard-mode)
        (dbt-dashboard-mode))
      (setq dbt-dashboard--root root
            default-directory root)
      (dbt-dashboard--render)
      (pop-to-buffer (current-buffer)))))

(provide 'dbt-dashboard)
;;; dbt-dashboard.el ends here
