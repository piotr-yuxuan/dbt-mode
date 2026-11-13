;;; dbt-mode-test-helpers.el --- What the tests of dbt-mode share  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; A copy of the fixture project for each test, visiting its files, and
;; waiting for what an asynchronous process does.

;;; Code:

(require 'ert)

(defconst dbt-test-directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The directory tests/.")

(defconst dbt-test-fixture (expand-file-name "fixtures/shop/" dbt-test-directory)
  "The fixture project, which the tests copy and never change.")

(defmacro dbt-test-with-project (root &rest body)
  "Run BODY with ROOT bound to the root of a fresh copy of the fixture project.
The copy and the buffers that visit it are removed afterwards."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((,root (file-name-as-directory (make-temp-file "dbt-project" t))))
     (unwind-protect
         (progn
           (copy-directory dbt-test-fixture ,root nil nil t)
           ,@body)
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer) (string-prefix-p ,root (buffer-file-name buffer)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory ,root t))))

(defun dbt-test-visit (root file)
  "Visit FILE, relative to ROOT, and make its buffer current."
  (set-buffer (find-file-noselect (expand-file-name file root))))

(defun dbt-test-file-text (root file)
  "Return the contents of FILE, relative to ROOT, as saved on disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file root))
    (buffer-string)))

(defun dbt-test-goto (text &optional offset)
  "Move point to the first occurrence of TEXT, then OFFSET characters on."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defun dbt-test-wait (predicate &optional seconds)
  "Wait until PREDICATE returns non-nil, and return its value.
Fail the test after SECONDS, 30 by default, while processes run."
  (let ((deadline (+ (float-time) (or seconds 30)))
        value)
    (while (and (not (setq value (funcall predicate))) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (or value (ert-fail (list "Timed out waiting for" predicate)))))

(defun dbt-test-lineage-rows ()
  "Return the rows of the lineage tree of the current buffer, in its order.
Each is (DIRECTION DEPTH TYPE NAME FILE), strings, DEPTH that of its
indentation, FILE empty when the node has none."
  (save-excursion
    (goto-char (point-min))
    (let (direction rows)
      (while (not (eobp))
        (let ((text (string-trim (buffer-substring-no-properties (point) (line-end-position)))))
          (if (not (get-text-property (point) 'dbt-lineage-node))
              (setq direction text)
            (let ((parts (split-string text "  ")))
              (push (list direction (number-to-string (/ (current-indentation) 2))
                          (nth 1 parts) (nth 0 parts) (or (nth 2 parts) ""))
                    rows))))
        (forward-line 1))
      (nreverse rows))))

(provide 'dbt-mode-test-helpers)
;;; dbt-mode-test-helpers.el ends here
