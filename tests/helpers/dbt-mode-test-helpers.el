;;; dbt-mode-test-helpers.el --- What the tests of dbt-mode share  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; A copy of the fixture project for each test, visiting its files, and
;; waiting for what an asynchronous process does; for the tests of
;; scripts/, child processes with a deadline and scratch git
;; repositories.

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

(defun dbt-test-write-state (root)
  "Write a state of ROOT's artifacts, and return its directory.
Its manifest gives the model orders the relation \"db\".\"prod\".\"orders\"."
  (let ((state (expand-file-name "prod-state/" root)))
    (make-directory state)
    (write-region "{\"nodes\": {\"model.shop.orders\": {\"relation_name\": \"\\\"db\\\".\\\"prod\\\".\\\"orders\\\"\"}}}"
                  nil (expand-file-name "manifest.json" state))
    state))

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

(defun dbt-test-mode-line (construct)
  "Return the text of the mode line CONSTRUCT, as redisplay draws it.
It is read in the current buffer, since `format-mode-line' gives an
empty string in batch.  This reads what the
header lines of the tests hold: a string, whose `%%' is `%', a list of
constructs, (:eval FORM), (SYMBOL THEN ELSE), and a symbol other than
nil and t, by its value; anything else, nil included, is empty.  Any
other %-construct is an error, as redisplay would not show it as
written."
  (pcase construct
    ((pred stringp)
     (when (string-search "%" (string-replace "%%" "" construct))
       (error "No %%-construct but %%%% is read here: %S" construct))
     (string-replace "%%" "%" construct))
    (`(:eval ,form) (dbt-test-mode-line (eval form t)))
    (`(,(and (pred symbolp) symbol) ,then . ,else)
     (dbt-test-mode-line (if (symbol-value symbol) then (car else))))
    ((pred consp) (mapconcat #'dbt-test-mode-line construct))
    ((guard (and (symbolp construct) (not (booleanp construct))))
     (dbt-test-mode-line (symbol-value construct)))
    (_ "")))

(defun dbt-test-run (program arguments &optional environment directory)
  "Run PROGRAM with ARGUMENTS and return (STATUS . OUTPUT).
OUTPUT holds standard output and standard error.  ENVIRONMENT, a
list of \"NAME=VALUE\" strings, is added to the environment; the
working directory is DIRECTORY, or the repository's root.  The
child is killed after 120 seconds, and STATUS is then the symbol
`timeout'."
  (let* ((process-environment (append environment process-environment))
         (default-directory (file-name-as-directory
                             (or directory (expand-file-name ".." dbt-test-directory))))
         (deadline (+ (float-time) 120))
         (process (make-process :name "dbt-test" :buffer (generate-new-buffer " *dbt-test*")
                                :command (cons program arguments)
                                :connection-type 'pipe :noquery t :sentinel #'ignore)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.1))
    (when (process-live-p process)
      (delete-process process))
    (while (accept-process-output process 0 nil t))
    (prog1 (cons (if (eq (process-status process) 'exit)
                     (process-exit-status process)
                   'timeout)
                 (with-current-buffer (process-buffer process) (buffer-string)))
      (kill-buffer (process-buffer process)))))

(defun dbt-test-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY; signal an error unless it succeeds.
The tests run in a scratch home, whose git has no configuration but
the identity tests@example.invalid."
  (let ((result (dbt-test-run "git" arguments nil directory)))
    (unless (equal (car result) 0)
      (error "Command git %s failed: %s" (mapconcat #'identity arguments " ") (cdr result)))
    (cdr result)))

(defun dbt-test-git-tree (files)
  "Make a scratch git repository tracking FILES and return its directory.
FILES is an alist of relative file names and their contents; they are
added, not committed."
  (let ((directory (file-name-as-directory (make-temp-file "repo-test" t))))
    (pcase-dolist (`(,name . ,contents) files)
      (let ((file (expand-file-name name directory)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert contents))))
    (dbt-test-git directory "init" "-q" "-b" "main")
    (dbt-test-git directory "add" "-A")
    directory))

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
