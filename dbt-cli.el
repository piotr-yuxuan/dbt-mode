;;; dbt-cli.el --- Run dbt's commands from a dbt buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `dbt-dispatch' is a transient menu of dbt's commands, run at the root
;; of the project in a compilation buffer whose errors link to the
;; files.  Its selection defaults to the resource of the current buffer,
;; and two switches add its upstream or its downstream.  `dbt-cli-show'
;; is what `dbt-preview' runs when no language server does.

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-rows)
(require 'transient)

(defconst dbt-cli-selecting-commands
  '("build" "run" "test" "compile" "show" "seed" "snapshot" "list" "format" "lint")
  "The commands of dbt that take a selection.")

(defconst dbt-cli-state-commands
  '("build" "run" "test" "seed" "snapshot" "compile" "show" "list" "lint" "format" "parse" "deps" "debug"
    "retry")
  "The commands of `dbt-dispatch' that take --defer, --state and --vars.
All of them but docs generate.")

(defconst dbt-cli-option-commands
  `(("--select" . ,dbt-cli-selecting-commands)
    ("--exclude" . ,dbt-cli-selecting-commands)
    ("--full-refresh" "build" "run" "compile" "seed")
    ("--limit" "build" "run" "test" "compile" "show" "list")
    ("--defer" . ,dbt-cli-state-commands)
    ("--state" . ,dbt-cli-state-commands)
    ("--vars" . ,dbt-cli-state-commands))
  "The options of `dbt-dispatch' that only some commands take, with these commands.
dbt 2.0.9 rejects --full-refresh and --limit on any other command, and
lists --defer, --state and --vars for every one but docs generate.  The
other options, --target and --fail-fast, go to every command.")

(defconst dbt-cli-error-regexp-alist
  ;; dbt 2 prints an error or a warning as a line "[error] [Name (dbtNNNN)]:
  ;; message", then, within a few lines that start no other, " -->
  ;; FILE:LINE:COLUMN", or " --> FILE" alone.  An error of the database
  ;; has no such line: its message ends with the SQL that ran, as
  ;; "(target/run/PACKAGE/PATH.sql)".
  '((dbt-location
     "^\\[\\(?:error\\|\\(warning\\)\\)\\][^\n]*\n\\(?:\\(?:[^[\n][^\n]*\\)?\n\\)\\{0,3\\}?[ \t]*--> \\([^:\n]+\\)\\(?::\\([0-9]+\\):\\([0-9]+\\)\\)?"
     2 3 4 (1))
    (dbt-database "^\\[error\\][^\n]*(\\([^()\n]+\\.sql\\))$" 1))
  "The errors and warnings of dbt's output.
For `compilation-error-regexp-alist'.")

(define-compilation-mode dbt-compilation-mode "dbt"
  "The output of a dbt command, whose errors link to their files."
  (setq-local compilation-error-regexp-alist-alist dbt-cli-error-regexp-alist
              compilation-error-regexp-alist (mapcar #'car dbt-cli-error-regexp-alist))
  ;; dbt show writes the header of its table in bold, colours or not.
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

(defun dbt-cli--save-project (root)
  "Offer to save the modified buffers of the project at ROOT, as `compile' does.
dbt reads the project's files, not its buffers."
  (save-some-buffers (not compilation-ask-about-save)
                     (lambda () (and buffer-file-name (string-prefix-p root (expand-file-name buffer-file-name))))))

(defun dbt-cli-run (command &optional arguments)
  "Run the dbt COMMAND with ARGUMENTS, a list of strings, in the project.
It runs at the root of the project of the current buffer, in a
compilation buffer of `dbt-compilation-mode'.  dbt runs without colours
or progress bars, which it would otherwise draw when Emacs passes on the
TERM of a terminal.  The project's modified buffers are offered for
saving first, under `compilation-ask-about-save'."
  (let* ((root (or (dbt-project-root) (user-error "Not in a dbt project")))
         (default-directory root)
         (compilation-environment (append '("NO_COLOR=1" "TERM=dumb") compilation-environment)))
    (dbt-cli--save-project root)
    (compilation-start (mapconcat #'shell-quote-argument (append (list dbt-executable) (split-string command)
                                                                 arguments)
                                  " ")
                       #'dbt-compilation-mode
                       (lambda (_mode) (format "*dbt %s: %s*" (dbt-project-name root) command)))))

(defun dbt-cli-show (name sql &optional limit rerun)
  "Show the rows of the query SQL, as NAME, or else of the resource NAME.
At most LIMIT rows, `dbt-preview-limit' by default, in the grid of
`dbt-rows-show'.  dbt prints them as JSON; when it fails, its output
shows in a buffer whose errors link to their files.  The project's
modified buffers are offered for saving first.  RERUN, a function of a
limit, asks for the rows again from their grid; by default, the same
query."
  (let* ((root (or (dbt-project-root) (user-error "Not in a dbt project")))
         (default-directory root)
         (limit (or limit dbt-preview-limit))
         (process-environment (append '("NO_COLOR=1" "TERM=dumb") process-environment))
         ;; Before any buffer or process exists, which a refusal would leave.
         (executable (progn (dbt-cli--save-project root) (dbt-project-executable)))
         (output (generate-new-buffer " *dbt show*"))
         (errors (make-pipe-process :name "dbt show errors" :buffer (generate-new-buffer " *dbt show errors*")
                                    :noquery t :sentinel #'ignore)))
    (message "dbt show runs for %s..." name)
    (make-process
     :name "dbt show" :buffer output :stderr errors :noquery t :connection-type 'pipe
     :command `(,executable "show" "--quiet" "--output" "json" "--limit" ,(number-to-string limit)
                ,@(if sql (list "--inline" sql) (list "--select" name)))
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (unwind-protect
             (let ((rows (and (zerop (process-exit-status process))
                              (dbt-rows-read-json (with-current-buffer output (buffer-string))))))
               (cond ((not rows)
                      ;; The errors may still be on their way.
                      (while (accept-process-output errors 0.05 nil t))
                      (with-current-buffer (get-buffer-create (format "*dbt show: %s*" name))
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert-buffer-substring output)
                          (insert-buffer-substring (process-buffer errors)))
                        (setq default-directory root)
                        (dbt-compilation-mode)
                        (display-buffer (current-buffer))))
                     (t (dbt-rows-show name (car rows) (cdr rows) limit
                                       (or rerun (lambda (limit)
                                                   (let ((default-directory root))
                                                     (dbt-cli-show name sql limit))))))))
           (kill-buffer output)
           (kill-buffer (process-buffer errors))
           (delete-process errors)))))))

(defun dbt-cli-preview-cte (name &optional limit)
  "Show the rows of the CTE NAME of the current model, at most LIMIT of them.
`dbt show' runs the query of `dbt-mode-cte-query', Jinja and all.  Asked
for again from its grid, the query is taken again from the model's file."
  (let ((file buffer-file-name))
    (dbt-cli-show (format "%s/%s" (file-name-base file) name) (cdr (dbt-mode-cte-query name)) limit
                  (lambda (limit)
                    (with-current-buffer (find-file-noselect file)
                      (dbt-cli-preview-cte name limit))))))

(defun dbt-cli-arguments (arguments)
  "Return the command-line arguments that the transient ARGUMENTS mean.
The switches --upstream and --downstream are `dbt-dispatch''s own: they
add a `+' before or after the selection."
  (let* ((upstream (member "--upstream" arguments))
         (downstream (member "--downstream" arguments)))
    (mapcan (lambda (argument)
              (cond ((member argument '("--upstream" "--downstream")) nil)
                    ((string-match "\\`--select=\\(.*\\)\\'" argument)
                     (list "--select" (concat (if upstream "+" "") (match-string 1 argument)
                                              (if downstream "+" ""))))
                    ((string-match "\\`\\(--[a-z-]+\\)=\\(.*\\)\\'" argument)
                     (list (match-string 1 argument) (match-string 2 argument)))
                    (t (list argument))))
            arguments)))

(defun dbt-cli-dispatch (command)
  "Run the dbt COMMAND with those arguments of `dbt-dispatch' that it takes.
See `dbt-cli-option-commands'."
  (dbt-cli-run command
               (dbt-cli-arguments
                (seq-filter (lambda (argument)
                              (let ((commands (assoc (car (split-string argument "="))
                                                     dbt-cli-option-commands)))
                                (or (not commands) (member command (cdr commands)))))
                            (transient-args 'dbt-dispatch)))))

;;;###autoload (autoload 'dbt-dispatch "dbt-cli" nil t)
(transient-define-prefix dbt-dispatch ()
  "Run a dbt command on the project, by default on the current model."
  ["Selection"
   ("-s" "Select" "--select="
    :init-value (lambda (option)
                  (oset option value (and (dbt-project-root) (dbt-mode-selection-at-point)))))
   ("-u" "With its upstream" "--upstream")
   ("-d" "With its downstream" "--downstream")
   ("-x" "Exclude" "--exclude=")]
  ["Options"
   ("-t" "Target" "--target=")
   ("-v" "Variables" "--vars=")
   ("-S" "State" "--state=" :reader transient-read-existing-directory)
   ("-e" "Defer to the state" "--defer")
   ("-f" "Full refresh" "--full-refresh")
   ("-F" "Fail fast" "--fail-fast")
   ("-l" "Limit of show" "--limit=")]
  [["Build"
    ("b" "build" (lambda () (interactive) (dbt-cli-dispatch "build")))
    ("r" "run" (lambda () (interactive) (dbt-cli-dispatch "run")))
    ("t" "test" (lambda () (interactive) (dbt-cli-dispatch "test")))
    ("S" "seed" (lambda () (interactive) (dbt-cli-dispatch "seed")))
    ("n" "snapshot" (lambda () (interactive) (dbt-cli-dispatch "snapshot")))]
   ["Inspect"
    ("c" "compile" (lambda () (interactive) (dbt-cli-dispatch "compile")))
    ("s" "show" (lambda () (interactive) (dbt-cli-dispatch "show")))
    ("l" "list" (lambda () (interactive) (dbt-cli-dispatch "list")))
    ("L" "lint" (lambda () (interactive) (dbt-cli-dispatch "lint")))
    ("=" "format" (lambda () (interactive) (dbt-cli-dispatch "format")))]
   ["Project"
    ("p" "parse" (lambda () (interactive) (dbt-cli-dispatch "parse")))
    ("d" "deps" (lambda () (interactive) (dbt-cli-dispatch "deps")))
    ("D" "debug" (lambda () (interactive) (dbt-cli-dispatch "debug")))
    ("R" "retry" (lambda () (interactive) (dbt-cli-dispatch "retry")))
    ("g" "docs generate" (lambda () (interactive) (dbt-cli-dispatch "docs generate")))]])

(provide 'dbt-cli)
;;; dbt-cli.el ends here
