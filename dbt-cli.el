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
(require 'transient)

(defconst dbt-cli-selecting-commands
  '("build" "run" "test" "compile" "show" "seed" "snapshot" "list" "format" "lint")
  "The commands of dbt that take a selection.")

(defconst dbt-cli-option-commands
  `(("--select" . ,dbt-cli-selecting-commands)
    ("--exclude" . ,dbt-cli-selecting-commands)
    ("--full-refresh" "build" "run" "compile" "seed")
    ("--limit" "build" "run" "test" "compile" "show" "list"))
  "The options of `dbt-dispatch' that only some commands take, with these commands.
dbt 2.0.9 rejects --full-refresh and --limit on any other command.  The
other options, --target and --fail-fast, go to every command.")

(defconst dbt-cli-error-regexp-alist
  ;; dbt 2 prints an error or a warning as a line "[error] [Name (dbtNNNN)]:
  ;; message", then, within a few lines that start no other, " -->
  ;; FILE:LINE:COLUMN", or " --> FILE" alone.
  '((dbt-location
     "^\\[\\(?:error\\|\\(warning\\)\\)\\][^\n]*\n\\(?:\\(?:[^[\n][^\n]*\\)?\n\\)\\{0,3\\}?[ \t]*--> \\([^:\n]+\\)\\(?::\\([0-9]+\\):\\([0-9]+\\)\\)?"
     2 3 4 (1)))
  "The errors and warnings of dbt's output.
For `compilation-error-regexp-alist'.")

(define-compilation-mode dbt-compilation-mode "dbt"
  "The output of a dbt command, whose errors link to their files."
  (setq-local compilation-error-regexp-alist-alist dbt-cli-error-regexp-alist
              compilation-error-regexp-alist (mapcar #'car dbt-cli-error-regexp-alist))
  ;; dbt show writes the header of its table in bold, colours or not.
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

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
    ;; dbt reads the project's files, not its buffers: offer to save them,
    ;; as `compile' does.
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda () (and buffer-file-name (string-prefix-p root (expand-file-name buffer-file-name)))))
    (compilation-start (mapconcat #'shell-quote-argument (append (list dbt-executable) (split-string command)
                                                                 arguments)
                                  " ")
                       #'dbt-compilation-mode
                       (lambda (_mode) (format "*dbt %s: %s*" (dbt-project-name root) command)))))

(defun dbt-cli-show (sql name)
  "Run `dbt show' on the query SQL, or else on the resource NAME.
Show at most `dbt-preview-limit' rows."
  (dbt-cli-run "show" (append (if sql (list "--inline" sql) (list "--select" name))
                              (list "--limit" (number-to-string dbt-preview-limit)))))

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
                  (oset option value (when-let* (((dbt-project-root))
                                                 (resource (dbt-mode-current-resource)))
                                       (dbt-mode-resource-selector resource)))))
   ("-u" "With its upstream" "--upstream")
   ("-d" "With its downstream" "--downstream")
   ("-x" "Exclude" "--exclude=")]
  ["Options"
   ("-t" "Target" "--target=")
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
