;;; dbt-lsp.el --- The client of dbt lsp for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; dbt 2.0 and later run a language server, `dbt lsp', on the standard
;; input and output.  This client registers it with lsp-mode for
;; `dbt-mode' buffers, which then get from it what lsp-mode offers:
;; definitions and references of models and macros, hover with a model's
;; parents, children and columns, completion, diagnostics, semantic
;; tokens, and code lenses that preview a CTE.  Column-level features
;; need a dbt account; the server says nothing about them otherwise.
;;
;; What lsp-mode does not do by itself, this client does with the
;; server's own requests and commands:
;;
;; - `dbt-lsp-rename-model' renames the file of a model, which has the
;;   server edit the uses it knows of: lsp-mode asks the server, by
;;   workspace/willRenameFiles, whenever a file is renamed;
;; - `dbt-lsp-compile-file' shows the compiled SQL (dbt.compileFile);
;; - `dbt-lsp-show' shows the rows of the model, or of a query
;;   (dbt.show); `dbt-lsp-preview-cte', which the code lenses "Preview
;;   CTE" run, those of a CTE of the compiled SQL;
;; - `dbt-lsp-lineage' shows the lineage that the server computes
;;   (dbt.listNodes);
;; - `dbt-lsp-format-buffer' formats the buffer.  The server formats the
;;   file on disk, not the buffer, so its edits would corrupt unsaved
;;   changes: this client hides formatting from lsp-mode, and this
;;   command saves the buffer first;
;; - `dbt-lsp-fix-all' fixes the lint issues of the buffer and formats it,
;;   and saves it first likewise.
;;
;; `dbt-lsp-deferred', for `dbt-mode-hook', makes the root of the dbt
;; project a folder of the lsp session before lsp-mode starts, so that
;; the server runs on the project, not on an enclosing repository; `dbt-lsp'
;; does the same at once.

;;; Code:

(require 'dbt-lineage)
(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-rows)
(require 'lsp-mode)
(require 'map)

(defcustom dbt-lsp-arguments nil
  "More arguments of `dbt lsp', such as (\"--profiles-dir\" \"/path\")."
  :type '(repeat string)
  :group 'dbt)

(defcustom dbt-lsp-proxy nil
  "Whether dbt lsp runs through dbt-lsp-plus, when that executable exists.
dbt-lsp-plus adds what dbt lsp lacks, such as the references of sources
over the project; dbt-mode steps aside where it says that it answers."
  :type 'boolean
  :group 'dbt)

(defcustom dbt-lsp-enable-linter nil
  "Whether the server lints the SQL, with dbt's built-in linter."
  :type 'boolean
  :group 'dbt)

(defvar dbt-lsp--servers (make-hash-table :test #'equal)
  "From the file name of a dbt executable to whether it runs `dbt lsp'.
The value is `yes' or `no'.")

(defvar dbt-lsp--probe-seconds 10
  "The seconds that dbt has to say whether it runs `dbt lsp'.")

(defun dbt-lsp-server-p ()
  "Return non-nil when `dbt-executable' runs a language server.
Only dbt 2.0 or later does; dbt Core 1.x has no `dbt lsp'.  The answer
is kept for each executable, whichever it is.  A dbt that does not
answer within `dbt-lsp--probe-seconds' runs none."
  (when-let* ((executable (executable-find dbt-executable)))
    (eq 'yes
        (with-memoization (gethash executable dbt-lsp--servers)
          (let ((process (ignore-errors
                           (make-process :name "dbt lsp --help" :command (list executable "lsp" "--help")
                                         :connection-type 'pipe :noquery t)))
                (deadline (+ (float-time) dbt-lsp--probe-seconds)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.1))
            (prog1 (if (and process (eq (process-status process) 'exit) (eq 0 (process-exit-status process)))
                       'yes
                     'no)
              (when process (delete-process process))))))))

(defun dbt-lsp--command ()
  "Return the command line of the server, for the workspace being started.
lsp-mode calls it with `default-directory' at the root of the workspace,
in the buffer that starts it.  With `dbt-lsp-proxy', dbt-lsp-plus runs
it, as `dbt-lsp-plus -- COMMAND'."
  (append (and dbt-lsp-proxy (executable-find "dbt-lsp-plus") (list "dbt-lsp-plus" "--"))
          (list dbt-executable "lsp" "--project-dir"
                (directory-file-name (or (and buffer-file-name (dbt-project-root buffer-file-name))
                                         (dbt-project-root default-directory)
                                         default-directory)))
          (and dbt-lsp-enable-linter (list "--lint-enabled" "true"))
          dbt-lsp-arguments))

(defun dbt-lsp-workspace ()
  "Return the workspace of dbt lsp of the current buffer, or nil."
  (and (bound-and-true-p lsp-mode)
       (seq-find (lambda (workspace)
                   (eq (lsp--client-server-id (lsp--workspace-client workspace)) 'dbt-lsp))
                 (lsp-workspaces))))

(defun dbt-lsp--hide-formatting (workspace)
  "Hide the formatting of the server of WORKSPACE from lsp-mode.
The server formats the file on disk rather than the buffer, so lsp-mode
would apply its edits to unsaved text; `dbt-lsp-format-buffer' saves
first.  Its code actions go too: it has one, which fixes lint issues
and formats, also from the file on disk; `dbt-lsp-fix-all' saves first."
  ;; A key with a nil value would still mean support to lsp-mode: the keys
  ;; go.  They are strings in a hash table, keywords in a plist.
  (let ((capabilities (lsp--workspace-server-capabilities workspace)))
    (dolist (key '(:documentFormattingProvider :documentRangeFormattingProvider :codeActionProvider))
      (setq capabilities (map-delete capabilities (if (hash-table-p capabilities) (lsp-keyword->string key) key))))
    (setf (lsp--workspace-server-capabilities workspace) capabilities)))

(defun dbt-lsp--library-folders (_workspace)
  "Return the folders whose files belong to the workspace of their project.
They are the packages that dbt installs, opened read-only."
  (when-let* ((root (dbt-project-root)))
    (list (expand-file-name (plist-get (dbt-project-config root) :packages-install-path) root)
          (expand-file-name "dbt_internal_packages" root))))

;;;###autoload
(defun dbt-lsp-add-project-folder ()
  "Make the root of the current dbt project a folder of the lsp session.
Unless the session has it already, or blocks it, or `dbt-executable'
runs no server, as the session keeps its folders.  lsp-mode then runs
the server on the project, not on an enclosing folder of the session or
on the repository it guesses: `lsp-auto-guess-root' is turned off in
the buffer, as lsp-mode would guess before it looks at the folders."
  (when-let* ((root (and buffer-file-name (dbt-project-root)))
              ((dbt-lsp-server-p))
              (folder (lsp-f-canonical root))
              (session (lsp-session)))
    (unless (seq-some (lambda (blocked) (lsp-f-ancestor-of? blocked folder))
                      (lsp-session-folders-blocklist session))
      (unless (member folder (lsp-session-folders session))
        (lsp-workspace-folders-add folder))
      (setq-local lsp-auto-guess-root nil))))

;;;###autoload
(defun dbt-lsp ()
  "Start dbt lsp for the current buffer, on the root of its dbt project."
  (interactive)
  (dbt-lsp-add-project-folder)
  (lsp))

;;;###autoload
(defun dbt-lsp-deferred ()
  "Start dbt lsp for the current buffer once it is shown.
On the root of its dbt project; for `dbt-mode-hook'."
  (dbt-lsp-add-project-folder)
  (lsp-deferred))

;;;; Commands of the server

(defun dbt-lsp--conforms-p (value schema)
  "Return non-nil when SCHEMA fits VALUE, read from an answer of dbt lsp.
SCHEMA is a predicate, such as `stringp' or `null'; (or SCHEMA...), when
one SCHEMA fits; (array SCHEMA), a vector of values that SCHEMA fits; or
\(object (KEY SCHEMA)...), a JSON object whose KEY, a keyword, holds a
value that its SCHEMA fits, a missing key holding null."
  (pcase schema
    (`(or . ,schemas) (seq-some (lambda (schema) (dbt-lsp--conforms-p value schema)) schemas))
    (`(array ,item) (and (vectorp value) (seq-every-p (lambda (element) (dbt-lsp--conforms-p element item)) value)))
    (`(object . ,fields)
     (and (or (hash-table-p value) (and lsp-use-plists (consp value)))
          (seq-every-p (pcase-lambda (`(,key ,field)) (dbt-lsp--conforms-p (lsp-get value key) field)) fields)))
    (_ (funcall schema value))))

(defun dbt-lsp-proxy-answers-p (request kind)
  "Return non-nil when the server of the buffer answers REQUEST on targets of KIND.
REQUEST is :references or :rename and KIND \"source\" or \"cte\", as
dbt-lsp-plus lists them under experimental.dbtLspPlus of its
capabilities: dbt-mode then steps aside."
  (let* ((workspace (dbt-lsp-workspace))
         (capabilities (and workspace (lsp--workspace-server-capabilities workspace))))
    (and (dbt-lsp--conforms-p capabilities `(object (:experimental (object (:dbtLspPlus (object (,request (array stringp))))))))
         (seq-contains-p (lsp-get (lsp-get (lsp-get capabilities :experimental) :dbtLspPlus) request) kind))))

(defun dbt-lsp--unreadable (method answer)
  "Return the message that dbt-mode cannot read ANSWER, of dbt lsp to METHOD."
  (format "dbt lsp: %s answered what dbt-mode cannot read: %s"
          method (truncate-string-to-width (json-encode answer) 200 nil nil "...")))

(defun dbt-lsp--execute (command arguments schema callback)
  "Ask the server to run COMMAND with ARGUMENTS, a list, and call CALLBACK.
CALLBACK receives the answer once the server gives it, if SCHEMA fits it,
as `dbt-lsp--conforms-p' tells.  Otherwise, or when the answer is null
or holds an error, a message says so."
  ;; From a buffer the server does not serve, such as that of the
  ;; lineage, the server of the project.
  (let ((workspace (or (dbt-lsp-workspace)
                       (lsp-find-workspace 'dbt-lsp (or buffer-file-name default-directory))
                       (user-error "No dbt lsp runs on this project"))))
    (with-lsp-workspace workspace
      (lsp-request-async "workspace/executeCommand"
                         (list :command command :arguments (vconcat arguments))
                         (lambda (answer)
                           (cond ((null answer) (message "dbt lsp: %s answered nothing" command))
                                 ((dbt-lsp--conforms-p answer '(object (:error stringp)))
                                  (message "dbt lsp: %s failed: %s" command (lsp-get answer :error)))
                                 ((dbt-lsp--conforms-p answer schema) (funcall callback answer))
                                 (t (message "%s" (dbt-lsp--unreadable command answer)))))
                         :mode 'detached
                         :error-handler (lambda (error)
                                          (message "dbt lsp: %s failed: %s" command (lsp-get error :message)))))))

(defun dbt-lsp-compiled (callback)
  "Save the current buffer, have the server compile it, then call CALLBACK.
CALLBACK runs with no argument in the buffer of the compiled SQL, which
is read-only.  The server compiles the file on disk, hence the save."
  (when (buffer-modified-p)
    (save-buffer))
  (dbt-lsp--execute
   "dbt.compileFile" (list (lsp--buffer-uri)) '(object (:file_uri stringp))
   (lambda (result)
     ;; The file changes at each compilation: no question, as it is read again.
     (with-current-buffer (find-file-noselect (lsp--uri-to-path (lsp-get result :file_uri)) t)
       (revert-buffer t t t)
       (read-only-mode 1)
       (funcall callback)))))

(defun dbt-lsp-compile-file ()
  "Show the SQL that the current model compiles to, in another window.
The buffer is saved first, as the server compiles the file on disk."
  (interactive)
  (dbt-lsp-compiled (lambda () (display-buffer (current-buffer)))))

(defun dbt-lsp-show (&optional sql title limit rerun)
  "Show the rows of the current model, or of the query SQL, as TITLE.
SQL may hold Jinja.  At most LIMIT rows, `dbt-preview-limit' by default.
TITLE defaults to the name of the model, or \"query\".  Without SQL, the
buffer is saved first, as the server reads the model on disk.  A nested
value shows as JSON.  JSON false reads as null in lsp-mode, so it shows
as NULL.  RERUN, a function of a limit, asks for the rows again from
their grid; by default, the same query from the model's file."
  (interactive)
  (unless sql (save-buffer))
  (let* ((file buffer-file-name)
         (title (or title (if sql "query" (file-name-base file))))
         (limit (or limit dbt-preview-limit))
         (rerun (or rerun (lambda (limit)
                            (with-current-buffer (find-file-noselect file)
                              (dbt-lsp-show sql title limit))))))
    (dbt-lsp--execute
     "dbt.show" (list (if sql
                          (list :inline sql :limit limit)
                        (list :uri (lsp--buffer-uri) :limit limit)))
     '(object (:columns (or null (array stringp))) (:data (or null (array (object)))))
     (lambda (result)
       (if (not (lsp-get result :columns))
           (message "dbt lsp: dbt.show has no rows to show for %s" title)
         (let ((columns (append (lsp-get result :columns) nil)))
           (dbt-rows-show title columns
                          (mapcar (lambda (row)
                                    (mapcar (lambda (column)
                                              (dbt-rows-cell (lsp-get row (intern (concat ":" column))) nil))
                                            columns))
                                  (append (lsp-get result :data) nil))
                          limit rerun)
           (message "%d rows of %s" (length (lsp-get result :data)) title)))))))

(defun dbt-lsp-preview-cte (name &optional limit)
  "Show the rows of the CTE NAME of the current model, at most LIMIT of them.
The CTE comes from the compiled SQL, where the Jinja that needs a model,
such as `is_incremental', is rendered.  The buffer is saved first, as
the server compiles the file on disk.  Asked for again from its grid,
the model is compiled again."
  (interactive (list (car (dbt-mode-cte-query))))
  (let ((source (current-buffer))
        (file buffer-file-name))
    (dbt-lsp-compiled
     (lambda ()
       (let ((query (dbt-mode-cte-query name)))
         (with-current-buffer source
           (dbt-lsp-show (cdr query) (format "%s/%s" (file-name-base file) (car query)) limit
                         (lambda (limit)
                           (with-current-buffer (find-file-noselect file)
                             (dbt-lsp-preview-cte name limit))))))))))

(defun dbt-lsp--preview-cte (command)
  "Show the rows of the CTE that COMMAND, the command of a code lens, names.
For the action handler of dbt.previewCte, which the server leaves to the
client.  Its arguments are the file and the CTE."
  (pcase (lsp-get command :arguments)
    ((and `[,(and (pred stringp) file) ,cte] (guard (dbt-lsp--conforms-p cte '(object (:name stringp)))))
     (with-current-buffer (find-file-noselect file)
       (dbt-lsp-preview-cte (lsp-get cte :name))))
    (arguments (error "%s" (dbt-lsp--unreadable "dbt.previewCte" arguments)))))

(defun dbt-lsp-lineage (selector id)
  "Show the lineage of the node ID, which SELECTOR selects, as the server sees it.
SELECTOR is dbt's selector of a model, seed or snapshot, such as
orders or orders.v2."
  (let ((root (dbt-project-root)))
    (dbt-lsp--execute
     "dbt.listNodes" (list (format "+%s+" selector))
     '(object (:nodes (array (object (:unique_id stringp) (:name stringp) (:resource_type stringp)
                                     (:package_name stringp) (:original_file_path stringp)
                                     (:depends_on (or null (object (:nodes (or null (array stringp))))))))))
     (lambda (result)
       (dbt-lineage-show
        root id
        (mapcar (lambda (node)
                  (let ((directory (nth 1 (assoc (lsp-get node :package_name) (dbt-project-packages root)))))
                    (list :id (lsp-get node :unique_id)
                          :name (lsp-get node :name)
                          :type (intern (lsp-get node :resource_type))
                          :package (lsp-get node :package_name)
                          :file (and directory (expand-file-name (lsp-get node :original_file_path) directory))
                          :parents (append (lsp-get (lsp-get node :depends_on) :nodes) nil))))
                (append (lsp-get result :nodes) nil))
        "dbt lsp"
        (lambda () (let ((default-directory root)) (dbt-lsp-lineage selector id))))))))

(defun dbt-lsp-rename-model (name)
  "Rename the model of the current buffer to NAME, and the uses of it.
lsp-mode asks the server what renaming the file changes, and the server
edits the `ref' calls of models and the YAML entry that name the model;
the edited buffers are left unsaved.  dbt lsp 2.0.9 leaves those of
analyses and YAML tests: the uses that still name the model are listed.
Renaming the file otherwise, such as in Dired, has the server do the
same, without the list.  The current buffer is saved first.  NAME must
not be that of another model, seed or snapshot of the package; a version
of a model is not renamed, as the server would edit nothing."
  (interactive (list (read-string "Rename model to: " (file-name-base buffer-file-name))))
  (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)
    (user-error "Not a name for a model: %s" name))
  (unless (dbt-lsp-workspace)
    (user-error "No dbt lsp runs in this buffer"))
  (let* ((root (dbt-project-root))
         (entity (dbt-project-file-entity root buffer-file-name))
         (file buffer-file-name)
         (old (file-name-base buffer-file-name))
         (new (expand-file-name (concat name "." (file-name-extension buffer-file-name))
                                (file-name-directory buffer-file-name))))
    (when (plist-get entity :version)
      (user-error "This file is version %s of %s, which dbt lsp does not rename"
                  (plist-get entity :version) (plist-get entity :name)))
    (when (file-exists-p new)
      (user-error "%s exists already" new))
    ;; The server would make each ref of the model one of that resource.
    (when (dbt-project-entities root '(model seed snapshot) name (plist-get entity :package))
      (user-error "A model, seed or snapshot named %s exists already" name))
    (save-buffer)
    (let ((uses (xref-backend-references 'dbt (concat "ref:" old))))
      (rename-visited-file new)
      ;; The next rename's check must know the new name.
      (dbt-project-forget root)
      ;; What still names the model where it was used: a call of it or its
      ;; YAML entry, in the buffers that the server edited or else in the
      ;; files.  An edit before a use on its line moves the use, and the
      ;; server edits every use of a line or none.
      (setq uses (seq-filter (lambda (item)
                               (let ((location (xref-item-location item)))
                                 ;; A use in the renamed file is now in its new one.
                                 (when (equal (xref-file-location-file location) file)
                                   (setf (xref-file-location-file location) new))
                                 (with-temp-buffer
                                   (if-let* ((buffer (get-file-buffer (xref-file-location-file location))))
                                       (insert-buffer-substring buffer)
                                     (insert-file-contents (xref-file-location-file location)))
                                   (goto-char (point-min))
                                   (forward-line (1- (xref-location-line location)))
                                   (forward-char (min (xref-file-location-column location)
                                                      (- (line-end-position) (point))))
                                   (and (looking-at (concat (alist-get 'ref dbt-project-call-regexps)
                                                            "\\|[ \t-]*name:[ \t]*['\"]?\\(?1:[^'\"\n \t]+\\)"))
                                        (equal (match-string 1) old)))))
                             uses))
      (if (not uses)
          (message "Renamed to %s; save the buffers it edited with C-x s" name)
        (xref-show-xrefs (lambda () uses) nil)
        (message "Renamed to %s; save the buffers it edited with C-x s; %d uses still name %s, which dbt lsp leaves"
                 name (length uses) old)))))

(defun dbt-lsp-format-buffer ()
  "Save the buffer, and format it with the server."
  (interactive)
  (save-buffer)
  (with-lsp-workspace (or (dbt-lsp-workspace) (user-error "No dbt lsp runs in this buffer"))
    ;; Without its requirement, lsp-mode sends the method that
    ;; `dbt-lsp--hide-formatting' hid, here only.
    (let* ((lsp-method-requirements (assoc-delete-all "textDocument/formatting"
                                                      (copy-alist lsp-method-requirements)))
           (edits (lsp-request "textDocument/formatting"
                               (list :textDocument (lsp--text-document-identifier)
                                     :options (list :tabSize tab-width
                                                    :insertSpaces (if indent-tabs-mode :json-false t))))))
      ;; lsp-mode gives the array of edits as a list.
      (unless (and (listp edits)
                   (dbt-lsp--conforms-p
                    (vconcat edits)
                    '(array (object (:range (object (:start (object (:line natnump) (:character natnump)))
                                                    (:end (object (:line natnump) (:character natnump)))))
                                    (:newText stringp)))))
        (error "%s" (dbt-lsp--unreadable "textDocument/formatting" edits)))
      (lsp--apply-text-edits edits 'format))))

(defun dbt-lsp-fix-all ()
  "Save the buffer, then fix its lint issues and format it with the server.
dbt lsp 2.0.9 offers this whether or not `dbt-lsp-enable-linter' is on."
  (interactive)
  (save-buffer)
  (with-lsp-workspace (or (dbt-lsp-workspace) (user-error "No dbt lsp runs in this buffer"))
    ;; Without its requirement, lsp-mode sends the method that
    ;; `dbt-lsp--hide-formatting' hid, here only.
    (let ((lsp-method-requirements (assoc-delete-all "textDocument/codeAction"
                                                     (copy-alist lsp-method-requirements))))
      (call-interactively #'lsp-fix-all))))

;;;; The client

(defun dbt-lsp--compiling (workspace compiling)
  "Say in the mode line of WORKSPACE's buffers whether its server is COMPILING."
  (dolist (buffer (lsp--workspace-buffers workspace))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq dbt-mode-compiling compiling))))
  (force-mode-line-update t))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'dbt-lsp--command #'dbt-lsp-server-p)
  :activation-fn (lambda (file-name mode)
                   (and (provided-mode-derived-p mode 'dbt-mode) (dbt-project-root file-name)))
  :priority 1
  :server-id 'dbt-lsp
  :library-folders-fn #'dbt-lsp--library-folders
  :initialized-fn #'dbt-lsp--hide-formatting
  ;; lsp-mode's own formatting and fixes on save would ask for what
  ;; `dbt-lsp--hide-formatting' hid, and log an error at each save.
  :after-open-fn (lambda () (setq-local lsp-format-buffer-on-save nil
                                        lsp-fix-all-on-save nil))
  ;; The server tells of its compilations with notifications of its own,
  ;; which the mode line shows; lsp-mode shows their progress, from
  ;; $/progress.
  :notification-handlers (lsp-ht ("dbt/lspCompileStart" (lambda (workspace _) (dbt-lsp--compiling workspace t)))
                                  ("dbt/lspCompileComplete" (lambda (workspace _) (dbt-lsp--compiling workspace nil)))
                                  ("dbt/lspBackgroundCompileStart"
                                   (lambda (workspace _) (dbt-lsp--compiling workspace t)))
                                  ("dbt/lspBackgroundCompileComplete"
                                   (lambda (workspace _) (dbt-lsp--compiling workspace nil)))
                                  ("dbt/renameModel" #'ignore))
  :action-handlers (lsp-ht ("dbt.previewCte" #'dbt-lsp--preview-cte))))

(provide 'dbt-lsp)
;;; dbt-lsp.el ends here
