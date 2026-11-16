;;; dbt-lsp.el --- The client of dbt lsp for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; dbt 2's proprietary build runs a language server, `dbt lsp', on the
;; standard input and output; dbt-oss has none.  This client registers
;; it with lsp-mode for `dbt-mode' buffers, which then get from it what
;; lsp-mode offers: definitions and references of models and macros,
;; hover with a model's parents, children and columns, completion,
;; diagnostics, semantic tokens, and code lenses that preview a CTE.
;; Column-level features need a dbt account; the server says nothing
;; about them otherwise.
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
;; does the same at once.  Loading this file keeps lsp-mode's file watchers
;; out of the outputs of a dbt project, for every server that it starts
;; on one: see `dbt-lsp--ignore-outputs'.

;;; Code:

(require 'dbt-lineage)
(require 'dbt-mode)
(require 'dbt-project)
(require 'dbt-rows)
(require 'lsp-mode)
(require 'map)

(defcustom dbt-lsp-arguments nil
  "More arguments of `dbt lsp', such as (\"--profiles-dir\" \"/path\").
dbt-lsp-plus alone, for dbt-oss, keeps only the profiles directory,
profile, target, vars and packages' path, for its `dbt parse'."
  :type '(repeat string)
  :group 'dbt)

(defcustom dbt-lsp-proxy nil
  "Whether dbt lsp runs through dbt-lsp-plus, when that executable exists.
dbt-lsp-plus adds what dbt lsp lacks, such as the references of sources
over the project; dbt-mode steps aside where it says that it answers.
For dbt-oss, which runs no dbt lsp, dbt-lsp-plus then runs alone."
  :type 'boolean
  :group 'dbt)

(defcustom dbt-lsp-enable-linter nil
  "Whether the server lints the SQL, with dbt's built-in linter.
dbt lsp lints; dbt-lsp-plus alone does not."
  :type 'boolean
  :group 'dbt)

(defun dbt-lsp-server-p ()
  "Return non-nil when dbt-mode runs a language server for `dbt-executable'.
That is `dbt lsp', which only dbt 2.0's proprietary build runs, through
dbt-lsp-plus with `dbt-lsp-proxy'; or, with `dbt-lsp-proxy' on and
dbt-lsp-plus found, dbt-lsp-plus alone for a dbt 2 that lacks dbt lsp,
such as dbt-oss, as the proxy decides by the same question.  A dbt
Core 1.x, which refuses dbt lsp otherwise, or no dbt, gets none."
  (pcase (dbt-project-command-answer "lsp")
    ('yes t)
    ('lacks (and dbt-lsp-proxy (executable-find "dbt-lsp-plus") t))))

(defun dbt-lsp--command ()
  "Return the command line of the server, for the workspace being started.
lsp-mode calls it with `default-directory' at the root of the workspace,
in the buffer that starts it.  With `dbt-lsp-proxy', dbt-lsp-plus runs
it, as `dbt-lsp-plus -- COMMAND'."
  (append (and dbt-lsp-proxy (executable-find "dbt-lsp-plus") (list "dbt-lsp-plus" "--"))
          ;; dbt-lsp-plus runs the server as given, from PATH, not
          ;; `exec-path'.
          (list (dbt-project-executable) "lsp" "--project-dir"
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

(defvar dbt-lsp--hidden (make-hash-table :test #'eq :weakness 'key)
  "From a workspace to the capabilities that `dbt-lsp--hide-formatting' hid.
Each is a keyword of those it hides, which the server announced.")

(defun dbt-lsp--hide-formatting (workspace)
  "Hide the formatting of the server of WORKSPACE from lsp-mode.
The server formats the file on disk rather than the buffer, so lsp-mode
would apply its edits to unsaved text; `dbt-lsp-format-buffer' saves
first.  Its code actions go too: it has one, which fixes lint issues
and formats, also from the file on disk; `dbt-lsp-fix-all' saves first."
  ;; A key with a nil value would still mean support to lsp-mode: the keys
  ;; go.  They are strings in a hash table, keywords in a plist.
  (let ((capabilities (lsp--workspace-server-capabilities workspace))
        (keys '(:documentFormattingProvider :documentRangeFormattingProvider :codeActionProvider)))
    (puthash workspace (seq-filter (lambda (key) (lsp-get capabilities key)) keys) dbt-lsp--hidden)
    (dolist (key keys)
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

(defun dbt-lsp--ignore-outputs ()
  "Keep lsp-mode's file watchers out of dbt's outputs in the starting workspace.
For `lsp-before-initialize-hook', so for every server that lsp-mode
starts, dbt lsp or another one on the same folder, before it asks for a
watcher: lsp-mode makes one watch per folder of its session, with the
option's value of that moment.  The outputs are the logs and the target
of the dbt project at the workspace's root, where dbt lsp writes
whatever it is told, and the target-path of its dbt_project.yml.  dbt
writes there at each run and dbt lsp at each compilation, while dbt lsp
asks to watch every file: a dbt-lsp-plus older than its plan 025 would
list the graph again at each event, and dbt lsp compile.  lsp-mode reads
`lsp-file-watch-ignored-directories' in a buffer at the workspace's
root, and matches true names: one regexp per project, of true names,
joins the option's global value for this Emacs session, and the user's
own entries stay.  A .dir-locals.el at the workspace's root that sets
the option for every mode replaces the whole list, this regexp
included."
  (when-let* ((root (dbt-project-root (lsp--workspace-root lsp--cur-workspace)))
              (regexp (format "\\`\\(?:%s[/\\\\]\\(?:logs\\|target\\)\\|%s\\)\\'"
                              (regexp-quote (directory-file-name (file-truename root)))
                              (regexp-quote (directory-file-name (file-truename (dbt-project-target root))))))
              ((not (member regexp (default-value 'lsp-file-watch-ignored-directories)))))
    (set-default 'lsp-file-watch-ignored-directories
                 (cons regexp (default-value 'lsp-file-watch-ignored-directories)))))

(add-hook 'lsp-before-initialize-hook #'dbt-lsp--ignore-outputs)

;;;###autoload
(defun dbt-lsp ()
  "Start dbt lsp for the current buffer, on the root of its dbt project.
Or dbt-lsp-plus alone, for dbt-oss, as `dbt-lsp-server-p' says.  A dbt
with neither is an error.  A dbt that did not answer in time whether it
runs one is asked again."
  (interactive)
  (dbt-project-ask-again "lsp")
  (when (eq (dbt-project-command-answer "lsp") 'timeout)
    (user-error "dbt did not answer dbt lsp --help in time: M-x dbt-lsp asks again"))
  (unless (dbt-lsp-server-p)
    (user-error "This dbt runs no dbt lsp: only the proprietary build of dbt 2 does, or dbt-lsp-plus, with dbt-lsp-proxy on, for dbt-oss"))
  (dbt-lsp-add-project-folder)
  (lsp))

;;;###autoload
(defun dbt-lsp-deferred ()
  "Start dbt lsp for the current buffer once it is shown.
On the root of its dbt project; for `dbt-mode-hook'.  Nothing starts
when dbt runs no server, such as dbt-oss, which lsp-mode would take for
a server to install."
  (when (dbt-lsp-server-p)
    (dbt-lsp-add-project-folder)
    (lsp-deferred)))

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
REQUEST is the key of a request that dbt-lsp-plus lists under
experimental.dbtLspPlus of its capabilities, such as :references or
:definition, and KIND a kind of target it lists there, such as
\"source\": dbt-mode then leaves that request to the server."
  (let* ((workspace (dbt-lsp-workspace))
         (capabilities (and workspace (lsp--workspace-server-capabilities workspace))))
    (and (dbt-lsp--conforms-p capabilities `(object (:experimental (object (:dbtLspPlus (object (,request (array stringp))))))))
         (seq-contains-p (lsp-get (lsp-get (lsp-get capabilities :experimental) :dbtLspPlus) request) kind))))

(defun dbt-lsp--unreadable (method answer)
  "Return the message that dbt-mode cannot read ANSWER, of dbt lsp to METHOD."
  (format "dbt lsp: %s answered what dbt-mode cannot read: %s"
          method (truncate-string-to-width (json-encode answer) 200 nil nil "...")))

(defun dbt-lsp--project-workspace ()
  "Return the workspace of dbt-lsp of the current project, or nil.
That is the buffer's, else the one on its file or directory: from a
buffer that the server does not serve, such as that of the lineage, the
server of the project, not that of an enclosing project, as for the
integration_tests of a package."
  (or (dbt-lsp-workspace)
      (when-let* ((workspace (lsp-find-workspace 'dbt-lsp (or buffer-file-name default-directory)))
                  ((equal (lsp--workspace-root workspace) (lsp-f-canonical (or (dbt-project-root) "")))))
        workspace)))

(defun dbt-lsp-command-p (command)
  "Return non-nil when the server of the project lists COMMAND.
That is under executeCommandProvider: dbt lsp lists dbt.compileFile,
dbt.show and dbt.listNodes; dbt-lsp-plus adds its own; alone, for a dbt
that lacks dbt lsp, it lists only its own, and fails the others."
  (when-let* ((workspace (dbt-lsp--project-workspace)))
    (seq-contains-p (lsp-get (lsp-get (lsp--workspace-server-capabilities workspace) :executeCommandProvider)
                             :commands)
                    command)))

(defun dbt-lsp-announced-p (&rest path)
  "Return non-nil when the server of the buffer announced the capability at PATH.
PATH is keywords, as :documentFormattingProvider, or :workspace
:fileOperations :willRename; those that `dbt-lsp--hide-formatting' hid
from lsp-mode count.  dbt lsp announces formatting, code actions and
the rename of files; dbt-lsp-plus alone, none of them.  The server is
the buffer's, as these act on the buffer's file."
  (when-let* ((workspace (dbt-lsp-workspace)))
    (or (memq (car path) (gethash workspace dbt-lsp--hidden))
        (seq-reduce #'lsp-get path (lsp--workspace-server-capabilities workspace)))))

(defun dbt-lsp--request (method params schema callback &optional failed)
  "Ask the server METHOD with PARAMS; give CALLBACK the answer SCHEMA fits.
SCHEMA is as `dbt-lsp--conforms-p' takes it.  An answer that it does not
fit, and an error, are said in a message instead, naming METHOD, or the
command of a workspace/executeCommand; FAILED, a function of no
argument, is then called."
  (let ((name (if (equal method "workspace/executeCommand") (plist-get params :command) method))
        (workspace (or (dbt-lsp--project-workspace) (user-error "No dbt lsp runs on this project"))))
    (with-lsp-workspace workspace
      ;; Unmerged, the answer of the one workspace asked: lsp-mode's
      ;; merging of answers would turn an array into a list.
      (lsp-request-async method params
                         (lambda (answers)
                           (let ((answer (cdar answers)))
                             (if (dbt-lsp--conforms-p answer schema)
                                 (funcall callback answer)
                               (message "%s" (dbt-lsp--unreadable name answer))
                               (when failed (funcall failed)))))
                         :mode 'detached
                         :no-merge t
                         :error-handler (lambda (error)
                                          (message "dbt lsp: %s failed: %s" name (lsp-get error :message))
                                          (when failed (funcall failed)))))))

(defun dbt-lsp--execute (command arguments schema callback &optional failed)
  "Ask the server to run COMMAND with ARGUMENTS, a list, and call CALLBACK.
CALLBACK receives the answer once the server gives it, if SCHEMA fits it,
as `dbt-lsp--conforms-p' tells.  Otherwise, or when the answer is null
or holds an error, a message says so, and FAILED, a function of no
argument, is then called, when given."
  (dbt-lsp--request "workspace/executeCommand" (list :command command :arguments (vconcat arguments))
                    `(or null (object (:error stringp)) ,schema)
                    (lambda (answer)
                      (if (and answer (not (dbt-lsp--conforms-p answer '(object (:error stringp)))))
                          (funcall callback answer)
                        (message "dbt lsp: %s %s" command
                                 (if answer (format "failed: %s" (lsp-get answer :error)) "answered nothing"))
                        (when failed
                          (funcall failed))))
                    failed))

(defun dbt-lsp-node-info (id callback otherwise)
  "Give CALLBACK the node ID as dbt-lsp-plus knows it, else call OTHERWISE.
When the server lists the command dbt-lsp-plus.nodeInfo, CALLBACK
receives the node, a hash table, once the proxy answers; a null answer,
no such node, and an error, such as -32803 until the proxy's first
parse or while its parse is refused, are said as `dbt-lsp--execute'
says them.  OTHERWISE, a function of no argument, is called then, and
when the server does not list the command, as dbt lsp alone."
  (if (dbt-lsp-command-p "dbt-lsp-plus.nodeInfo")
      (dbt-lsp--execute "dbt-lsp-plus.nodeInfo" (list id) '(object)
                        ;; One shape whatever `lsp-use-plists' says, false
                        ;; read as nil, as lsp-mode reads it.
                        (lambda (node) (funcall callback (json-parse-string (json-encode node) :null-object nil)))
                        otherwise)
    (funcall otherwise)))

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

(defun dbt-lsp-hierarchy-p ()
  "Return non-nil when the server of the buffer announces a call hierarchy.
dbt-lsp-plus does; dbt lsp alone does not."
  (let ((workspace (dbt-lsp-workspace)))
    (and workspace (lsp-get (lsp--workspace-server-capabilities workspace) :callHierarchyProvider))))

(defun dbt-lsp-symbols-p ()
  "Return non-nil when the server of the buffer announces document symbols.
dbt-lsp-plus does; dbt lsp alone announces false, which `lsp-feature?'
takes for support all the same, as it reads false as nil and a nil
option as support."
  (let ((workspace (dbt-lsp-workspace)))
    (and workspace (lsp-get (lsp--workspace-server-capabilities workspace) :documentSymbolProvider))))

(defconst dbt-lsp--item
  '(object (:name stringp) (:uri stringp) (:range (object (:start (object (:line natnump)))))
           (:data (object (:uniqueId stringp) (:resourceType stringp))))
  "The schema of an item of dbt-lsp-plus's call hierarchy, as dbt-mode reads it.
`data.uniqueId' is dbt's unique id of the node.")

(defun dbt-lsp-hierarchy-lineage (file line id otherwise)
  "Show the lineage of the node ID, at LINE of FILE, from the call hierarchy.
dbt-lsp-plus answers it from the project's files, and dbt lsp's nodes
once listed, macros included.  The item of the node comes from the start
of LINE, which must be that of ID; then outgoing calls give what each
node depends on, upstream, and incoming calls what depends on it,
downstream, one level at a time, as `dbt-lineage-show' asks them.
OTHERWISE, a function of no argument, is called when the server has no
item of ID there: a snapshot whose block does not start its line, one
defined in YAML, a version of a model before the proxy listed dbt lsp's
nodes."
  (let* ((root (dbt-project-root))
         (items (make-hash-table :test #'equal))
         (node (lambda (item)
                 (let ((node-id (lsp-get (lsp-get item :data) :uniqueId))
                       (path (lsp--uri-to-path (lsp-get item :uri))))
                   (puthash node-id item items)
                   (list :id node-id :name (lsp-get item :name)
                         :type (intern (lsp-get (lsp-get item :data) :resourceType))
                         :package (nth 1 (split-string node-id "\\.")) :file (and (file-regular-p path) path)
                         :line (1+ (lsp-get (lsp-get (lsp-get item :range) :start) :line))))))
         (calls (lambda (ids direction callback)
                  (pcase-let* ((`(,method ,key) (if (eq direction 'upstream)
                                                    '("callHierarchy/outgoingCalls" :to)
                                                  '("callHierarchy/incomingCalls" :from)))
                               (left (length ids))
                               (found nil)
                               ;; Once each node of the level answered, or
                               ;; failed, as said: the others go on.
                               (done (lambda ()
                                       (when (zerop (setq left (1- left)))
                                         (funcall callback (nreverse found))))))
                    (dolist (asked ids)
                      (let ((default-directory root))
                        (dbt-lsp--request
                         method (list :item (gethash asked items)) `(or null (array (object (,key ,dbt-lsp--item))))
                         (lambda (answer)
                           (seq-doseq (call answer)
                             (let ((other (funcall node (lsp-get call key))))
                               ;; What ASKED depends on, upstream; what depends on it, downstream.
                               (push (if (eq key :to)
                                         (list other asked (plist-get other :id))
                                       (list other (plist-get other :id) asked))
                                     found)))
                           (funcall done))
                         done)))))))
    (dbt-lsp--request
     "textDocument/prepareCallHierarchy"
     (list :textDocument (list :uri (lsp--path-to-uri file)) :position (list :line (1- line) :character 0))
     `(or null (array ,dbt-lsp--item))
     (lambda (answer)
       (if-let* ((item (seq-find (lambda (item) (equal (lsp-get (lsp-get item :data) :uniqueId) id)) answer)))
           (dbt-lineage-show root id (list (funcall node item)) "dbt-lsp-plus"
                             (lambda ()
                               (let ((default-directory root))
                                 (dbt-lsp-hierarchy-lineage file line id otherwise)))
                             calls)
         (funcall otherwise))))))

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
  ;; lsp-mode asks the server about the rename only when it says it
  ;; answers, which dbt lsp does and dbt-lsp-plus alone does not.
  (unless (dbt-lsp-announced-p :workspace :fileOperations :willRename)
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

(defun dbt-lsp-format-buffer (&optional then)
  "Save the buffer, then have the server format it.
The server formats the file on disk, whose text is the buffer's once
saved: its edits apply when it answers, while the buffer's text is
still that, and leave the buffer modified.  THEN, when non-nil, is a
function of no argument that runs in the buffer once they did.  An
answer that dbt-mode cannot read, or one that comes after an edit, is a
message."
  (interactive)
  (save-buffer)
  (let ((buffer (current-buffer))
        (tick (buffer-chars-modified-tick)))
    (with-lsp-workspace (or (dbt-lsp-workspace) (user-error "No dbt lsp runs in this buffer"))
      ;; Without its requirement, lsp-mode sends the method that
      ;; `dbt-lsp--hide-formatting' hid, here only.
      (let ((lsp-method-requirements (assoc-delete-all "textDocument/formatting"
                                                       (copy-alist lsp-method-requirements))))
        (lsp-request-async
         "textDocument/formatting"
         (list :textDocument (lsp--text-document-identifier)
               :options (list :tabSize tab-width :insertSpaces (if indent-tabs-mode :json-false t)))
         (lambda (edits)
           ;; lsp-mode gives the array of edits as a list.
           (cond ((not (and (listp edits)
                            (dbt-lsp--conforms-p
                             (vconcat edits)
                             '(array (object (:range (object (:start (object (:line natnump) (:character natnump)))
                                                             (:end (object (:line natnump) (:character natnump)))))
                                             (:newText stringp))))))
                  (message "dbt could not format %s: %s" (buffer-name buffer)
                           (dbt-lsp--unreadable "textDocument/formatting" edits)))
                 ((not (buffer-live-p buffer)))
                 ((/= tick (buffer-chars-modified-tick buffer))
                  (message "dbt formatted %s as saved: your edits since win, unformatted" (buffer-name buffer)))
                 (t
                  (with-current-buffer buffer
                    (lsp--apply-text-edits edits 'format)
                    (when then
                      (funcall then))))))
         :mode 'detached)))))

(defun dbt-lsp-fix-all ()
  "Save the buffer, then fix its lint issues and format it with the server.
dbt lsp 2.0.9 offers this whether or not `dbt-lsp-enable-linter' is on."
  (interactive)
  ;; dbt lsp offers code actions, and dbt-lsp-plus alone none.
  (let ((workspace (if (dbt-lsp-announced-p :codeActionProvider)
                       (dbt-lsp-workspace)
                     (user-error "No dbt lsp runs in this buffer"))))
    (save-buffer)
    (with-lsp-workspace workspace
      ;; Without its requirement, lsp-mode sends the method that
      ;; `dbt-lsp--hide-formatting' hid, here only.
      (let ((lsp-method-requirements (assoc-delete-all "textDocument/codeAction"
                                                       (copy-alist lsp-method-requirements))))
        (call-interactively #'lsp-fix-all)))))

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
  :after-open-fn (lambda ()
                   (setq-local lsp-format-buffer-on-save nil
                               lsp-fix-all-on-save nil)
                   ;; Once no server serves the buffer, nothing compiles it.
                   (add-hook 'lsp-unconfigure-hook
                             (lambda ()
                               (setq dbt-mode-compiling nil)
                               (force-mode-line-update))
                             nil t))
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
