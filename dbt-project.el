;;; dbt-project.el --- What a dbt project holds, read from its files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The project model of dbt-mode, which needs no dbt executable: the root
;; of a project, its configuration, and an index of what it defines.  The
;; index reads the files themselves: models, seeds and analyses by their
;; file names; macros, generic tests, snapshots and docs blocks by their
;; Jinja blocks; sources, their tables, exposures and the properties of
;; the other resources by a scan of the YAML files that keeps the line of
;; each entry.  Installed packages, under the packages-install-path of
;; the project, dbt_packages by default, are indexed with the project,
;; each under its own name.
;;
;; An entity of the index is a plist:
;;
;;   (:kind KIND :name NAME :package PACKAGE :file FILE :line LINE
;;    :source SOURCE :section SECTION :version VERSION)
;;
;; KIND is one of `model', `seed', `snapshot', `analysis', `macro', `test'
;; (a generic test), `doc', `source', `table' (a table of the source
;; SOURCE), `exposure' and `properties' (an entry of the YAML section
;; SECTION, such as "models").  LINE counts from 1.  VERSION is that of
;; a version of a model, whose NAME is the model's.  A snapshot written
;; in YAML has :relation, the text of its relation.
;;
;; `dbt-project-graph' reads the `ref' and `source' calls of every model,
;; snapshot and analysis, or the relation of a snapshot written in YAML,
;; for a lineage that needs no server.  Its nodes
;; have the shape that dbt-lineage.el shows: a plist with :id, :name,
;; :type, :package, :file and :parents, the identifiers of the nodes it
;; depends on.

;;; Code:

(require 'seq)
(require 'subr-x)

(defgroup dbt nil
  "Editing dbt projects."
  :group 'languages
  :prefix "dbt-")

(defcustom dbt-executable "dbt"
  "The dbt executable: a name looked up in the variable `exec-path', or a file.
The language server needs dbt 2's proprietary build, which runs it as
`dbt lsp'; dbt-oss runs the rest."
  :type 'string)

(defcustom dbt-preview-limit 50
  "The number of rows that a preview of a model or a query shows."
  :type 'natnum)

(defcustom dbt-protected-targets '("prod")
  "The targets on which a Babel block, the data diff and freshness ask first.
That is an Org Babel block of dbt, `dbt-data-diff' and
`dbt-dashboard-freshness', which run SQL on the warehouse; dbt-dispatch
and previews do not ask.  While any target is protected, a default
target that dbt-mode cannot read asks too."
  :type '(repeat string))

(defun dbt-project-executable ()
  "Return the file of the dbt to run, as `executable-find' finds it.
That is `dbt-executable', a file or a name on the variable `exec-path'.
Signal a `user-error' when there is none, before anything starts."
  (or (executable-find dbt-executable)
      (user-error "No dbt to run: no executable %s" dbt-executable)))

(defvar dbt-project--commands (make-hash-table :test #'equal)
  "From a dbt executable and a command to whether it runs it.
The key is (FILE TIME COMMAND): FILE the true name of the executable,
TIME its modification time, so that a dbt replaced in place, or a link
pointed at another, as an upgrade or a new profile does, is asked
again.  The value is `yes' or `no'.")

(defvar dbt-project--probe-seconds 10
  "The seconds that dbt has to say whether it runs a command.")

(defun dbt-project-command-p (command)
  "Return non-nil when the dbt of `dbt-executable' runs COMMAND.
It does when `dbt COMMAND --help' succeeds: dbt 2.0's proprietary build
runs `lsp' and `format', which dbt-oss 2.0 and dbt Core 1.x lack.  The
answer is kept for each command while the executable's file stays the
same, as `dbt-project--commands' says.  A dbt that does not answer
within `dbt-project--probe-seconds' runs none; nil without dbt."
  (when-let* ((executable (executable-find dbt-executable)))
    (eq 'yes
        (with-memoization (gethash (let ((file (file-truename executable)))
                                     (list file (file-attribute-modification-time (file-attributes file)) command))
                                   dbt-project--commands)
          (let ((process (ignore-errors
                           (make-process :name (format "dbt %s --help" command)
                                         :command (list executable command "--help")
                                         :connection-type 'pipe :noquery t)))
                (deadline (+ (float-time) dbt-project--probe-seconds)))
            (while (and (process-live-p process) (< (float-time) deadline))
              (accept-process-output process 0.1))
            (prog1 (if (and process (eq (process-status process) 'exit) (eq 0 (process-exit-status process)))
                       'yes
                     'no)
              (when process (delete-process process))))))))

(defcustom dbt-project-index-lifetime 30
  "Seconds for which an index of a project is used without reading it again.
Saving a buffer of the project forgets the index at once; this bounds how
long a change made outside Emacs, such as a checkout, goes unseen."
  :type 'natnum)

(defconst dbt-project-file "dbt_project.yml"
  "The file that marks the root of a dbt project.")

(defconst dbt-project-skipped-directories
  '("target" "logs" "dbt_packages" "dbt_internal_packages" "node_modules")
  "Directories under a project root that hold none of its own sources.
dbt writes the first four; installed packages are indexed apart.")

(defun dbt-project-root (&optional file)
  "Return the root of the dbt project that holds FILE, or nil.
FILE defaults to the visited file, or else `default-directory'.  The root
is the nearest directory holding a dbt_project.yml, unless that project
is a package of the project above it: installed under its
packages-install-path or dbt_internal_packages, or a local package that
it declares.  A project nested for another reason, such as the
integration_tests of a package, is a project of its own."
  (let ((directory (or file buffer-file-name default-directory))
        roots)
    ;; ROOTS holds the projects above FILE, the outermost first, then the
    ;; innermost first once reversed.
    (while-let ((found (and directory (locate-dominating-file directory dbt-project-file))))
      (push (file-name-as-directory (expand-file-name found)) roots)
      (setq directory (file-name-directory (directory-file-name (car roots)))))
    (setq roots (nreverse roots))
    (while (and (cdr roots)
                (let ((packages (dbt-project-packages (cadr roots))))
                  (or (member (file-name-directory (directory-file-name (car roots)))
                              (list (file-name-as-directory
                                     (expand-file-name (plist-get (nth 2 (car packages)) :packages-install-path)
                                                       (cadr roots)))
                                    (expand-file-name "dbt_internal_packages/" (cadr roots))))
                      (member (car roots) (mapcar #'cadr (cdr packages))))))
      (pop roots))
    (car roots)))

;;;; Reading YAML

(defun dbt-project-yaml-unquote (value)
  "Return the YAML scalar VALUE without its quotes; nil when empty or nil."
  (cond ((or (null value) (string-empty-p value)) nil)
        ((string-match "\\`\\(['\"]\\)\\(.*\\)\\1\\'" value) (match-string 2 value))
        (t value)))

(defun dbt-project-yaml-line (line)
  "Return the key or the list item of the YAML LINE as a plist, or nil.
The plist has :column, the column of the key or the value, which follows
the \"- \" of a list item, :key, nil for an item that is a scalar,
:item, non-nil when the line starts a list item, :value, unquoted and
nil when empty, and :raw, the value as written, empty when there is
none.  A comment after the value is dropped.  Of an item
written as a flow mapping, such as \"- {name: x}\", the key is its `name'.
A blank line or a comment gives nil."
  (when (seq-find (lambda (regexp) (string-match regexp line))
                  '("\\`\\(?1: *\\(?4:- +\\)\\){\\(?:[^}]*,\\)? *\\(?2:name\\): *\\(?3:[^,}]*?\\) *[,}]"
                    "\\`\\(?1: *\\(?4:- +\\)?\\)\\(?2:[A-Za-z_][A-Za-z0-9_-]*\\): *\\(?3:.*?\\) *\\(?:#.*\\)?\\'"
                    "\\`\\(?1: *\\(?4:- +\\)\\)\\(?3:.*?\\) *\\(?:#.*\\)?\\'"))
    (let ((raw (match-string 3 line)))
      (list :column (match-end 1)
            :key (match-string 2 line)
            :item (and (match-beginning 4) t)
            ;; Unquoting matches again: read the rest first.
            :value (dbt-project-yaml-unquote raw)
            :raw raw))))

(defun dbt-project-yaml-scan (text)
  "Return the keys and list items of TEXT, a YAML document, in order.
Each is the plist of `dbt-project-yaml-line' with :line, its number from
1, and :path, the keys that hold it, innermost first: those of the lines
above whose column is lower, up to the last line at column 0."
  (let ((number 0) keys lines)
    ;; KEYS holds the open keys, innermost first, as (COLUMN . KEY); a
    ;; line closes those at its column or deeper.
    (dolist (line (split-string text "\n"))
      (setq number (1+ number))
      (when-let* ((parsed (dbt-project-yaml-line line)))
        (while (and keys (>= (car (car keys)) (plist-get parsed :column)))
          (pop keys))
        (push (append (list :line number :path (mapcar #'cdr keys)) parsed) lines)
        (when (plist-get parsed :key)
          (push (cons (plist-get parsed :column) (plist-get parsed :key)) keys))))
    (nreverse lines)))

(defconst dbt-project-yaml-sections
  '("models" "seeds" "snapshots" "analyses" "macros" "sources" "exposures"
    "semantic_models" "metrics" "saved_queries")
  "The top-level YAML sections whose entries the index keeps.")

(defconst dbt-project-section-kinds
  '(("models" . model) ("seeds" . seed) ("snapshots" . snapshot) ("analyses" . analysis)
    ("macros" . macro) ("sources" . source) ("tables" . table) ("exposures" . exposure))
  "Alist from a section of a properties file to the kind its entries name.
An entry of a source, a table or an exposure defines it; another entry
gives the properties of a resource that a file defines.")

(defconst dbt-project-resource-sections '("models" "seeds" "snapshots" "analyses")
  "The sections whose entries describe a model, a seed, a snapshot or an analysis.")

(defconst dbt-project-yaml-lists
  '((("tables" "sources") "tables" :source)
    (("versions" "models") "versions" :name))
  "The lists within an entry whose items are entries too.
Each is (PATH SECTION PROPERTY): PATH names the keys that hold the list,
innermost first, SECTION is that of its entries, and PROPERTY keeps the
name of the entry that holds the list.")

(defconst dbt-project-yaml-properties
  '(("name" . :name) ("relation" . :relation) ("latest_version" . :latest-version)
    ("v" . :version) ("defined_in" . :defined-in))
  "Alist from the keys of an entry that the index keeps to their property.")

(defun dbt-project-yaml-entries (text)
  "Return the entries of TEXT, the contents of a dbt properties file.
Each entry is a plist (:section SECTION :line LINE :name NAME ...): an
item of a top-level section of `dbt-project-yaml-sections', or of a list
of `dbt-project-yaml-lists': a table of the source SOURCE, with SECTION
\"tables\", or a version of the model NAME, with SECTION \"versions\".
LINE, from 1, is that of its name, or of its item for a version.  The
keys of `dbt-project-yaml-properties' at the first level of the item,
in any order, give the other properties."
  (let (open entries)
    ;; OPEN maps the path of each list whose items are entries to the
    ;; entry being read there.
    (dolist (line (dbt-project-yaml-scan text))
      (let* ((path (plist-get line :path))
             (list (assoc path dbt-project-yaml-lists))
             (section (if list
                          (nth 1 list)
                        (and (null (cdr path)) (car (member (car path) dbt-project-yaml-sections))))))
        (when (and section (plist-get line :item))
          (push (append (list :section section :line (plist-get line :line))
                        (and list (list (nth 2 list) (plist-get (cdr (assoc (cdr path) open)) :name))))
                entries)
          (setf (alist-get path open nil nil #'equal) (car entries)))
        (when-let* ((entry (cdr (assoc path open)))
                    (property (cdr (assoc (plist-get line :key) dbt-project-yaml-properties))))
          (plist-put entry property (plist-get line :value))
          (when (eq property :name)
            (plist-put entry :line (plist-get line :line))))))
    (seq-filter (lambda (entry) (plist-get entry :name)) (nreverse entries))))

(defconst dbt-project-config-keys
  '(("name" :name nil) ("profile" :profile nil) ("target-path" :target-path "target")
    ("packages-install-path" :packages-install-path "dbt_packages")
    ("model-paths" :model-paths ("models")) ("seed-paths" :seed-paths ("seeds"))
    ("analysis-paths" :analysis-paths ("analyses")) ("macro-paths" :macro-paths ("macros"))
    ("snapshot-paths" :snapshot-paths ("snapshots")) ("test-paths" :test-paths ("tests"))
    ("require_ref_searches_node_package_before_root" :package-first "true" "flags"))
  "The keys of dbt_project.yml that the configuration keeps.
Each is (KEY PROPERTY DEFAULT . PATH): DEFAULT is dbt's, a list for a
list of paths, and \"true\" or \"false\" for a boolean, which reads as
`dbt-project-yaml-booleans' says; PATH, the keys that hold KEY,
innermost first, is nil for a key at the top.")

(defconst dbt-project-yaml-booleans
  '(("true" . "true") ("True" . "true") ("TRUE" . "true")
    ("false" . "false") ("False" . "false") ("FALSE" . "false"))
  "The booleans of YAML that dbt reads, written unquoted, to their value.
Any other value of a boolean, a quoted \"false\" included, is a string,
which dbt 2.0.9 ignores for its default.")

(defun dbt-project-target (root)
  "Return the directory where dbt writes the artifacts of the project at ROOT."
  (expand-file-name (plist-get (dbt-project-config root) :target-path) root))

(defun dbt-project-config (root)
  "Return the configuration of the project at ROOT as a plist.
The plist has a property for each key of `dbt-project-config-keys', with
dbt's default when dbt_project.yml does not set it, or cannot be read.
A list of paths is either [a, b] on the line of its key, or the items
below it.  A key under a path is read in block style only: under
`flags: {...}' it keeps its default."
  (let ((lines (dbt-project-yaml-scan (with-temp-buffer
                                        (ignore-error file-error
                                          (insert-file-contents (expand-file-name dbt-project-file root)))
                                        (buffer-string)))))
    (mapcan
     (pcase-lambda (`(,key ,property ,default . ,path))
       (let* ((line (seq-find (lambda (line)
                                (and (equal (plist-get line :path) path) (equal (plist-get line :key) key)))
                              lines))
              (value (plist-get line :value))
              (items (delq nil (mapcar (lambda (line)
                                        (and (plist-get line :item) (equal (plist-get line :path) (cons key path))
                                             (plist-get line :value)))
                                      lines))))
         (list property
               (cond ((member default '("true" "false"))
                      (alist-get (plist-get line :raw) dbt-project-yaml-booleans default nil #'equal))
                     ((not (consp default)) (or value default))
                     ((and value (string-match "\\`\\[\\(.*\\)\\]\\'" value))
                      (mapcar #'dbt-project-yaml-unquote (split-string (match-string 1 value) "," t "[ \t]+")))
                     (t (or items default))))))
     dbt-project-config-keys)))

;;;; Reading Jinja

(defconst dbt-project-block-regexps
  '((macro . "{%-?[ \t\n]*macro[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*(")
    (test . "{%-?[ \t\n]*test[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*(")
    (snapshot . "{%-?[ \t\n]*snapshot[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*-?%}")
    (doc . "{%-?[ \t\n]*docs[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*-?%}"))
  "Alist from a kind of entity to the regexp of the Jinja block defining it.
The first group matches the name.")

(defun dbt-project-call-regexp (kind qualifier &optional typing)
  "Return the regexp of a call of KIND, `ref', `source' or `doc'.
QUALIFIER is `required' when the call gives a package or a source before
the name, `optional' when it may, and nil when it does not.  Without
TYPING, the regexp matches a complete call: group 1 the name, group 2
the qualifier, and for a `ref', group 3 the version that its argument v
or version gives.  With TYPING, it matches a call being typed, up to the
point where `looking-back' tries it: group 1 the part of the name typed,
possibly empty, group 2 the qualifier."
  (concat "\\_<" (symbol-name kind) "[ \t\n]*([ \t\n]*['\"]"
          (pcase qualifier
            ('required "\\(?2:[^'\"\n]+\\)['\"][ \t\n]*,[ \t\n]*['\"]")
            ('optional "\\(?:\\(?2:[^'\"\n]+\\)['\"][ \t\n]*,[ \t\n]*['\"]\\)?"))
          (if typing "\\(?1:[A-Za-z0-9_]*\\)" "\\(?1:[^'\"\n]+\\)['\"]")
          (and (eq kind 'ref) (not typing)
               (concat "\\(?:[ \t\n]*,[ \t\n]*\\(?:v\\|version\\)[ \t\n]*=[ \t\n]*['\"]?"
                       "\\(?3:[^'\")\n \t,]+\\)\\)?"))))

(defconst dbt-project-call-regexps
  `((ref . ,(dbt-project-call-regexp 'ref 'optional))
    (source . ,(dbt-project-call-regexp 'source 'required))
    (doc . ,(dbt-project-call-regexp 'doc 'optional)))
  "Alist from the kind of a call of dbt's Jinja to the regexp of a complete call.
Group 1 matches the name that the call gives; group 2 the package of a
`ref' or a `doc', when it is given, or the source of a `source'; group
3 the version that a `ref' gives.")

;;;; The index

(defun dbt-project-file-kind (file directory config)
  "Return the kind of resource FILE is by its place, or nil.
DIRECTORY is the root of the project or package that holds FILE, and
CONFIG its configuration.  A model, a seed or an analysis is its file:
its extension and the path of the configuration it lies under tell
which."
  (seq-some (pcase-lambda (`(,key ,kind . ,extensions))
              (and (member (file-name-extension file) extensions)
                   (seq-some (lambda (path)
                               (string-prefix-p (file-name-as-directory (expand-file-name path directory))
                                                file))
                             (plist-get config key))
                   kind))
            '((:model-paths model "sql" "py") (:seed-paths seed "csv") (:analysis-paths analysis "sql"))))

(defvar dbt-project--cache (make-hash-table :test #'equal)
  "What was read of projects, from (ROOT . WHAT) to (TIME . VALUE).")

(defvar dbt-project--files (make-hash-table :test #'equal)
  "What was read of each file, from its name to (STAMP . CONTENTS).
STAMP is the modification time, the size and the mode of the file when
it was read.")

(defun dbt-project-forget (&optional root)
  "Forget what was read of the project at ROOT, or of every project.
Interactively, ROOT is the project of the current buffer.  What a file
holds is kept until the file changes, unless every project is forgotten,
so that reading a project again reads only the files that changed."
  (interactive (list (dbt-project-root)))
  (if root
      (dolist (key (hash-table-keys dbt-project--cache))
        (when (equal (car key) root) (remhash key dbt-project--cache)))
    (clrhash dbt-project--cache)
    (clrhash dbt-project--files)))

(defun dbt-project-cached (root what compute)
  "Return the value of WHAT for the project at ROOT, from the cache if fresh.
Otherwise call COMPUTE with ROOT, and keep and return its value."
  (let* ((key (cons root what))
         (cached (gethash key dbt-project--cache)))
    (if (and cached (< (float-time (time-since (car cached))) dbt-project-index-lifetime))
        (cdr cached)
      (let ((value (funcall compute root)))
        (puthash key (cons (current-time) value) dbt-project--cache)
        value))))

(defun dbt-project-packages (root)
  "Return the projects of ROOT, each as (NAME DIRECTORY CONFIG).
The project itself comes first, then the local packages that its
packages.yml or dependencies.yml declares, then each package installed
under its packages-install-path.  A package comes once, at its first
place: a local package is edited where it lives, not in its installed
copy.  NAME falls back to the name of the directory."
  (dbt-project-cached
   root 'packages
   (lambda (root)
     (let ((installed (expand-file-name (plist-get (dbt-project-config root) :packages-install-path) root))
           (locals (mapcan (lambda (file)
                             (with-temp-buffer
                               (ignore-errors (insert-file-contents (expand-file-name file root)))
                               (let (directories)
                                 (while (re-search-forward
                                         "^[ \t]*-?[ \t]*local:[ \t]*\\([^#\n]*?\\)[ \t]*\\(?:#.*\\)?$" nil t)
                                   (push (expand-file-name (dbt-project-yaml-unquote (match-string 1)) root)
                                         directories))
                                 (nreverse directories))))
                           '("packages.yml" "dependencies.yml"))))
       (seq-uniq
        (mapcan (lambda (directory)
                  (when (file-exists-p (expand-file-name dbt-project-file directory))
                    (let ((config (dbt-project-config directory)))
                      (list (list (or (plist-get config :name)
                                      (file-name-nondirectory (directory-file-name directory)))
                                  (file-name-as-directory directory) config)))))
                (append (list root) locals
                        (and (file-directory-p installed) (directory-files installed t "\\`[^.]"))))
        (lambda (package other) (equal (car package) (car other))))))))

(defun dbt-project-name (root)
  "Return the name of the project at ROOT."
  (car (car (dbt-project-packages root))))

(defun dbt-project-source-files (directory)
  "Return the source files of the project or package at DIRECTORY.
They are its SQL, Python, CSV, Markdown and YAML files, outside hidden
directories, those of `dbt-project-skipped-directories', those of
nested projects, such as local packages, and Python environments, such
as the dbt-env or venv that holds dbt itself: a directory with the
pyvenv.cfg of venv, virtualenv and uv, or the conda-meta of conda.
Hidden files go too, such as the lock files .#NAME of the buffers being
edited."
  (directory-files-recursively
   directory "\\`[^.].*\\.\\(?:sql\\|py\\|csv\\|md\\|ya?ml\\)\\'" nil
   (lambda (subdirectory)
     (not (or (string-prefix-p "." (file-name-nondirectory subdirectory))
              ;; What cannot be read holds nothing, rather than an error.
              (not (file-accessible-directory-p subdirectory))
              (member (file-relative-name subdirectory directory)
                      dbt-project-skipped-directories)
              (seq-some (lambda (marker) (file-exists-p (expand-file-name marker subdirectory)))
                        (list dbt-project-file "pyvenv.cfg" "conda-meta")))))))

(defvar-local dbt-project--manifest-stamp nil
  "The manifest whose text this buffer holds, as (FILE TIME SIZE).
FILE is its true name, TIME and SIZE those of the file it names.")

(defun dbt-project-manifest-node (file id)
  "Return the node ID of the manifest FILE, as a hash table, or nil.
Nil when FILE cannot be read or holds no such node.  Only the node is
parsed, as a manifest grows to tens of MB: its id is the key of an
object in \"nodes\" or \"sources\" only, as elsewhere, such as in
\"child_map\", its value is an array.  The text of the last manifest
read is kept, in one buffer, and read again once the file that FILE
names, through its links, changes its modification time or its size, as
the mode line of each model asks.  The manifest is dbt's, read as it is."
  (with-current-buffer (get-buffer-create " *dbt manifest*")
    (let* ((true (file-truename file))
           (attributes (file-attributes true))
           (stamp (list true (file-attribute-modification-time attributes) (file-attribute-size attributes))))
      (unless (equal stamp dbt-project--manifest-stamp)
        (erase-buffer)
        (ignore-errors (insert-file-contents true))
        (setq dbt-project--manifest-stamp stamp)))
    (goto-char (point-min))
    (and (re-search-forward (concat "\"" (regexp-quote id) "\"[ \t\n\r]*:[ \t\n\r]*{") nil t)
         (progn (backward-char)
                (ignore-errors (json-parse-buffer :null-object nil))))))

(defun dbt-project-default-target (root)
  "Return the target that dbt runs on in ROOT when none is given, or nil.
It is that of DBT_TARGET, else the target of the project's profile in
the profiles.yml of DBT_PROFILES_DIR, else of ROOT, else of ~/.dbt, as
dbt looks for them.  A target that is a call of env_var, with or
without a default, is read as dbt reads it, from Emacs's environment,
which dbt inherits.  Nil when they do not tell, as when the target is
other Jinja."
  (or (getenv "DBT_TARGET")
      (let* ((profile (plist-get (dbt-project-config root) :profile))
             (file (seq-find #'file-readable-p
                             (mapcar (lambda (directory) (expand-file-name "profiles.yml" directory))
                                     (delq nil (list (getenv "DBT_PROFILES_DIR") root "~/.dbt")))))
             (target (and profile file
                          (plist-get (seq-find (lambda (line)
                                                 (and (equal (plist-get line :key) "target")
                                                      (equal (plist-get line :path) (list profile))))
                                               (dbt-project-yaml-scan (with-temp-buffer
                                                                        (insert-file-contents file)
                                                                        (buffer-string))))
                                     :value))))
        (cond ((not (and target (string-search "{{" target))) target)
              ((string-match (concat "\\`{{[ \t]*env_var([ \t]*['\"]\\([^'\"]+\\)['\"][ \t]*"
                                     "\\(?:,[ \t]*['\"]\\([^'\"]*\\)['\"][ \t]*\\)?)[ \t]*}}\\'")
                             target)
               (or (getenv (match-string 1 target)) (match-string 2 target)))))))

(defun dbt-project-confirm-target (root target)
  "Ask before SQL runs on TARGET in ROOT when it is protected; return its label.
TARGET is the one given to dbt, or nil for dbt's default, which
`dbt-project-default-target' reads.  The question comes on a target of
`dbt-protected-targets', or on a default target that dbt-mode cannot
read while a target is protected; a no is a `user-error'.  The label
names the target for messages, as \"target dev\", or \"dbt's default
target\" when dbt-mode cannot read it."
  (let* ((target (or target (dbt-project-default-target root)))
         (label (if target (concat "target " target) "dbt's default target")))
    (when (and (if target (member target dbt-protected-targets) dbt-protected-targets)
               (not (yes-or-no-p (format "Protected: %s.  Run on %s%s? "
                                         (string-join dbt-protected-targets ", ") label
                                         (if target "" ", which dbt-mode cannot read")))))
      (user-error "Not run on %s" label))
    label))

(defun dbt-project-calls ()
  "Return the calls of `ref' and `source' in the current buffer, in order.
Each is a target plist (:kind KIND :qualifier QUALIFIER :name NAME
:version VERSION), as `dbt-project-resolve' reads it; the calls of `ref'
come first."
  (let (calls)
    (dolist (kind '(ref source))
      (goto-char (point-min))
      (while (re-search-forward (alist-get kind dbt-project-call-regexps) nil t)
        (push (list :kind kind :qualifier (match-string-no-properties 2) :name (match-string-no-properties 1)
                    :version (match-string-no-properties 3))
              calls)))
    (nreverse calls)))

(defun dbt-project-file-contents (file)
  "Return what FILE defines and calls, as (:entities ENTITIES :calls CALLS).
ENTITIES are its Jinja blocks and, in a YAML file, its entries, as
entities without :package and :file: an entry defines a source, a table,
an exposure or a snapshot that has a relation, and gives the properties
of any other resource; CALLS are as `dbt-project-calls'
returns them.  What a Jinja comment holds is left out.  FILE is read
again only when its modification time, its size or its mode changed,
the mode telling when an unreadable file becomes readable.  A file that
cannot be read holds nothing."
  (let* ((attributes (file-attributes (file-chase-links file)))
         (stamp (list (file-attribute-modification-time attributes) (file-attribute-size attributes)
                      (file-attribute-modes attributes)))
         (cached (gethash file dbt-project--files)))
    (if (equal (car cached) stamp)
        (cdr cached)
      (with-temp-buffer
        (ignore-error file-error (insert-file-contents file))
        ;; Blank each {# #} comment, newlines kept, so that what follows
        ;; keeps its line.
        (while (search-forward "{#" nil t)
          (let ((start (match-beginning 0)))
            (when (search-forward "#}" nil t)
              (insert (replace-regexp-in-string "[^\n]" " " (delete-and-extract-region start (point)))))))
        (let (entities)
          (pcase-dolist (`(,kind . ,regexp) dbt-project-block-regexps)
            (goto-char (point-min))
            (while (re-search-forward regexp nil t)
              (push (list :kind kind :name (match-string-no-properties 1)
                          :line (line-number-at-pos (match-beginning 0)))
                    entities)))
          (when (string-match-p "\\.ya?ml\\'" file)
            (dolist (entry (dbt-project-yaml-entries (buffer-string)))
              (let ((kind (alist-get (plist-get entry :section) dbt-project-section-kinds nil nil #'equal)))
                (push (append (list :kind (if (or (plist-get entry :relation) (memq kind '(source table exposure)))
                                              kind
                                            'properties))
                              entry)
                      entities))))
          (let ((contents (list :entities (nreverse entities) :calls (dbt-project-calls))))
            (puthash file (cons stamp contents) dbt-project--files)
            contents))))))

(defun dbt-project-index (root)
  "Return the entities that the project at ROOT and its packages define.
A file that cannot be read defines what its place tells only.  The file
of a version of a model, NAME_vVERSION or the file its defined_in
names, is the model NAME with :version VERSION; another model has a nil
:version."
  (dbt-project-cached
   root 'index
   (lambda (root)
     (let ((files (mapcan (pcase-lambda (`(,package ,directory ,config))
                            (mapcar (lambda (file) (list file package directory config))
                                    (dbt-project-source-files directory)))
                          (dbt-project-packages root)))
           (versions (make-hash-table :test #'equal))
           entities)
       ;; VERSIONS maps the package and the base name of the file of a
       ;; version to its entry.
       (pcase-dolist (`(,file ,package) files)
         (unless (string-suffix-p ".csv" file)
           (dolist (entity (plist-get (dbt-project-file-contents file) :entities))
             (let ((entity (append entity (list :package package :file file))))
               (push entity entities)
               (when (plist-get entity :version)
                 (puthash (list package (or (plist-get entity :defined-in)
                                            (format "%s_v%s" (plist-get entity :name) (plist-get entity :version))))
                          entity versions))))))
       (nconc
        (mapcan (pcase-lambda (`(,file ,package ,directory ,config))
                  (when-let* ((kind (dbt-project-file-kind file directory config)))
                    (let ((version (and (eq kind 'model) (gethash (list package (file-name-base file)) versions))))
                      (list (list :kind kind :name (or (plist-get version :name) (file-name-base file))
                                  :version (plist-get version :version) :line 1 :package package :file file)))))
                files)
        (nreverse entities))))))

(defvar dbt-project--names (make-hash-table :test #'eq :weakness 'key)
  "Each index, to a table from a name to its entities in the order of the index.")

(defun dbt-project-entities (root kinds &optional name package)
  "Return the entities of ROOT whose kind is one of KINDS, in index order.
Keep only those named NAME, and of PACKAGE, when they are given.  A name
is looked up in a table, built once for each index."
  (let* ((index (dbt-project-index root))
         (names (and name
                     (with-memoization (gethash index dbt-project--names)
                       (let ((table (make-hash-table :test #'equal)))
                         (dolist (entity (reverse index))
                           (push entity (gethash (plist-get entity :name) table)))
                         table)))))
    (seq-filter (lambda (entity)
                  (and (memq (plist-get entity :kind) kinds)
                       (or (null package) (equal package (plist-get entity :package)))))
                (if name (gethash name names) index))))

(defun dbt-project-file-package (root file)
  "Return the package of ROOT that FILE belongs to, as (NAME DIRECTORY CONFIG).
It is the package whose directory is the nearest above FILE: an
installed package rather than the project, the project rather than a
local package that holds it, such as the `../' of a package's
integration_tests.  Nil when FILE is in none."
  (let ((file (expand-file-name file)))
    (car (seq-sort-by (lambda (package) (length (nth 1 package))) #'>
                      (seq-filter (lambda (package) (string-prefix-p (nth 1 package) file))
                                  (dbt-project-packages root))))))

(defun dbt-project-file-entity (root file)
  "Return the model, seed, snapshot or analysis that FILE of ROOT defines.
It is the first that the index has of FILE, a snapshot of a Jinja block
included: a YAML file defines no resource of its own.  FILE need not
exist yet, nor the index know it: a model, a seed or an analysis is told
by its place alone, in the package of `dbt-project-file-package'."
  (let* ((file (expand-file-name file))
         (package (dbt-project-file-package root file))
         (kind (dbt-project-file-kind file (nth 1 package) (nth 2 package))))
    (or (seq-find (lambda (entity)
                    (and (equal (plist-get entity :file) file)
                         (memq (plist-get entity :kind) '(model seed snapshot analysis))
                         (not (plist-get entity :relation))))
                  (dbt-project-index root))
        (and kind (list :kind kind :name (file-name-base file) :package (car package) :file file :line 1)))))

(defun dbt-project-lookup-rank (root caller)
  "Return the rank of an entity of ROOT for an unqualified call from CALLER.
The value is a function of an entity, which gives 0 for the first
package that a `ref' or a `source' of CALLER, the package of the calling
file, looks in, 1 for the second, 2 for the others.  dbt 2.0.9 looks in
CALLER, then in the project, unless the project's dbt_project.yml sets
the flag require_ref_searches_node_package_before_root to false, which
puts the project first.  A call in a macro takes the macro's package for
CALLER, though dbt 2.0.9 takes that of the node calling the macro."
  (let* ((project (car (dbt-project-packages root)))
         (order (if (equal (plist-get (nth 2 project) :package-first) "false")
                    (list (car project) caller)
                  (list caller (car project)))))
    (lambda (entity) (or (seq-position order (plist-get entity :package)) 2))))

(defun dbt-project-resolve-ref (root package name &optional caller version)
  "Return what `ref' with PACKAGE, NAME and VERSION means in ROOT, best first.
PACKAGE is nil for a call with one argument.  The candidates are models,
seeds and snapshots named NAME: those of PACKAGE when it is given;
otherwise all, in the order of `dbt-project-lookup-rank' for CALLER, the
package of the calling file.  As in dbt 2.0.11, with VERSION only the
models that have that version are candidates, none without versions;
without VERSION, a model with versions is a candidate through one
version, the latest_version of the model's package or else the highest
version in that package, as a package's model and the project's
namesake have versions of their own."
  (let* ((candidates (seq-filter (lambda (entity) (or (null package) (equal package (plist-get entity :package))))
                                 (dbt-project-entities root '(model seed snapshot) name)))
         (latest-version (lambda (owner)
                           (or (seq-some (lambda (entry) (plist-get entry :latest-version))
                                         (dbt-project-entities root '(properties) name owner))
                               (car (sort (delq nil (mapcar (lambda (entity)
                                                              (and (equal owner (plist-get entity :package))
                                                                   (plist-get entity :version)))
                                                            candidates))
                                          (lambda (one other) (string-version-lessp other one))))))))
    (seq-sort-by (dbt-project-lookup-rank root caller)
                 #'<
                 (seq-filter (lambda (entity)
                               (let ((own (plist-get entity :version)))
                                 (if version
                                     (equal version own)
                                   (or (null own) (equal (funcall latest-version (plist-get entity :package)) own)))))
                             candidates))))

(defun dbt-project-resolve (root target &optional caller)
  "Return the entities of ROOT that define TARGET, best first.
TARGET is a plist (:kind KIND :qualifier QUALIFIER :name NAME :version
VERSION).  KIND is `ref', `source', `doc', `macro' or `analysis', which
no call names.  QUALIFIER is the package of a `ref', a `doc', a macro
or an analysis, or the source of a table, and nil for a source itself.
VERSION is that of a `ref', nil for the latest.  CALLER is the package
of the calling file, which a `ref' or a `source' looks in as
`dbt-project-lookup-rank' says; a `doc' looks in the project first,
whatever the flag that it reads, then in the packages by the order of
their names, as dbt 2.0.11 does whatever packages.yml lists first."
  (let ((qualifier (plist-get target :qualifier))
        (name (plist-get target :name)))
    (pcase (plist-get target :kind)
      ('ref (dbt-project-resolve-ref root qualifier name caller (plist-get target :version)))
      ('source (seq-sort-by (dbt-project-lookup-rank root caller)
                            #'<
                            (if qualifier
                                (seq-filter (lambda (table) (equal qualifier (plist-get table :source)))
                                            (dbt-project-entities root '(table) name))
                              (dbt-project-entities root '(source) name))))
      ('doc (let ((project (dbt-project-name root)))
              (seq-sort-by (lambda (doc)
                             (let ((package (plist-get doc :package)))
                               (if (equal package project) "" package)))
                           #'string<
                           (dbt-project-entities root '(doc) name qualifier))))
      ('analysis (dbt-project-entities root '(analysis) name qualifier))
      ('macro (dbt-project-entities root '(macro test) name qualifier)))))

;;;; The lineage that needs no server

(defun dbt-project-node-id (entity)
  "Return the unique identifier of ENTITY, in dbt's form.
That of a version of a model ends with .vVERSION."
  (if (eq (plist-get entity :kind) 'table)
      (format "source.%s.%s.%s" (plist-get entity :package)
              (plist-get entity :source) (plist-get entity :name))
    (format "%s.%s.%s%s" (plist-get entity :kind) (plist-get entity :package) (plist-get entity :name)
            (if (plist-get entity :version) (format ".v%s" (plist-get entity :version)) ""))))

(defun dbt-project-graph (root)
  "Return the nodes of ROOT and its packages, each with what it depends on."
  (dbt-project-cached
   root 'graph
   (lambda (root)
     (mapcar
      (lambda (entity)
        (let ((table (eq (plist-get entity :kind) 'table))
              (calls (cond ((memq (plist-get entity :kind) '(seed table)) nil)
                           ((plist-get entity :relation)
                            (with-temp-buffer
                              (insert (plist-get entity :relation))
                              (dbt-project-calls)))
                           (t (plist-get (dbt-project-file-contents (plist-get entity :file)) :calls)))))
          (list :id (dbt-project-node-id entity)
                :name (if table
                          (format "%s.%s" (plist-get entity :source) (plist-get entity :name))
                        (plist-get entity :name))
                :type (if table 'source (plist-get entity :kind))
                :package (plist-get entity :package)
                :file (plist-get entity :file)
                :line (plist-get entity :line)
                :parents (seq-uniq
                          (delq nil (mapcar (lambda (call)
                                              (when-let* ((target (car (dbt-project-resolve
                                                                        root call (plist-get entity :package)))))
                                                (dbt-project-node-id target)))
                                            calls))))))
      (dbt-project-entities root '(model seed snapshot analysis table))))))

(provide 'dbt-project)
;;; dbt-project.el ends here
