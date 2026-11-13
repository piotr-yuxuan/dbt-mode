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
;; each entry.  Installed packages, under dbt_packages, are indexed with
;; the project, each under its own name.
;;
;; An entity of the index is a plist:
;;
;;   (:kind KIND :name NAME :package PACKAGE :file FILE :line LINE
;;    :source SOURCE :section SECTION)
;;
;; KIND is one of `model', `seed', `snapshot', `analysis', `macro', `test'
;; (a generic test), `doc', `source', `table' (a table of the source
;; SOURCE), `exposure' and `properties' (an entry of the YAML section
;; SECTION, such as "models").  LINE counts from 1.
;;
;; `dbt-project-graph' reads the `ref' and `source' calls of every model,
;; snapshot and analysis, for a lineage that needs no server.  Its nodes
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
The language server needs dbt 2.0 or later, which runs it as `dbt lsp'."
  :type 'string)

(defcustom dbt-preview-limit 50
  "The number of rows that a preview of a model or a query shows."
  :type 'natnum)

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
is the outermost directory holding a dbt_project.yml, as each package
installed under dbt_packages has one of its own."
  (let ((directory (or file buffer-file-name default-directory))
        root)
    (while-let ((found (and directory (locate-dominating-file directory dbt-project-file))))
      (setq root (file-name-as-directory (expand-file-name found))
            directory (file-name-directory (directory-file-name root))))
    root))

;;;; Reading YAML

(defun dbt-project-yaml-unquote (value)
  "Return the YAML scalar VALUE without its quotes; nil when empty or nil."
  (cond ((or (null value) (string-empty-p value)) nil)
        ((string-match "\\`\\(['\"]\\)\\(.*\\)\\1\\'" value) (match-string 2 value))
        (t value)))

(defun dbt-project-yaml-line (line)
  "Return the key of the YAML LINE as a plist, or nil when it holds none.
The plist has :column, the column of the key, which follows the \"- \"
of a list item, :key, :item, non-nil when the line starts a list item,
and :value, unquoted and nil when empty.  A comment after the value is
dropped.  Of an item written as a flow mapping, such as \"- {name: x}\",
the key is its `name'."
  (when (seq-find (lambda (regexp) (string-match regexp line))
                  '("\\`\\(?1: *\\(?4:- +\\)\\){\\(?:[^}]*,\\)? *\\(?2:name\\): *\\(?3:[^,}]*?\\) *[,}]"
                    "\\`\\(?1: *\\(?4:- +\\)?\\)\\(?2:[A-Za-z_][A-Za-z0-9_-]*\\): *\\(?3:.*?\\) *\\(?:#.*\\)?\\'"))
    (list :column (match-end 1)
          :key (match-string 2 line)
          :item (and (match-beginning 4) t)
          :value (dbt-project-yaml-unquote (match-string 3 line)))))

(defconst dbt-project-yaml-sections
  '("models" "seeds" "snapshots" "analyses" "macros" "sources" "exposures"
    "semantic_models" "metrics" "saved_queries")
  "The top-level YAML sections whose entries the index keeps.")

(defun dbt-project-yaml-entries (text)
  "Return the entries of TEXT, the contents of a dbt properties file.
Each entry is a plist (:section SECTION :name NAME :line LINE :source
SOURCE): an item of a top-level section of `dbt-project-yaml-sections',
or, with SECTION \"tables\", a table of the source SOURCE.  LINE counts
from 1.  An item is an entry when its `name' key sits at the first level
of its section, or right under the `tables' of a source."
  (let ((number 0) section source keys entries)
    ;; KEYS holds the open keys other than `name', innermost first, as
    ;; (COLUMN . KEY); a line closes those at its column or deeper.
    ;; SOURCE is the name of the last item at the first level.
    (dolist (line (split-string text "\n"))
      (setq number (1+ number))
      (when-let* ((parsed (dbt-project-yaml-line line)))
        (let ((column (plist-get parsed :column))
              (key (plist-get parsed :key))
              (name (plist-get parsed :value)))
          (while (and keys (>= (car (car keys)) column))
            (pop keys))
          (cond ((zerop column)
                 (setq section (car (member key dbt-project-yaml-sections)) keys nil))
                ((not (equal key "name"))
                 (push (cons column key) keys))
                ((null keys)
                 (push (list :section section :name name :line number) entries)
                 (setq source name))
                ((equal (list section (cdr (car keys))) '("sources" "tables"))
                 (push (list :section "tables" :name name :line number :source source)
                       entries))))))
    (seq-filter (lambda (entry) (and (plist-get entry :section) (plist-get entry :name)))
                (nreverse entries))))

(defun dbt-project-config (root)
  "Return the configuration of the project at ROOT as a plist.
The plist has :name, nil when dbt_project.yml sets none, and :model-paths,
:seed-paths, :analysis-paths and :target-path, with dbt's defaults when
it does not set them."
  (let ((text (with-temp-buffer
                (insert-file-contents (expand-file-name dbt-project-file root))
                (buffer-string)))
        (config (list :name nil :target-path "target" :model-paths '("models")
                      :seed-paths '("seeds") :analysis-paths '("analyses"))))
    (dolist (key '(:name :target-path))
      (when (string-match (format "^%s: *\\([^#\n]*?\\) *\\(?:#.*\\)?$" (substring (symbol-name key) 1))
                          text)
        (setq config (plist-put config key (dbt-project-yaml-unquote (match-string 1 text))))))
    ;; A list of paths is either [a, b] on the key's line, or items below it.
    (dolist (key '(:model-paths :seed-paths :analysis-paths))
      (when (string-match (format "^%s:\\(?: *\\[\\([^]\n]*\\)\\]\\| *\\(?:#.*\\)?\\(\\(?:\n +- +.*\\)+\\)\\)"
                                  (substring (symbol-name key) 1))
                          text)
        (let ((flow (match-string 1 text)))
          (setq config
                (plist-put config key
                           (mapcar (lambda (path) (dbt-project-yaml-unquote (string-trim path "[ \t-]+")))
                                   (split-string (or flow (match-string 2 text)) (if flow "," "\n") t
                                                 "[ \t]+")))))))
    config))

;;;; Reading Jinja

(defconst dbt-project-block-regexps
  '((macro . "{%-?[ \t\n]*macro[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*(")
    (test . "{%-?[ \t\n]*test[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*(")
    (snapshot . "{%-?[ \t\n]*snapshot[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*-?%}")
    (doc . "{%-?[ \t\n]*docs[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*-?%}"))
  "Alist from a kind of entity to the regexp of the Jinja block defining it.
The first group matches the name.")

(defconst dbt-project-call-regexps
  (let ((call (lambda (function qualified)
                ;; Group 1 matches the name, and group 2 what qualifies it:
                ;; optional for a package, required for a source.
                (concat "\\_<" function "[ \t\n]*([ \t\n]*['\"]"
                        "\\(?:\\(?2:[^'\"\n]+\\)['\"][ \t\n]*,[ \t\n]*['\"]\\)" (if qualified "" "?")
                        "\\(?1:[^'\"\n]+\\)['\"]"))))
    `((ref . ,(funcall call "ref" nil))
      (source . ,(funcall call "source" t))
      (doc . ,(funcall call "doc" nil))))
  "Alist from the kind of a call of dbt's Jinja to its regexp.
Group 1 matches the name that the call gives; group 2 the package of a
`ref' or a `doc', when it is given, or the source of a `source'.")

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

(defun dbt-project-forget (&optional root)
  "Forget what was read of the project at ROOT, or of every project.
Interactively, ROOT is the project of the current buffer."
  (interactive (list (dbt-project-root)))
  (if root
      (dolist (key (hash-table-keys dbt-project--cache))
        (when (equal (car key) root) (remhash key dbt-project--cache)))
    (clrhash dbt-project--cache)))

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
under dbt_packages.  A package comes once, at its first place: a local
package is edited where it lives, not in its copy under dbt_packages.
NAME falls back to the name of the directory."
  (dbt-project-cached
   root 'packages
   (lambda (root)
     (let ((installed (expand-file-name "dbt_packages" root))
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
directories, those of `dbt-project-skipped-directories' and those of
nested projects, such as local packages.  Hidden files go too, such as
the lock files .#NAME of the buffers being edited."
  (directory-files-recursively
   directory "\\`[^.].*\\.\\(?:sql\\|py\\|csv\\|md\\|ya?ml\\)\\'" nil
   (lambda (subdirectory)
     (not (or (string-prefix-p "." (file-name-nondirectory subdirectory))
              (member (file-relative-name subdirectory directory)
                      dbt-project-skipped-directories)
              (file-exists-p (expand-file-name dbt-project-file subdirectory)))))))

(defun dbt-project-index (root)
  "Return the entities that the project at ROOT and its packages define."
  (dbt-project-cached
   root 'index
   (lambda (root)
     (let (entities)
       (pcase-dolist (`(,package ,directory ,config) (dbt-project-packages root))
         (dolist (file (dbt-project-source-files directory))
           (let ((extension (file-name-extension file))
                 (head (list :package package :file file)))
             (when-let* ((kind (dbt-project-file-kind file directory config)))
               (push (append (list :kind kind :name (file-name-base file) :line 1) head) entities))
             (unless (equal extension "csv")
               (with-temp-buffer
                 (insert-file-contents file)
                 (pcase-dolist (`(,kind . ,regexp) dbt-project-block-regexps)
                   (goto-char (point-min))
                   (while (re-search-forward regexp nil t)
                     (push (append (list :kind kind :name (match-string-no-properties 1)
                                         :line (line-number-at-pos (match-beginning 0)))
                                   head)
                           entities)))
                 (when (string-prefix-p "y" extension)
                   (dolist (entry (dbt-project-yaml-entries (buffer-string)))
                     (push (append (list :kind (alist-get (plist-get entry :section)
                                                          '(("sources" . source) ("tables" . table)
                                                            ("exposures" . exposure))
                                                          'properties nil #'equal))
                                   entry head)
                           entities))))))))
       (nreverse entities)))))

(defun dbt-project-entities (root kinds &optional name package)
  "Return the entities of ROOT whose kind is one of KINDS.
Keep only those named NAME, and of PACKAGE, when they are given."
  (seq-filter (lambda (entity)
                (and (memq (plist-get entity :kind) kinds)
                     (or (null name) (equal name (plist-get entity :name)))
                     (or (null package) (equal package (plist-get entity :package)))))
              (dbt-project-index root)))

(defun dbt-project-file-entity (root file)
  "Return the model, seed, snapshot or analysis that FILE of ROOT defines.
FILE need not exist yet, nor the index know it: a model, a seed or an
analysis is told by its place alone.  A snapshot is the first that FILE
defines, by the index."
  (let* ((file (expand-file-name file))
         (package (seq-find (lambda (package) (string-prefix-p (nth 1 package) file))
                            (reverse (dbt-project-packages root))))
         (kind (dbt-project-file-kind file (nth 1 package) (nth 2 package))))
    (if kind
        (list :kind kind :name (file-name-base file) :package (car package) :file file :line 1)
      (seq-find (lambda (entity)
                  (and (eq (plist-get entity :kind) 'snapshot) (equal (plist-get entity :file) file)))
                (dbt-project-index root)))))

(defun dbt-project-resolve-ref (root package name &optional caller)
  "Return what `ref' with PACKAGE and NAME means in ROOT, best first.
PACKAGE is nil for a call with one argument.  The candidates are models,
seeds and snapshots named NAME: those of PACKAGE when it is given;
otherwise those of CALLER, the package of the calling file, then those
of the project, then the others."
  (let ((project (dbt-project-name root)))
    (seq-sort-by (lambda (entity)
                   (let ((owner (plist-get entity :package)))
                     (cond ((equal owner (or package caller)) 0)
                           ((equal owner project) 1)
                           (t 2))))
                 #'<
                 (seq-filter (lambda (entity) (or (null package) (equal package (plist-get entity :package))))
                             (dbt-project-entities root '(model seed snapshot) name)))))

(defun dbt-project-resolve (root target &optional caller)
  "Return the entities of ROOT that define TARGET, best first.
TARGET is a plist (:kind KIND :qualifier QUALIFIER :name NAME).  KIND is
`ref', `source', `doc' or `macro'.  QUALIFIER is the package of a `ref',
a `doc' or a macro, or the source of a table, and nil for a source
itself.  CALLER is the package of the calling file, for a `ref'."
  (let ((qualifier (plist-get target :qualifier))
        (name (plist-get target :name)))
    (pcase (plist-get target :kind)
      ('ref (dbt-project-resolve-ref root qualifier name caller))
      ('source (if qualifier
                   (seq-filter (lambda (table) (equal qualifier (plist-get table :source)))
                               (dbt-project-entities root '(table) name))
                 (dbt-project-entities root '(source) name)))
      ('doc (dbt-project-entities root '(doc) name qualifier))
      ('macro (dbt-project-entities root '(macro test) name qualifier)))))

;;;; The lineage that needs no server

(defun dbt-project-node-id (entity)
  "Return the unique identifier of ENTITY, in dbt's form."
  (if (eq (plist-get entity :kind) 'table)
      (format "source.%s.%s.%s" (plist-get entity :package)
              (plist-get entity :source) (plist-get entity :name))
    (format "%s.%s.%s" (plist-get entity :kind) (plist-get entity :package) (plist-get entity :name))))

(defun dbt-project-graph (root)
  "Return the nodes of ROOT and its packages, each with what it depends on."
  (dbt-project-cached
   root 'graph
   (lambda (root)
     (mapcar
      (lambda (entity)
        (let ((table (eq (plist-get entity :kind) 'table))
              parents)
          (unless (memq (plist-get entity :kind) '(seed table))
            (with-temp-buffer
              (insert-file-contents (plist-get entity :file))
              (pcase-dolist (`(,kind . ,regexp) dbt-project-call-regexps)
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (when-let* ((target (car (dbt-project-resolve
                                            root (list :kind kind :qualifier (match-string 2) :name (match-string 1))
                                            (plist-get entity :package))))
                              ((not (eq kind 'doc))))
                    (push (dbt-project-node-id target) parents))))))
          (list :id (dbt-project-node-id entity)
                :name (if table
                          (format "%s.%s" (plist-get entity :source) (plist-get entity :name))
                        (plist-get entity :name))
                :type (if table 'source (plist-get entity :kind))
                :package (plist-get entity :package)
                :file (plist-get entity :file)
                :line (plist-get entity :line)
                :parents (seq-uniq (nreverse parents)))))
      (dbt-project-entities root '(model seed snapshot analysis table))))))

(provide 'dbt-project)
;;; dbt-project.el ends here
