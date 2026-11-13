;;; dbt-new.el --- New models, macros, snapshots and YAML entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Commands that write the files and YAML entries a dbt project grows by,
;; in the layout of dbt Labs' style guide, and visit them with point where
;; the user types next:
;;
;; - `dbt-new-staging-model': models/staging/SOURCE/stg_SOURCE__TABLE.sql
;;   from a source table, and its entry in _SOURCE__models.yml;
;; - `dbt-new-model' and `dbt-new-incremental-model': a model file;
;; - `dbt-new-macro': macros/NAME.sql;
;; - `dbt-new-snapshot': a snapshot defined in YAML, as dbt 1.9 and later
;;   define them;
;; - `dbt-add-properties': the YAML entry of the current model;
;; - `dbt-add-unit-test': a unit test of the current model, with one input
;;   for each model or source it reads.
;;
;; `dbt-new' reads which of them to run.  Each command refuses to replace
;; a file that exists, and saves what it writes.

;;; Code:

(require 'dbt-mode)
(require 'dbt-project)

(defun dbt-new--write-file (file before after)
  "Write the new FILE with BEFORE then AFTER, visit it, and put point between.
Refuse a FILE that exists."
  (when (file-exists-p file)
    (user-error "%s exists already" file))
  (make-directory (file-name-directory file) t)
  (find-file file)
  (insert before)
  (save-excursion (insert after))
  (save-buffer))

(defun dbt-new--append-yaml (file section entry)
  "Add ENTRY, YAML text, to the top-level list SECTION of FILE, and save it.
FILE is created with `version: 2' when it does not exist, and SECTION
when FILE has none.  Return the buffer of FILE, with point at ENTRY."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" (regexp-quote section) ":[ \t]*$") nil t)
        ;; The list ends before the next top-level key.
        (progn (forward-line 1)
               (if (re-search-forward "^[^ \t\n#]" nil t) (goto-char (match-beginning 0)) (goto-char (point-max))))
      (goto-char (point-max))
      (insert (if (= (point-min) (point-max)) "version: 2\n" "") "\n" section ":\n"))
    (unless (bolp) (insert "\n"))
    (save-excursion (insert entry))
    (save-buffer)
    (current-buffer)))

(defun dbt-new--properties-file (resource)
  "Return the YAML file that holds, or should hold, the entry of RESOURCE.
It is the file of its entry when it has one, else a YAML file of its
directory with a models section, else _DIRECTORY__models.yml there."
  (let* ((root (dbt-mode--root))
         (directory (file-name-directory (plist-get resource :file)))
         (sections (seq-filter (lambda (entity) (member (plist-get entity :section) dbt-project-resource-sections))
                               (dbt-project-entities root '(properties)))))
    (or (plist-get (seq-find (lambda (entity) (equal (plist-get entity :name) (plist-get resource :name))) sections)
                   :file)
        (plist-get (seq-find (lambda (entity) (equal (file-name-directory (plist-get entity :file)) directory))
                             sections)
                   :file)
        (expand-file-name (format "_%s__models.yml" (file-name-nondirectory (directory-file-name directory)))
                          directory))))

(defun dbt-new--model-file (prompt)
  "Read the file of a new model with PROMPT, under the project's model paths."
  (let* ((root (dbt-mode--root))
         (models (file-name-as-directory
                  (expand-file-name (car (plist-get (dbt-project-config root) :model-paths)) root))))
    (read-file-name prompt (if (and buffer-file-name (string-prefix-p models buffer-file-name))
                               (file-name-directory buffer-file-name)
                             models))))

(defun dbt-new-staging-model (source table)
  "Write the staging model of the TABLE of SOURCE, with its YAML entry.
The model is models/staging/SOURCE/stg_SOURCE__TABLE.sql and its entry
goes into _SOURCE__models.yml beside it, as dbt Labs' style guide lays
them out."
  (interactive
   (let* ((tables (mapcar (lambda (table) (format "%s.%s" (plist-get table :source) (plist-get table :name)))
                          (dbt-project-entities (dbt-mode--root) '(table))))
          (choice (completing-read "Staging model of source table: " tables nil t)))
     (split-string choice "\\.")))
  (let* ((root (dbt-mode--root))
         (directory (expand-file-name (format "%s/staging/%s/" (car (plist-get (dbt-project-config root) :model-paths))
                                              source)
                                      root))
         (name (format "stg_%s__%s" source table)))
    (dbt-new--write-file (expand-file-name (concat name ".sql") directory)
                         (format "with\n\nsource as (\n\n    select * from {{ source('%s', '%s') }}\n\n),\n\nrenamed as (\n\n    select\n        "
                                 source table)
                         "*\n\n    from source\n\n)\n\nselect * from renamed\n")
    (dbt-new--append-yaml (expand-file-name (format "_%s__models.yml" source) directory) "models"
                          (format "  - name: %s\n    description: \"\"\n" name))))

(defun dbt-new-model (file)
  "Write the new model FILE, a select from a `ref' to fill in."
  (interactive (list (dbt-new--model-file "New model: ")))
  (dbt-new--write-file file "select *\nfrom {{ ref('" "') }}\n"))

(defun dbt-new-incremental-model (file)
  "Write the new incremental model FILE, which reads only new rows once built."
  (interactive (list (dbt-new--model-file "New incremental model: ")))
  (dbt-new--write-file file
                       "{{\n    config(\n        materialized='incremental',\n        unique_key='id'\n    )\n}}\n\nselect *\nfrom {{ ref('"
                       "') }}\n\n{% if is_incremental() %}\nwhere updated_at > (select max(updated_at) from {{ this }})\n{% endif %}\n"))

(defun dbt-new-macro (name)
  "Write the macro NAME in its own file of the project's first macro path."
  (interactive (list (read-string "New macro: ")))
  (unless (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" name)
    (user-error "Not a name for a macro: %s" name))
  (let ((root (dbt-mode--root)))
    (dbt-new--write-file (expand-file-name (format "%s/%s.sql" (car (plist-get (dbt-project-config root) :macro-paths))
                                                   name)
                                           root)
                         (format "{%% macro %s(" name) ") %}\n\n{% endmacro %}\n")))

(defun dbt-new-snapshot (name relation)
  "Add the snapshot NAME of RELATION, a ref or a source call, in YAML.
It goes into _snapshots.yml of the project's first snapshot path, with
the timestamp strategy to adjust."
  (interactive
   (let ((root (dbt-mode--root)))
     (list (read-string "New snapshot: ")
           (completing-read "Snapshot of: "
                            (append (mapcar (lambda (entity) (format "ref('%s')" (plist-get entity :name)))
                                            (dbt-project-entities root '(model seed)))
                                    (mapcar (lambda (table) (format "source('%s', '%s')" (plist-get table :source)
                                                                    (plist-get table :name)))
                                            (dbt-project-entities root '(table))))
                            nil t))))
  (let ((root (dbt-mode--root)))
    (pop-to-buffer
     (dbt-new--append-yaml (expand-file-name (format "%s/_snapshots.yml"
                                                     (car (plist-get (dbt-project-config root) :snapshot-paths)))
                                             root)
                           "snapshots"
                           (format (concat "  - name: %s\n    relation: %s\n    config:\n      unique_key: id\n"
                                           "      strategy: timestamp\n      updated_at: updated_at\n")
                                   name relation)))))

(defun dbt-add-properties ()
  "Visit the YAML entry of the current model, adding one if it has none."
  (interactive)
  (let ((resource (or (dbt-mode-current-resource) (user-error "No model here"))))
    (if (dbt-project-entities (dbt-mode--root) '(properties) (plist-get resource :name))
        (dbt-visit-properties)
      (pop-to-buffer (dbt-new--append-yaml (dbt-new--properties-file resource) "models"
                                           (format "  - name: %s\n    description: \"\"\n"
                                                   (plist-get resource :name)))))))

(defun dbt-add-unit-test ()
  "Add a unit test of the current model to its YAML file, and visit it.
The test has an input for each model, seed, snapshot and source the
model reads, with rows to fill in."
  (interactive)
  (let* ((root (dbt-mode--root))
         (resource (or (dbt-mode-current-resource) (user-error "No model here")))
         (graph (dbt-project-graph root))
         (node (seq-find (lambda (node) (equal (plist-get node :id) (dbt-project-node-id resource))) graph))
         (inputs (mapcar (lambda (id)
                           (let ((parent (seq-find (lambda (node) (equal (plist-get node :id) id)) graph)))
                             (format "      - input: %s\n        rows:\n          - {}\n"
                                     (if (eq (plist-get parent :type) 'source)
                                         (apply #'format "source('%s', '%s')" (split-string (plist-get parent :name) "\\."))
                                       (format "ref('%s')" (plist-get parent :name))))))
                         (plist-get node :parents))))
    (pop-to-buffer
     (dbt-new--append-yaml (dbt-new--properties-file resource) "unit_tests"
                           (format "  - name: test_%s\n    model: %s\n    given:\n%s    expect:\n      rows:\n        - {}\n"
                                   (plist-get resource :name) (plist-get resource :name)
                                   (if inputs (apply #'concat inputs) "      []\n"))))))

(defconst dbt-new-commands
  '(("Staging model of a source table" . dbt-new-staging-model)
    ("Model" . dbt-new-model)
    ("Incremental model" . dbt-new-incremental-model)
    ("Macro" . dbt-new-macro)
    ("Snapshot" . dbt-new-snapshot)
    ("YAML entry of this model" . dbt-add-properties)
    ("Unit test of this model" . dbt-add-unit-test))
  "The commands of `dbt-new', by what they make.")

(defun dbt-new (command)
  "Run COMMAND, one of `dbt-new-commands', read with completion."
  (interactive (list (alist-get (completing-read "New: " dbt-new-commands nil t) dbt-new-commands nil nil #'equal)))
  (call-interactively command))

(provide 'dbt-new)
;;; dbt-new.el ends here
