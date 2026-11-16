;;; dbt-mode.el --- Edit dbt projects: Jinja SQL, navigation and dbt lsp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 piotr-yuxuan
;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;; Author: piotr-yuxuan <piotr-yuxuan@users.noreply.github.com>
;; Maintainer: piotr-yuxuan <piotr-yuxuan@users.noreply.github.com>
;; URL: https://github.com/piotr-yuxuan/dbt-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (lsp-mode "9.0.0"))
;; Keywords: languages, sql, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the European Union Public Licence, version 1.2 (see
;; the file LICENSE), or under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version (see the file COPYING).

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the licences
;; for more details.

;;; Commentary:

;; dbt-mode edits the models, macros, snapshots and tests of a dbt
;; project.  It is `sql-mode' that also reads Jinja: `{# #}' comments are
;; comments, and the tags `{{ }}' and `{% %}' show their keywords, dbt's
;; functions and the macros they call.  A `.sql' file below a
;; dbt_project.yml opens in it, unless it lies under the target-path of
;; the project, target by default, or under logs.
;;
;; Without any dbt executable, it reads the project itself (see
;; dbt-project.el) for:
;;
;; - `xref': on a `ref', a `source', a `doc' or a macro call, `M-.' goes
;;   to its definition, and `M-?' lists where it is used;
;; - completion of the arguments of `ref', `source' and `doc', and of
;;   macros, packages' included;
;; - `imenu' entries for the CTEs and the Jinja blocks;
;; - `dbt-lineage', the upstream and downstream of a model;
;; - `dbt-find-resource', `dbt-visit-properties' and `dbt-insert-ref'.
;;
;; With dbt 2's proprietary build, `dbt-lsp-deferred' starts `dbt lsp'
;; through lsp-mode, which then takes over definitions, references,
;; hover, completion and diagnostics, and the commands above use the
;; server where it knows better: compiled SQL, previews of results and
;; lineage.  dbt-lsp.el is that client.  `dbt-dispatch' runs dbt's
;; commands on the current model.

;;; Code:

(require 'dbt-project)
(require 'sql)
(require 'xref)

(defvar font-lock-beg)
(defvar font-lock-end)

(autoload 'dbt-dispatch "dbt-cli" nil t)
(autoload 'dbt-breadcrumb-mode "dbt-breadcrumb" nil t)
(autoload 'dbt-describe "dbt-node" nil t)
(autoload 'dbt-node-eldoc "dbt-node")
(autoload 'dbt-lsp-rename-model "dbt-lsp" nil t)
(autoload 'dbt-lsp-format-buffer "dbt-lsp" nil t)
(autoload 'dbt-lsp-fix-all "dbt-lsp" nil t)
(autoload 'dbt-new "dbt-new" nil t)
(autoload 'dbt-new-staging-model "dbt-new" nil t)
(autoload 'dbt-new-model "dbt-new" nil t)
(autoload 'dbt-new-incremental-model "dbt-new" nil t)
(autoload 'dbt-new-macro "dbt-new" nil t)
(autoload 'dbt-new-snapshot "dbt-new" nil t)
(autoload 'dbt-add-properties "dbt-new" nil t)
(autoload 'dbt-add-unit-test "dbt-new" nil t)
(autoload 'dbt-close-block "dbt-edit" nil t)
(autoload 'dbt-jump-to-matching-tag "dbt-edit" nil t)
(autoload 'dbt-edit-delete-backward "dbt-edit" nil t)
(autoload 'dbt-edit-pair-jinja-opener "dbt-edit")
(autoload 'dbt-edit-join-jinja-sign "dbt-edit")
(autoload 'dbt-edit-skip-jinja-closer "dbt-edit")
(autoload 'dbt-edit-beginning-of-cte "dbt-edit")
(autoload 'dbt-edit-end-of-cte "dbt-edit")
(autoload 'dbt-edit-blocks "dbt-edit")
(autoload 'dbt-edit-indent-line "dbt-edit")
(autoload 'dbt-edit-indent-region "dbt-edit")
(autoload 'dbt-edit-electric-indent-p "dbt-edit")
(autoload 'dbt-compiled-show "dbt-compiled")
(autoload 'dbt-compiled-diff "dbt-compiled" nil t)
(autoload 'dbt-cli-show "dbt-cli")
(autoload 'dbt-cli-preview-cte "dbt-cli")
(autoload 'dbt-cli-format "dbt-cli")
(autoload 'dbt-dashboard "dbt-dashboard" nil t)
(declare-function dbt-lineage-show "dbt-lineage")
(declare-function dbt-lsp-workspace "dbt-lsp")
(declare-function dbt-lsp-proxy-answers-p "dbt-lsp")
(declare-function dbt-lsp-show "dbt-lsp")
(declare-function dbt-lsp-preview-cte "dbt-lsp")
(declare-function dbt-lsp-lineage "dbt-lsp")
(declare-function dbt-lsp-hierarchy-p "dbt-lsp")
(declare-function dbt-lsp-hierarchy-lineage "dbt-lsp")

;;;; Faces

(defface dbt-jinja-delimiter-face
  '((t :inherit font-lock-preprocessor-face))
  "Face of the delimiters of Jinja tags, such as `{{' and `%}'."
  :group 'dbt)

;;;; Jinja

(defconst dbt-mode-jinja-keywords
  '("if" "elif" "else" "endif" "for" "endfor" "in" "is" "not" "and" "or"
    "macro" "endmacro" "set" "endset" "call" "endcall" "filter" "endfilter"
    "do" "import" "from" "as" "with" "endwith" "include" "block" "endblock"
    "raw" "endraw" "break" "continue" "return" "materialization"
    "endmaterialization" "test" "endtest" "snapshot" "endsnapshot" "docs"
    "enddocs")
  "Jinja's keywords, and those that dbt adds.")

(defconst dbt-mode-functions
  '("ref" "source" "config" "var" "env_var" "doc" "is_incremental" "log" "print"
    "run_query" "statement" "load_result" "return" "dispatch" "zip" "fromjson"
    "tojson" "fromyaml" "toyaml" "as_bool" "as_number" "as_text" "as_native"
    "local_md5" "set_strict" "diff_of_two_dicts" "load_relation" "render"
    "store_result" "store_raw_result" "set" "exceptions")
  "The functions of dbt's Jinja context.")

(defconst dbt-mode-variables
  '("this" "target" "model" "graph" "execute" "flags" "invocation_id"
    "run_started_at" "modules" "builtins" "project_name" "schemas"
    "database_schemas" "selected_resources" "results" "adapter" "api"
    "context" "sql" "pre_hooks" "post_hooks" "true" "false" "none" "True"
    "False" "None")
  "The variables of dbt's Jinja context, and Jinja's constants.")

(defcustom dbt-mode-jinja-max-tag-length 4000
  "The longest Jinja tag, in characters, that dbt-mode looks across.
Font lock extends a region to the whole tag it starts or ends in, and
finds the tag around point, within this many characters."
  :type 'natnum
  :group 'dbt)

(defun dbt-mode--tag-end (limit &optional mark)
  "Return the end of the Jinja tag whose opener ends at point, or nil.
The closer `}}' or `%}' ends the tag, outside its strings and the braces
of its dict literals.  When another opener, or LIMIT, comes first, the
tag is not closed: return nil, with point at that opener or at LIMIT.
With MARK, give the quotes of each string of the tag the syntax of
string fences, for `syntax-propertize-function'."
  (let ((depth 0))
    (catch 'end
      (while (re-search-forward
              (concat "{[{%#]\\|[{}]\\|%}\\|\\("
                      "\"[^\"\\]*\\(?:\\\\\\(?:.\\|\n\\)[^\"\\]*\\)*\""
                      "\\|'[^'\\]*\\(?:\\\\\\(?:.\\|\n\\)[^'\\]*\\)*'\\)")
              limit 'move)
        (cond ((match-beginning 1)
               (when mark
                 (put-text-property (match-beginning 1) (1+ (match-beginning 1))
                                    'syntax-table (string-to-syntax "|"))
                 (put-text-property (1- (match-end 1)) (match-end 1) 'syntax-table (string-to-syntax "|"))))
              ((equal (match-string 0) "{") (setq depth (1+ depth)))
              ((equal (match-string 0) "}")
               (if (and (zerop depth) (eq (char-after) ?}))
                   (throw 'end (1+ (point)))
                 (setq depth (max 0 (1- depth)))))
              ((equal (match-string 0) "%}") (when (zerop depth) (throw 'end (point))))
              (t (goto-char (match-beginning 0))
                 (throw 'end nil)))))))

(defun dbt-mode-jinja-bounds (&optional position)
  "Return (START . END) of the Jinja tag `{{ }}' or `{% %}' around POSITION.
POSITION defaults to point.  Return nil outside a tag, or when the tag
is longer than `dbt-mode-jinja-max-tag-length'.  END is after the
closing delimiter, or nil when the tag is not closed yet."
  (save-excursion
    (let* ((position (or position (point)))
           (limit dbt-mode-jinja-max-tag-length)
           (floor (max (point-min) (- position limit)))
           bounds)
      (goto-char position)
      ;; The nearest opener may be that of a tag in a string of the tag
      ;; around POSITION, which ends before it.
      (while (and (not bounds) (re-search-backward "{[{%]" floor t))
        (let ((start (point))
              (state (syntax-ppss)))
          (forward-char 2)
          (let ((end (dbt-mode--tag-end (min (point-max) (+ position limit)))))
            (cond ((not (if end (> end position) (>= (point) position)))
                   ;; A tag that ends before POSITION is in no other tag
                   ;; but one around the SQL string it is in: the search
                   ;; stops there, or at the start of that string.
                   (unless (eq (nth 3 state) t)
                     (setq floor (max floor (if (nth 3 state) (nth 8 state) start)))))
                  ;; In a comment, a tag is no tag.
                  ((not (nth 4 state))
                   (setq bounds (cons start end)))))
          (goto-char start)))
      bounds)))

(defun dbt-mode-cte-regexp (&optional name)
  "Return the regexp of the start of a CTE, up to its parenthesis.
The CTE is named NAME, or anything; group 1 matches the name.  It
needs `case-fold-search', as SQL ignores case."
  (format "^[ \t]*\\(?:)[ \t]*\\)?\\(?:with[ \t]+\\|,[ \t]*\\)?\\(%s\\)[ \t]+as[ \t]*("
          (if name (regexp-quote name) "[A-Za-z_][A-Za-z0-9_]*")))

(defun dbt-mode--extend-region ()
  "Extend the region of font lock to the whole Jinja tags at its ends.
For `font-lock-extend-region-functions'."
  (let ((start (car (dbt-mode-jinja-bounds font-lock-beg)))
        (end (cdr (dbt-mode-jinja-bounds font-lock-end))))
    (when (and start (< start font-lock-beg))
      (setq font-lock-beg start))
    (when (and end (> end font-lock-end))
      (setq font-lock-end end))
    (or start end)))

(defun dbt-mode--match-tag (limit)
  "Find the next Jinja tag before LIMIT, outside a comment, for font lock.
Group 1 matches its contents, and groups 2 and 3 its delimiters.  An
opener that meets another before its closer is still being typed: it
makes no tag, so that it does not swallow the next tag's closer."
  (let (end)
    (while (and (not end) (re-search-forward "{[{%]-?" limit t))
      (let ((start (match-beginning 0))
            (open (match-end 0)))
        (setq end (and (not (nth 4 (save-excursion (syntax-ppss start))))
                       (dbt-mode--tag-end limit)))
        (if (not end)
            (goto-char (1+ start))
          (let ((close (- end (if (eq (char-before (- end 2)) ?-) 3 2))))
            (goto-char end)
            (set-match-data (list start end open (max open close) start open close end))))))
    end))

(defconst dbt-mode--font-lock-keywords
  (let ((within '(progn (goto-char (match-beginning 1)) (match-end 1)))
        (after '(goto-char (match-end 0))))
    `((dbt-mode--match-tag
       (2 'dbt-jinja-delimiter-face t)
       (3 'dbt-jinja-delimiter-face t)
       ("\\_<\\(?:macro\\|test\\|materialization\\)[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)"
        ,within ,after (1 'font-lock-function-name-face))
       (,(concat "\\_<" (regexp-opt dbt-mode-jinja-keywords t) "\\_>")
        ,within ,after (1 'font-lock-keyword-face))
       (,(concat "\\_<" (regexp-opt dbt-mode-functions t) "[ \t\n]*(")
        ,within ,after (1 'font-lock-builtin-face))
       (,(concat "\\_<" (regexp-opt dbt-mode-variables t) "\\_>")
        ,within ,after (1 'font-lock-constant-face))
       ("\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)\\.[A-Za-z_][A-Za-z0-9_]*[ \t\n]*("
        ,within ,after (1 'font-lock-type-face))
       ("\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*("
        ,within ,after (1 'font-lock-function-call-face)))
      ;; The name of a CTE, which `sql-mode' leaves bare, or takes for a
      ;; keyword, such as final or source.
      (,(dbt-mode-cte-regexp) (1 'font-lock-function-name-face))))
  "Font lock of the Jinja tags and of the CTEs, before that of `sql-mode'.")

(defconst dbt-mode--syntax-propertize
  (syntax-propertize-rules
   ;; As `sql-mode', keep `/-' and `-*' from starting a comment, in the
   ;; pass of the Jinja rules, so that each decision sees those before it.
   ("\\(/-\\)" (1 "."))
   ("\\(-\\*\\)"
    (1 (unless (ppss-comment-depth (save-excursion (syntax-ppss (match-beginning 1))))
         (string-to-syntax "."))))
   ("\\({\\)#"
    (1 (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 1))))
         (string-to-syntax "!"))))
   ("#\\(}\\)"
    (1 (when (eq (nth 7 (save-excursion (syntax-ppss (match-beginning 1)))) 'syntax-table)
         (string-to-syntax "!"))))
   ("{[{%]"
    (0 (ignore
        (let ((start (match-beginning 0))
              (state (save-excursion (syntax-ppss (match-beginning 0)))))
          (unless (or (nth 4 state) (eq (nth 3 state) t))
            (save-excursion
              (put-text-property start (or (dbt-mode--tag-end (min (point-max) (+ (point) dbt-mode-jinja-max-tag-length))
                                                              t)
                                           (point))
                                 'syntax-multiline t))))))))
  "Make each Jinja comment `{# #}' a comment, and the strings of a tag strings.
As in `sql-mode', `/-' and `-*' start no comment.
A comment is fenced by generic comment fences, each decided alone, from
the state before it: `{#' opens a comment outside a string or a comment,
and `#}' closes the comment that a fence opened.  So the comment is one
whatever the chunks that `syntax-propertize' reads, and when its closer
is typed after.  The quotes of a string of a tag are string fences, so
that a quote of the other kind, or one that a backslash escapes, ends
nothing; a tag within a SQL string stays in it.  A tag is marked as one
multiline construct, read again whole after a change within it.")

;;;; What point is on

(defun dbt-mode-goto-cte (name)
  "Move point to the end of the start of the CTE NAME, and return point.
It is the first CTE NAME outside parentheses, else the first one: in
compiled SQL, the CTEs of an inlined ephemeral model lie in the
parentheses of its `__dbt__cte__' CTE, before the model's own.  The
match data are those of `dbt-mode-cte-regexp'.  Return nil when there is
no such CTE."
  (let ((regexp (dbt-mode-cte-regexp name))
        (case-fold-search t)
        found first)
    (goto-char (point-min))
    (while (and (not found) (re-search-forward regexp nil t))
      (if (= 0 (save-excursion (save-match-data (car (syntax-ppss (match-beginning 1))))))
          (setq found (match-beginning 0))
        (setq first (or first (match-beginning 0)))))
    (when (or found first)
      (goto-char (or found first))
      (re-search-forward regexp nil t))))

(defun dbt-mode-cte-query (&optional name)
  "Return the name of a CTE and a query of its rows, as a pair.
The CTE is that named NAME, or else the last that starts before the end
of the line of point.  The query is the buffer up to the end of that
CTE, then a select from it; Jinja stays as it is, but for the tags that
open a block still open at the WITH of the CTE's chain, as that of a
snapshot, which the query would not close."
  (let ((case-fold-search t))
    (save-excursion
      (unless (if name
                  (dbt-mode-goto-cte name)
                (end-of-line)
                (re-search-backward (dbt-mode-cte-regexp) nil t))
        (user-error "No CTE here"))
      (let* ((name (match-string-no-properties 1))
             (end (progn (goto-char (1- (match-end 0))) (forward-sexp) (point))))
        ;; The WITH of the chain is the nearest one outside parentheses,
        ;; strings and comments.
        (while (and (re-search-backward "\\_<with\\_>" nil 'move)
                    (let ((state (syntax-ppss)))
                      (or (> (car state) 0) (nth 8 state)))))
        ;; What comes before the WITH defines what the CTE may use, sets in
        ;; their if blocks included; the tag of a block open there goes.
        (let ((start (point-min))
              parts)
          (dolist (block (reverse (cdr (dbt-edit-blocks (point)))))
            (push (buffer-substring-no-properties start (nth 1 block)) parts)
            (setq start (nth 2 block)))
          (cons name (format "%s%s\nselect * from %s"
                             (apply #'concat (nreverse parts)) (buffer-substring-no-properties start end) name)))))))

(defconst dbt-mode-macro-call-regexp
  "\\_<\\(?:\\(?2:[A-Za-z_][A-Za-z0-9_]*\\)\\.\\)?\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*("
  "The regexp of a macro call, up to its parenthesis.
Group 1 matches the name of the macro, and group 2 its package, when the
call gives one.")

(defun dbt-mode-call-at-point ()
  "Return the call that point is on, as a target plist, or nil.
The plist is (:kind KIND :qualifier QUALIFIER :name NAME :version
VERSION), as `dbt-project-resolve' reads it: a call of `ref', `source'
or `doc', or, within a Jinja tag, of a macro."
  (let ((position (point))
        (limit dbt-mode-jinja-max-tag-length))
    (save-excursion
      (seq-some
       (pcase-lambda (`(,kind . ,regexp))
         (let ((bound (min (point-max) (+ position limit)))
               matched)
           (goto-char (max (point-min) (- position limit)))
           (while (and (setq matched (re-search-forward regexp bound t)) (< (match-end 0) position)))
           (and matched (<= (match-beginning 0) position)
                (list :kind kind :qualifier (match-string-no-properties 2)
                      :name (match-string-no-properties 1) :version (match-string-no-properties 3)))))
       (append dbt-project-call-regexps
               (and (dbt-mode-jinja-bounds) `((macro . ,dbt-mode-macro-call-regexp))))))))

(defun dbt-mode-entity-at-point (kinds)
  "Return the entity of the index that the current line defines, or nil.
Its kind is one of KINDS.  It is the entry of a YAML file whose `name'
is on the line, or a snapshot block, as the index read the file when
last saved."
  (when-let* ((root (dbt-project-root)))
    (let ((line (line-number-at-pos)))
      (seq-find (lambda (entity)
                  (and (equal (plist-get entity :file) buffer-file-name)
                       (eql (plist-get entity :line) line)))
                (dbt-project-entities root kinds)))))

(defun dbt-mode-target-at-point ()
  "Return what point is on, as a target plist, or nil.
It is the call of `dbt-mode-call-at-point', or else what the entity of
the line stands for, as `dbt-mode-entity-target' tells: in a YAML file,
the `name' of an entry stands for what the entry describes."
  (or (dbt-mode-call-at-point)
      (when-let* ((entity (dbt-mode-entity-at-point '(properties source table exposure snapshot))))
        (dbt-mode-entity-target entity (dbt-project-name (dbt-project-root))))))

;;;; Cross-references

(defun dbt-mode-identifier (target)
  "Return the xref identifier of TARGET, a plist of `dbt-mode-target-at-point'.
It reads KIND:NAME, or KIND:QUALIFIER/NAME, then @VERSION when TARGET
names a version of a model."
  (format "%s:%s%s%s" (plist-get target :kind)
          (if (plist-get target :qualifier) (concat (plist-get target :qualifier) "/") "")
          (plist-get target :name)
          (if (plist-get target :version) (concat "@" (plist-get target :version)) "")))

(defun dbt-mode-parse-identifier (identifier)
  "Return the target plist of IDENTIFIER, as `dbt-mode-identifier' writes it.
IDENTIFIER may be typed at the prompt of xref: refuse one that is not
KIND:NAME."
  (if (string-match "\\`\\(ref\\|source\\|doc\\|macro\\|analysis\\):\\(?:\\([^/]+\\)/\\)?\\([^@]+\\)\\(?:@\\(.+\\)\\)?\\'"
                    identifier)
      (list :kind (intern (match-string 1 identifier))
            :qualifier (match-string 2 identifier)
            :name (match-string 3 identifier)
            :version (match-string 4 identifier))
    (user-error "Not a dbt identifier: %s; complete one, such as ref:NAME" identifier)))

(defun dbt-mode-entity-target (entity project)
  "Return the target that names ENTITY, an entity of the PROJECT's index, or nil.
An entry of the properties of a resource names what its section holds,
as `dbt-project-section-kinds' tells, of its own package: dbt 2.0.9
calls a root entry that names a package's model unused (dbt1089).  An
item of a model's versions names that version of the model; an entry of
another section, such as semantic_models, names nothing, and neither
does an exposure, which no call names.  Another resource of a package
other than PROJECT is qualified by its package, and a version of a
model by its version."
  (let* ((entry (eq (plist-get entity :kind) 'properties))
         (kind (if entry
                   (alist-get (plist-get entity :section) (cons '("versions" . model) dbt-project-section-kinds)
                              nil nil #'equal)
                 (plist-get entity :kind)))
         (call (alist-get kind '((model . ref) (seed . ref) (snapshot . ref) (analysis . analysis)
                                 (table . source) (source . source) (macro . macro) (test . macro) (doc . doc))))
         (package (and (or entry (not (equal (plist-get entity :package) project))) (plist-get entity :package))))
    (when call
      (list :kind call
            :qualifier (pcase kind
                         ('table (plist-get entity :source))
                         ('source nil)
                         (_ package))
            :name (plist-get entity :name)
            :version (plist-get entity :version)))))

(defun dbt-mode-xref-backend ()
  "Return the xref backend of dbt-mode in a dbt project.
For `xref-backend-functions', ahead of lsp-mode's.  Where dbt lsp runs,
it answers only what dbt-mode does not resolve, such as dbt's own
macros: dbt lsp 2.0.9 lists the uses in the current file only, ignores
the version of a `ref' and resolves no `doc'.  dbt-lsp-plus answers
a request on a source, where it says so: the request goes to it."
  (and (dbt-project-root)
       (or (not (dbt-mode-server-p))
           ;; dbt lsp serves SQL buffers alone, where a target that no
           ;; call names is on the line of a snapshot block only: the
           ;; index is not read for the rest, which the server answers.
           (let ((target (if (save-excursion
                               (forward-line 0)
                               (looking-at-p (concat "[^\n]*" (alist-get 'snapshot dbt-project-block-regexps))))
                             (dbt-mode-target-at-point)
                           (dbt-mode-call-at-point))))
             (and target (dbt-mode-definitions target))))
       'dbt))

(defun dbt-mode--proxy-answers-p (request identifier)
  "Return non-nil when dbt-lsp-plus answers REQUEST on IDENTIFIER.
IDENTIFIER is that of a source at point.  REQUEST is :definition or
:references, as `dbt-lsp-proxy-answers-p' takes it: the proxy contract
steps aside per request and per kind.  The server answers for point: an
identifier typed at the prompt, which names another target, stays
dbt-mode's."
  (and (equal identifier (xref-backend-identifier-at-point 'dbt))
       (eq (plist-get (dbt-mode-parse-identifier identifier) :kind) 'source)
       (dbt-mode-server-p)
       (dbt-lsp-proxy-answers-p request "source")))

(cl-defmethod xref-backend-definitions :around ((_backend (eql dbt)) identifier)
  "Leave the definition of a source to dbt-lsp-plus where it answers it."
  (if (dbt-mode--proxy-answers-p :definition identifier)
      (xref-backend-definitions 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp))
    (cl-call-next-method)))

(cl-defmethod xref-backend-references :around ((_backend (eql dbt)) identifier)
  "Leave the uses of a source to dbt-lsp-plus where it answers them."
  (if (dbt-mode--proxy-answers-p :references identifier)
      (xref-backend-references 'xref-lsp (xref-backend-identifier-at-point 'xref-lsp))
    (cl-call-next-method)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql dbt)))
  "Return the identifier of what point is on, or nil."
  (when-let* ((target (dbt-mode-target-at-point)))
    (dbt-mode-identifier target)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql dbt)))
  "Return the identifiers of what the project defines."
  (let ((root (dbt-project-root)))
    (delete-dups
     (mapcar (lambda (entity) (dbt-mode-identifier (dbt-mode-entity-target entity (dbt-project-name root))))
             (dbt-project-entities root '(model seed snapshot analysis macro test doc source table))))))

(defun dbt-mode-caller (root)
  "Return the name of the package of ROOT whose calls the current file makes.
dbt resolves the calls of a model, of YAML properties or of a singular
test from the package of their file, and those of a macro from the
package of the node that calls it, which no file tells.  Nil for a
buffer that visits no file."
  (car (and buffer-file-name (dbt-project-file-package root buffer-file-name))))

(defun dbt-mode-definitions (target)
  "Return the entities that define TARGET, from the current buffer.
They are best first, as `dbt-project-resolve' orders them for a call
from `dbt-mode-caller'."
  (let ((root (dbt-project-root)))
    (dbt-project-resolve root target (dbt-mode-caller root))))

(defun dbt-mode--xref (entity &optional summary)
  "Return the xref item of ENTITY, whose summary is SUMMARY or its description."
  (xref-make (or summary (format "%s %s%s" (plist-get entity :kind)
                                 (if (plist-get entity :source) (concat (plist-get entity :source) ".") "")
                                 (plist-get entity :name)))
             (xref-make-file-location (plist-get entity :file) (plist-get entity :line) 0)))

(defvar dbt-mode--every-definition nil
  "Non-nil while a command asks for every definition of an identifier.
`dbt-find-resource' does: an identifier that it reads names resources,
not a call that dbt resolves.")

(cl-defmethod xref-backend-definitions ((_backend (eql dbt)) identifier)
  "Return the definitions of IDENTIFIER, the one dbt takes first.
For a `ref', a `source' or a `doc', the project's definition, when dbt
takes it from the current file, is the only one: dbt takes it then from
any file of the project, and from a package's file that defines no
namesake.  It is wrong for a macro of the project that a package's node
calls, as through dispatch, which dbt resolves from that package, a
rare case.  A package's own definition, when dbt takes it, is listed
first, then the others: dbt skips a package's model that the project
disables to override it, which the index does not read, and resolves a
call in a package's macro from the node that calls the macro.  With
`dbt-mode--every-definition', every definition is listed.  A call that
dbt rejects as ambiguous, as `dbt-project-ambiguous' tells, lists its
definitions, each of which says so.

When IDENTIFIER names a package of the project that lacks the name, and
other packages define it, signal an error that names them, as for a root
YAML entry of a package's model, which dbt reads as unused.  xref's
contract asks for nil, but the command that finds definitions is xref's
own: the backend is the one place that can say it.  The qualifier of a
source is a source, and that of a macro may be dbt's own package or a
name of its context such as `builtins', which the index lacks: neither
is an error.  Where dbt lsp serves the buffer, the error never shows, as
`dbt-mode-xref-backend' leaves to the server what dbt-mode does not
resolve."
  (let* ((target (dbt-mode-parse-identifier identifier))
         (definitions (dbt-mode-definitions target)))
    (when-let* (((null definitions))
                (package (plist-get target :qualifier))
                ((not (eq (plist-get target :kind) 'source)))
                ((assoc package (dbt-project-packages (dbt-project-root))))
                (elsewhere (dbt-mode-definitions (plist-put (copy-sequence target) :qualifier nil))))
      (user-error "No %s in package %s, only in %s" (dbt-mode-resource-selector target) package
                  (string-join (delete-dups (mapcar (lambda (entity) (plist-get entity :package)) elsewhere)) ", ")))
    (cond (dbt-mode--every-definition (mapcar #'dbt-mode--xref definitions))
          ((and (memq (plist-get target :kind) '(ref source doc))
                (equal (plist-get (car definitions) :package) (dbt-project-name (dbt-project-root))))
           (list (dbt-mode--xref (car definitions))))
          (t
           ;; Said by each item, which every display of xref shows.
           (let ((said (when-let* ((packages (dbt-project-ambiguous (dbt-project-root) target definitions
                                                                    (dbt-mode-caller (dbt-project-root)))))
                         (format "%s, which dbt rejects as ambiguous: %s and %s define it" identifier
                                 (string-join (butlast packages) ", ") (car (last packages))))))
             (mapcar (lambda (entity) (dbt-mode--xref entity said)) definitions))))))

(cl-defmethod xref-backend-references ((_backend (eql dbt)) identifier)
  "Return the uses of IDENTIFIER in the project and its packages.
A use is a call that resolves, from the package of its file, to what
IDENTIFIER resolves to from the current buffer; a call of any table of
a source uses that source.  The definition of a macro is not a use of
it.  The YAML entries of what IDENTIFIER resolves to count as uses."
  (let* ((root (dbt-project-root))
         (target (dbt-mode-parse-identifier identifier))
         (kind (plist-get target :kind))
         (definition (car (dbt-mode-definitions target)))
         ;; A definition of a macro matches the first pattern, which gives
         ;; no name, rather than the pattern of a call.
         (regexp (alist-get kind `((macro . ,(concat "\\_<\\(?:macro\\|test\\|materialization\\)[ \t\n]+"
                                                     "[A-Za-z_][A-Za-z0-9_]*\\|" dbt-mode-macro-call-regexp))
                                   ,@dbt-project-call-regexps)
                            regexp-unmatchable))
         (source (and (eq kind 'source) (null (plist-get target :qualifier))))
         uses)
    (pcase-dolist (`(,package ,directory) (dbt-project-packages root))
      (dolist (file (seq-remove (lambda (file) (string-suffix-p ".csv" file)) (dbt-project-source-files directory)))
        (with-temp-buffer
          ;; A dangling link or an unreadable file holds no use.
          (ignore-error file-error (insert-file-contents file))
          ;; A use names the target: a file without its name holds none,
          ;; and the literal search costs a fraction of the regexp's.
          (when (search-forward (plist-get target :name) nil t)
            (goto-char (point-min))
            (while (re-search-forward regexp nil t)
              ;; Resolving the call may read the index anew, which
              ;; changes the match data: the call's place is taken first.
              (let ((call (if source
                              (list :kind kind :name (match-string 2))
                            (list :kind kind :qualifier (match-string 2) :name (match-string 1)
                                  :version (match-string 3))))
                    (start (match-beginning 0)))
                (when (and (equal (plist-get call :name) (plist-get target :name))
                           (equal (car (dbt-project-resolve root call package)) definition))
                  (push (xref-make (buffer-substring (line-beginning-position) (line-end-position))
                                   (xref-make-file-location file (line-number-at-pos start)
                                                            (- start (line-beginning-position))))
                        uses))))))))
    (append (nreverse uses)
            (and definition
                 (mapcar #'dbt-mode--xref
                         (seq-filter (lambda (entry)
                                       (eq (alist-get (plist-get entry :section) dbt-project-section-kinds
                                                      nil nil #'equal)
                                           (plist-get definition :kind)))
                                     (dbt-project-entities root '(properties) (plist-get definition :name)
                                                           (plist-get definition :package))))))))

;;;; Completion

(defun dbt-mode--candidates (entities &optional key)
  "Return the values of KEY, :name by default, of ENTITIES, for completion.
KEY is a property, or a function of an entity.  Each candidate carries
the kind and package of its entity, for the annotations."
  (delete-dups
   (mapcar (lambda (entity)
             (propertize (if (functionp key) (funcall key entity) (plist-get entity (or key :name)))
                         'dbt-annotation (format " %s %s" (plist-get entity :kind) (plist-get entity :package))))
           entities)))

(defun dbt-mode-completion-at-point ()
  "Complete the arguments of `ref', `source' and `doc', and macros.
For `completion-at-point-functions'.  The table is not exclusive, so
that other functions, such as those of a language server, still run."
  (when-let* ((root (dbt-project-root))
              (found
               ;; Group 1 matches the text to complete; group 2, when there
               ;; is one, what qualifies it: a package or a source.
               (seq-some
                (pcase-lambda (`(,regexp . ,candidates))
                  (when (looking-back regexp (max (point-min) (- (point) 300)))
                    (cons (match-beginning 1) (funcall candidates (match-string-no-properties 2)))))
                `((,(dbt-project-call-regexp 'ref 'required t)
                   . ,(lambda (package) (dbt-project-entities root '(model seed snapshot) nil package)))
                  (,(dbt-project-call-regexp 'ref nil t)
                   . ,(lambda (_) (dbt-project-entities root '(model seed snapshot))))
                  (,(dbt-project-call-regexp 'source 'required t)
                   . ,(lambda (source) (seq-filter (lambda (table) (equal source (plist-get table :source)))
                                                   (dbt-project-entities root '(table)))))
                  (,(dbt-project-call-regexp 'source nil t)
                   . ,(lambda (_) (dbt-project-entities root '(source))))
                  (,(dbt-project-call-regexp 'doc nil t)
                   . ,(lambda (_) (dbt-project-entities root '(doc))))
                  ("\\_<\\(?2:[A-Za-z_][A-Za-z0-9_]*\\)\\.\\(?1:[A-Za-z0-9_]*\\)"
                   . ,(lambda (package) (and (dbt-mode-jinja-bounds)
                                             (dbt-project-entities root '(macro) nil package))))
                  ("\\_<\\(?1:[A-Za-z_][A-Za-z0-9_]*\\)"
                   . ,(lambda (_) (and (dbt-mode-jinja-bounds)
                                       (append (dbt-project-entities root '(macro))
                                               (mapcar (lambda (name) (list :name name :kind 'function :package "dbt"))
                                                       dbt-mode-functions)))))))))
    (when (cdr found)
      (list (car found)
            (save-excursion (skip-chars-forward "A-Za-z0-9_") (point))
            (dbt-mode--candidates (cdr found))
            :exclusive 'no
            :annotation-function (lambda (candidate) (get-text-property 0 'dbt-annotation candidate))))))

;;;; Commands

(defun dbt-mode-server-p ()
  "Return non-nil when dbt lsp serves the current buffer."
  (and (featurep 'dbt-lsp) (dbt-lsp-workspace)))

(defun dbt-mode--root ()
  "Return the root of the current project, or signal a `user-error'."
  (or (dbt-project-root) (user-error "Not in a dbt project: no dbt_project.yml above")))

(defun dbt-mode-current-resource ()
  "Return the model, seed, snapshot or analysis of the current buffer, or nil.
In a YAML file, it is the resource whose entry holds point."
  (let ((root (dbt-mode--root)))
    (or (and buffer-file-name (dbt-project-file-entity root buffer-file-name))
        (when-let* ((target (dbt-mode-target-at-point))
                    ((memq (plist-get target :kind) '(ref analysis))))
          (car (dbt-mode-definitions target))))))

(defun dbt-mode-resource-selector (resource)
  "Return dbt's selector of RESOURCE alone: NAME, or NAME.vVERSION of a version.
RESOURCE is a resource or a target: only its :name and :version count."
  (concat (plist-get resource :name)
          (and (plist-get resource :version) (format ".v%s" (plist-get resource :version)))))

(defvar-local dbt-mode--status nil
  "What the mode line says of the buffer: the target, the materialization.")

(defvar-local dbt-mode-compiling nil
  "Non-nil while dbt lsp compiles the project of the buffer, as it notifies.")

(defvar-local dbt-mode--node-id nil
  "The file of the buffer and the node id of its resource, or nil for none.
Found once for each file name: a save, which forgets the project, would
read its index again.")

(defun dbt-mode--update-status ()
  "Set what the mode line says: the target that dbt runs on, the materialization.
The materialization is that of the model's config() call, else that of
its node in the manifest of the user's runs, read only then.  For
`after-save-hook', and once the buffer is shown, which finds its node."
  (when-let* ((root (dbt-project-root)))
    (let* ((id (cdr (if (equal (car dbt-mode--node-id) buffer-file-name)
                        dbt-mode--node-id
                      (setq dbt-mode--node-id
                            (cons buffer-file-name (when-let* ((resource (dbt-mode-current-resource)))
                                                     (dbt-project-node-id resource)))))))
           (materialization
            (or (save-excursion
                  (goto-char (point-min))
                  (and (re-search-forward "\\_<materialized[ \t\n]*=[ \t\n]*['\"]\\([A-Za-z_]+\\)" nil t)
                       (match-string-no-properties 1)))
                (let* ((manifest (expand-file-name "manifest.json" (dbt-project-target root)))
                       (node (and id (dbt-project-manifest-node manifest id)))
                       (config (and (hash-table-p node) (gethash "config" node))))
                  (and (hash-table-p config) (gethash "materialized" config))))))
      (setq dbt-mode--status
            (string-join (delq nil (list (dbt-project-default-target root) (and (stringp materialization) materialization)))
                         " ")))))

(defun dbt-mode-resource-at-point ()
  "Return the resource that point is on, else the current one, or nil.
On a `ref' or a `source', it is the model, seed, snapshot or source
table that the call names, the first that `xref-find-definitions'
lists; else the current model, seed, snapshot or analysis."
  (let ((target (dbt-mode-target-at-point)))
    (or (and (memq (plist-get target :kind) '(ref source)) (car (dbt-mode-definitions target)))
        (dbt-mode-current-resource))))

(defun dbt-mode-entity-selection (entity)
  "Return dbt's selector of ENTITY, an entity of the index.
A model, seed or snapshot is NAME or NAME.vVERSION; a source table is
source:SOURCE.TABLE, a source source:SOURCE, an exposure exposure:NAME,
as a bare NAME selects a model of that name too."
  (pcase (plist-get entity :kind)
    ('table (format "source:%s.%s" (plist-get entity :source) (plist-get entity :name)))
    ('source (format "source:%s" (plist-get entity :name)))
    ('exposure (format "exposure:%s" (plist-get entity :name)))
    (_ (dbt-mode-resource-selector entity))))

(defun dbt-mode-selection-at-point ()
  "Return dbt's selector of what point is on, else of the current file.
On a `ref', it is the model, seed or snapshot that it names, as NAME or
NAME.vVERSION; on a `source', source:SOURCE.TABLE; on the name of an
exposure, exposure:NAME, as a bare NAME selects a model of that name
too.  Else it is the current resource, or path:FILE for a file of the
project that holds none, such as a singular test or a properties file,
as dbt selects by path.  Nil outside a file of a project."
  (let ((entity (or (dbt-mode-resource-at-point) (dbt-mode-entity-at-point '(exposure)))))
    (if entity
        (dbt-mode-entity-selection entity)
      (and buffer-file-name (concat "path:" (file-relative-name buffer-file-name (dbt-project-root)))))))

(defun dbt-mode-parse-selector (selector)
  "Return the name and the version that SELECTOR names, as (NAME . VERSION).
SELECTOR is NAME, or NAME.vVERSION for a version of a model; VERSION is
nil for a bare name."
  (if (string-match "\\`\\([^.]+\\)\\.v\\(.+\\)\\'" selector)
      (cons (match-string 1 selector) (match-string 2 selector))
    (list selector)))

(defun dbt-mode-read-resource (prompt &optional no-default)
  "Read a model, seed or snapshot with PROMPT, as its selector.
A version of a model is NAME.vVERSION, and NAME is the model itself, its
latest version, as for an unpinned `ref'.  The default is the resource
of the current buffer, unless NO-DEFAULT, or unless it is none of these."
  (let ((resource (dbt-mode-current-resource))
        (entities (dbt-project-entities (dbt-mode--root) '(model seed snapshot))))
    (completing-read prompt (append (dbt-mode--candidates entities #'dbt-mode-resource-selector)
                                    (dbt-mode--candidates (seq-filter (lambda (entity) (plist-get entity :version))
                                                                      entities)))
                     nil t nil nil (and (not no-default)
                                        (memq (plist-get resource :kind) '(model seed snapshot))
                                        (dbt-mode-resource-selector resource)))))

(defun dbt-find-resource (identifier)
  "Visit the definition of IDENTIFIER, read with completion.
The identifiers name every model, seed, snapshot, analysis, macro, test,
docs block, source and table of the project and its packages, whether
or not a language server runs.  Every definition of IDENTIFIER is
listed, as it names resources, not a call that dbt resolves."
  (interactive
   (list (completing-read "Find dbt resource: "
                          ;; The category lets embark act on a candidate.
                          (completion-table-with-metadata (xref-backend-identifier-completion-table 'dbt)
                                                          '((category . dbt-identifier)))
                          nil t)))
  (let ((xref-backend-functions (list (lambda () 'dbt)))
        (dbt-mode--every-definition t))
    (xref-find-definitions identifier)))

(defun dbt-visit-properties (&optional resource)
  "Visit the YAML entry of the current model, seed, snapshot or analysis.
It is an entry of the resource's own package, as dbt 2.0.9 reads one.
RESOURCE, an entity of the index, is visited instead of the current
one."
  (interactive)
  (let* ((resource (or resource (dbt-mode-current-resource) (user-error "No model here")))
         (entries (seq-filter (lambda (entity)
                                (member (plist-get entity :section) dbt-project-resource-sections))
                              (dbt-project-entities (dbt-mode--root) '(properties) (plist-get resource :name)
                                                    (plist-get resource :package)))))
    (unless entries
      (user-error "No YAML entry names %s" (plist-get resource :name)))
    (xref-show-definitions-completing-read
     (lambda () (mapcar (lambda (entity) (dbt-mode--xref entity (format "properties of %s" (plist-get entity :name))))
                        entries))
     nil)))

(defun dbt-mode-compiled-file (directory)
  "Return the file of the current model's SQL that dbt wrote in DIRECTORY.
DIRECTORY is \"compiled\", where `dbt compile' and every run write the
compiled SQL, or \"run\", where a run writes the SQL it ran, its DDL
included.  Return nil when dbt has not written it."
  (let* ((root (dbt-mode--root))
         (config (dbt-project-config root))
         (resource (or (dbt-mode-current-resource) (user-error "No model here")))
         (package (plist-get resource :package))
         ;; dbt writes the SQL of a file under the directory of its
         ;; package: at its path from the root for the project, and for
         ;; another package at its path in the copy that dbt installs,
         ;; wherever the package lives.
         (file (expand-file-name
                (if (equal package (dbt-project-name root))
                    (file-relative-name (plist-get resource :file) root)
                  (file-name-concat (plist-get config :packages-install-path) package
                                    (file-relative-name (plist-get resource :file)
                                                        (nth 1 (assoc package (dbt-project-packages root))))))
                (expand-file-name (concat directory "/" package) (dbt-project-target root))))
         ;; dbt 2 writes that of a snapshot in a directory of that path,
         ;; named after the snapshot.
         (file (if (file-directory-p file)
                   (expand-file-name (concat (plist-get resource :name) ".sql") file)
                 file)))
    (and (file-regular-p file) file)))

(defun dbt-visit-run ()
  "Show the SQL that the last `dbt run' or `dbt build' ran for the current model.
That is its compiled SQL within the DDL that dbt ran."
  (interactive)
  (with-current-buffer (find-file-noselect (or (dbt-mode-compiled-file "run")
                                               (user-error "Not run yet: run dbt run or dbt build")))
    (read-only-mode 1)
    (pop-to-buffer (current-buffer))))

(defun dbt-visit-compiled (&optional run)
  "Show the SQL that the current model compiles to, beside the model.
With dbt lsp, the buffer is saved and the server compiles it; otherwise
it is what the last `dbt compile' or run wrote.  While it shows, it
faces point and follows saves, as `dbt-compiled-follow-mode' says.  With
RUN, interactively a prefix argument, show the SQL of the last run
instead, as `dbt-visit-run' does."
  (interactive "P")
  (if run
      (dbt-visit-run)
    (dbt-compiled-show)))

(defun dbt-preview (&optional start end cte)
  "Show the rows of the current model, of the SQL from START to END, or a CTE's.
Interactively, START and END are those of the active region, and with a
prefix argument, CTE is non-nil: the rows are those of the CTE around
point, as `dbt-mode-cte-query' writes its query.  With dbt lsp, the
server runs the query, on the compiled SQL for a CTE; otherwise `dbt
show' runs it, on the Jinja of the CTE."
  (interactive (cond (current-prefix-arg (list nil nil t))
                     ((use-region-p) (list (region-beginning) (region-end)))))
  ;; QUERY is the name of the rows and their SQL, or nil for the file's:
  ;; dbt lsp shows any file, dbt show a resource.
  (let ((query (cond (cte (dbt-mode-cte-query))
                     (start (cons (format "%s/query" (file-name-base (buffer-name)))
                                  (buffer-substring-no-properties start end))))))
    (cond (cte (funcall (if (dbt-mode-server-p) #'dbt-lsp-preview-cte #'dbt-cli-preview-cte) (car query)))
          ((dbt-mode-server-p) (dbt-lsp-show (cdr query) (car query)))
          (t (dbt-cli-show (or (car query)
                               (dbt-mode-resource-selector
                                (or (dbt-mode-current-resource) (user-error "No model here"))))
                           (cdr query))))))

(defun dbt-data-diff (state)
  "Show the rows of the current model that differ from its relation in STATE.
STATE is a directory of another target's artifacts, such as
production's, as dbt's --state takes it: the model's relation there is
the relation_name of its node in STATE's manifest.json, a name of
identifiers and dots.  One query on the default target compares the
model as last built there with that relation: the rows of each side
that the other lacks, as `{{ dbt.except() }}' finds them, set
difference, each in a first column dbt_diff that names its side.  The
columns are the model's, by name, as its relation on the default target
has them: the other relation must have them all, readable from the same
connection, else the database's error shows.  A protected target asks
first, as `dbt-project-confirm-target' says.  The rows show as a
preview's do."
  (interactive (list (read-directory-name "Compare with the state in: " nil nil t)))
  (let* ((root (dbt-mode--root))
         (resource (or (dbt-mode-current-resource) (user-error "No model here")))
         (selector (dbt-mode-resource-selector resource))
         (node (dbt-project-manifest-node (expand-file-name "manifest.json" state) (dbt-project-node-id resource)))
         (relation (and (hash-table-p node) (gethash "relation_name" node)))
         ;; The manifest is dbt's: a relation is identifiers and dots, as
         ;; quoted by its adapter, and no Jinja, which dbt would run.
         (part "\\(?:\"[^\"{}%#\n]*\"\\|`[^`{}%#\n]*`\\|\\[[^]{}%#\n]*]\\|[A-Za-z0-9_$]+\\)")
         (relation (if (and (stringp relation) (string-match-p (concat "\\`" part "\\(?:\\." part "\\)*\\'") relation))
                       relation
                     (user-error "No relation of %s in %s" selector (abbreviate-file-name state))))
         (_ (dbt-project-confirm-target root nil))
         ;; A side named in a string of SQL, with no quote nor Jinja.
         (side (lambda (name) (format "'only in %s'" (replace-regexp-in-string "[^[:alnum:] ._/-]" "_" name))))
         (call (format "ref('%s', '%s'%s)" (plist-get resource :package) (plist-get resource :name)
                       (if (plist-get resource :version) (format ", v=%s" (plist-get resource :version)) "")))
         (sql (format (concat "{%% set dbt_diff_columns %%}{%% for column in adapter.get_columns_in_relation(%s) %%}"
                              "{{ adapter.quote(column.name) }}{%% if not loop.last %%}, {%% endif %%}"
                              "{%% endfor %%}{%% endset %%}\n"
                              "with dbt_diff_here as (select {{ dbt_diff_columns }} from {{ %s }}),\n"
                              "dbt_diff_there as (select {{ dbt_diff_columns }} from %s)\n"
                              "select %s as dbt_diff, a.* from (select * from dbt_diff_here {{ dbt.except() }} "
                              "select * from dbt_diff_there) as a\nunion all\n"
                              "select %s as dbt_diff, b.* from (select * from dbt_diff_there {{ dbt.except() }} "
                              "select * from dbt_diff_here) as b")
                      call call relation
                      (funcall side (or (dbt-project-default-target root) "the default target"))
                      (funcall side (file-name-nondirectory (directory-file-name state)))))
         (title (format "%s/diff" selector)))
    (if (dbt-mode-server-p)
        (dbt-lsp-show sql title)
      (dbt-cli-show title sql))))

(defun dbt-lineage (selector &optional resource)
  "Show the upstream and downstream of the model, seed or snapshot SELECTOR.
SELECTOR is its name, or NAME.vVERSION for a version of a model; a name
alone is that of its latest version, as for `ref'.  RESOURCE, the
entity of the index that SELECTOR names, settles which of namesakes it
is.  With dbt-lsp-plus, its call hierarchy gives the lineage, one level
at a time; with dbt lsp alone, the server's list of nodes; otherwise
the `ref' and `source' calls of the project's files, read again when
the lineage is asked for again."
  (interactive (list (dbt-mode-read-resource "Lineage of: ")))
  (require 'dbt-lineage)
  (let* ((root (dbt-mode--root))
         (wanted (dbt-mode-parse-selector selector))
         (entity (or resource
                     (car (dbt-project-resolve-ref root nil (car wanted) nil (cdr wanted)))
                     (user-error "No model, seed or snapshot %s" selector)))
         (id (dbt-project-node-id entity))
         refresh)
    (cond
     ((and (dbt-mode-server-p) (dbt-lsp-hierarchy-p))
      (dbt-lsp-hierarchy-lineage (plist-get entity :file) (plist-get entity :line) id
                                 (lambda () (dbt-lsp-lineage (dbt-mode-resource-selector entity) id))))
     ((dbt-mode-server-p)
      (dbt-lsp-lineage (dbt-mode-resource-selector entity) id))
     (t
      (setq refresh (lambda ()
                      (dbt-project-forget root)
                      (dbt-lineage-show root id (dbt-project-graph root) "ref and source calls" refresh)))
      ;; Read again on `g' only: the cached graph serves the first time.
      (dbt-lineage-show root id (dbt-project-graph root) "ref and source calls" refresh)))))

(defun dbt-insert-ref (selector)
  "Insert a call of `ref' to SELECTOR, read with completion.
SELECTOR is a name, or NAME.vVERSION, which calls that version; the
completion offers each name once.  Outside a Jinja tag, the call comes
within `{{ }}'.  It calls the resource that dbt resolves SELECTOR to
from the current file, the first that `xref-find-definitions' lists,
but for the resource that the file defines, which a call would make a
cycle: a model's namesake in another package comes instead, as an
override of a package's model calls it.  A resource of the file's
package or of the project comes by name alone, any other with its
package.  Within a macro, a generic test or a materialization, which dbt
resolves from whichever node calls it, any resource but the project's
comes with its package.  A name that dbt would find ambiguous from the
file, as `dbt-project-ambiguous' tells once the file's own resource is
left out, asks whose resource to call, the first package's by default.
To call another namesake, write the call with its package: completion
at point offers that package's models."
  (interactive (list (dbt-mode-read-resource "ref: " t)))
  (let* ((root (dbt-mode--root))
         (caller (dbt-mode-caller root))
         (wanted (dbt-mode-parse-selector selector))
         ;; Not `dbt-mode-current-resource', which is the call at point
         ;; in a file that defines no resource, nor every resource of the
         ;; file, which may define several snapshots.
         (candidates (remove (and buffer-file-name (dbt-project-file-entity root buffer-file-name))
                             (dbt-project-resolve-ref root nil (car wanted) caller (cdr wanted))))
         (packages (dbt-project-ambiguous root (list :kind 'ref) candidates caller))
         (entity (or (if packages
                         (let ((package (completing-read (format-prompt "dbt finds ref:%s ambiguous; call whose"
                                                                        (car packages) (car wanted))
                                                         packages nil t nil nil (car packages))))
                           (seq-find (lambda (entity) (equal (plist-get entity :package) package)) candidates))
                       (car candidates))
                     (user-error "No model, seed or snapshot `%s' but this one" selector)))
         ;; Probed on both builds: ref('util') in a package's macro is
         ;; ambiguous when a project's model calling the macro resolves
         ;; it; the package's own model calling it gets the package's.
         (from (if (seq-some (lambda (block) (member (car block) '("macro" "test" "materialization")))
                             (cdr (dbt-edit-blocks (point))))
                   (dbt-project-name root)
                 caller))
         (call (format "ref(%s'%s'%s)"
                       (if (< (funcall (dbt-project-lookup-rank root from) entity) 2)
                           ""
                         (format "'%s', " (plist-get entity :package)))
                       (car wanted)
                       (if (cdr wanted) (format ", v=%s" (cdr wanted)) ""))))
    (insert (if (dbt-mode-jinja-bounds) call (format "{{ %s }}" call)))))

(defun dbt-copy-relation (&optional resource)
  "Copy the relation, database.schema.alias, of what point is on.
That is the model, seed, snapshot or source table that the `ref' or
`source' at point names, or else the current model, or RESOURCE, an
entity of the index, when it is given.  It comes from
target/manifest.json, which `dbt parse', `dbt compile' and every run
write; the message says when it was written."
  (interactive)
  (let* ((root (dbt-mode--root))
         (entity (or resource (dbt-mode-resource-at-point) (user-error "No model or source here")))
         (file (expand-file-name "manifest.json" (dbt-project-target root)))
         (id (dbt-project-node-id entity))
         (node (if (file-readable-p file)
                   (dbt-project-manifest-node file id)
                 (user-error "No readable %s: run dbt parse first" (file-relative-name file root))))
         ;; The manifest is dbt's, read as it is: anything but a table
         ;; holding a string is no relation.
         (relation (and (hash-table-p node) (gethash "relation_name" node))))
    (unless (stringp relation)
      (user-error "%s has no relation in the manifest: run dbt parse again" id))
    (kill-new relation)
    (message "Copied %s, from the manifest written at %s" relation
             (format-time-string "%F %R" (file-attribute-modification-time (file-attributes file))))))

(defun dbt-copy-build-command (&optional family)
  "Copy the command that builds what point is on, dbt build -s SELECTOR.
That is the model, seed or snapshot of the `ref' at point, the source
of the `source' at point, or else the current resource, as
`dbt-mode-selection-at-point' selects it.  With FAMILY, interactively a
prefix argument, the selection takes in its upstream and downstream:
+SELECTOR+.  An exposure, which dbt has nothing to build for, needs
FAMILY to build what the exposure reads."
  (interactive "P")
  (let* ((selector (or (dbt-mode-selection-at-point) (user-error "No model here")))
         (command (format "dbt build -s %s" (if family (format "+%s+" selector) selector))))
    (kill-new command)
    (message "Copied %s" command)))

(defun dbt-visit-tests ()
  "Show the tests of the current model.
They are its YAML entries, where its data tests sit, the data tests
elsewhere that name it, such as a relationships test, and the singular
tests of the project's test paths that read it.  Its unit tests are not
listed: their `model' key is not a call."
  (interactive)
  (let* ((root (dbt-mode--root))
         (resource (or (dbt-mode-current-resource) (user-error "No model here")))
         (directories (mapcar (lambda (path) (file-name-as-directory (expand-file-name path root)))
                              (plist-get (dbt-project-config root) :test-paths)))
         (scans (make-hash-table :test #'equal))
         (items (seq-filter
                 (lambda (item)
                   (let* ((location (xref-item-location item))
                          (file (xref-location-group location)))
                     (if (string-match-p "\\.ya?ml\\'" file)
                         ;; The keys that hold the line tell what it is.
                         (let ((path (plist-get (seq-find (lambda (line)
                                                            (= (plist-get line :line) (xref-location-line location)))
                                                          (with-memoization (gethash file scans)
                                                            (dbt-project-yaml-scan
                                                             (with-temp-buffer
                                                               (insert-file-contents file)
                                                               (buffer-string)))))
                                                :path)))
                           (or (seq-intersection path '("data_tests" "tests"))
                               (and (equal (length path) 1) (member (car path) dbt-project-resource-sections))))
                       (seq-some (lambda (directory) (string-prefix-p directory file)) directories))))
                 (xref-backend-references
                  'dbt (dbt-mode-identifier (dbt-mode-entity-target resource (plist-get resource :package)))))))
    (unless items
      (user-error "No test reads %s" (plist-get resource :name)))
    (xref-show-xrefs (lambda () items) nil)))

(defvar dbt-mode--formatting nil
  "The buffer that a command or a save formats, while it does.
A buffer saved meanwhile, as by a timer, is formatted as usual.")

(defun dbt-format-buffer ()
  "Save the buffer, then format it.
With dbt lsp, the server formats it as it answers, as
`dbt-lsp-format-buffer' does, and leaves it modified.  Otherwise `dbt
format' formats the saved file, and the buffer shows the result once
dbt ends, unless it was modified meanwhile.  A dbt with neither, such
as dbt-oss, cannot format: that is an error, once the buffer is saved.
A dbt that did not answer in time whether it formats is asked again."
  (interactive)
  (unless buffer-file-name
    (user-error "%s visits no file for dbt to format" (buffer-name)))
  (let ((dbt-mode--formatting (current-buffer)))
    (if (dbt-mode-server-p)
        (dbt-lsp-format-buffer)
      (save-buffer)
      (dbt-project-ask-again "format")
      (pcase (dbt-project-command-answer "format")
        ('yes (dbt-cli-format (current-buffer)))
        ('timeout (user-error "dbt did not answer dbt format --help in time: M-x dbt-format-buffer asks again"))
        (_
         ;; Without any dbt, that is what it says.
         (dbt-project-executable)
         (user-error "This dbt cannot format: it has no dbt format, which the proprietary build of dbt 2 runs (on dbt-oss, dbt system upgrade-distribution installs it)"))))))

(defun dbt-mode--format-after-save ()
  "Format the buffer just saved, and save it again when that changed it.
For `after-save-hook', in `dbt-format-on-save-mode'.  A failure is a
message: the save stands, as without any dbt.  A dbt that cannot
format leaves it alone."
  (unless (eq dbt-mode--formatting (current-buffer))
    (let ((dbt-mode--formatting (current-buffer)))
      (condition-case err
          (cond ((dbt-mode-server-p)
                 ;; The server answers later: the save that follows its
                 ;; edits formats nothing again.
                 (dbt-lsp-format-buffer (lambda ()
                                          (when (buffer-modified-p)
                                            (let ((dbt-mode--formatting (current-buffer)))
                                              (save-buffer))))))
                ((dbt-project-command-p "format") (dbt-cli-format (current-buffer)))
                ;; Without any dbt, that is the message.
                (t (dbt-project-executable)))
        (error (message "dbt could not format %s: %s" (buffer-name) (error-message-string err)))))))

(defvar dbt-mode--told-cannot-format nil
  "The dbt executables that `dbt-format-on-save-mode' said cannot format.")

(define-minor-mode dbt-format-on-save-mode
  "Format the buffer after each save, as `dbt-format-buffer' does.
With dbt lsp, the formatted buffer is saved again when formatting
changed it.  Without a server, `dbt format' formats the saved file
while you go on, and the buffer shows the result unless you modified it
meanwhile.  A failure to format is a message, and the save stands.  A
dbt with neither a server nor `dbt format', such as dbt-oss, leaves
saves as they are, which the mode says once for each dbt, in the first
buffer that turns it on.  A dbt that did not answer in time leaves
saves as they are too, as the message of the question says, until
`dbt-format-buffer' asks it again."
  :lighter " Fmt"
  (if dbt-format-on-save-mode
      (progn
        (add-hook 'after-save-hook #'dbt-mode--format-after-save nil 'local)
        ;; dbt lsp, which formats too, comes with dbt format, in the
        ;; proprietary build; dbt-oss has neither.
        (when-let* ((executable (executable-find dbt-executable))
                    ((not (member executable dbt-mode--told-cannot-format)))
                    ((eq (dbt-project-command-answer "format") 'no)))
          (push executable dbt-mode--told-cannot-format)
          (message "This dbt cannot format, so saves stay as they are: dbt format comes with the proprietary build of dbt 2 (on dbt-oss, dbt system upgrade-distribution installs it)")))
    (remove-hook 'after-save-hook #'dbt-mode--format-after-save 'local)))

(defun dbt-mode--forget-project ()
  "Forget what was read of the project of the current buffer.
For `after-save-hook', so that the index sees what was just saved."
  (when-let* ((root (dbt-project-root)))
    (dbt-project-forget root)))

;;;; The mode

(defvar-keymap dbt-mode-map
  :doc "Keymap of `dbt-mode'.
It keeps the keys of `sql-mode', but for C-M-a and C-M-e, which move by
CTE rather than by statement."
  "C-c C-d" #'dbt-dispatch
  "C-c C-v" #'dbt-visit-compiled
  "C-c C-p" #'dbt-preview
  "C-c C-g" #'dbt-lineage
  "C-c C-y" #'dbt-visit-properties
  "C-c C-f" #'dbt-find-resource
  "C-c C-e" #'dbt-insert-ref
  "C-c C-o" #'dbt-describe
  "C-c C-t" #'dbt-visit-tests
  "C-c C-w" #'dbt-copy-relation
  "C-c M-w" #'dbt-copy-build-command
  "C-c C-a" #'dbt-new
  "C-c C-q" #'dbt-format-buffer
  "C-c /" #'dbt-close-block
  "C-c %" #'dbt-jump-to-matching-tag
  ;; `sql-mode' remaps them to its statement motions.
  "<remap> <beginning-of-defun>" #'beginning-of-defun
  "<remap> <end-of-defun>" #'end-of-defun
  "<remap> <delete-backward-char>" #'dbt-edit-delete-backward)

(easy-menu-define dbt-mode-menu dbt-mode-map
  "Menu of `dbt-mode'."
  '("dbt"
    ["Run a dbt command..." dbt-dispatch]
    ["Compiled SQL" dbt-visit-compiled]
    ["Preview rows" dbt-preview]
    ["Lineage" dbt-lineage]
    ["YAML properties" dbt-visit-properties]
    ["Find resource..." dbt-find-resource]
    ["Insert ref..." dbt-insert-ref]
    ["Describe node" dbt-describe]
    ["Tests of this model" dbt-visit-tests]
    ["SQL of the last run" dbt-visit-run]
    ["Compiled SQL against a state..." dbt-compiled-diff]
    ["Data against a state..." dbt-data-diff]
    ["Copy the relation" dbt-copy-relation]
    ["Copy the build command" dbt-copy-build-command]
    ["Dashboard of the project" dbt-dashboard]
    "---"
    ["New..." dbt-new]
    ["Close the Jinja block" dbt-close-block]
    ["Matching Jinja tag" dbt-jump-to-matching-tag]
    "---"
    ["Rename model..." dbt-lsp-rename-model :active (dbt-mode-server-p)]
    ["Format buffer" dbt-format-buffer]
    ["Format on save" dbt-format-on-save-mode :style toggle :selected dbt-format-on-save-mode]
    ["Fix lint issues" dbt-lsp-fix-all :active (dbt-mode-server-p)]
    ["Reread the project" dbt-project-forget]))

(defvar dbt-mode-imenu-generic-expression
  (append (mapcar (pcase-lambda (`(,title . ,kind)) (list title (alist-get kind dbt-project-block-regexps) 1))
                  '(("Macros" . macro) ("Tests" . test) ("Snapshots" . snapshot) ("Docs" . doc)))
          `((nil ,(dbt-mode-cte-regexp) 1)))
  "The entries of `imenu' in `dbt-mode': CTEs at top, then Jinja blocks.")

;;;###autoload
(define-derived-mode dbt-mode sql-mode "dbt"
  "Major mode for the SQL and Jinja files of a dbt project.

It is `sql-mode', with Jinja's comments, tags and keywords, and what
the project defines for `xref', completion and `imenu'.  See the
commentary of dbt-mode.el for the rest.

\\{dbt-mode-map}"
  (let ((sql-propertize syntax-propertize-function))
    ;; Jinja first, as dbt renders it before SQL is read, with the SQL
    ;; that starts no comment; then the rest of `sql-mode''s rules, which
    ;; must not meet the states that `syntax-ppss' cached without their
    ;; own properties.
    (setq-local syntax-propertize-function
                (lambda (start end)
                  (funcall dbt-mode--syntax-propertize start end)
                  (syntax-ppss-flush-cache start)
                  (funcall sql-propertize start end))))
  (add-hook 'syntax-propertize-extend-region-functions #'syntax-propertize-multiline 'append 'local)
  (add-hook 'font-lock-extend-region-functions #'dbt-mode--extend-region 'append 'local)
  (setq-local imenu-generic-expression dbt-mode-imenu-generic-expression
              imenu-case-fold-search t)
  (add-hook 'xref-backend-functions #'dbt-mode-xref-backend -50 'local)
  (add-hook 'completion-at-point-functions #'dbt-mode-completion-at-point -50 'local)
  (add-hook 'eldoc-documentation-functions #'dbt-node-eldoc nil 'local)
  (add-hook 'after-save-hook #'dbt-mode--forget-project nil 'local)
  ;; The target, the materialization and the compilation, after the name.
  (setq-local mode-line-process '((dbt-mode--status (" " dbt-mode--status)) (dbt-mode-compiling " compiling")))
  (add-hook 'after-save-hook #'dbt-mode--update-status nil 'local)
  (let ((buffer (current-buffer)))
    (run-with-idle-timer 0 nil (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer (dbt-mode--update-status))))))
  (dolist (function '(dbt-edit-pair-jinja-opener dbt-edit-join-jinja-sign dbt-edit-skip-jinja-closer))
    (add-hook 'post-self-insert-hook function nil 'local))
  (add-hook 'electric-indent-functions #'dbt-edit-electric-indent-p nil 'local)
  (setq-local beginning-of-defun-function #'dbt-edit-beginning-of-cte
              end-of-defun-function #'dbt-edit-end-of-cte
              indent-line-function #'dbt-edit-indent-line
              indent-region-function #'dbt-edit-indent-region
              ;; dbt's files indent with spaces.
              indent-tabs-mode nil))

(font-lock-add-keywords 'dbt-mode dbt-mode--font-lock-keywords)

(defvar-keymap dbt-properties-mode-map
  :doc "Keymap of `dbt-properties-mode'."
  "C-c C-o" #'dbt-describe)

;;;###autoload
(define-minor-mode dbt-properties-mode
  "Navigate from the YAML files of a dbt project, and complete in them.
On the `name' of a model, \\[xref-find-definitions] goes to its file,
and on a call of `ref', `source' or `doc' in a test or a description,
to what it names; \\[xref-find-references] lists where it is used;
\\[dbt-describe] shows what dbt knows of it, and eldoc says its
relation, as in `dbt-mode'.  `dbt-properties-mode-maybe' turns it on in a
dbt project's YAML files."
  :lighter " dbt"
  (if dbt-properties-mode
      (progn
        (add-hook 'xref-backend-functions #'dbt-mode-xref-backend -50 'local)
        (add-hook 'completion-at-point-functions #'dbt-mode-completion-at-point -50 'local)
        (add-hook 'eldoc-documentation-functions #'dbt-node-eldoc nil 'local)
        (add-hook 'after-save-hook #'dbt-mode--forget-project nil 'local))
    (remove-hook 'xref-backend-functions #'dbt-mode-xref-backend 'local)
    (remove-hook 'completion-at-point-functions #'dbt-mode-completion-at-point 'local)
    (remove-hook 'eldoc-documentation-functions #'dbt-node-eldoc 'local)
    (remove-hook 'after-save-hook #'dbt-mode--forget-project 'local)))

;;;###autoload
(defun dbt-properties-mode-maybe ()
  "Turn on `dbt-properties-mode' when the buffer visits a dbt project's YAML.
For `yaml-mode-hook' and `yaml-ts-mode-hook'."
  (when (and buffer-file-name (dbt-project-root))
    (dbt-properties-mode 1)))

;;;###autoload
(defun dbt-mode-file-p ()
  "Return non-nil when the buffer visits the SQL of a dbt project.
That is a `.sql' file of the project that `dbt-project-root' finds, but
not under its target-path or logs, where dbt writes plain SQL.  For
`magic-mode-alist'."
  (when-let* ((file buffer-file-name)
              ((string-match-p "\\.sql\\'" file))
              (root (dbt-project-root file)))
    (not (seq-some (lambda (directory)
                     (string-prefix-p (file-name-as-directory (expand-file-name directory root))
                                      (expand-file-name file)))
                   (list (plist-get (dbt-project-config root) :target-path) "logs")))))

;;;###autoload
(add-to-list 'magic-mode-alist '(dbt-mode-file-p . dbt-mode))

(provide 'dbt-mode)
;;; dbt-mode.el ends here
