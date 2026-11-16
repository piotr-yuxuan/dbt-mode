;;; dbt-mode-test.el --- Tests of dbt-mode without a server  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user gets from dbt-mode with no dbt executable: the mode of a
;; file, Jinja's syntax and faces, xref, completion, imenu and the
;; commands that read the project.

;;; Code:

(require 'cl-lib)
(require 'dbt-mode-test-helpers)
(require 'ert-x)
(require 'dbt-mode)
(require 'imenu)

(ert-deftest dbt-mode-opens-the-sql-of-a-project ()
  "A .sql file of a project opens in dbt-mode, but not what dbt writes."
  (dbt-test-with-project root
    (make-directory (expand-file-name "target/compiled" root) t)
    (write-region "select 1" nil (expand-file-name "target/compiled/x.sql" root))
    (should (eq (with-current-buffer (dbt-test-visit root "models/marts/orders.sql") major-mode) 'dbt-mode))
    (should (eq (with-current-buffer (dbt-test-visit root "dbt_packages/shop_utils/macros/is_positive.sql")
                  major-mode)
                'dbt-mode))
    (should (eq (with-current-buffer (dbt-test-visit root "target/compiled/x.sql") major-mode) 'sql-mode)))
  (let ((file (make-temp-file "plain" nil ".sql" "select 1")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (should (eq major-mode 'sql-mode))
          (kill-buffer))
      (delete-file file))))

(ert-deftest dbt-mode-leaves-the-target-path-alone ()
  "What dbt writes under the configured target-path, or logs, opens in sql-mode."
  (dbt-test-with-project root
    (write-region "target-path: build\n" nil (expand-file-name "dbt_project.yml" root) 'append)
    (dolist (case '(("build/compiled/x.sql" . sql-mode) ("target/x.sql" . dbt-mode)
                    ("logs/query_log.sql" . sql-mode) ("models/logs/x.sql" . dbt-mode)))
      (make-directory (file-name-directory (expand-file-name (car case) root)) t)
      (write-region "select 1" nil (expand-file-name (car case) root))
      (should (eq (with-current-buffer (dbt-test-visit root (car case)) major-mode) (cdr case))))))

(ert-deftest dbt-mode-reads-jinja-comments-as-comments ()
  "An apostrophe in {# #} starts no string, and the comment ends at #}."
  (with-temp-buffer
    (insert "{# it's a comment #}\nselect 'a' from {# another\n one #} t\n")
    (dbt-mode)
    (dbt-test-goto "s a comment")
    (should (nth 4 (syntax-ppss)))
    (dbt-test-goto "select")
    (should-not (nth 8 (syntax-ppss)))
    (dbt-test-goto " t\n")
    (should-not (nth 8 (syntax-ppss)))))

(ert-deftest dbt-mode-keeps-the-comments-of-sql-mode ()
  "As in sql-mode, neither /- nor -* starts a comment."
  (with-temp-buffer
    (insert "select revenue/-1 as neg, b from t\nselect 1 as a -*\nfrom x */ select 2 as two\n")
    (dbt-mode)
    (dbt-test-goto "b from")
    (should-not (nth 4 (syntax-ppss)))
    (dbt-test-goto "from x")
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest dbt-mode-reads-jinja-after-what-starts-no-comment ()
  "After /- or -*, a Jinja comment or tag holding an apostrophe is read as one.
Jinja's rules see that /- and -* start no comment."
  (dolist (text '("select revenue/-1 as neg, {# it's old #} b\nfrom t\nselect 2 as z\n"
                  "select revenue/-1 as neg, {{ log(\"it's\") }} b\nfrom t\nselect 2 as z\n"
                  "select 1 as a -*\n{# it's old #}\nfrom t\nselect 2 as z\n"
                  ;; A banner after a Jinja comment holding a quote closes.
                  "{# it's #}\n/*----*/\nfrom t\nselect 2 as z\n"))
    (with-temp-buffer
      (insert text)
      (dbt-mode)
      (dolist (word '("from t" "2 as z"))
        (dbt-test-goto word)
        (should-not (nth 8 (syntax-ppss)))))))

(ert-deftest dbt-mode-reads-a-long-jinja-comment-in-pieces ()
  "A comment that the chunks of syntax and font lock cut is still a comment."
  (with-temp-buffer
    (insert "select\n    id\n")
    (dotimes (i 45) (insert (format "    , c%02d as col_%02d\n" i i)))
    (insert "{# dropped in v2:\n")
    (dotimes (i 50) (insert (format "    , old_%02d as legacy_%02d\n" i i)))
    (insert "#}\nfrom {{ ref('orders') }}\n")
    (dbt-mode)
    (font-lock-set-defaults)
    ;; The first screen alone, then the rest, as when the file opens.
    (syntax-ppss 200)
    (font-lock-fontify-region (point-min) 1501)
    (font-lock-fontify-region 1501 (point-max))
    (dolist (text '("old_10" "legacy_45"))
      (dbt-test-goto text)
      (should (nth 4 (syntax-ppss)))
      (should (eq (get-text-property (point) 'face) 'font-lock-comment-face)))
    (dbt-test-goto "from {{")
    (should-not (nth 8 (syntax-ppss)))))

(ert-deftest dbt-mode-reads-a-jinja-comment-closed-later ()
  "Typing #} after a {# on an earlier line makes the lines between a comment."
  (with-temp-buffer
    (insert "select a,\n  b,\n  c\nfrom t\n")
    (dbt-mode)
    (font-lock-ensure)
    (dbt-test-goto "  b,")
    (insert "{# ")
    (font-lock-ensure)
    (dbt-test-goto "  c" 3)
    (insert " #}")
    (font-lock-ensure)
    (dbt-test-goto "b,")
    (should (nth 4 (syntax-ppss)))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))
    (dbt-test-goto "from")
    (should-not (nth 4 (syntax-ppss))))
  (with-temp-buffer
    (insert "{# select a,\n  b #}\nselect c from t\n")
    (dbt-mode)
    (font-lock-ensure)
    (dbt-test-goto " #}")
    (delete-char 3)
    (font-lock-ensure)
    (dbt-test-goto " from t" 7)
    (insert " #}")
    (font-lock-ensure)
    (dbt-test-goto "select c")
    (should (nth 4 (syntax-ppss)))
    (should (eq (get-text-property (point) 'face) 'font-lock-comment-face))))

(defun dbt-test-face (text &optional offset)
  "Return the face at the first occurrence of TEXT, OFFSET characters on."
  (dbt-test-goto text offset)
  (get-text-property (point) 'face))

(ert-deftest dbt-mode-fontifies-jinja ()
  "Jinja's delimiters, keywords, dbt's functions and macros get their faces."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (font-lock-ensure)
    (should (eq (dbt-test-face "{{") 'dbt-jinja-delimiter-face))
    (should (eq (dbt-test-face "config(") 'font-lock-builtin-face))
    (should (eq (dbt-test-face "ref(") 'font-lock-builtin-face))
    (should (eq (dbt-test-face "shop_utils") 'font-lock-type-face))
    (should (eq (dbt-test-face "is_positive") 'font-lock-function-call-face))
    (should (eq (dbt-test-face "{% if" 3) 'font-lock-keyword-face))
    (should (eq (dbt-test-face "this") 'font-lock-constant-face))
    (should (eq (dbt-test-face "It's") 'font-lock-comment-face))
    (should (eq (dbt-test-face "left join") 'font-lock-keyword-face))))

(ert-deftest dbt-mode-fontifies-cte-names ()
  "A CTE's name gets the face of a definition, keyword or not."
  (with-temp-buffer
    (insert "with source as (select 1),\nfinal as (select 2\n), failures as (select 3)\nselect * from final\n")
    (dbt-mode)
    (font-lock-ensure)
    (should (eq (dbt-test-face "source") 'font-lock-function-name-face))
    (should (eq (dbt-test-face "final") 'font-lock-function-name-face))
    (should (eq (dbt-test-face "failures") 'font-lock-function-name-face))
    (should (equal (mapcar #'car (funcall imenu-create-index-function)) '("source" "final" "failures")))))

(ert-deftest dbt-mode-leaves-an-unclosed-tag-alone ()
  "A tag being typed does not take the closer of the next tag."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-test-goto "with orders")
    (insert "{{ ")
    (font-lock-ensure)
    (should-not (eq (dbt-test-face "{{ with") 'dbt-jinja-delimiter-face))
    (should (eq (dbt-test-face "{{ ref") 'dbt-jinja-delimiter-face))
    (should (eq (dbt-test-face "ref(") 'font-lock-builtin-face))))

(ert-deftest dbt-mode-fontifies-a-tag-from-its-middle ()
  "Refontifying a line inside a multi-line tag fontifies the whole tag."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (font-lock-ensure)
    (remove-text-properties (point-min) (point-max) '(face nil))
    (dbt-test-goto "materialized")
    (font-lock-fontify-region (line-beginning-position) (line-end-position))
    (should (eq (dbt-test-face "config(") 'font-lock-builtin-face))))

(ert-deftest dbt-mode-jinja-bounds-find-the-tag-around-point ()
  "Point inside a tag gets its bounds; outside, or in an open tag, none or no end.
A tag inside the string of a tag is part of that string."
  (with-temp-buffer
    (dbt-mode)
    (insert "select {{ ref('a') }} from t {% if x")
    (dbt-test-goto "ref")
    (should (equal (dbt-mode-jinja-bounds) (cons 8 22)))
    (dbt-test-goto "from")
    (should-not (dbt-mode-jinja-bounds))
    (goto-char (point-max))
    (should (equal (dbt-mode-jinja-bounds) (cons 30 nil))))
  (with-temp-buffer
    (dbt-mode)
    (insert "{{ f(\"a {{ b }} c\", x) }}")
    (dbt-test-goto "x)")
    (should (equal (dbt-mode-jinja-bounds) (cons 1 26)))))

(ert-deftest dbt-mode-reads-the-strings-of-a-tag ()
  "A quote of the other kind, or escaped, in a Jinja string ends nothing."
  (with-temp-buffer
    (insert "{{ exceptions.raise_compiler_error(\"Model doesn't exist\") }}\n"
            "select id, -- the key\n"
            "{% set msg = \"it's late\" %}\n"
            "{{ log('can\\'t run', info=true) }}\n"
            "select 2 from {{ ref('customers') }}\n")
    (dbt-mode)
    (font-lock-ensure)
    (dolist (text '("Model" "it's" "can"))
      (should (eq (dbt-test-face text) 'font-lock-string-face)))
    (dolist (text '("select id" "select 2"))
      (should (eq (dbt-test-face text) 'font-lock-keyword-face))
      (should-not (nth 8 (syntax-ppss))))
    (should (eq (dbt-test-face "the key") 'font-lock-comment-face))
    (should (eq (dbt-test-face "ref(") 'font-lock-builtin-face)))
  (with-temp-buffer
    (insert "{{\n  log(\n    \"it's\")\n}}\nselect 1\n")
    (dbt-mode)
    (font-lock-ensure)
    ;; A change within the tag, on a line of its own, reads it again whole.
    (dbt-test-goto "\"it's")
    (insert "'a', ")
    (font-lock-ensure)
    (dbt-test-goto "select")
    (should-not (nth 8 (syntax-ppss)))))

(ert-deftest dbt-mode-reads-a-tag-in-a-sql-string-as-the-string ()
  "A tag within a SQL literal leaves the literal one string."
  (with-temp-buffer
    (insert "select * from t\nwhere d >= '{{ var('start_date') }}' and x = 1\n")
    (dbt-mode)
    (font-lock-ensure)
    (dbt-test-goto "start_date")
    (should (nth 3 (syntax-ppss)))
    (should (eq (dbt-test-face "start_date") 'font-lock-string-face))
    (should (eq (dbt-test-face "{{") 'dbt-jinja-delimiter-face))
    (dbt-test-goto "and x")
    (should-not (nth 3 (syntax-ppss)))
    (should (eq (dbt-test-face "and x") 'font-lock-keyword-face))))

(ert-deftest dbt-mode-finds-no-tag-at-once-between-many ()
  "Between the tags of a file dense with them, no tag is found at once.
Font lock asks at each change, twice; a model of 3,000 lines with a tag
on each takes well under a second for 3,000 such questions, whether the
tags are code, in SQL strings or in comments."
  (dolist (line '("    {{ ref('m_%d') }} as c_%d,\n" "    and status = '{{ var(\"s_%d\") }}' -- %d\n"
                  "-- {{ ref('m_%d') }} as c_%d\n"))
    (with-temp-buffer
      (dotimes (i 3000) (insert (format line i i)))
      (dbt-mode)
      (let ((start (float-time)))
        (goto-char (point-min))
        (while (not (eobp))
          (end-of-line)
          (should-not (dbt-mode-jinja-bounds))
          (forward-line 1))
        (should (< (- (float-time) start) 1.0))))))

(ert-deftest dbt-mode-ends-a-tag-at-its-own-closer ()
  "A closer that ends a nested dict, or in a string, does not end the tag."
  (with-temp-buffer
    (insert "{{ config(meta={'owner': {'team': 'x'}}, post_hook=audit_hook()) }}\n"
            "{% set m = {'a': {'b': 1}} %}\n"
            "{{ config(materialized='table', post_hook=\"grant select on {{ this }} to role r\") }}\n"
            "{{ log('}}', info=true) }}\n")
    (dbt-mode)
    (font-lock-ensure)
    (dolist (case '(("audit_hook" . "'x'}}") ("'b': 1" . "1}}") ("materialized" . "this }}") ("info" . "'}}")))
      (dbt-test-goto (car case))
      (should (equal (dbt-mode-jinja-bounds) (cons (line-beginning-position) (line-end-position))))
      (should (eq (get-text-property (- (line-end-position) 2) 'face) 'dbt-jinja-delimiter-face))
      (dbt-test-goto (cdr case) (1- (length (cdr case))))
      (should-not (eq (get-text-property (point) 'face) 'dbt-jinja-delimiter-face)))
    (dbt-test-goto "audit_hook")
    (should (equal (dbt-mode-target-at-point) '(:kind macro :qualifier nil :name "audit_hook" :version nil)))
    (should (eq (dbt-test-face "config(materialized") 'font-lock-builtin-face))))

(ert-deftest dbt-mode-target-at-point-names-calls-and-entries ()
  "Calls of ref, source, doc and macros, and YAML entries, are targets."
  (with-temp-buffer
    (insert "{{ ref('pkg', 'm') }} {{ source('raw', 'orders') }} {{ ref('n', v=2) }} {{ pkg.mac(1) }} x\n")
    (dbt-mode)
    (dolist (case '(("'m'" 1 (:kind ref :qualifier "pkg" :name "m" :version nil))
                    ("orders" 1 (:kind source :qualifier "raw" :name "orders" :version nil))
                    ("'n'" 1 (:kind ref :qualifier nil :name "n" :version "2"))
                    ("mac" 1 (:kind macro :qualifier "pkg" :name "mac" :version nil))
                    (" x" 1 nil)))
      (dbt-test-goto (car case) (nth 1 case))
      (should (equal (dbt-mode-target-at-point) (nth 2 case)))))
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/_sources.yml")
    (dbt-test-goto "raw_orders")
    (should (equal (dbt-mode-target-at-point) '(:kind source :qualifier "raw" :name "raw_orders" :version nil)))
    (dbt-test-visit root "models/staging/_staging.yml")
    (dbt-test-goto "stg_orders")
    (should (equal (dbt-mode-target-at-point) '(:kind ref :qualifier "shop" :name "stg_orders" :version nil)))
    (dbt-test-goto "order_status")
    (should (equal (dbt-mode-target-at-point) '(:kind doc :qualifier nil :name "order_status" :version nil)))))

(ert-deftest dbt-mode-target-at-point-qualifies-the-entries-of-a-package ()
  "The YAML entry of a package's model names that model, not the project's."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/orders.sql" root))
    (write-region "models:\n  - name: orders\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/_utils.yml" root))
    (dbt-test-visit root "dbt_packages/shop_utils/models/_utils.yml")
    (dbt-test-goto "orders")
    (should (equal (dbt-mode-target-at-point) '(:kind ref :qualifier "shop_utils" :name "orders" :version nil)))
    (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                   '(("dbt_packages/shop_utils/models/orders.sql" . 1))))))

(ert-deftest dbt-mode-target-at-point-is-no-model-of-a-semantic-entry ()
  "The name of a semantic model, a metric or a saved query names no model, though a package's model has it; its ref does."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/util_orders.sql" root))
    (write-region (concat "semantic_models:\n  - name: util_orders\n    model: ref('util_orders')\n"
                          "metrics:\n  - name: order_total\n"
                          "saved_queries:\n  - name: weekly_orders\n")
                  nil (expand-file-name "models/_semantic.yml" root))
    (dbt-test-visit root "models/_semantic.yml")
    (dolist (name '("util_orders" "order_total" "weekly_orders"))
      (dbt-test-goto name)
      (should-not (dbt-mode-target-at-point)))
    (dbt-test-goto "util_orders')")
    (should (equal (dbt-mode-target-at-point) '(:kind ref :qualifier nil :name "util_orders" :version nil)))))

(ert-deftest dbt-mode-finds-no-model-from-an-exposure ()
  "M-. on the name of an exposure asks what to find, though a model has that name; on a seed's or a snapshot's entry it visits that file.
No `ref' names an exposure.  M-x dbt-copy-build-command there selects
it as dbt 2.0.11 does, exposure:NAME: a bare NAME would select the model
too, and path:FILE the other entries of the file.  On a semantic model,
which names nothing either, it selects the file."
  (dbt-test-with-project root
    (write-region (concat "exposures:\n  - name: orders\n    type: dashboard\n"
                          "    depends_on:\n      - ref('customers')\n"
                          "seeds:\n  - name: raw_orders\n"
                          "snapshots:\n  - name: orders_snapshot\n"
                          "semantic_models:\n  - name: orders_semantic\n    model: ref('orders')\n")
                  nil (expand-file-name "models/_exposures.yml" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/_exposures.yml"))
      (dbt-properties-mode-maybe)
      (dbt-test-goto "orders")
      (execute-kbd-macro (kbd "M-x dbt-copy-build-command RET"))
      (should (equal (car kill-ring) "dbt build -s exposure:orders"))
      (execute-kbd-macro (kbd "C-u M-x dbt-copy-build-command RET"))
      (should (equal (car kill-ring) "dbt build -s +exposure:orders+"))
      (dbt-test-goto "orders_semantic")
      (execute-kbd-macro (kbd "M-x dbt-copy-build-command RET"))
      (should (equal (car kill-ring) "dbt build -s path:models/_exposures.yml"))
      (dbt-test-goto "orders")
      (execute-kbd-macro (kbd "M-. ref:customers RET"))
      (should (equal (file-relative-name buffer-file-name root) "models/marts/customers.sql"))
      (dolist (case '(("raw_orders" . "seeds/raw_orders.csv") ("orders_snapshot" . "snapshots/orders_snapshot.sql")))
        (switch-to-buffer (dbt-test-visit root "models/_exposures.yml"))
        (dbt-test-goto (car case))
        (execute-kbd-macro (kbd "M-."))
        (should (equal (file-relative-name buffer-file-name root) (cdr case)))))))

(ert-deftest dbt-mode-finds-no-package-model-from-a-root-entry ()
  "M-. on a root YAML entry that names only a package's model finds nothing, as dbt 2.0.9 calls the entry unused.
It says which package defines the model instead.  A root entry of a
root model finds that model."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/util_orders.sql" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/stg_orders.sql" root))
    (write-region "models:\n  - name: util_orders\n" nil (expand-file-name "models/_utils.yml" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/_utils.yml"))
      ;; As the hook of YAML files does.
      (dbt-properties-mode-maybe)
      (dbt-test-goto "util_orders")
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "M-.")) :type 'user-error))
                     "No util_orders in package shop, only in shop_utils")))
    (dbt-test-visit root "models/staging/_staging.yml")
    (dbt-test-goto "stg_orders")
    (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                   '(("models/staging/stg_orders.sql" . 1))))))

(ert-deftest dbt-mode-names-each-package-that-defines-the-name ()
  "M-. on a root YAML entry of a name that two packages define names both, each once."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (make-directory (expand-file-name "dbt_packages/aaa_utils/models" root) t)
    (make-directory (expand-file-name "dbt_packages/aaa_utils/seeds" root))
    (write-region "name: 'aaa_utils'\n" nil (expand-file-name "dbt_packages/aaa_utils/dbt_project.yml" root))
    (dolist (file '("dbt_packages/shop_utils/models/util_orders.sql" "dbt_packages/aaa_utils/models/util_orders.sql"))
      (write-region "select 1 as id" nil (expand-file-name file root)))
    (write-region "id\n1\n" nil (expand-file-name "dbt_packages/aaa_utils/seeds/util_orders.csv" root))
    (write-region "models:\n  - name: util_orders\n" nil (expand-file-name "models/_utils.yml" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/_utils.yml"))
      (dbt-properties-mode-maybe)
      (dbt-test-goto "util_orders")
      ;; In either order.
      (should (string-match-p "\\`No util_orders in package shop, only in \\(aaa_utils, shop_utils\\|shop_utils, aaa_utils\\)\\'"
                              (cadr (should-error (execute-kbd-macro (kbd "M-.")) :type 'user-error)))))))

(ert-deftest dbt-mode-goes-to-the-project-s-namesake-that-dbt-takes ()
  "M-. on a ref, a source or a doc goes to the project's definition when dbt takes it, though a package defines a namesake.
A package's own definition, which dbt takes for the package's calls
unless the project disables it, which the index does not read, is
listed with the project's, first.  A macro call, which dbt resolves
from the caller's package first, and an identifier read by C-c C-f,
which names resources, list every definition too."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (let ((calls (concat "select * from {{ ref('stg_orders') }}\n"
                         "join {{ source('raw', 'raw_orders') }} using (id)\n"
                         "where {{ cents_to_dollars('x') }} > 0\n")))
      (pcase-dolist (`(,file . ,text)
                     `(("dbt_packages/shop_utils/models/stg_orders.sql" . "select 1 as id\n")
                       ("dbt_packages/shop_utils/models/_utils.yml"
                        . ,(concat "sources:\n  - name: raw\n    tables:\n      - name: raw_orders\n"
                                   "models:\n  - name: pkg_calls\n    description: '{{ doc(\"order_status\") }}'\n"))
                       ("dbt_packages/shop_utils/models/docs.md"
                        . "{% docs order_status %}\nIn the package.\n{% enddocs %}\n")
                       ("dbt_packages/shop_utils/macros/cents_to_dollars.sql"
                        . "{% macro cents_to_dollars(x) %}{{ x }}{% endmacro %}\n")
                       ("dbt_packages/shop_utils/models/pkg_calls.sql" . ,calls)
                       ("models/marts/calls.sql" . ,calls)))
        (write-region text nil (expand-file-name file root))))
    (save-window-excursion
      (pcase-dolist (`(,file ,keys ,visited)
                     '(("models/marts/calls.sql" "stg_orders" "models/staging/stg_orders.sql")
                       ("models/marts/calls.sql" "raw_orders" "models/staging/_sources.yml")
                       ("models/staging/_staging.yml" "order_status" "models/docs.md")
                       ("dbt_packages/shop_utils/models/_utils.yml" "order_status" "models/docs.md")))
        (switch-to-buffer (dbt-test-visit root file))
        (when (string-suffix-p ".yml" file)
          (dbt-properties-mode-maybe))
        (dbt-test-goto keys)
        (execute-kbd-macro (kbd "M-."))
        (should (equal (file-relative-name buffer-file-name root) visited)))
      ;; EARLIER, then LATER, in the list.
      (pcase-dolist (`(,file ,point-on ,keys ,earlier ,later)
                     '(("dbt_packages/shop_utils/models/pkg_calls.sql" "stg_orders" "M-."
                        "shop_utils/models/stg_orders.sql" "staging/stg_orders.sql")
                       ("dbt_packages/shop_utils/models/pkg_calls.sql" "raw_orders" "M-."
                        "shop_utils/models/_utils.yml" "staging/_sources.yml")
                       ("models/marts/calls.sql" "cents_to_dollars" "M-."
                        "cents_to_dollars.sql" "cents_to_dollars.sql")
                       ("models/marts/calls.sql" nil "C-c C-f ref:stg_orders RET"
                        "staging/stg_orders.sql" "shop_utils/models/stg_orders.sql")
                       ;; Though point is on a call of the same identifier.
                       ("models/marts/calls.sql" "stg_orders" "C-c C-f ref:stg_orders RET"
                        "staging/stg_orders.sql" "shop_utils/models/stg_orders.sql")))
        (switch-to-buffer (dbt-test-visit root file))
        (if point-on (dbt-test-goto point-on) (goto-char (point-max)))
        (execute-kbd-macro (kbd keys))
        (should (equal (buffer-name) "*xref*"))
        (should (string-search later (buffer-string) (1+ (string-search earlier (buffer-string)))))))))

(ert-deftest dbt-mode-finds-no-source-named-as-a-package ()
  "M-. on source('shop_utils', 'events') finds nothing: no source is shop_utils, and a package of that name is no source.
The project has a source events, which a message taking shop_utils for
a package would name."
  (dbt-test-with-project root
    (write-region "select * from {{ source('shop_utils', 'events') }}\n" nil
                  (expand-file-name "models/from_utils.sql" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/from_utils.sql"))
      (dbt-test-goto "events")
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "M-.")) :type 'user-error))
                     "No definitions found for: source:shop_utils/events")))))

(ert-deftest dbt-mode-finds-no-macro-of-a-package-the-index-lacks ()
  "M-. on a macro of dbt's own package finds nothing, though a package defines a namesake.
So does a macro of a package that is not installed, while the project,
whose package lacks the macro, says where it is."
  (dbt-test-with-project root
    (write-region "{% macro date_spine() %}select 1{% endmacro %}\n" nil
                  (expand-file-name "dbt_packages/shop_utils/macros/date_spine.sql" root))
    (write-region "{{ dbt.date_spine() }} {{ codegen.date_spine() }} {{ shop.date_spine() }}\n" nil
                  (expand-file-name "models/spine.sql" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/spine.sql"))
      (dolist (case '(("dbt.date_spine" . "No definitions found for: macro:dbt/date_spine")
                      ("codegen.date_spine" . "No definitions found for: macro:codegen/date_spine")
                      ("shop.date_spine" . "No date_spine in package shop, only in shop_utils")))
        (dbt-test-goto (car case) (1+ (string-search "." (car case))))
        (should (equal (cadr (should-error (execute-kbd-macro (kbd "M-.")) :type 'user-error)) (cdr case)))))))

(ert-deftest dbt-mode-names-the-version-a-package-lacks ()
  "M-. on a version of a package's model that only the project has says that the package lacks that version."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (dolist (file '("dbt_packages/shop_utils/models/v_v1" "dbt_packages/shop_utils/models/v_v2" "models/marts/v_v3"))
      (write-region "select 1" nil (expand-file-name (concat file ".sql") root)))
    (write-region "models:\n  - name: v\n    versions:\n      - v: 1\n      - v: 2\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/_v.yml" root))
    (write-region "models:\n  - name: v\n    versions:\n      - v: 3\n" nil
                  (expand-file-name "models/marts/_v.yml" root))
    (write-region "select * from {{ ref('shop_utils', 'v', v=3) }}\n" nil
                  (expand-file-name "models/marts/uses_v.sql" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/uses_v.sql"))
      (dbt-test-goto "'v'" 1)
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "M-.")) :type 'user-error))
                     "No v.v3 in package shop_utils, only in shop")))))

(ert-deftest dbt-mode-resolves-from-the-package-of-any-file ()
  "A call in a package's YAML properties, singular test or macro resolves in that package first; M-? on its source entry lists its uses.
The project's namesakes and their uses come after, or not at all."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (make-directory (expand-file-name "dbt_packages/shop_utils/tests" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/stg_orders.sql" root))
    (write-region "sources:\n  - name: raw\n    tables:\n      - name: raw_orders\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/_sources.yml" root))
    (write-region "select * from {{ source('raw', 'raw_orders') }}\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/pkg_src.sql" root))
    (write-region "select * from {{ ref('stg_orders') }} where id < 0\n" nil
                  (expand-file-name "dbt_packages/shop_utils/tests/assert_pkg.sql" root))
    (write-region (concat "models:\n  - name: pkg_src\n    data_tests:\n      - relationships:\n"
                          "          arguments:\n            to: ref('stg_orders')\n            field: id\n")
                  nil (expand-file-name "dbt_packages/shop_utils/models/_props.yml" root))
    ;; dbt 2.0.9 takes the package of the node that calls the macro,
    ;; which dbt-mode cannot know, as `dbt-project-lookup-rank' says.
    (write-region "{% macro pkg_orders() %}\n  select * from {{ ref('stg_orders') }}\n{% endmacro %}\n" nil
                  (expand-file-name "dbt_packages/shop_utils/macros/pkg_orders.sql" root))
    (dolist (file '("dbt_packages/shop_utils/tests/assert_pkg.sql" "dbt_packages/shop_utils/models/_props.yml"
                    "dbt_packages/shop_utils/macros/pkg_orders.sql"))
      (dbt-test-visit root file)
      (dbt-test-goto "stg_orders")
      (should (equal (car (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt))
                                               root))
                     '("dbt_packages/shop_utils/models/stg_orders.sql" . 1))))
    (dbt-test-visit root "dbt_packages/shop_utils/models/_sources.yml")
    (dbt-test-goto "raw_orders")
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                   '(("dbt_packages/shop_utils/models/pkg_src.sql" . 1))))))

(ert-deftest dbt-mode-resolves-from-a-package-reached-through-a-link ()
  "A call in a local package's file, visited through the link that dbt deps makes in dbt_packages, resolves in that package first.
So does one in a file of a project reached through a link."
  (dbt-test-with-project root
    (let ((link (concat (directory-file-name root) "-link")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "libs/zzz/models" root) t)
            (write-region "name: 'aaa'\n" nil (expand-file-name "libs/zzz/dbt_project.yml" root))
            (write-region "select 1 as id" nil (expand-file-name "libs/zzz/models/stg_orders.sql" root))
            (write-region "select * from {{ ref('stg_orders') }}\n" nil
                          (expand-file-name "libs/zzz/models/aaa_calls.sql" root))
            (write-region "  - local: libs/zzz\n" nil (expand-file-name "packages.yml" root) 'append)
            (make-symbolic-link "../libs/zzz" (expand-file-name "dbt_packages/aaa" root))
            (make-symbolic-link (directory-file-name root) link)
            (pcase-dolist (`(,top ,file) `((,root "dbt_packages/aaa/models/aaa_calls.sql")
                                           (,(file-name-as-directory link) "libs/zzz/models/aaa_calls.sql")))
              (with-current-buffer (dbt-test-visit top file)
                (dbt-test-goto "stg_orders")
                (should (equal (car (dbt-test-xref-files (xref-backend-definitions
                                                          'dbt (xref-backend-identifier-at-point 'dbt))
                                                         top))
                               '("libs/zzz/models/stg_orders.sql" . 1)))
                ;; Else the next visit of the same file finds this buffer.
                (kill-buffer))))
        (dolist (buffer (buffer-list))
          (when (string-prefix-p link (or (buffer-file-name buffer) ""))
            (kill-buffer buffer)))
        (delete-file link)))))

(ert-deftest dbt-mode-visits-the-properties-of-its-own-package ()
  "C-c C-y visits the YAML entry of the model in its own package, never that of a namesake in another.
A package's model without an entry of its own has none to visit."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/stg_orders.sql" root))
    (write-region "select 1 as id" nil (expand-file-name "dbt_packages/shop_utils/models/stg_customers.sql" root))
    (write-region "models:\n  - name: stg_orders\n" nil (expand-file-name "dbt_packages/shop_utils/models/_props.yml" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/staging/stg_orders.sql"))
      (execute-kbd-macro (kbd "C-c C-y"))
      (should (equal (file-relative-name buffer-file-name root) "models/staging/_staging.yml"))
      (switch-to-buffer (dbt-test-visit root "dbt_packages/shop_utils/models/stg_orders.sql"))
      (execute-kbd-macro (kbd "C-c C-y"))
      (should (equal (file-relative-name buffer-file-name root) "dbt_packages/shop_utils/models/_props.yml"))
      (switch-to-buffer (dbt-test-visit root "dbt_packages/shop_utils/models/stg_customers.sql"))
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "C-c C-y")) :type 'user-error))
                     "No YAML entry names stg_customers")))))

(ert-deftest dbt-mode-identifiers-round-trip ()
  "An identifier reads back as the target it was written from."
  (dolist (target '((:kind ref :qualifier nil :name "orders" :version nil)
                    (:kind ref :qualifier "pkg" :name "orders" :version "2")
                    (:kind source :qualifier "raw" :name "orders" :version nil)
                    (:kind macro :qualifier nil :name "m" :version nil)
                    (:kind analysis :qualifier nil :name "counts" :version nil)))
    (should (equal (dbt-mode-parse-identifier (dbt-mode-identifier target)) target)))
  (should-error (dbt-mode-parse-identifier "model:orders") :type 'user-error))

(defun dbt-test-xref-files (items root)
  "Return the files of the xref ITEMS, relative to ROOT, with their lines."
  (mapcar (lambda (item)
            (let ((location (xref-item-location item)))
              (cons (file-relative-name (xref-location-group location) root) (xref-location-line location))))
          items))

(ert-deftest dbt-mode-xref-finds-definitions ()
  "M-. goes to the model of a ref, the YAML of a source, a macro, a docs block."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should (equal (xref-find-backend) 'dbt))
    (dolist (case '(("ref:stg_orders" ("models/staging/stg_orders.sql" . 1))
                    ("source:raw/raw_customers" ("models/staging/_sources.yml" . 10))
                    ("source:events" ("models/staging/_sources.yml" . 12))
                    ("macro:shop_utils/is_positive" ("dbt_packages/shop_utils/macros/is_positive.sql" . 1))
                    ("macro:cents_to_dollars" ("macros/cents_to_dollars.sql" . 1))
                    ("doc:order_status" ("models/docs.md" . 1))))
      (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (car case)) root) (cdr case))))
    (dbt-test-goto "is_positive" 3)
    (should (equal (xref-backend-identifier-at-point 'dbt) "macro:shop_utils/is_positive"))
    (should (member "ref:stg_orders" (xref-backend-identifier-completion-table 'dbt)))
    (should (member "macro:shop_utils/is_positive" (xref-backend-identifier-completion-table 'dbt)))))

(ert-deftest dbt-mode-finds-a-doc-a-source-and-a-generic-test ()
  "C-c C-f completes a docs block, a source and a generic test by the call that names each, and visits it.
TAB completes from the identifiers of the project: a keyboard macro
that ends in the minibuffer returns its text, whatever it matches."
  (dbt-test-with-project root
    (write-region "{% test positive(model, column_name) %}\nselect 1\n{% endtest %}\n" nil
                  (expand-file-name "macros/positive.sql" root))
    (save-window-excursion
      (dolist (case '(("doc:order_st" . "models/docs.md") ("source:ra" . "models/staging/_sources.yml")
                      ("macro:positiv" . "macros/positive.sql")))
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        (execute-kbd-macro (kbd (concat "C-c C-f " (car case) " TAB RET")))
        (should (equal (file-relative-name buffer-file-name root) (cdr case)))))))

(ert-deftest dbt-mode-finds-analyses ()
  "C-c C-f visits an analysis, and so does M-. on its YAML entry."
  (dbt-test-with-project root
    (write-region "analyses:\n  - name: order_counts\n" nil (expand-file-name "analyses/_analyses.yml" root))
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-find-resource
     (seq-find (lambda (identifier) (string-suffix-p ":order_counts" identifier))
               (xref-backend-identifier-completion-table 'dbt)))
    (should (equal (file-relative-name buffer-file-name root) "analyses/order_counts.sql"))
    (dbt-test-visit root "analyses/_analyses.yml")
    (dbt-test-goto "order_counts")
    (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                   '(("analyses/order_counts.sql" . 1))))))

(ert-deftest dbt-mode-xref-finds-references ()
  "M-? lists the calls of a model, and its YAML entry; the calls of a macro, not its definition.
A dangling link among the project's files holds none and stops nothing."
  (dbt-test-with-project root
    (make-symbolic-link "/nonexistent/model.sql" (expand-file-name "models/dangling.sql" root))
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (should (equal (sort (mapcar #'car (dbt-test-xref-files (xref-backend-references 'dbt "ref:stg_orders") root))
                         #'string<)
                   '("analyses/order_counts.sql" "models/marts/orders.sql" "models/staging/_staging.yml"
                     "snapshots/orders_snapshot.sql" "tests/assert_positive_amount.sql")))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "macro:cents_to_dollars") root)
                   '(("models/staging/stg_orders.sql" . 13))))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "doc:order_status") root)
                   '(("models/staging/_staging.yml" . 5))))
    ;; A name typed at the prompt, which is no KIND:NAME, is refused.
    (should-error (xref-backend-references 'dbt "orders") :type 'user-error)
    (write-region "select * from {{ source('raw', 'raw_orders') }}" nil
                  (expand-file-name "models/staging/from_raw.sql" root))
    (dolist (case '(("source:raw" ("models/staging/from_raw.sql" . 1))
                    ("source:raw/raw_orders" ("models/staging/from_raw.sql" . 1))
                    ("source:raw/raw_customers")
                    ("source:events")))
      (should (equal (dbt-test-xref-files (xref-backend-references 'dbt (car case)) root) (cdr case))))))

(ert-deftest dbt-mode-xref-finds-references-while-the-index-expires ()
  "M-? lists the calls at their lines and columns though the index expires within its scan.
A lifetime of 0 makes every call outlive it, as a scan longer than the
lifetime does on a large project.  A call's column is that of its ref."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (let ((dbt-project-index-lifetime 0))
      (should (equal (sort (mapcar (lambda (item)
                                     (let ((location (xref-item-location item)))
                                       (list (file-relative-name (xref-location-group location) root)
                                             (xref-location-line location)
                                             (xref-file-location-column location))))
                                   (xref-backend-references 'dbt "ref:stg_orders"))
                           (lambda (a b) (string< (car a) (car b))))
                     '(("analyses/order_counts.sql" 1 32) ("models/marts/orders.sql" 12 21)
                       ("models/staging/_staging.yml" 4 0) ("snapshots/orders_snapshot.sql" 3 17)
                       ("tests/assert_positive_amount.sql" 1 17)))))))

;; A model with versions, as dbt documents them: one file per version,
;; NAME_vVERSION, and the latest version given in its YAML entry.
(defconst dbt-test-versioned-model
  '(("models/marts/dim_customers_v1.sql" . "select 1 as id")
    ("models/marts/dim_customers_v2.sql" . "select 2 as id")
    ("models/marts/_dim_customers.yml"
     . "models:\n  - name: dim_customers\n    latest_version: 2\n    versions:\n      - v: 1\n      - v: 2\n"))
  "The files of a model with two versions, relative to the root.")

(ert-deftest dbt-mode-xref-goes-to-the-version-a-ref-names ()
  "M-. on a ref with a version goes to that version; without one, to the latest."
  (dbt-test-with-project root
    (pcase-dolist (`(,file . ,text) dbt-test-versioned-model)
      (write-region text nil (expand-file-name file root)))
    (dbt-test-visit root "models/marts/customers.sql")
    (goto-char (point-max))
    (insert "{{ ref('dim_customers', v=1) }} {{ ref('dim_customers') }}")
    (dolist (case '(("dim_customers', v" . "models/marts/dim_customers_v1.sql")
                    ("dim_customers')" . "models/marts/dim_customers_v2.sql")))
      (dbt-test-goto (car case))
      (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                     (list (cons (cdr case) 1)))))
    (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt "ref:dim_customers@1") root)
                   '(("models/marts/dim_customers_v1.sql" . 1))))))

(ert-deftest dbt-mode-goes-to-the-version-of-an-item-of-versions ()
  "M-. on an item of a model's versions in its YAML entry goes to that version."
  (dbt-test-with-project root
    (pcase-dolist (`(,file . ,text) dbt-test-versioned-model)
      (write-region text nil (expand-file-name file root)))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/_dim_customers.yml"))
      (dbt-properties-mode-maybe)
      (dbt-test-goto "v: 1" 3)
      (execute-kbd-macro (kbd "M-."))
      (should (equal (file-relative-name buffer-file-name root) "models/marts/dim_customers_v1.sql")))))

(ert-deftest dbt-mode-xref-finds-the-references-of-one-of-two-namesakes ()
  "M-? on a model lists the calls that resolve to it, not those of a namesake."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (pcase-dolist (`(,file . ,text) '(("dbt_packages/shop_utils/models/orders.sql" . "select 1 as id")
                                      ("dbt_packages/shop_utils/models/uses_orders.sql"
                                       . "select * from {{ ref('orders') }}")
                                      ("models/marts/utils_orders.sql"
                                       . "select * from {{ ref('shop_utils', 'orders') }}")))
      (write-region text nil (expand-file-name file root)))
    (dbt-test-visit root "models/marts/customers.sql")
    (dolist (case '(("ref:shop_utils/orders" "dbt_packages/shop_utils/models/uses_orders.sql"
                     "models/marts/utils_orders.sql")
                    ("ref:orders" "models/marts/customers.sql")))
      (should (equal (sort (mapcar #'car (dbt-test-xref-files (xref-backend-references 'dbt (car case)) root))
                           #'string<)
                     (cdr case))))))

(defun dbt-test-completions (text)
  "Insert TEXT at the end of the buffer, and return what completes there."
  (goto-char (point-max))
  (insert text)
  (let ((completion (dbt-mode-completion-at-point)))
    (and completion
         (sort (all-completions (buffer-substring-no-properties (car completion) (point)) (nth 2 completion))
               #'string<))))

(ert-deftest dbt-mode-completes-arguments-and-macros ()
  "ref, source and doc complete their arguments; a tag completes macros."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/customers.sql")
    (should (equal (dbt-test-completions "\n{{ ref('stg_") '("stg_customers" "stg_orders")))
    (should (equal (dbt-test-completions "') }}\n{{ source('") '("events" "raw")))
    (should (equal (dbt-test-completions "raw', 'raw_c") '("raw_customers")))
    (should (equal (dbt-test-completions "') }}\n{{ doc(\"") '("order_status")))
    (should (equal (dbt-test-completions "\") }}\n{{ shop_utils.") '("is_positive")))
    (should (member "cents_to_dollars" (dbt-test-completions " }}\n{{ cents")))
    (should-not (dbt-test-completions " }}\n{{ myref('stg_"))
    (should-not (dbt-test-completions " }}\nselect cents"))))

(ert-deftest dbt-mode-imenu-lists-ctes ()
  "imenu lists the CTEs of a model, in order, and the macros of a file."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should (equal (mapcar #'car (funcall imenu-create-index-function)) '("orders" "customers" "final")))
    (dbt-test-visit root "macros/cents_to_dollars.sql")
    (should (equal (mapcar (lambda (entry) (cons (car entry) (mapcar #'car (cdr entry))))
                           (funcall imenu-create-index-function))
                   '(("Macros" "cents_to_dollars"))))))

(ert-deftest dbt-mode-cte-query-selects-from-the-cte ()
  "A CTE's query is the model up to the CTE, then a select from it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-test-goto "customers.first_name")
    (let ((query (dbt-mode-cte-query)))
      (should (equal (car query) "final"))
      (should (string-prefix-p (buffer-substring-no-properties (point-min) (progn (dbt-test-goto "with orders as (")
                                                                                     (point)))
                               (cdr query)))
      (should (string-suffix-p "{% endif %}\n)\nselect * from final" (cdr query))))
    (let ((query (dbt-mode-cte-query "customers")))
      (should (string-suffix-p "{{ ref('stg_customers') }}\n)\nselect * from customers" (cdr query))))
    (goto-char (point-min))
    (should-error (dbt-mode-cte-query) :type 'user-error)))

(ert-deftest dbt-mode-cte-query-keeps-what-comes-before-the-with ()
  "What comes before the WITH comes first in a CTE's query, sets in their blocks.
The tag of a Jinja block open at the WITH, as a legacy snapshot's, does not."
  (let ((before (concat "{# {% set old = 1 %} #}\n{% if target.name == 'prod' %}\n{% set cols = ['a'] %}\n"
                        "{% else %}\n{% set cols = ['a', 'b'] %}\n{% endif %}\n-- {% set commented = 'x' %}\n"
                        "{% set pattern = 'p%' %}\n"))
        (chain "with x as (\n    select {% for c in cols %}{{ c }}{% endfor %}\n)"))
    (with-temp-buffer
      (insert before chain "\nselect * from x\n")
      (dbt-mode)
      (dbt-test-goto "select {%")
      (should (equal (cdr (dbt-mode-cte-query)) (concat before chain "\nselect * from x"))))
    (with-temp-buffer
      (insert "{% snapshot s %}\n{{ config(unique_key='a') }}\n{% set cols = ['a'] %}\n"
              chain "\nselect * from x\n{% endsnapshot %}\n")
      (dbt-mode)
      (dbt-test-goto "select {%")
      (should (equal (cdr (dbt-mode-cte-query))
                     (concat "\n{{ config(unique_key='a') }}\n{% set cols = ['a'] %}\n" chain "\nselect * from x"))))
    (with-temp-buffer
      (insert "{% set cols = ['a'] %}\n{% if execute %}{% if true %}\n" chain "\nselect * from x\n{% endif %}{% endif %}\n")
      (dbt-mode)
      (dbt-test-goto "select {%")
      (should (equal (cdr (dbt-mode-cte-query)) (concat "{% set cols = ['a'] %}\n\n" chain "\nselect * from x"))))))

(ert-deftest dbt-mode-knows-when-no-server-runs ()
  "A buffer that no dbt lsp serves has no server, whether dbt-lsp is loaded or not."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-not (dbt-mode-server-p))
    (require 'dbt-lsp)
    (should-not (dbt-mode-server-p))))

(ert-deftest dbt-mode-visits-the-compiled-sql ()
  "C-c C-v without a server shows what dbt compile wrote, read-only."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-visit-compiled) :type 'user-error)
    (let ((compiled (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)))
      (make-directory (file-name-directory compiled) t)
      (write-region "select 1" nil compiled)
      (dbt-visit-compiled)
      (with-current-buffer (get-file-buffer compiled)
        (should buffer-read-only)))))

(ert-deftest dbt-mode-visits-the-compiled-sql-of-snapshots-and-packages ()
  "C-c C-v finds where dbt 2 compiles a snapshot, and a package's model.
dbt compiles a local package in the copy it installs."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as one" nil (expand-file-name "dbt_packages/shop_utils/models/util_one.sql" root))
    (make-directory (expand-file-name "local_pkgs/util/models" root) t)
    (write-region "name: 'util'\nconfig-version: 2\n" nil (expand-file-name "local_pkgs/util/dbt_project.yml" root))
    (write-region "select 2 as two" nil (expand-file-name "local_pkgs/util/models/util_two.sql" root))
    (write-region "  - local: local_pkgs/util\n" nil (expand-file-name "packages.yml" root) 'append)
    (with-temp-file (expand-file-name "snapshots/orders_snapshot.sql" root)
      (insert "{% snapshot snap_orders %}\nselect * from {{ ref('stg_orders') }}\n{% endsnapshot %}\n"))
    ;; Where dbt 2.0.9 wrote them.
    (dolist (case '(("snapshots/orders_snapshot.sql"
                     . "target/compiled/shop/snapshots/orders_snapshot.sql/snap_orders.sql")
                    ("dbt_packages/shop_utils/models/util_one.sql"
                     . "target/compiled/shop_utils/dbt_packages/shop_utils/models/util_one.sql")
                    ("local_pkgs/util/models/util_two.sql"
                     . "target/compiled/util/dbt_packages/util/models/util_two.sql")))
      (let ((compiled (expand-file-name (cdr case) root)))
        (make-directory (file-name-directory compiled) t)
        (write-region "select 1" nil compiled)
        (dbt-test-visit root (car case))
        (dbt-visit-compiled)
        (should (get-file-buffer compiled))
        (with-current-buffer (get-file-buffer compiled)
          (should buffer-read-only))))))

(ert-deftest dbt-mode-visits-the-properties-of-a-model ()
  "C-c C-y goes to the YAML entry of the model, and back to it by M-."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-visit-properties)
    (should (equal (file-relative-name buffer-file-name root) "models/staging/_staging.yml"))
    (should (equal (line-number-at-pos) 4))
    (should (equal (plist-get (dbt-mode-current-resource) :file)
                   (expand-file-name "models/staging/stg_orders.sql" root)))))

(ert-deftest dbt-mode-lineage-reads-the-project-again ()
  "g in a lineage that no server gives shows what the project holds now, each time."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-lineage "orders")
    (with-current-buffer "*dbt lineage: orders*"
      (unwind-protect
          (dolist (name '("order_report" "order_audit"))
            (should-not (member name (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
            (write-region "select * from {{ ref('orders') }}" nil
                          (expand-file-name (format "models/marts/%s.sql" name) root))
            (revert-buffer)
            (should (member name (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows)))))
        (kill-buffer)))))

(ert-deftest dbt-mode-inserts-refs ()
  "A ref goes inside braces outside a tag, bare inside one."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/customers.sql")
    (goto-char (point-max))
    (dbt-insert-ref "stg_orders")
    (should (string-suffix-p "{{ ref('stg_orders') }}" (buffer-string)))
    (insert " {{ ")
    (dbt-insert-ref "raw_orders")
    (should (string-suffix-p "{{ ref('raw_orders')" (buffer-string)))))

(defun dbt-test-write-namesakes (root)
  "Write in ROOT two packages whose models share names with each other and the project.
shop_utils defines util and stg_orders, the project's namesake; a_utils,
listed before it, defines util and only_in_a.  Namesakes have relations
of their own, as dbt requires.  shop_utils's model pkg_calls and the
project's model calls are empty; shop_utils's macro pkg_macro and the
project's macro project_macro are too, and so is shop_utils's singular
test pkg_test."
  (dolist (directory '("dbt_packages/shop_utils/models" "dbt_packages/shop_utils/tests" "dbt_packages/a_utils/models"))
    (make-directory (expand-file-name directory root) t))
  (pcase-dolist (`(,file . ,text)
                 '(("packages.yml" . "packages:\n  - local: dbt_packages/a_utils\n  - local: dbt_packages/shop_utils\n")
                   ("dbt_packages/a_utils/dbt_project.yml" . "name: 'a_utils'\nversion: '1.0.0'\nconfig-version: 2\n")
                   ("dbt_packages/shop_utils/models/util.sql" . "select 1 as id\n")
                   ("dbt_packages/shop_utils/models/stg_orders.sql"
                    . "{{ config(alias='shop_utils_stg_orders') }}\nselect 1 as id\n")
                   ("dbt_packages/a_utils/models/util.sql" . "{{ config(alias='a_utils_util') }}\nselect 1 as id\n")
                   ("dbt_packages/a_utils/models/only_in_a.sql" . "select 1 as id\n")
                   ("dbt_packages/shop_utils/models/pkg_calls.sql" . "")
                   ("models/marts/calls.sql" . "")
                   ("dbt_packages/shop_utils/macros/pkg_macro.sql" . "{% macro pkg_macro() %}\n{% endmacro %}\n")
                   ("macros/project_macro.sql" . "{% macro project_macro() %}\n{% endmacro %}\n")
                   ("dbt_packages/shop_utils/tests/pkg_test.sql" . "")))
    (write-region text nil (expand-file-name file root))))

(ert-deftest dbt-mode-inserts-the-ref-that-dbt-takes-from-a-package ()
  "C-c C-e in a package's model calls what dbt takes from there: its package's own resource, else the project's, by name.
Another package's resource comes with its package, from a package's
model as from the project's.  In a macro, which dbt resolves from the
node that calls it, any resource but the project's comes with its
package.  A buffer that visits no file calls as the project does."
  (dbt-test-with-project root
    (dbt-test-write-namesakes root)
    (save-window-excursion
      (pcase-dolist (`(,file ,name ,call)
                     '(("dbt_packages/shop_utils/models/pkg_calls.sql" "util" "{{ ref('util') }}")
                       ("dbt_packages/shop_utils/models/pkg_calls.sql" "stg_orders" "{{ ref('stg_orders') }}")
                       ("dbt_packages/shop_utils/models/pkg_calls.sql" "customers" "{{ ref('customers') }}")
                       ("dbt_packages/shop_utils/models/pkg_calls.sql" "only_in_a" "{{ ref('a_utils', 'only_in_a') }}")
                       ("models/marts/calls.sql" "stg_orders" "{{ ref('stg_orders') }}")
                       ("models/marts/calls.sql" "only_in_a" "{{ ref('a_utils', 'only_in_a') }}")
                       ("dbt_packages/shop_utils/macros/pkg_macro.sql" "util" "{{ ref('shop_utils', 'util') }}")
                       ("dbt_packages/shop_utils/macros/pkg_macro.sql" "customers" "{{ ref('customers') }}")
                       ("macros/project_macro.sql" "stg_orders" "{{ ref('stg_orders') }}")
                       ("macros/project_macro.sql" "only_in_a" "{{ ref('a_utils', 'only_in_a') }}")
                       ("dbt_packages/shop_utils/tests/pkg_test.sql" "util" "{{ ref('util') }}")
                       (nil "stg_orders" "{{ ref('stg_orders') }}")
                       (nil "only_in_a" "{{ ref('a_utils', 'only_in_a') }}")))
        (if file
            (switch-to-buffer (dbt-test-visit root file))
          (switch-to-buffer "*dbt no file*")
          (setq default-directory root)
          (dbt-mode))
        ;; Within the macro's block, else at the end.
        (if (string-search "macros/" (or file ""))
            (progn (goto-char (point-min)) (end-of-line))
          (goto-char (point-max)))
        (insert "\n")
        (execute-kbd-macro (kbd (format "C-c C-e %s RET" name)))
        (should (equal (buffer-substring-no-properties (line-beginning-position) (point)) call)))
      (kill-buffer "*dbt no file*"))))

(ert-deftest dbt-mode-live-inserts-the-refs-that-dbt-takes-from-a-package ()
  "With the real dbt, the refs that C-c C-e inserts in a package's model and macro depend on what was read.
dbt parses the model and lists what it depends on: shop_utils's util
and stg_orders, the project's customers, a_utils's only_in_a.  The ref
inserted in the package's macro takes shop_utils's util whether a model
of the package or one of the project calls the macro."
  (unless (getenv "DBT_MODE_TEST_DBT") (ert-skip "no dbt 2 executable"))
  (dbt-test-with-project root
    (dbt-test-write-namesakes root)
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "dbt_packages/shop_utils/models/pkg_calls.sql"))
      (insert "select * from ")
      (execute-kbd-macro (kbd "C-c C-e util RET"))
      (insert ", ")
      (execute-kbd-macro (kbd "C-c C-e stg_orders RET"))
      (insert ", ")
      (execute-kbd-macro (kbd "C-c C-e customers RET"))
      (insert ", ")
      (execute-kbd-macro (kbd "C-c C-e only_in_a RET"))
      (insert "\n")
      (save-buffer)
      (switch-to-buffer (dbt-test-visit root "dbt_packages/shop_utils/macros/pkg_macro.sql"))
      (goto-char (point-min))
      (end-of-line)
      (execute-kbd-macro (kbd "C-c C-e util RET"))
      (save-buffer))
    (write-region "select * from {{ pkg_macro() }}\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/pkg_uses.sql" root))
    (write-region "select * from {{ shop_utils.pkg_macro() }}\n" nil
                  (expand-file-name "models/marts/uses_macro.sql" root))
    (let ((default-directory root))
      (should (eq 0 (call-process (getenv "DBT_MODE_TEST_DBT") nil nil nil "parse" "--profiles-dir" root))))
    (let ((depends-on (lambda (id)
                        (sort (append (gethash "nodes" (gethash "depends_on" (dbt-project-manifest-node
                                                                               (expand-file-name "target/manifest.json" root)
                                                                               id)))
                                      nil)
                              #'string<))))
      (should (equal (funcall depends-on "model.shop_utils.pkg_calls")
                     '("model.a_utils.only_in_a" "model.shop.customers" "model.shop_utils.stg_orders"
                       "model.shop_utils.util")))
      (should (equal (funcall depends-on "model.shop_utils.pkg_uses") '("model.shop_utils.util")))
      (should (equal (funcall depends-on "model.shop.uses_macro") '("model.shop_utils.util"))))))

(ert-deftest dbt-mode-inserts-no-ref-to-nothing ()
  "RET on an empty answer inserts no ref: not to the buffer's own model, nor to nothing."
  (dbt-test-with-project root
    (dolist (file '("models/marts/customers.sql" "analyses/order_counts.sql" "macros/cents_to_dollars.sql"))
      (dbt-test-visit root file)
      (goto-char (point-max))
      (let ((text (buffer-string)))
        ;; The minibuffer: RET gives the default, or else the empty string.
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _collection &optional _predicate _require _initial _history default &rest _)
                     (or (car-safe default) default ""))))
          (should-error (call-interactively #'dbt-insert-ref) :type 'user-error))
        (should (equal (buffer-string) text))))))

(ert-deftest dbt-properties-mode-navigates-from-yaml ()
  "In a project's YAML, the mode turns on and M-. finds the model of an entry."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/_staging.yml")
    (dbt-properties-mode-maybe)
    (should dbt-properties-mode)
    (dbt-test-goto "stg_customers")
    (should (equal (xref-find-backend) 'dbt))
    (should (equal (dbt-test-xref-files (xref-backend-definitions 'dbt (xref-backend-identifier-at-point 'dbt)) root)
                   '(("models/staging/stg_customers.sql" . 1))))))

(defun dbt-test-write-versions (root)
  "Write a model dim of ROOT with versions 1 and 2, the latest, and a user of v1."
  (dolist (file '("dim_v1" "dim_v2"))
    (write-region "select * from {{ ref('stg_orders') }}" nil (expand-file-name (format "models/marts/%s.sql" file) root)))
  (write-region "models:\n  - name: dim\n    latest_version: 2\n    versions:\n      - v: 1\n      - v: 2\n"
                nil (expand-file-name "models/marts/_dim.yml" root))
  (write-region "select * from {{ ref('dim', v=1) }}" nil (expand-file-name "models/marts/uses_v1.sql" root)))

(ert-deftest dbt-mode-names-a-version-as-dbt-selects-it ()
  "A version is NAME.vVERSION: its lineage, the ref inserted, the choices offered.
A name alone is the latest version, as for `ref'."
  (dbt-test-with-project root
    (dbt-test-write-versions root)
    (dbt-test-visit root "models/marts/dim_v2.sql")
    (should (equal (dbt-mode-resource-selector (dbt-mode-current-resource)) "dim.v2"))
    ;; dbt show selects one node only.
    (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory)))
      (dbt-preview)
      (with-current-buffer (dbt-test-wait (lambda () (get-buffer "*dbt show: dim.v2*")))
        (should (string-match-p "^arguments: show .* --select dim\\.v2$" (buffer-string)))
        (kill-buffer)))
    (should (equal (dbt-mode-parse-selector "dim.v2") '("dim" . "2")))
    (should (equal (dbt-mode-parse-selector "dim") '("dim")))
    (let ((completing-read-function (lambda (_prompt collection &rest _) (all-completions "dim" collection))))
      (should (equal (sort (dbt-mode-read-resource "Lineage of: ") #'string<) '("dim" "dim.v1" "dim.v2"))))
    (dbt-lineage "dim.v1")
    (with-current-buffer "*dbt lineage: dim.v1*"
      (should (member '("downstream" "1" "model" "uses_v1" "models/marts/uses_v1.sql")
                      (dbt-test-lineage-rows)))
      (kill-buffer))
    (dbt-lineage "stg_orders")
    (with-current-buffer "*dbt lineage: stg_orders*"
      (should (member "dim.v1" (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
      (should (member "dim.v2" (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
      (kill-buffer))
    (dbt-lineage "dim")
    (with-current-buffer "*dbt lineage: dim.v2*"
      (should-not (assoc "downstream" (dbt-test-lineage-rows)))
      (kill-buffer))
    (goto-char (point-max))
    (insert "\n")
    (dbt-insert-ref "dim.v1")
    (should (looking-back "{{ ref('dim', v=1) }}" (line-beginning-position)))
    ;; The model itself, unpinned, floats to its latest version.
    (insert "\n")
    (dbt-insert-ref "dim")
    (should (looking-back "^{{ ref('dim') }}" (line-beginning-position)))))

(ert-deftest dbt-mode-offers-no-analysis-as-a-resource ()
  "In an analysis, the resource read has no default: an analysis has no lineage."
  (dbt-test-with-project root
    (dbt-test-visit root "analyses/order_counts.sql")
    ;; The seventh argument of `completing-read' is the default.
    (let ((completing-read-function (lambda (&rest arguments) (nth 6 arguments))))
      (should-not (dbt-mode-read-resource "Lineage of: ")))))

(ert-deftest dbt-mode-copies-relations-from-the-manifest ()
  "The relation of the model, or of the ref or source at point, comes from the manifest.
It is that of the node with this very id, not a list of its children,
nor a node whose id it begins."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-copy-relation) :type 'user-error)
    (make-directory (expand-file-name "target" root))
    (write-region (concat "{\"child_map\": {\"model.shop.orders\": [\"model.shop.x\"]},"
                          " \"nodes\": {\"model.shop.orders_v2\": {\"relation_name\": \"wrong\"},"
                          " \"model.shop.orders\": {\"relation_name\": \"\\\"db\\\".\\\"main\\\".\\\"orders\\\"\"},"
                          " \"model.shop.stg_orders\": {\"relation_name\": null}},"
                          " \"sources\": {\"source.shop.raw.raw_orders\": {\"relation_name\": \"raw.main.raw_orders\"}}}")
                  nil (expand-file-name "target/manifest.json" root))
    (goto-char (point-max))
    (dbt-copy-relation)
    (should (equal (current-kill 0) "\"db\".\"main\".\"orders\""))
    (dbt-test-goto "ref('stg_orders')" 6)
    (should-error (dbt-copy-relation) :type 'user-error)
    (dbt-test-visit root "models/staging/stg_customers.sql")
    (goto-char (point-max))
    (insert "union all select * from {{ source('raw', 'raw_orders') }}\n")
    (dbt-test-goto "raw_orders")
    (dbt-copy-relation)
    (should (equal (current-kill 0) "raw.main.raw_orders"))))

(ert-deftest dbt-mode-shows-the-target-and-the-materialization ()
  "The mode line says the target dbt runs on and the model's materialization.
It is that of the config() call, else that of the manifest; DBT_TARGET
names the target.  In batch, `format-mode-line' formats nothing at all,
so the test reads what `mode-line-process' shows."
  (dbt-test-with-project root
    (make-directory (expand-file-name "target" root))
    (write-region "{\"nodes\": {\"model.shop.stg_orders\": {\"config\": {\"materialized\": \"view\"}}}}"
                  nil (expand-file-name "target/manifest.json" root))
    (dbt-test-visit root "models/marts/orders.sql")
    (set-buffer-modified-p t)
    (save-buffer)
    (should (member '(dbt-mode--status (" " dbt-mode--status)) mode-line-process))
    (should (equal dbt-mode--status "dev table"))
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (let ((process-environment (cons "DBT_TARGET=prod" process-environment)))
      (set-buffer-modified-p t)
      (save-buffer))
    (should (equal dbt-mode--status "prod view"))))

(ert-deftest dbt-mode-reads-the-materialization-from-the-manifest-only-without-config ()
  "Without a config() call the materialization is the manifest's; a config() call wins over it.
The mode line says so once the buffer opens, unsaved; a save reads the
project's index no more, as the opening found the buffer's node."
  (dbt-test-with-project root
    (make-directory (expand-file-name "target" root))
    (write-region (concat "{\"nodes\": {\"model.shop.customers\": {\"config\": {\"materialized\": \"incremental\"}},"
                          " \"model.shop.orders\": {\"config\": {\"materialized\": \"view\"}}}}")
                  nil (expand-file-name "target/manifest.json" root))
    (with-current-buffer (dbt-test-visit root "models/marts/customers.sql")
      ;; The user goes idle.
      (ert-run-idle-timers)
      (should (equal dbt-mode--status "dev incremental"))
      (dbt-project-forget root)
      (set-buffer-modified-p t)
      (save-buffer)
      (should (equal dbt-mode--status "dev incremental"))
      (should-not (gethash (cons root 'index) dbt-project--cache)))
    ;; config() names the materialization of orders: no manifest is read.
    (when (get-buffer " *dbt manifest*")
      (kill-buffer " *dbt manifest*"))
    (with-current-buffer (dbt-test-visit root "models/marts/orders.sql")
      (ert-run-idle-timers)
      (should (equal dbt-mode--status "dev table"))
      (should-not (get-buffer " *dbt manifest*")))))

(ert-deftest dbt-mode-copies-the-build-command ()
  "The build command selects the model, or with a prefix its family.
On a ref or a source, it selects what they name."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (goto-char (point-max))
    (dbt-copy-build-command)
    (should (equal (current-kill 0) "dbt build -s orders"))
    (dbt-copy-build-command t)
    (should (equal (current-kill 0) "dbt build -s +orders+"))
    (dbt-test-goto "ref('stg_orders')" 6)
    (dbt-copy-build-command)
    (should (equal (current-kill 0) "dbt build -s stg_orders"))
    (insert "{{ source('raw', 'raw_orders') }} ")
    (dbt-test-goto "raw_orders')")
    (should (equal (dbt-mode-selection-at-point) "source:raw.raw_orders"))))

(ert-deftest dbt-mode-lists-the-tests-of-a-model ()
  "The tests of a model are its YAML entries, the data tests that name it and
the singular tests that read it; an exposure or another model's unit test
that reads it are none of its tests."
  (dbt-test-with-project root
    (write-region (concat "exposures:\n  - name: board\n    depends_on:\n      - ref('stg_orders')\n"
                          "models:\n  - name: orders\n    columns:\n      - name: order_id\n        data_tests:\n"
                          "          - relationships:\n              to: ref('stg_orders')\n              field: order_id\n"
                          "unit_tests:\n  - name: test_orders\n    model: orders\n    given:\n"
                          "      - input: ref('stg_orders')\n")
                  nil (expand-file-name "models/marts/_marts.yml" root))
    ;; tests: is the older name of data_tests.
    (write-region (concat "models:\n  - name: orders\n    columns:\n      - name: customer_id\n        tests:\n"
                          "          - relationships:\n              to: ref('stg_orders')\n")
                  nil (expand-file-name "models/marts/_legacy.yml" root))
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (let* (shown
           (xref-show-xrefs-function (lambda (fetcher _alist) (setq shown (funcall fetcher)))))
      (dbt-visit-tests)
      (should (equal (sort (dbt-test-xref-files shown root) (lambda (a b) (string< (format "%S" a) (format "%S" b))))
                     '(("models/marts/_legacy.yml" . 7) ("models/marts/_marts.yml" . 11)
                       ("models/staging/_staging.yml" . 4) ("tests/assert_positive_amount.sql" . 1)))))
    (dbt-test-visit root "models/marts/customers.sql")
    (should-error (dbt-visit-tests) :type 'user-error)))

(ert-deftest dbt-mode-visits-the-sql-of-the-last-run ()
  "With a prefix, C-c C-v shows what the last run ran, or says there was none."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-visit-compiled t) :type 'user-error)
    (let ((run (expand-file-name "target/run/shop/models/marts/orders.sql" root)))
      (make-directory (file-name-directory run) t)
      (write-region "create table x as select 1" nil run)
      (dbt-visit-compiled t)
      (should (get-file-buffer run))
      (with-current-buffer (get-file-buffer run)
        (should buffer-read-only)))))

(ert-deftest dbt-mode-binds-the-editing-commands ()
  "The keys that the README lists run their commands, and sql-mode keeps its own.
C-M-a and C-M-e move by CTE, where `sql-mode' moves by statement."
  (with-temp-buffer
    (dbt-mode)
    (dolist (binding '(("C-c /" . dbt-close-block) ("C-c %" . dbt-jump-to-matching-tag)
                       ("C-c C-t" . dbt-visit-tests) ("C-c C-w" . dbt-copy-relation)
                       ("C-c M-w" . dbt-copy-build-command) ("C-c C-a" . dbt-new)
                       ("C-c C-n" . sql-send-line-and-next) ("C-c C-c" . sql-send-paragraph)
                       ("C-M-a" . beginning-of-defun) ("C-M-e" . end-of-defun)
                       ("DEL" . dbt-edit-delete-backward)))
      (should (eq (key-binding (kbd (car binding))) (cdr binding))))))

(provide 'dbt-mode-test)
;;; dbt-mode-test.el ends here
