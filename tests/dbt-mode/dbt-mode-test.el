;;; dbt-mode-test.el --- Tests of dbt-mode without a server  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user gets from dbt-mode with no dbt executable: the mode of a
;; file, Jinja's syntax and faces, xref, completion, imenu and the
;; commands that read the project.

;;; Code:

(require 'cl-lib)
(require 'dbt-mode-test-helpers)
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
  "What dbt writes under the configured target-path opens in sql-mode."
  (dbt-test-with-project root
    (write-region "target-path: build\n" nil (expand-file-name "dbt_project.yml" root) 'append)
    (dolist (case '(("build/compiled/x.sql" . sql-mode) ("target/x.sql" . dbt-mode)))
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
  "Point inside a tag gets its bounds; outside, or in an open tag, none or no end."
  (with-temp-buffer
    (dbt-mode)
    (insert "select {{ ref('a') }} from t {% if x")
    (dbt-test-goto "ref")
    (should (equal (dbt-mode-jinja-bounds) (cons 8 22)))
    (dbt-test-goto "from")
    (should-not (dbt-mode-jinja-bounds))
    (goto-char (point-max))
    (should (equal (dbt-mode-jinja-bounds) (cons 30 nil)))))

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
    (should (equal (dbt-mode-target-at-point) '(:kind ref :qualifier nil :name "stg_orders" :version nil)))
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

(ert-deftest dbt-mode-identifiers-round-trip ()
  "An identifier reads back as the target it was written from."
  (dolist (target '((:kind ref :qualifier nil :name "orders" :version nil)
                    (:kind ref :qualifier "pkg" :name "orders" :version "2")
                    (:kind source :qualifier "raw" :name "orders" :version nil)
                    (:kind macro :qualifier nil :name "m" :version nil)
                    (:kind analysis :qualifier nil :name "counts" :version nil)))
    (should (equal (dbt-mode-parse-identifier (dbt-mode-identifier target)) target)))
  (should-not (dbt-mode-parse-identifier "model:orders")))

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
  "M-? lists the calls of a model, and its YAML entry; the calls of a macro, not its definition."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (should (equal (sort (mapcar #'car (dbt-test-xref-files (xref-backend-references 'dbt "ref:stg_orders") root))
                         #'string<)
                   '("analyses/order_counts.sql" "models/marts/orders.sql" "models/staging/_staging.yml"
                     "snapshots/orders_snapshot.sql" "tests/assert_positive_amount.sql")))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "macro:cents_to_dollars") root)
                   '(("models/staging/stg_orders.sql" . 13))))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "doc:order_status") root)
                   '(("models/staging/_staging.yml" . 5))))
    (write-region "select * from {{ source('raw', 'raw_orders') }}" nil
                  (expand-file-name "models/staging/from_raw.sql" root))
    (dolist (case '(("source:raw" ("models/staging/from_raw.sql" . 1))
                    ("source:raw/raw_orders" ("models/staging/from_raw.sql" . 1))
                    ("source:raw/raw_customers")
                    ("source:events")))
      (should (equal (dbt-test-xref-files (xref-backend-references 'dbt (car case)) root) (cdr case))))))

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
  "A CTE's query is the model's WITH up to the CTE, then a select from it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-test-goto "customers.first_name")
    (let ((query (dbt-mode-cte-query)))
      (should (equal (car query) "final"))
      (should (string-prefix-p "with orders as (" (cdr query)))
      (should (string-suffix-p "{% endif %}\n)\nselect * from final" (cdr query))))
    (let ((query (dbt-mode-cte-query "customers")))
      (should (string-suffix-p "{{ ref('stg_customers') }}\n)\nselect * from customers" (cdr query))))
    (goto-char (point-min))
    (should-error (dbt-mode-cte-query) :type 'user-error)))

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
  "C-c C-v finds where dbt 2 compiles a snapshot, and a package's model."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as one" nil (expand-file-name "dbt_packages/shop_utils/models/util_one.sql" root))
    (with-temp-file (expand-file-name "snapshots/orders_snapshot.sql" root)
      (insert "{% snapshot snap_orders %}\nselect * from {{ ref('stg_orders') }}\n{% endsnapshot %}\n"))
    ;; Where dbt 2.0.9 wrote them.
    (dolist (case '(("snapshots/orders_snapshot.sql"
                     . "target/compiled/shop/snapshots/orders_snapshot.sql/snap_orders.sql")
                    ("dbt_packages/shop_utils/models/util_one.sql"
                     . "target/compiled/shop_utils/dbt_packages/shop_utils/models/util_one.sql")))
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
            (should-not (member name (mapcar (lambda (entry) (aref (cadr entry) 3)) tabulated-list-entries)))
            (write-region "select * from {{ ref('orders') }}" nil
                          (expand-file-name (format "models/marts/%s.sql" name) root))
            (revert-buffer)
            (should (member name (mapcar (lambda (entry) (aref (cadr entry) 3)) tabulated-list-entries))))
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

(ert-deftest dbt-mode-copies-relations-from-the-manifest ()
  "The relation of the model, or of the ref or source at point, comes from the manifest."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-copy-relation) :type 'user-error)
    (make-directory (expand-file-name "target" root))
    (write-region (concat "{\"nodes\": {\"model.shop.orders\": {\"relation_name\": \"\\\"db\\\".\\\"main\\\".\\\"orders\\\"\"},"
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

(ert-deftest dbt-mode-copies-the-build-command ()
  "The build command selects the model, or with a prefix its family."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-copy-build-command)
    (should (equal (current-kill 0) "dbt build -s orders"))
    (dbt-copy-build-command t)
    (should (equal (current-kill 0) "dbt build -s +orders+"))))

(ert-deftest dbt-mode-lists-the-tests-of-a-model ()
  "The tests of a model are its YAML entries and the singular tests that read it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (let* (shown
           (xref-show-xrefs-function (lambda (fetcher _alist) (setq shown (funcall fetcher)))))
      (dbt-visit-tests)
      (should (equal (sort (mapcar #'car (dbt-test-xref-files shown root)) #'string<)
                     '("models/staging/_staging.yml" "tests/assert_positive_amount.sql"))))
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
  "The keys that the README lists run their commands."
  (dolist (binding '(("C-c /" . dbt-close-block) ("C-c %" . dbt-jump-to-matching-tag) ("C-c C-t" . dbt-visit-tests)
                     ("C-c C-w" . dbt-copy-relation) ("C-c M-w" . dbt-copy-build-command) ("C-c C-n" . dbt-new)))
    (should (eq (keymap-lookup dbt-mode-map (car binding)) (cdr binding)))))

(provide 'dbt-mode-test)
;;; dbt-mode-test.el ends here
