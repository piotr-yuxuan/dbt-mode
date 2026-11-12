;;; dbt-mode-test.el --- Tests of dbt-mode without a server  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user gets from dbt-mode with no dbt executable: the mode of a
;; file, Jinja's syntax and faces, xref, completion, imenu and the
;; commands that read the project.

;;; Code:

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

(ert-deftest dbt-mode-target-at-point-names-calls-and-entries ()
  "Calls of ref, source, doc and macros, and YAML entries, are targets."
  (with-temp-buffer
    (insert "{{ ref('pkg', 'm') }} {{ source('raw', 'orders') }} {{ ref('n', v=2) }} {{ pkg.mac(1) }} x\n")
    (dbt-mode)
    (dolist (case '(("'m'" 1 (:kind ref :qualifier "pkg" :name "m"))
                    ("orders" 1 (:kind source :qualifier "raw" :name "orders"))
                    ("'n'" 1 (:kind ref :qualifier nil :name "n"))
                    ("mac" 1 (:kind macro :qualifier "pkg" :name "mac"))
                    (" x" 1 nil)))
      (dbt-test-goto (car case) (nth 1 case))
      (should (equal (dbt-mode-target-at-point) (nth 2 case)))))
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/_sources.yml")
    (dbt-test-goto "raw_orders")
    (should (equal (dbt-mode-target-at-point) '(:kind source :qualifier "raw" :name "raw_orders")))
    (dbt-test-visit root "models/staging/_staging.yml")
    (dbt-test-goto "stg_orders")
    (should (equal (dbt-mode-target-at-point) '(:kind ref :qualifier nil :name "stg_orders")))
    (dbt-test-goto "order_status")
    (should (equal (dbt-mode-target-at-point) '(:kind doc :qualifier nil :name "order_status")))))

(ert-deftest dbt-mode-identifiers-round-trip ()
  "An identifier reads back as the target it was written from."
  (dolist (target '((:kind ref :qualifier nil :name "orders")
                    (:kind ref :qualifier "pkg" :name "orders")
                    (:kind source :qualifier "raw" :name "orders")
                    (:kind macro :qualifier nil :name "m")))
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

(ert-deftest dbt-mode-xref-finds-references ()
  "M-? lists the calls of a model, and its YAML entry; the calls of a macro."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (should (equal (sort (mapcar #'car (dbt-test-xref-files (xref-backend-references 'dbt "ref:stg_orders") root))
                         #'string<)
                   '("analyses/order_counts.sql" "models/marts/orders.sql" "models/staging/_staging.yml"
                     "snapshots/orders_snapshot.sql" "tests/assert_positive_amount.sql")))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "macro:cents_to_dollars") root)
                   '(("macros/cents_to_dollars.sql" . 1) ("models/staging/stg_orders.sql" . 13))))
    (should (equal (dbt-test-xref-files (xref-backend-references 'dbt "doc:order_status") root)
                   '(("models/staging/_staging.yml" . 5))))))

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
    (should-not (dbt-test-completions " }}\nselect cents"))))

(ert-deftest dbt-mode-imenu-lists-ctes ()
  "imenu lists the CTEs of a model, in order."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should (equal (mapcar #'car (funcall imenu-create-index-function)) '("orders" "customers" "final")))))

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

(ert-deftest dbt-mode-visits-the-properties-of-a-model ()
  "C-c C-y goes to the YAML entry of the model, and back to it by M-."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-visit-properties)
    (should (equal (file-relative-name buffer-file-name root) "models/staging/_staging.yml"))
    (should (equal (line-number-at-pos) 4))
    (should (equal (plist-get (dbt-mode-current-resource) :file)
                   (expand-file-name "models/staging/stg_orders.sql" root)))))

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

(provide 'dbt-mode-test)
;;; dbt-mode-test.el ends here
