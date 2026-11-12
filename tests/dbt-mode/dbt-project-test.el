;;; dbt-project-test.el --- Tests of the project model of dbt-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What dbt-project.el reads of a project.  The expectations come from
;; the files of the fixture, tests/fixtures/shop, and from dbt's
;; documented defaults and YAML layout.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-project)

(ert-deftest dbt-project-yaml-line-reads-keys ()
  "A key's column follows the dash of a list item; quotes and comments go."
  (should (equal (dbt-project-yaml-line "  - name: \"events\"  # a comment")
                 '(:column 4 :key "name" :value "events")))
  (should (equal (dbt-project-yaml-line "version: 2") '(:column 0 :key "version" :value "2")))
  (should (equal (dbt-project-yaml-line "    tables:") '(:column 4 :key "tables" :value nil)))
  (should-not (dbt-project-yaml-line "      - unique"))
  (should-not (dbt-project-yaml-line "# models:")))

(ert-deftest dbt-project-yaml-entries-keep-sections-and-tables ()
  "Entries are the items of a section and the tables of a source, with lines."
  (should (equal (dbt-project-yaml-entries
                  (string-join
                   '("version: 2"
                     "models:"
                     "  - description: the name comes second"
                     "    name: first"
                     "    columns:"
                     "      - name: not_an_entry"
                     "  - name: second"
                     "    config:"
                     "      name: not_an_entry_either"
                     "sources:"
                     "  - name: raw"
                     "    tables:"
                     "      - name: orders"
                     "        columns:"
                     "          - name: id"
                     "      - name: 'customers'"
                     "  - name: events"
                     "unknown:"
                     "  - name: ignored")
                   "\n"))
                 '((:section "models" :name "first" :line 4)
                   (:section "models" :name "second" :line 7)
                   (:section "sources" :name "raw" :line 11)
                   (:section "tables" :name "orders" :line 13 :source "raw")
                   (:section "tables" :name "customers" :line 16 :source "raw")
                   (:section "sources" :name "events" :line 17))))
  (should-not (dbt-project-yaml-entries "models:\n  shop:\n    +materialized: view\n")))

(ert-deftest dbt-project-config-reads-paths ()
  "dbt_project.yml gives the name and paths, in either list form; dbt's defaults otherwise."
  (dbt-test-with-project root
    (let ((config (dbt-project-config root)))
      (should (equal (plist-get config :name) "shop"))
      (should (equal (plist-get config :model-paths) '("models")))
      (should (equal (plist-get config :target-path) "target")))
    (with-temp-file (expand-file-name "dbt_project.yml" root)
      (insert "name: \"other\"  # comment\nmodel-paths:\n  - models\n  - 'more_models'\n"
              "seed-paths: [\"data\", seeds]\ntarget-path: build\n"))
    (let ((config (dbt-project-config root)))
      (should (equal (plist-get config :name) "other"))
      (should (equal (plist-get config :model-paths) '("models" "more_models")))
      (should (equal (plist-get config :seed-paths) '("data" "seeds")))
      (should (equal (plist-get config :analysis-paths) '("analyses")))
      (should (equal (plist-get config :target-path) "build")))))

(ert-deftest dbt-project-root-is-the-outermost-project ()
  "A package's file belongs to the project that installed it."
  (dbt-test-with-project root
    (should (equal (dbt-project-root (expand-file-name "dbt_packages/shop_utils/macros/is_positive.sql" root))
                   root))
    (should (equal (dbt-project-root (expand-file-name "models/marts/" root)) root))
    (should-not (dbt-project-root temporary-file-directory))))

(defun dbt-test-entity (root kind name)
  "Return the entity of ROOT of KIND named NAME, as (KIND NAME PACKAGE FILE LINE)."
  (let ((entity (car (dbt-project-entities root (list kind) name))))
    (and entity (list (plist-get entity :kind) (plist-get entity :name) (plist-get entity :package)
                      (file-relative-name (plist-get entity :file) root) (plist-get entity :line)))))

(ert-deftest dbt-project-index-finds-what-the-project-defines ()
  "The index holds each kind of resource of the fixture, packages included."
  (dbt-test-with-project root
    (dolist (expected '((model "orders" "shop" "models/marts/orders.sql" 1)
                        (seed "raw_orders" "shop" "seeds/raw_orders.csv" 1)
                        (analysis "order_counts" "shop" "analyses/order_counts.sql" 1)
                        (snapshot "orders_snapshot" "shop" "snapshots/orders_snapshot.sql" 1)
                        (macro "cents_to_dollars" "shop" "macros/cents_to_dollars.sql" 1)
                        (macro "is_positive" "shop_utils" "dbt_packages/shop_utils/macros/is_positive.sql" 1)
                        (doc "order_status" "shop" "models/docs.md" 1)
                        (source "raw" "shop" "models/staging/_sources.yml" 4)
                        (table "raw_customers" "shop" "models/staging/_sources.yml" 10)
                        (properties "stg_orders" "shop" "models/staging/_staging.yml" 4)))
      (should (equal (dbt-test-entity root (car expected) (nth 1 expected)) expected)))
    (should-not (dbt-project-entities root '(model) "assert_positive_amount"))
    (should (equal (plist-get (car (dbt-project-entities root '(table) "raw_orders")) :source) "raw"))))

(ert-deftest dbt-project-index-files-local-packages-under-their-name ()
  "A local package's macro belongs to it, once, at its source, not to the project."
  (dbt-test-with-project root
    (dolist (directory '("local_packages/vocab/macros/" "dbt_packages/vocab/macros/"))
      (make-directory (expand-file-name directory root) t)
      (write-region "name: 'vocab'\n" nil (expand-file-name (concat directory "../dbt_project.yml") root))
      (write-region "{% macro normalise(x) %}lower({{ x }}){% endmacro %}" nil
                    (expand-file-name (concat directory "normalise.sql") root)))
    (write-region "packages:\n  - local: local_packages/vocab  # ours\n  - local: \"dbt_packages/shop_utils\"\n" nil
                  (expand-file-name "packages.yml" root))
    (should (equal (mapcar #'car (dbt-project-packages root)) '("shop" "vocab" "shop_utils")))
    (should (equal (mapcar (lambda (entity) (list (plist-get entity :package)
                                                  (file-relative-name (plist-get entity :file) root)))
                           (dbt-project-entities root '(macro) "normalise"))
                   '(("vocab" "local_packages/vocab/macros/normalise.sql"))))))

(ert-deftest dbt-project-index-sees-saved-files-once-forgotten ()
  "A new model shows once the project is forgotten, as saving a buffer does."
  (dbt-test-with-project root
    (should-not (dbt-project-entities root '(model) "new_model"))
    (write-region "select 1" nil (expand-file-name "models/new_model.sql" root))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(model) "new_model"))))

(ert-deftest dbt-project-file-entity-needs-no-index ()
  "A file under the model paths is a model even before it exists."
  (dbt-test-with-project root
    (should (equal (plist-get (dbt-project-file-entity root (expand-file-name "models/unsaved.sql" root)) :kind)
                   'model))
    (should (equal (plist-get (dbt-project-file-entity root (expand-file-name "snapshots/orders_snapshot.sql" root))
                              :name)
                   "orders_snapshot"))
    (should-not (dbt-project-file-entity root (expand-file-name "macros/cents_to_dollars.sql" root)))))

(ert-deftest dbt-project-resolve-prefers-the-caller-then-the-project ()
  "A ref resolves in the given package; else the caller's, then the project's."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1" nil (expand-file-name "dbt_packages/shop_utils/models/orders.sql" root))
    (let ((packages (lambda (&rest arguments)
                      (mapcar (lambda (entity) (plist-get entity :package))
                              (apply #'dbt-project-resolve root arguments)))))
      (should (equal (funcall packages '(:kind ref :name "orders")) '("shop" "shop_utils")))
      (should (equal (funcall packages '(:kind ref :name "orders") "shop_utils") '("shop_utils" "shop")))
      (should (equal (funcall packages '(:kind ref :qualifier "shop_utils" :name "orders")) '("shop_utils"))))
    (should (equal (plist-get (car (dbt-project-resolve root '(:kind source :qualifier "raw" :name "raw_orders")))
                              :line)
                   8))
    (should (equal (plist-get (car (dbt-project-resolve root '(:kind source :name "events"))) :line) 12))
    (should-not (dbt-project-resolve root '(:kind source :qualifier "events" :name "raw_orders")))
    (should (dbt-project-resolve root '(:kind macro :qualifier "shop_utils" :name "is_positive")))
    (should-not (dbt-project-resolve root '(:kind macro :qualifier "shop" :name "is_positive")))))

(ert-deftest dbt-project-graph-follows-ref-and-source ()
  "Each node depends on what its ref and source calls name."
  (dbt-test-with-project root
    (write-region "select * from {{ source('raw', 'raw_orders') }}" nil
                  (expand-file-name "models/staging/from_source.sql" root))
    (let ((parents (lambda (id) (plist-get (seq-find (lambda (node) (equal (plist-get node :id) id))
                                                     (dbt-project-graph root))
                                           :parents))))
      (should (equal (sort (funcall parents "model.shop.orders") #'string<)
                     '("model.shop.stg_customers" "model.shop.stg_orders")))
      (should (equal (funcall parents "model.shop.stg_orders") '("seed.shop.raw_orders")))
      (should (equal (funcall parents "snapshot.shop.orders_snapshot") '("model.shop.stg_orders")))
      (should (equal (funcall parents "model.shop.from_source") '("source.shop.raw.raw_orders")))
      (should (equal (funcall parents "seed.shop.raw_orders") nil)))))

(provide 'dbt-project-test)
;;; dbt-project-test.el ends here
