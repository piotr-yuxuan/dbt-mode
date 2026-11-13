;;; dbt-project-test.el --- Tests of the project model of dbt-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What dbt-project.el reads of a project.  The expectations come from
;; the files of the fixture, tests/fixtures/shop, and from dbt's
;; documented defaults and YAML layout.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-project)

(ert-deftest dbt-project-defines-the-preview-limit ()
  "dbt-project.el alone defines the rows of a preview, 50, in the group dbt."
  (with-temp-buffer
    (should (zerop (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                 "-Q" "--batch" "-L" (file-name-directory (locate-library "dbt-project"))
                                 "-l" "dbt-project"
                                 "--eval" "(prin1 (list dbt-preview-limit (assq 'dbt-preview-limit (get 'dbt 'custom-group))))")))
    (should (equal (car (read-from-string (buffer-string))) '(50 (dbt-preview-limit custom-variable))))))

(ert-deftest dbt-project-yaml-line-reads-keys ()
  "A key's column follows the dash of a list item; quotes and comments go."
  (should (equal (dbt-project-yaml-line "  - name: \"events\"  # a comment")
                 '(:column 4 :key "name" :item t :value "events")))
  (should (equal (dbt-project-yaml-line "version: 2") '(:column 0 :key "version" :item nil :value "2")))
  (should (equal (dbt-project-yaml-line "    tables:") '(:column 4 :key "tables" :item nil :value nil)))
  (should (equal (dbt-project-yaml-line "- models  # sql models") '(:column 2 :key nil :item t :value "models")))
  (should-not (dbt-project-yaml-line "# models:"))
  (should-not (dbt-project-yaml-line "   ")))

(ert-deftest dbt-project-yaml-line-reads-the-name-of-a-flow-mapping ()
  "An item written as a flow mapping gives its name, wherever it stands."
  (should (equal (dbt-project-yaml-line "      - {name: accounts}")
                 '(:column 8 :key "name" :item t :value "accounts")))
  (should (equal (dbt-project-yaml-line "      - {identifier: tbl_contacts, name: 'contacts'}")
                 '(:column 8 :key "name" :item t :value "contacts"))))

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
                 '((:section "models" :line 4 :name "first")
                   (:section "models" :line 7 :name "second")
                   (:section "sources" :line 11 :name "raw")
                   (:section "tables" :line 13 :source "raw" :name "orders")
                   (:section "tables" :line 16 :source "raw" :name "customers")
                   (:section "sources" :line 17 :name "events"))))
  (should-not (dbt-project-yaml-entries "models:\n  shop:\n    +materialized: view\n"))
  (should (equal (dbt-project-yaml-entries
                  "sources:\n  - name: crm\n    tables:\n      - {name: accounts}\n      - {name: contacts, identifier: c}\n")
                 '((:section "sources" :line 2 :name "crm")
                   (:section "tables" :line 4 :source "crm" :name "accounts")
                   (:section "tables" :line 5 :source "crm" :name "contacts")))))

(ert-deftest dbt-project-call-regexp-reads-calls ()
  "A complete call gives its name and qualifier; a call being typed, what is typed."
  (let ((match (lambda (regexp text)
                 (and (string-match regexp text) (list (match-string 1 text) (match-string 2 text)))))
        (typed (lambda (regexp text)
                 (with-temp-buffer
                   (insert text)
                   (and (looking-back regexp nil) (list (match-string 1) (match-string 2)))))))
    (should (equal (funcall match (dbt-project-call-regexp 'ref 'optional) "{{ ref( 'orders' ) }}") '("orders" nil)))
    (should (equal (funcall match (dbt-project-call-regexp 'ref 'optional) "ref(\"shop_utils\",\n 'orders')")
                   '("orders" "shop_utils")))
    (should (equal (funcall match (dbt-project-call-regexp 'source 'required) "source('raw', 'raw_orders')")
                   '("raw_orders" "raw")))
    (should-not (funcall match (dbt-project-call-regexp 'source 'required) "source('raw')"))
    (should-not (funcall match (dbt-project-call-regexp 'ref 'optional) "preferred('orders')"))
    (should (equal (funcall typed (dbt-project-call-regexp 'ref nil t) "{{ ref('ord") '("ord" nil)))
    (should (equal (funcall typed (dbt-project-call-regexp 'ref nil t) "{{ ref(\"") '("" nil)))
    (should (equal (funcall typed (dbt-project-call-regexp 'ref 'required t) "ref('shop_utils', 'ord")
                   '("ord" "shop_utils")))
    (should-not (funcall typed (dbt-project-call-regexp 'ref 'required t) "ref('ord"))
    (should (equal (funcall typed (dbt-project-call-regexp 'source nil t) "source('ra") '("ra" nil)))
    (should (equal (alist-get 'source dbt-project-call-regexps) (dbt-project-call-regexp 'source 'required)))))

(ert-deftest dbt-project-call-regexp-reads-the-version-of-a-ref ()
  "A complete ref gives in group 3 the version that v= or version= names."
  (let ((match (lambda (text)
                 (and (string-match (dbt-project-call-regexp 'ref 'optional) text)
                      (list (match-string 1 text) (match-string 2 text) (match-string 3 text))))))
    (should (equal (funcall match "{{ ref('dim_customers', v=1) }}") '("dim_customers" nil "1")))
    (should (equal (funcall match "ref('shop', 'dim_customers', version = '2')") '("dim_customers" "shop" "2")))
    (should (equal (funcall match "ref('dim_customers')") '("dim_customers" nil nil)))))

(ert-deftest dbt-project-yaml-entries-keep-the-relation-of-a-snapshot ()
  "A snapshot written in YAML keeps its relation, before or after its name."
  (should (equal (dbt-project-yaml-entries
                  (string-join '("snapshots:"
                                 "  - name: first"
                                 "    relation: ref('stg_customers')  # the model"
                                 "    config:"
                                 "      relation: not_this_one"
                                 "  - relation: \"source('raw', 'raw_orders')\""
                                 "    name: second")
                               "\n"))
                 '((:section "snapshots" :line 2 :name "first" :relation "ref('stg_customers')")
                   (:section "snapshots" :line 7 :relation "source('raw', 'raw_orders')" :name "second")))))

(ert-deftest dbt-project-yaml-entries-keep-the-versions-of-a-model ()
  "Each version of a model is an entry named after the model."
  (should (equal (dbt-project-yaml-entries
                  (string-join '("models:"
                                 "  - name: dim_customers"
                                 "    latest_version: 2"
                                 "    versions:"
                                 "      - v: 1"
                                 "      - v: 2"
                                 "        defined_in: dim_customers_current"
                                 "        columns:"
                                 "          - name: id")
                               "\n"))
                 '((:section "models" :line 2 :name "dim_customers" :latest-version "2")
                   (:section "versions" :line 5 :name "dim_customers" :version "1")
                   (:section "versions" :line 6 :name "dim_customers" :version "2"
                             :defined-in "dim_customers_current")))))

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

(ert-deftest dbt-project-config-reads-block-lists-as-yaml-does ()
  "Items of a path list may carry comments, sit at column 0 or among blank lines.
An empty list is empty, as dbt keeps its default for a missing key only."
  (dbt-test-with-project root
    (with-temp-file (expand-file-name "dbt_project.yml" root)
      (insert "name: shop\nmodel-paths:\n  - models  # sql models\n\n  # more below\n  - \"more\"\n"
              "seed-paths:\n- data\n- seeds\nanalysis-paths: []\nvars:\n  model-paths: not_a_key\n"))
    (let ((config (dbt-project-config root)))
      (should (equal (plist-get config :model-paths) '("models" "more")))
      (should (equal (plist-get config :seed-paths) '("data" "seeds")))
      (should (equal (plist-get config :analysis-paths) nil)))
    (with-temp-file (expand-file-name "dbt_project.yml" root)
      (insert "name: shop\nmodel-paths:\n  - models  # sql models\n"))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(model) "orders"))))

(ert-deftest dbt-project-root-of-a-package-is-the-project-using-it ()
  "A package's file belongs to the project that installed or declared it."
  (dbt-test-with-project root
    (should (equal (dbt-project-root (expand-file-name "dbt_packages/shop_utils/macros/is_positive.sql" root))
                   root))
    (should (equal (dbt-project-root (expand-file-name "models/marts/" root)) root))
    (should-not (dbt-project-root temporary-file-directory))
    (dolist (directory '("dbt_internal_packages/dbt_adapters/" "local_packages/vocab/"))
      (make-directory (expand-file-name directory root) t)
      (write-region "name: inner\n" nil (expand-file-name (concat directory "dbt_project.yml") root)))
    (write-region "packages:\n  - local: local_packages/vocab\n" nil (expand-file-name "packages.yml" root))
    (dbt-project-forget root)
    (dolist (directory '("dbt_internal_packages/dbt_adapters/" "local_packages/vocab/"))
      (should (equal (dbt-project-root (expand-file-name (concat directory "macros/m.sql") root)) root)))))

(ert-deftest dbt-project-root-of-a-nested-project-is-its-own ()
  "A project nested in another but not one of its packages is a project apart."
  (dbt-test-with-project root
    (let ((tests (expand-file-name "integration_tests/" root)))
      (make-directory (expand-file-name "models" tests) t)
      (write-region "name: shop_tests\n" nil (expand-file-name "dbt_project.yml" tests))
      (write-region "packages:\n  - local: ../\n" nil (expand-file-name "packages.yml" tests))
      (write-region "select 1" nil (expand-file-name "models/test_base.sql" tests))
      (write-region "select * from {{ ref('test_base') }}" nil (expand-file-name "models/test_uses.sql" tests))
      (should (equal (dbt-project-root (expand-file-name "models/test_uses.sql" tests)) tests))
      (should (equal (plist-get (car (dbt-project-resolve tests '(:kind ref :name "test_base"))) :file)
                     (expand-file-name "models/test_base.sql" tests)))
      (should (equal (dbt-project-root (expand-file-name "models/marts/orders.sql" root)) root)))))

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

(ert-deftest dbt-project-index-finds-packages-at-their-install-path ()
  "Packages installed under packages-install-path are the project's packages."
  (dbt-test-with-project root
    (rename-file (expand-file-name "dbt_packages" root) (expand-file-name "dbt_modules" root))
    (with-temp-file (expand-file-name "dbt_project.yml" root)
      (insert "name: shop\npackages-install-path: dbt_modules\n"))
    (should (equal (mapcar #'car (dbt-project-packages root)) '("shop" "shop_utils")))
    (should (equal (dbt-test-entity root 'macro "is_positive")
                   '(macro "is_positive" "shop_utils" "dbt_modules/shop_utils/macros/is_positive.sql" 1)))))

(ert-deftest dbt-project-index-names-a-file-it-cannot-read ()
  "A dangling link or an unreadable file is a model by its name, and breaks nothing.
A package whose dbt_project.yml cannot be read is named after its directory."
  (dbt-test-with-project root
    (make-symbolic-link (expand-file-name "missing/shared_model.sql" root)
                        (expand-file-name "models/shared_model.sql" root))
    (write-region "select 1" nil (expand-file-name "models/secret.sql" root))
    (set-file-modes (expand-file-name "models/secret.sql" root) 0)
    (set-file-modes (expand-file-name "dbt_packages/shop_utils/dbt_project.yml" root) 0)
    (should (equal (dbt-test-entity root 'model "shared_model")
                   '(model "shared_model" "shop" "models/shared_model.sql" 1)))
    (should (equal (dbt-test-entity root 'model "secret") '(model "secret" "shop" "models/secret.sql" 1)))
    (should (dbt-project-entities root '(macro) "cents_to_dollars"))
    (should (dbt-project-entities root '(macro) "is_positive" "shop_utils"))
    (should (seq-find (lambda (node) (equal (plist-get node :id) "model.shop.secret")) (dbt-project-graph root)))))

(ert-deftest dbt-project-index-sees-saved-files-once-forgotten ()
  "A new model shows once the project is forgotten, as saving a buffer does."
  (dbt-test-with-project root
    (should-not (dbt-project-entities root '(model) "new_model"))
    (write-region "select 1" nil (expand-file-name "models/new_model.sql" root))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(model) "new_model"))))

(ert-deftest dbt-project-cached-keeps-a-slow-value-for-its-lifetime ()
  "A value is fresh for the lifetime after it is ready, however long it took."
  (let* ((dbt-project-index-lifetime 0.5)
         (computed 0)
         (slow (lambda (_root) (setq computed (1+ computed)) (sleep-for 0.6) computed)))
    (unwind-protect
        (progn
          (should (= (dbt-project-cached "/slow/" 'value slow) 1))
          (should (= (dbt-project-cached "/slow/" 'value slow) 1)))
      (dbt-project-forget "/slow/"))))

(ert-deftest dbt-project-index-reads-again-only-the-files-that-changed ()
  "Once forgotten, the index reads the files that changed, and those alone.
A file rewritten with its size, time and mode kept still gives what it held."
  (dbt-test-with-project root
    (should (dbt-project-entities root '(macro) "cents_to_dollars"))
    (let* ((file (expand-file-name "macros/cents_to_dollars.sql" root))
           (time (file-attribute-modification-time (file-attributes file)))
           (text (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (write-region (string-replace "cents_to_dollars" "cents_to_dollarz" text) nil file)
      (set-file-times file time))
    (write-region "{% macro renamed(x) %}{{ x }}{% endmacro %}" nil
                  (expand-file-name "dbt_packages/shop_utils/macros/is_positive.sql" root))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(macro) "cents_to_dollars"))
    (should-not (dbt-project-entities root '(macro) "cents_to_dollarz"))
    (should (dbt-project-entities root '(macro) "renamed"))
    (should-not (dbt-project-entities root '(macro) "is_positive"))))

(ert-deftest dbt-project-index-finds-snapshots-written-in-yaml ()
  "A snapshot of a YAML file is a snapshot, whose parents its relation names."
  (dbt-test-with-project root
    (write-region (concat "snapshots:\n  - name: customers_snapshot\n    relation: ref('stg_customers')\n"
                          "    config:\n      unique_key: customer_id\n      strategy: check\n      check_cols: all\n")
                  nil (expand-file-name "snapshots/_snapshots.yml" root))
    (write-region "select * from {{ ref('customers_snapshot') }}" nil
                  (expand-file-name "models/marts/from_snap.sql" root))
    (should (equal (dbt-test-entity root 'snapshot "customers_snapshot")
                   '(snapshot "customers_snapshot" "shop" "snapshots/_snapshots.yml" 2)))
    (should (equal (plist-get (car (dbt-project-resolve root '(:kind ref :name "customers_snapshot"))) :kind)
                   'snapshot))
    (let ((parents (lambda (id) (plist-get (seq-find (lambda (node) (equal (plist-get node :id) id))
                                                     (dbt-project-graph root))
                                           :parents))))
      (should (equal (funcall parents "model.shop.from_snap") '("snapshot.shop.customers_snapshot")))
      (should (equal (funcall parents "snapshot.shop.customers_snapshot") '("model.shop.stg_customers"))))
    (should-not (dbt-project-file-entity root (expand-file-name "snapshots/_snapshots.yml" root)))))

(ert-deftest dbt-project-index-reads-a-file-that-becomes-readable ()
  "A file unreadable when indexed is read once its mode lets it be."
  (dbt-test-with-project root
    (let ((file (expand-file-name "macros/secret.sql" root)))
      (write-region "{% macro secret_macro() %}1{% endmacro %}\n" nil file)
      (set-file-modes file #o000)
      (unwind-protect
          (progn
            (should-not (dbt-project-entities root '(macro) "secret_macro"))
            (set-file-modes file #o644)
            (dbt-project-forget root)
            (should (dbt-project-entities root '(macro) "secret_macro")))
        (set-file-modes file #o644)))))

(ert-deftest dbt-project-index-skips-a-directory-it-cannot-read ()
  "A directory that cannot be read holds nothing, and stops nothing."
  (dbt-test-with-project root
    (let ((private (expand-file-name "models/private" root)))
      (make-directory private)
      (set-file-modes private #o000)
      (unwind-protect
          (should (dbt-project-entities root '(model) "orders"))
        (set-file-modes private #o755)))))

(ert-deftest dbt-project-index-skips-python-environments ()
  "A virtual environment or a conda environment in the project is not read."
  (dbt-test-with-project root
    (dolist (environment '(("dbt-env" . "pyvenv.cfg") ("conda" . "conda-meta/history")))
      (let ((directory (expand-file-name (car environment) root)))
        (make-directory (expand-file-name "lib" directory) t)
        (make-directory (file-name-directory (expand-file-name (cdr environment) directory)) t)
        (write-region "" nil (expand-file-name (cdr environment) directory))
        (write-region (format "{%% docs leaked_%s %%}x{%% enddocs %%}\n" (car environment)) nil
                      (expand-file-name "lib/README.md" directory))))
    (should-not (dbt-project-entities root '(doc) "leaked_dbt-env"))
    (should-not (dbt-project-entities root '(doc) "leaked_conda"))
    (should (dbt-project-entities root '(doc) "order_status"))))

(ert-deftest dbt-project-file-entity-takes-the-nearest-package ()
  "In integration_tests that installs its parent with local: ../, a new model is its own."
  (dbt-test-with-project outer
    (let ((root (expand-file-name "integration_tests/" outer)))
      (make-directory (expand-file-name "models" root) t)
      (write-region "name: 'shop_tests'\nconfig-version: 2\n" nil (expand-file-name "dbt_project.yml" root))
      (write-region "packages:\n  - local: ../\n" nil (expand-file-name "packages.yml" root))
      (should (equal (dbt-project-root (expand-file-name "models/new.sql" root)) root))
      (let ((entity (dbt-project-file-entity root (expand-file-name "models/new.sql" root))))
        (should (equal (plist-get entity :kind) 'model))
        (should (equal (plist-get entity :package) "shop_tests"))))))

(ert-deftest dbt-project-index-names-versioned-models-after-the-model ()
  "A version's file is the model with that version; a ref without one means the latest."
  (dbt-test-with-project root
    (dolist (file '("dim_customers_v1" "dim_customers_v2" "dim_customers_current" "fct_v1" "fct_v10"))
      (write-region "select 1" nil (expand-file-name (format "models/marts/%s.sql" file) root)))
    (write-region (concat "models:\n  - name: dim_customers\n    latest_version: 2\n    versions:\n"
                          "      - v: 1\n      - v: 2\n      - v: 3\n        defined_in: dim_customers_current\n"
                          "  - name: fct\n    versions:\n      - v: 1\n      - v: 10\n")
                  nil (expand-file-name "models/marts/_dim.yml" root))
    (write-region "select * from {{ ref('dim_customers') }} join {{ ref('dim_customers', v=1) }} join {{ ref('fct') }}"
                  nil (expand-file-name "models/marts/uses_dim.sql" root))
    (let ((file (lambda (target)
                  (file-relative-name (plist-get (car (dbt-project-resolve root target)) :file) root))))
      (should (equal (funcall file '(:kind ref :name "dim_customers")) "models/marts/dim_customers_v2.sql"))
      (should (equal (funcall file '(:kind ref :name "dim_customers" :version "1"))
                     "models/marts/dim_customers_v1.sql"))
      (should (equal (funcall file '(:kind ref :name "dim_customers" :version "3"))
                     "models/marts/dim_customers_current.sql"))
      (should (equal (funcall file '(:kind ref :name "fct")) "models/marts/fct_v10.sql"))
      (should (equal (funcall file '(:kind ref :name "orders" :version "1")) "models/marts/orders.sql")))
    (should-not (dbt-project-entities root '(model) "dim_customers_v1"))
    (should (equal (plist-get (seq-find (lambda (node) (equal (plist-get node :id) "model.shop.uses_dim"))
                                        (dbt-project-graph root))
                              :parents)
                   '("model.shop.dim_customers.v2" "model.shop.dim_customers.v1" "model.shop.fct.v10")))
    (let ((entity (dbt-project-file-entity root (expand-file-name "models/marts/dim_customers_v1.sql" root))))
      (should (equal (list (plist-get entity :name) (plist-get entity :version)) '("dim_customers" "1"))))))

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
    (should (equal (mapcar (lambda (entity) (file-relative-name (plist-get entity :file) root))
                           (dbt-project-resolve root '(:kind analysis :name "order_counts")))
                   '("analyses/order_counts.sql")))
    (should-not (dbt-project-resolve root '(:kind ref :name "order_counts")))
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

(ert-deftest dbt-project-leaves-jinja-comments-out ()
  "A macro or a call within {# #} is not there; lines after a comment keep their number."
  (dbt-test-with-project root
    (write-region "{# {% macro hidden(x) %}{% endmacro %}\n\n#}\n{% macro after(x) %}{{ x }}{% endmacro %}\n" nil
                  (expand-file-name "macros/commented.sql" root))
    (write-region "{# {{ ref('stg_customers') }} #}\nselect * from {{ ref('stg_orders') }}" nil
                  (expand-file-name "models/commented.sql" root))
    (should-not (dbt-project-entities root '(macro) "hidden"))
    (should (equal (dbt-test-entity root 'macro "after") '(macro "after" "shop" "macros/commented.sql" 4)))
    (should (equal (plist-get (seq-find (lambda (node) (equal (plist-get node :id) "model.shop.commented"))
                                        (dbt-project-graph root))
                              :parents)
                   '("model.shop.stg_orders")))))

(ert-deftest dbt-project-graph-grows-with-the-calls ()
  "The graph costs about the same per call however large the project.
Eight times the models, each with three calls, take about eight times
as long, not sixty-four."
  (let ((seconds
         (mapcar
          (lambda (models)
            (dbt-test-with-project root
              (dotimes (n models)
                (write-region (format "select * from {{ ref('m%d') }}, {{ ref('m%d') }}, {{ source('raw', 'raw_orders') }}"
                                      (/ n 2) (/ n 3))
                              nil (expand-file-name (format "models/m%d.sql" n) root) nil 'silent))
              (dbt-project-index root)
              (let ((start (float-time)))
                (dbt-project-graph root)
                (- (float-time) start))))
          '(400 3200))))
    (should (< (nth 1 seconds) (+ 0.1 (* 16 (nth 0 seconds)))))))

(ert-deftest dbt-project-reads-the-manifest-again-once-it-changes ()
  "A node of the manifest is read from its text as it is since dbt last wrote it."
  (dbt-test-with-project root
    (let ((manifest (expand-file-name "target/manifest.json" root)))
      (make-directory (file-name-directory manifest))
      (write-region "{\"nodes\": {\"model.shop.orders\": {\"n\": 1}}}" nil manifest)
      (should (equal (gethash "n" (dbt-project-manifest-node manifest "model.shop.orders")) 1))
      (write-region "{\"nodes\": {\"model.shop.orders\": {\"n\": 2}}}" nil manifest)
      (set-file-times manifest (time-add (current-time) 10))
      (should (equal (gethash "n" (dbt-project-manifest-node manifest "model.shop.orders")) 2))
      ;; Kept while the file keeps its time and its size.
      (let ((time (file-attribute-modification-time (file-attributes manifest))))
        (write-region "{\"nodes\": {\"model.shop.orders\": {\"n\": 3}}}" nil manifest)
        (set-file-times manifest time))
      (should (equal (gethash "n" (dbt-project-manifest-node manifest "model.shop.orders")) 2))
      ;; Read through a link, by the file it names.
      (let ((link (expand-file-name "target/link.json" root)))
        (make-symbolic-link manifest link)
        (should (equal (gethash "n" (dbt-project-manifest-node link "model.shop.orders")) 2))
        (write-region "{\"nodes\": {\"model.shop.orders\": {\"n\": 44}}}" nil manifest)
        (set-file-times manifest (time-add (current-time) 20))
        (should (equal (gethash "n" (dbt-project-manifest-node link "model.shop.orders")) 44)))
      (delete-file manifest)
      (should-not (dbt-project-manifest-node manifest "model.shop.orders")))))

(provide 'dbt-project-test)
;;; dbt-project-test.el ends here
