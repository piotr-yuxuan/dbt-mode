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
                 '((:section "models" :name "first" :line 4)
                   (:section "models" :name "second" :line 7)
                   (:section "sources" :name "raw" :line 11)
                   (:section "tables" :name "orders" :line 13 :source "raw")
                   (:section "tables" :name "customers" :line 16 :source "raw")
                   (:section "sources" :name "events" :line 17))))
  (should-not (dbt-project-yaml-entries "models:\n  shop:\n    +materialized: view\n"))
  (should (equal (dbt-project-yaml-entries
                  "sources:\n  - name: crm\n    tables:\n      - {name: accounts}\n      - {name: contacts, identifier: c}\n")
                 '((:section "sources" :name "crm" :line 2)
                   (:section "tables" :name "accounts" :line 4 :source "crm")
                   (:section "tables" :name "contacts" :line 5 :source "crm")))))

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
A file made unreadable without changing still gives what it held."
  (dbt-test-with-project root
    (should (dbt-project-entities root '(macro) "cents_to_dollars"))
    (set-file-modes (expand-file-name "macros/cents_to_dollars.sql" root) 0)
    (write-region "{% macro renamed(x) %}{{ x }}{% endmacro %}" nil
                  (expand-file-name "dbt_packages/shop_utils/macros/is_positive.sql" root))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(macro) "cents_to_dollars"))
    (should (dbt-project-entities root '(macro) "renamed"))
    (should-not (dbt-project-entities root '(macro) "is_positive"))))

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

(provide 'dbt-project-test)
;;; dbt-project-test.el ends here
