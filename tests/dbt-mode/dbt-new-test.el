;;; dbt-new-test.el --- Tests of the new files and YAML entries  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The files and entries that the commands of dbt-new.el write, in the
;; layout of dbt Labs' style guide, and what the index then knows.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-new)

(defun dbt-test-file-text (root file)
  "Return the contents of FILE, relative to ROOT, as saved on disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file root))
    (buffer-string)))

(ert-deftest dbt-new-writes-a-staging-model-and-its-entry ()
  "A staging model reads its source table; its entry goes beside it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-new-staging-model "raw" "raw_orders")
    (should (equal (file-relative-name buffer-file-name root) "models/staging/raw/stg_raw__raw_orders.sql"))
    (should-not (buffer-modified-p))
    (should (string-match-p "select \\* from {{ source('raw', 'raw_orders') }}"
                            (dbt-test-file-text root "models/staging/raw/stg_raw__raw_orders.sql")))
    (should (string-prefix-p "version: 2\n\nmodels:\n  - name: stg_raw__raw_orders\n"
                             (dbt-test-file-text root "models/staging/raw/_raw__models.yml")))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(model) "stg_raw__raw_orders"))
    (should (dbt-project-entities root '(properties) "stg_raw__raw_orders"))
    (should (member "source.shop.raw.raw_orders"
                    (plist-get (seq-find (lambda (node) (equal (plist-get node :name) "stg_raw__raw_orders"))
                                         (dbt-project-graph root))
                               :parents)))
    (should-error (dbt-new-staging-model "raw" "raw_orders") :type 'user-error)
    (with-temp-buffer
      (insert (dbt-test-file-text root "models/staging/raw/_raw__models.yml"))
      (goto-char (point-min))
      (should (= 1 (how-many "stg_raw__raw_orders"))))))

(ert-deftest dbt-new-writes-models-and-macros ()
  "A model and an incremental model leave point in their ref; a macro is named."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-new-model (expand-file-name "models/marts/report.sql" root))
    (should (looking-back "ref('" (line-beginning-position)))
    (dbt-new-incremental-model (expand-file-name "models/marts/events.sql" root))
    (should (string-match-p "materialized='incremental'\\(?:.\\|\n\\)*{% if is_incremental() %}"
                            (dbt-test-file-text root "models/marts/events.sql")))
    (dbt-new-model (expand-file-name "models/marts/bare" root))
    (should (equal (file-relative-name buffer-file-name root) "models/marts/bare.sql"))
    (should-error (dbt-new-model (expand-file-name "models/marts/py_model.py" root)) :type 'user-error)
    (should-error (dbt-new-model (expand-file-name "macros/not_a_model.sql" root)) :type 'user-error)
    (should-not (file-exists-p (expand-file-name "macros/not_a_model.sql" root)))
    (dbt-new-macro "dollars")
    (should (equal (file-relative-name buffer-file-name root) "macros/dollars.sql"))
    (should (string-prefix-p "{% macro dollars(" (buffer-string)))
    (should-error (dbt-new-macro "two words") :type 'user-error)
    (dbt-project-forget root)
    (should (dbt-project-entities root '(macro) "dollars"))))

(ert-deftest dbt-new-adds-a-snapshot-in-yaml ()
  "A snapshot is a YAML entry of the snapshot path, which the index knows.
The snapshot path is made when it does not exist; a name is used once."
  (dbt-test-with-project root
    (delete-directory (expand-file-name "snapshots" root) t)
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-new-snapshot "orders_history" "ref('orders')")
    (should (string-match-p "snapshots:\n  - name: orders_history\n    relation: ref('orders')\n"
                            (dbt-test-file-text root "snapshots/_snapshots.yml")))
    (dbt-project-forget root)
    (should (dbt-project-entities root '(snapshot) "orders_history"))
    (should-error (dbt-new-snapshot "orders_history" "ref('customers')") :type 'user-error)
    (should-error (dbt-new-snapshot "../../escape" "ref('orders')") :type 'user-error)))

(ert-deftest dbt-new-refuses-names-that-are-paths ()
  "A source, a table or a name read from YAML never steers a file elsewhere."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-new-staging-model "../../../escape" "t") :type 'user-error)
    (should-error (dbt-new-staging-model "raw" "a/b") :type 'user-error)
    (should-not (file-exists-p (expand-file-name "../escape__t.sql" root)))))

(ert-deftest dbt-new-adds-the-entry-of-a-model ()
  "A model without entry gets one in its directory; one with an entry visits it."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/customers.sql")
    (dbt-add-properties)
    (should (equal (file-relative-name buffer-file-name root) "models/marts/_marts__models.yml"))
    (should (string-match-p "models:\n  - name: customers\n" (dbt-test-file-text root "models/marts/_marts__models.yml")))
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-add-properties)
    (should (equal (file-relative-name buffer-file-name root) "models/staging/_staging.yml"))
    (should (equal (line-number-at-pos) 4))))

(defun dbt-test-add-entry-to (root yaml)
  "Write YAML as models/staging/_staging.yml of ROOT, then add stg_new's entry.
Return the YAML file's text afterwards.  The buffers visiting the files
are killed first, as they would ask whether to read them again."
  (dolist (file '("models/staging/_staging.yml" "models/staging/stg_new.sql"))
    (when-let* ((buffer (get-file-buffer (expand-file-name file root))))
      (kill-buffer buffer)))
  (write-region yaml nil (expand-file-name "models/staging/_staging.yml" root))
  (write-region "select 1 as id\n" nil (expand-file-name "models/staging/stg_new.sql" root))
  (dbt-project-forget root)
  (save-window-excursion
    (dbt-test-visit root "models/staging/stg_new.sql")
    (dbt-add-properties))
  (dbt-test-file-text root "models/staging/_staging.yml"))

(ert-deftest dbt-new-appends-to-the-list-in-its-own-style ()
  "An entry joins the list as its items are written: at column 0 or 4 too.
A comment after the key is no other list."
  (dbt-test-with-project root
    (should (equal (dbt-test-add-entry-to
                    root "version: 2\n\nmodels:\n- name: stg_orders\n  columns:\n  - name: order_id\n- name: stg_customers\n")
                   (concat "version: 2\n\nmodels:\n- name: stg_orders\n  columns:\n  - name: order_id\n"
                           "- name: stg_customers\n- name: stg_new\n  description: \"\"\n")))
    (should (equal (dbt-test-add-entry-to
                    root "models:  # staging models\n    - name: stg_orders\n      description: x\n\nsources:\n  - name: s\n")
                   (concat "models:  # staging models\n    - name: stg_orders\n      description: x\n"
                           "    - name: stg_new\n      description: \"\"\n\nsources:\n  - name: s\n")))))

(ert-deftest dbt-new-adds-the-entry-under-the-section-of-its-kind ()
  "A snapshot's entry goes under snapshots; a semantic model is no model entry."
  (dbt-test-with-project root
    (dbt-test-visit root "snapshots/orders_snapshot.sql")
    (dbt-add-properties)
    (should (equal (file-relative-name buffer-file-name root) "snapshots/_snapshots__snapshots.yml"))
    (should (string-match-p "^snapshots:\n  - name: orders_snapshot\n" (buffer-string)))
    (write-region "semantic_models:\n  - name: customers\n    model: ref('customers')\n" nil
                  (expand-file-name "models/marts/_semantic.yml" root))
    (dbt-project-forget root)
    (dbt-test-visit root "models/marts/customers.sql")
    (dbt-add-properties)
    (should (string-match-p "^models:\n  - name: customers\n" (buffer-string)))))

(ert-deftest dbt-new-adds-unit-tests-with-an-input-per-parent ()
  "A unit test reads each model and source of the model; tests share a section."
  (dbt-test-with-project root
    (write-region "select * from {{ source('raw', 'raw_orders') }}\n" nil
                  (expand-file-name "models/staging/from_source.sql" root))
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-add-unit-test "test_orders")
    (let ((text (dbt-test-file-text root "models/marts/_marts__models.yml")))
      (should (string-match-p "unit_tests:\n  - name: test_orders\n    model: orders\n" text))
      (should (string-match-p "- input: ref('stg_orders')" text))
      (should (string-match-p "- input: ref('stg_customers')" text)))
    (dbt-test-visit root "models/staging/from_source.sql")
    (dbt-add-unit-test "test_from_source")
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-add-unit-test "test_stg_orders")
    (let ((text (dbt-test-file-text root "models/staging/_staging.yml")))
      (should (string-match-p "- input: source('raw', 'raw_orders')" text))
      (should (string-match-p "  - name: test_stg_orders\n" text))
      (should (= 1 (with-temp-buffer (insert text) (goto-char (point-min)) (how-many "^unit_tests:"))))
      (should (string-match-p "^models:\n" text)))))

(ert-deftest dbt-new-names-each-unit-test ()
  "The name read is test_MODEL by default; a name the file's unit tests have is refused.
A list written in flow style is refused rather than doubled."
  (dbt-test-with-project root
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (execute-kbd-macro (kbd "M-x dbt-add-unit-test RET RET")))
    (should (string-match-p "  - name: test_orders\n" (dbt-test-file-text root "models/marts/_marts__models.yml")))
    (dbt-test-visit root "models/marts/orders.sql")
    (should-error (dbt-add-unit-test "test_orders") :type 'user-error)
    (dbt-add-unit-test "test_orders_late")
    (should (= 2 (with-temp-buffer
                   (insert (dbt-test-file-text root "models/marts/_marts__models.yml"))
                   (goto-char (point-min))
                   (how-many "^  - name: test_orders"))))
    (write-region "version: 2\n\nmodels:\n  - name: stg_orders\n\nunit_tests: []\n" nil
                  (expand-file-name "models/staging/_staging.yml" root))
    (dbt-project-forget root)
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (should-error (dbt-add-unit-test "test_stg_orders") :type 'user-error)
    (should (equal (dbt-test-file-text root "models/staging/_staging.yml")
                   "version: 2\n\nmodels:\n  - name: stg_orders\n\nunit_tests: []\n"))))

(provide 'dbt-new-test)
;;; dbt-new-test.el ends here
