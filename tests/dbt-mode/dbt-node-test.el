;;; dbt-node-test.el --- Tests of the buffer of a node  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What C-c C-o shows of a node from the user's manifest, and what its
;; keys do.  Through a server, see dbt-lsp-test.el.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)

(defun dbt-test-write-manifest (root)
  "Write in ROOT's target a manifest of stg_orders, raw_orders, the table raw.raw_orders and a disabled model off.
Its shape is dbt 2's: stg_orders with its files, a relation, a
configuration, columns, what it depends on and a description; off under
\"disabled\", the first of an array."
  (make-directory (expand-file-name "target" root) t)
  (write-region
   (json-encode
    `((nodes . ((model.shop.stg_orders
                 . ((unique_id . "model.shop.stg_orders") (name . "stg_orders") (resource_type . "model")
                    (package_name . "shop") (relation_name . "\"shop\".\"main\".\"stg_orders\"")
                    (original_file_path . "models/staging/stg_orders.sql")
                    (patch_path . "shop://models/staging/_staging.yml")
                    (config . ((materialized . "view") (tags . ["daily" "finance"]) (enabled . t)
                               (meta . ((owner . "data team"))) (unique_key . "order_id")
                               (access . "protected") (contract . ((enforced . :json-false)))))
                    (columns . ((order_id . ((name . "order_id") (data_type . "integer")
                                             (description . "One per order.")))
                                (amount . ((name . "amount") (data_type) (description . "")))))
                    (depends_on . ((nodes . ["seed.shop.raw_orders"]) (macros . ["macro.shop.cents_to_dollars"])))
                    (description . "The orders, cleaned.")))
                (seed.shop.raw_orders
                 . ((unique_id . "seed.shop.raw_orders") (name . "raw_orders") (resource_type . "seed")
                    (package_name . "shop")))))
      (sources . ((source.shop.raw.raw_orders
                   . ((unique_id . "source.shop.raw.raw_orders") (name . "raw_orders") (resource_type . "source")
                      (package_name . "shop") (relation_name . "\"shop\".\"main\".\"raw_orders\"")))))
      (disabled . ((model.shop.off . [((unique_id . "model.shop.off") (name . "off") (resource_type . "model")
                                       (package_name . "shop") (config . ((enabled . :json-false))))])))))
   nil (expand-file-name "target/manifest.json" root))
  (write-region "{{ config(enabled=false) }}\nselect 1 as id\n" nil (expand-file-name "models/marts/off.sql" root)))

(defun dbt-test-node-lines ()
  "Return the lines of the current buffer, without properties."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(ert-deftest dbt-node-shows-a-node-from-the-manifest ()
  "C-c C-o on a ref shows its node from the manifest: files, configuration, columns, tests, what it depends on, what uses it, its description.
The heading names the node, its kind and package, and the manifest it
comes from, with when dbt wrote it.  A value that dbt leaves empty, null
or false shows nothing.  The tests are left to dbt-lsp-plus.  What uses
it is what dbt's child_map names, but for tests: an analysis, a model, a
snapshot.  C-c C-o again, and g, show the node once.  RET visits its
file, its YAML entry, what it depends on and what uses it."
  (dbt-test-with-project root
    (dbt-test-write-manifest root)
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (dbt-test-goto "stg_orders')")
      (execute-kbd-macro (kbd "C-c C-o"))
      (with-current-buffer "*dbt node: model.shop.stg_orders*"
        (let ((lines (dbt-test-node-lines)))
          (should (equal (car lines) "stg_orders, a model of shop"))
          (should (string-match-p "\\`from target/manifest\\.json, written at [0-9-]+ [0-9:]+\\'" (nth 1 lines)))
          (should (equal (nthcdr 2 lines)
                         '(""
                           "Files"
                           "  file          models/staging/stg_orders.sql"
                           "  properties    models/staging/_staging.yml"
                           "Configuration"
                           "  relation      \"shop\".\"main\".\"stg_orders\""
                           "  owner         data team"
                           "  materialized  view"
                           "  unique key    order_id"
                           "  tags          daily, finance"
                           "  access        protected"
                           "Columns"
                           "  order_id  integer  One per order."
                           "  amount"
                           "Tests"
                           "  known to dbt-lsp-plus only: C-c C-t visits the YAML that holds them"
                           "Depends on"
                           "  seed      raw_orders"
                           "Used by"
                           "  analysis  order_counts"
                           "  model     orders"
                           "  snapshot  orders_snapshot"
                           "Description"
                           "  The orders, cleaned."
                           ""))))
        (switch-to-buffer (current-buffer))
        (execute-kbd-macro (kbd "g"))
        (should (equal (seq-count (lambda (line) (equal line "Configuration")) (dbt-test-node-lines)) 1))
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        (execute-kbd-macro (kbd "C-c C-o"))
        (switch-to-buffer "*dbt node: model.shop.stg_orders*")
        (should (equal (seq-count (lambda (line) (equal line "Configuration")) (dbt-test-node-lines)) 1))
        (pcase-dolist (`(,line ,file) '(("file          models" "models/staging/stg_orders.sql")
                                        ("properties    models" "models/staging/_staging.yml")
                                        ("seed      raw_orders" "seeds/raw_orders.csv")
                                        ("model     orders" "models/marts/orders.sql")))
          (switch-to-buffer "*dbt node: model.shop.stg_orders*")
          (goto-char (point-min))
          (search-forward line)
          (execute-kbd-macro (kbd "RET"))
          (should (equal (file-relative-name buffer-file-name root) file)))
        (kill-buffer "*dbt node: model.shop.stg_orders*")))))

(ert-deftest dbt-node-shows-a-source-a-disabled-node-and-says-what-is-missing ()
  "C-c C-o shows a disabled model as disabled, and a source table as the lineage names it, each in a buffer of its own unique id.
Without a manifest, or without the node in it, it says so."
  (dbt-test-with-project root
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "C-c C-o")) :type 'user-error))
                     "No readable target/manifest.json: run dbt parse first"))
      (dbt-test-write-manifest root)
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "C-c C-o")) :type 'user-error))
                     "No node model.shop.orders in target/manifest.json: run dbt parse again"))
      (switch-to-buffer (dbt-test-visit root "models/marts/off.sql"))
      (execute-kbd-macro (kbd "C-c C-o"))
      (with-current-buffer "*dbt node: model.shop.off*"
        (should (member "  disabled      yes" (dbt-test-node-lines)))
        (kill-buffer))
      (write-region "select * from {{ source('raw', 'raw_orders') }}\n" nil
                    (expand-file-name "models/staging/from_raw.sql" root))
      (switch-to-buffer (dbt-test-visit root "models/staging/from_raw.sql"))
      (dbt-test-goto "raw_orders'")
      (execute-kbd-macro (kbd "C-c C-o"))
      (with-current-buffer "*dbt node: source.shop.raw.raw_orders*"
        (should (equal (car (dbt-test-node-lines)) "raw.raw_orders, a source of shop"))
        (kill-buffer)))))

(ert-deftest dbt-node-gives-eldoc-the-relation-from-a-manifest-held ()
  "Without a server, eldoc on a ref says the materialization and the relation of what it names.
Only once dbt-mode holds the project's manifest as it is, never reading
it at idle time; nothing elsewhere than on a ref or a source."
  (dbt-test-with-project root
    (dbt-test-write-manifest root)
    (when (get-buffer " *dbt manifest*")
      (kill-buffer " *dbt manifest*"))
    (let ((said nil)
          (manifest (expand-file-name "target/manifest.json" root)))
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (dbt-test-goto "stg_orders')")
      (run-hook-with-args-until-success 'eldoc-documentation-functions (lambda (text &rest _) (setq said text)))
      (should-not said)
      (should-not (get-buffer " *dbt manifest*"))
      (dbt-project-manifest-node manifest "model.shop.stg_orders")
      (run-hook-with-args-until-success 'eldoc-documentation-functions (lambda (text &rest _) (setq said text)))
      (should (equal said "view \"shop\".\"main\".\"stg_orders\""))
      (setq said nil)
      (goto-char (point-min))
      (run-hook-with-args-until-success 'eldoc-documentation-functions (lambda (text &rest _) (setq said text)))
      (should-not said)
      ;; A manifest that dbt wrote again is not held any more.
      (dbt-test-goto "stg_orders')")
      (set-file-times manifest (time-add (current-time) 10))
      (run-hook-with-args-until-success 'eldoc-documentation-functions (lambda (text &rest _) (setq said text)))
      (should-not said))))

(provide 'dbt-node-test)
;;; dbt-node-test.el ends here
