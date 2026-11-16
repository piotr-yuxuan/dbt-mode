;;; dbt-node-test.el --- Tests of the buffer of a node  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What C-c C-o shows of a node from the user's manifest, and what its
;; keys do.  Through a server, see dbt-lsp-test.el.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)

(defun dbt-test-write-manifest (root)
  "Write in ROOT's target a manifest of stg_orders, raw_orders and a disabled model off.
Its shape is dbt 2's: stg_orders with a relation, a configuration,
columns, what it depends on and a description; off under \"disabled\",
the first of an array."
  (make-directory (expand-file-name "target" root) t)
  (write-region
   (json-encode
    `((nodes . ((model.shop.stg_orders
                 . ((unique_id . "model.shop.stg_orders") (name . "stg_orders") (resource_type . "model")
                    (package_name . "shop") (relation_name . "\"shop\".\"main\".\"stg_orders\"")
                    (config . ((materialized . "view") (tags . ["daily" "finance"]) (enabled . t)
                               (access . "protected") (contract . ((enforced . :json-false)))))
                    (columns . ((order_id . ((name . "order_id") (data_type . "integer")
                                             (description . "One per order.")))
                                (amount . ((name . "amount") (data_type) (description . "")))))
                    (depends_on . ((nodes . ["seed.shop.raw_orders"]) (macros . ["macro.shop.cents_to_dollars"])))
                    (description . "The orders, cleaned.")))
                (seed.shop.raw_orders
                 . ((unique_id . "seed.shop.raw_orders") (name . "raw_orders") (resource_type . "seed")
                    (package_name . "shop")))))
      (disabled . ((model.shop.off . [((unique_id . "model.shop.off") (name . "off") (resource_type . "model")
                                       (package_name . "shop") (config . ((enabled . :json-false))))])))))
   nil (expand-file-name "target/manifest.json" root))
  (write-region "{{ config(enabled=false) }}\nselect 1 as id\n" nil (expand-file-name "models/marts/off.sql" root)))

(defun dbt-test-node-lines ()
  "Return the lines of the current buffer, without properties."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(ert-deftest dbt-node-shows-a-node-from-the-manifest ()
  "C-c C-o on a ref shows its node from the manifest: configuration, columns, what it depends on, what uses it, its description.
The heading names the node, its kind and package, and the manifest it
comes from, with when dbt wrote it.  A value that dbt leaves empty, null
or false shows nothing.  What uses it is what dbt's child_map names,
but for tests: an analysis, a model, a snapshot.  RET on what it
depends on, and on what uses it, visits that node."
  (dbt-test-with-project root
    (dbt-test-write-manifest root)
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (dbt-test-goto "stg_orders')")
      (execute-kbd-macro (kbd "C-c C-o"))
      (with-current-buffer "*dbt node: stg_orders*"
        (let ((lines (dbt-test-node-lines)))
          (should (equal (car lines) "stg_orders, a model of shop"))
          (should (string-match-p "\\`from target/manifest\\.json, written at [0-9-]+ [0-9:]+\\'" (nth 1 lines)))
          (should (equal (nthcdr 2 lines)
                         '(""
                           "Configuration"
                           "  relation      \"shop\".\"main\".\"stg_orders\""
                           "  materialized  view"
                           "  tags          daily, finance"
                           "  access        protected"
                           "Columns"
                           "  order_id  integer  One per order."
                           "  amount"
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
        (goto-char (point-min))
        (search-forward "seed      raw_orders")
        (execute-kbd-macro (kbd "RET"))
        (should (equal (file-relative-name buffer-file-name root) "seeds/raw_orders.csv"))
        (switch-to-buffer "*dbt node: stg_orders*")
        (goto-char (point-min))
        (search-forward "model     orders")
        (execute-kbd-macro (kbd "RET"))
        (should (equal (file-relative-name buffer-file-name root) "models/marts/orders.sql"))
        (kill-buffer "*dbt node: stg_orders*")))))

(ert-deftest dbt-node-shows-a-disabled-node-and-says-what-is-missing ()
  "C-c C-o in a disabled model shows it as disabled; without a manifest, or without the node in it, it says so."
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
      (with-current-buffer "*dbt node: off*"
        (should (member "  disabled      yes" (dbt-test-node-lines)))
        (kill-buffer)))))

(provide 'dbt-node-test)
;;; dbt-node-test.el ends here
