;;; dbt-lineage-test.el --- Tests of the lineage of dbt-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The depths of the upstream and downstream of a node, and their table.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-lineage)
(require 'dbt-mode)

(defconst dbt-test-diamond
  '((:id "a" :name "a" :type seed :parents nil)
    (:id "b" :name "b" :type model :parents ("a"))
    (:id "c" :name "c" :type model :parents ("a" "b"))
    (:id "d" :name "d" :type model :parents ("b" "c"))
    (:id "e" :name "e" :type model :parents ("e" "d")))
  "A diamond with a shortcut, and a node that depends on itself.")

(ert-deftest dbt-lineage-depths-are-shortest-paths ()
  "Each node comes once, at the length of its shortest path."
  (should (equal (dbt-lineage-depths dbt-test-diamond "d" 'upstream) '(("b" . 1) ("c" . 1) ("a" . 2))))
  (should (equal (dbt-lineage-depths dbt-test-diamond "a" 'downstream)
                 '(("b" . 1) ("c" . 1) ("d" . 2) ("e" . 3))))
  (should (equal (dbt-lineage-depths dbt-test-diamond "e" 'downstream) nil)))

(ert-deftest dbt-lineage-shows-the-project-lineage ()
  "C-c C-g without a server lists upstream and downstream rows."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-lineage "stg_orders")
    (with-current-buffer "*dbt lineage: stg_orders*"
      (should (equal (mapcar (lambda (entry) (append (cadr entry) nil)) tabulated-list-entries)
                     '(("upstream" "1" "seed" "raw_orders" "seeds/raw_orders.csv")
                       ("downstream" "1" "analysis" "order_counts" "analyses/order_counts.sql")
                       ("downstream" "1" "model" "orders" "models/marts/orders.sql")
                       ("downstream" "1" "snapshot" "orders_snapshot" "snapshots/orders_snapshot.sql")
                       ("downstream" "2" "model" "customers" "models/marts/customers.sql"))))
      (goto-char (point-min))
      (dbt-lineage-visit)
      (should (equal (file-relative-name buffer-file-name root) "seeds/raw_orders.csv"))
      (kill-buffer "*dbt lineage: stg_orders*"))))

(provide 'dbt-lineage-test)
;;; dbt-lineage-test.el ends here
