;;; dbt-dashboard-test.el --- Tests of the dashboard of a project  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What the dashboard shows of the artifacts of a run, as dbt 2.0.9 writes
;; them, and what its keys do.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-dashboard)
(require 'dbt-mode)

(defun dbt-test-write-artifacts (root)
  "Write in ROOT's target the artifacts of a failing build and of freshness.
They have the shape of dbt 2.0.9's: a warned test, a failed one with its
failing rows, an errored model whose message gives its cause second, a
model of a package, a skipped node, one with no message, statuses of
freshness capitalised, a freshness that failed."
  (let ((target (expand-file-name "target" root)))
    (make-directory target t)
    (write-region
     (json-encode
      `((metadata . ((dbt_version . "2.0.9")))
        (args . ((which . "build")))
        (results . [((status . "warn") (unique_id . "test.shop.not_null_stg_orders_order_id.81cfe2fe64")
                     (execution_time . 0.0625) (message . "Warn") (failures . 2))
                    ((status . "error") (unique_id . "model.shop.orders") (execution_time . 1.5)
                     (message . "Database Error in model orders (target/run/shop/models/marts/orders.sql)\n  Catalog Error: Table with name nowhere does not exist!"))
                    ((status . "success") (unique_id . "model.shop.stg_orders") (execution_time . 0.25)
                     (message . "Succeeded"))
                    ((status . "fail") (unique_id . "test.shop.assert_positive_amount") (execution_time . 0.5)
                     (message . "Error") (failures . 1))
                    ((status . "skipped") (unique_id . "model.shop.customers") (execution_time . 0)
                     (message . "Skipped"))
                    ((status . "skipped") (unique_id . "test.shop.not_null_customers_customer_id.5c9bf9911d")
                     (execution_time . 0) (message))
                    ((status . "error") (unique_id . "model.shop_utils.util") (execution_time . 0.75)
                     (message . "Compilation Error"))
                    ((status . "pass") (unique_id . "test.shop.unique_stg_orders_order_id.e3b841c71a")
                     (execution_time . 0.125) (message . "Pass"))])))
     nil (expand-file-name "run_results.json" target))
    (write-region
     (json-encode
      '((results . [((unique_id . "source.shop.raw.raw_orders") (status . "Warn")
                     (max_loaded_at . "2018-01-04T00:00:00Z") (max_loaded_at_time_ago_in_s . 279519786.0))
                    ((unique_id . "source.shop.raw.raw_customers") (status . "Pass")
                     (max_loaded_at . "2026-11-13T00:00:00Z") (max_loaded_at_time_ago_in_s . 60.0))
                    ((unique_id . "source.shop.events.page_views") (status . "runtime error")
                     (error . "Catalog Error: Table page_views does not exist"))])))
     nil (expand-file-name "sources.json" target))
    (write-region
     (json-encode
      '((nodes . ((model.shop.orders . ((package_name . "shop") (original_file_path . "models/marts/orders.sql")))
                  (model.shop_utils.util . ((package_name . "shop_utils") (original_file_path . "models/util.sql")))))))
     nil (expand-file-name "manifest.json" target))))

(defun dbt-test-dashboard-lines ()
  "Return the lines of the dashboard of the current buffer, without properties."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(ert-deftest dbt-dashboard-says-ages-at-a-glance ()
  "An age is in the largest unit it reaches, singular for one."
  (should (equal (mapcar #'dbt-dashboard--age '(0 1 59 60 119 7200 86400 172800))
                 '("0 seconds" "1 second" "59 seconds" "1 minute" "1 minute" "2 hours" "1 day" "2 days"))))

(ert-deftest dbt-dashboard-shows-what-to-fix-the-slowest-and-stale-sources ()
  "The dashboard lists the nodes to fix, the most severe first, with dbt's cause; the slowest; the stale sources.
Each heading says which command wrote its file and how long ago;
statuses read whatever their case; a freshness that failed shows its
error; a missing file says so."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-dashboard)
    (with-current-buffer (format "*dbt dashboard: shop*")
      (should (equal (dbt-test-dashboard-lines)
                     '("Failed, warned or skipped in dbt runs: no results in run_results.json"
                       ""
                       "Slowest nodes: no results in run_results.json"
                       ""
                       "Stale sources, of dbt source freshness: no results in sources.json"
                       "" "")))
      (dbt-test-write-artifacts root)
      (let ((dbt-dashboard-slowest 2))
        (revert-buffer))
      (let ((lines (dbt-test-dashboard-lines)))
        (should (string-match-p "\\`Failed, warned or skipped in dbt build, from run_results.json, written [0-9]+ seconds? ago\\'"
                                (nth 0 lines)))
        (should (equal (seq-take (cdr lines) 7)
                       '("  error   orders  Database Error in model orders (target/run/shop/models/marts/orders.sql): Catalog Error: Table with name nowhere does not exist!"
                         "  error   util  Compilation Error"
                         "  fail    assert_positive_amount  Error, 1 failing rows"
                         "  warn    not_null_stg_orders_order_id.81cfe2fe64  Warn, 2 failing rows"
                         "  skipped customers  Skipped"
                         "  skipped not_null_customers_customer_id.5c9bf9911d  "
                         "")))
        (should (equal (seq-take (nthcdr 9 lines) 3)
                       '("     1.50 s  orders  error" "     0.75 s  util  error" "")))
        (should (member "  warn    raw.raw_orders  loaded 2018-01-04T00:00:00Z, 3235 days ago" lines))
        (should (member "  runtime error events.page_views  Catalog Error: Table page_views does not exist" lines))
        (should-not (seq-find (lambda (line) (string-search "raw_customers" line)) lines)))
      (kill-buffer))))

(ert-deftest dbt-dashboard-visits-folds-and-runs-freshness ()
  "RET visits a node's file, a package's included; TAB folds a section; f runs dbt source freshness.
The dashboard is read again once dbt ends.  On a protected default
target, f asks first, and runs nothing on a no."
  (dbt-test-with-project root
    (dbt-test-write-artifacts root)
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id\n" nil (expand-file-name "dbt_packages/shop_utils/models/util.sql" root))
    (dbt-test-visit root "models/marts/customers.sql")
    (save-window-excursion
      (dbt-dashboard)
      (with-current-buffer "*dbt dashboard: shop*"
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (execute-kbd-macro (kbd "TAB"))
        (should (save-excursion (search-forward "  error") (outline-invisible-p)))
        (outline-show-all)
        (search-forward "  error   orders")
        (end-of-line)
        (execute-kbd-macro (kbd "RET"))
        (should (equal (file-relative-name buffer-file-name root) "models/marts/orders.sql"))
        (switch-to-buffer "*dbt dashboard: shop*")
        (search-forward "  error   util")
        (execute-kbd-macro (kbd "RET"))
        (should (equal (file-relative-name buffer-file-name root) "dbt_packages/shop_utils/models/util.sql"))
        (switch-to-buffer "*dbt dashboard: shop*")
        (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
              (process-environment (append '("DBT_TARGET=prod" "FAKE_DBT_SLEEP=0.5") process-environment))
              (asked nil))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil)))
            (should-error (execute-kbd-macro (kbd "f")) :type 'user-error))
          (should (string-match-p "Run on target prod" asked))
          (should-not (get-buffer "*dbt shop: source freshness*"))
          (delete-file (expand-file-name "target/sources.json" root))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (execute-kbd-macro (kbd "f")))
          ;; Until dbt ends, the dashboard shows what it read.
          (should (string-search "raw.raw_orders" (buffer-string)))
          (with-current-buffer "*dbt shop: source freshness*"
            (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
            (goto-char (point-min))
            (should (re-search-forward "^arguments: source freshness$" nil t)))
          (dbt-test-wait (lambda () (string-search "no results in sources.json" (buffer-string)))))
        (kill-buffer)))))

(provide 'dbt-dashboard-test)
;;; dbt-dashboard-test.el ends here
