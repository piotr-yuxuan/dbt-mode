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
They have the shape of dbt 2.0.9's: a failed test, an errored model
whose message runs on, statuses of freshness capitalised."
  (let ((target (expand-file-name "target" root)))
    (make-directory target t)
    (write-region
     (json-encode
      `((metadata . ((dbt_version . "2.0.9")))
        (args . ((which . "build")))
        (results . [((status . "error") (unique_id . "model.shop.orders") (execution_time . 1.5)
                     (message . "Database Error in model orders (target/run/shop/models/marts/orders.sql)\n  Catalog Error"))
                    ((status . "success") (unique_id . "model.shop.stg_orders") (execution_time . 0.25)
                     (message . "Succeeded"))
                    ((status . "fail") (unique_id . "test.shop.assert_positive_amount") (execution_time . 0.5)
                     (message . "Error") (failures . 1))
                    ((status . "pass") (unique_id . "test.shop.unique_stg_orders_order_id.e3b841c71a")
                     (execution_time . 0.125) (message . "Pass"))])))
     nil (expand-file-name "run_results.json" target))
    (write-region
     (json-encode
      '((results . [((unique_id . "source.shop.raw.raw_orders") (status . "Warn")
                     (max_loaded_at . "2018-01-04T00:00:00Z") (max_loaded_at_time_ago_in_s . 279519786.0))
                    ((unique_id . "source.shop.raw.raw_customers") (status . "Pass")
                     (max_loaded_at . "2026-11-13T00:00:00Z") (max_loaded_at_time_ago_in_s . 60.0))])))
     nil (expand-file-name "sources.json" target))
    (write-region
     (json-encode
      '((nodes . ((model.shop.orders . ((package_name . "shop") (original_file_path . "models/marts/orders.sql")))))))
     nil (expand-file-name "manifest.json" target))))

(defun dbt-test-dashboard-lines ()
  "Return the lines of the dashboard of the current buffer, without properties."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(ert-deftest dbt-dashboard-shows-failures-the-slowest-and-stale-sources ()
  "The dashboard shows the failures with dbt's message, the slowest nodes, the stale sources.
Each heading says which command wrote its file and how long ago;
freshness statuses read whatever their case; a missing file says so."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-dashboard)
    (with-current-buffer (format "*dbt dashboard: shop*")
      (should (equal (dbt-test-dashboard-lines)
                     '("Failures and warnings of dbt runs: no results in run_results.json"
                       ""
                       "Slowest nodes: no results in run_results.json"
                       ""
                       "Stale sources, of dbt source freshness: no results in sources.json"
                       "" "")))
      (dbt-test-write-artifacts root)
      (revert-buffer)
      (let ((lines (dbt-test-dashboard-lines)))
        (should (string-match-p "\\`Failures and warnings of dbt build, from run_results.json, written [0-9]+ seconds ago\\'"
                                (nth 0 lines)))
        (should (equal (seq-take (cdr lines) 3)
                       '("  error  orders  Database Error in model orders (target/run/shop/models/marts/orders.sql)"
                         "  fail   assert_positive_amount  Error"
                         "")))
        (should (string-match-p "\\`Slowest nodes, from run_results.json, written [0-9]+ seconds ago\\'" (nth 4 lines)))
        (should (equal (seq-take (nthcdr 5 lines) 4)
                       '("     1.50 s  orders  error"
                         "     0.50 s  assert_positive_amount  fail"
                         "     0.25 s  stg_orders  success"
                         "     0.12 s  unique_stg_orders_order_id.e3b841c71a  pass")))
        (should (member "  warn   raw.raw_orders  loaded 2018-01-04T00:00:00Z, 3235 days ago" lines))
        (should-not (seq-find (lambda (line) (string-search "raw_customers" line)) lines)))
      (kill-buffer))))

(ert-deftest dbt-dashboard-visits-folds-and-runs-freshness ()
  "RET visits a node's file, TAB folds a section, f runs dbt source freshness and reads the files again.
On a protected default target, f asks first."
  (dbt-test-with-project root
    (dbt-test-write-artifacts root)
    (dbt-test-visit root "models/marts/customers.sql")
    (save-window-excursion
      (dbt-dashboard)
      (with-current-buffer "*dbt dashboard: shop*"
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (execute-kbd-macro (kbd "TAB"))
        (should (save-excursion (search-forward "  error") (outline-invisible-p)))
        (outline-show-all)
        (search-forward "  error")
        (end-of-line)
        (execute-kbd-macro (kbd "RET"))
        (should (equal (file-relative-name buffer-file-name root) "models/marts/orders.sql"))
        (switch-to-buffer "*dbt dashboard: shop*")
        (let ((dbt-executable (expand-file-name "bin/dbt" dbt-test-directory))
              (process-environment (cons "DBT_TARGET=prod" process-environment))
              (asked nil))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (setq asked prompt) nil)))
            (should-error (execute-kbd-macro (kbd "f")) :type 'user-error))
          (should (string-match-p "Run on target prod" asked))
          (delete-file (expand-file-name "target/sources.json" root))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (execute-kbd-macro (kbd "f")))
          (with-current-buffer "*dbt shop: source freshness*"
            (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
            (goto-char (point-min))
            (should (re-search-forward "^arguments: source freshness$" nil t)))
          (dbt-test-wait (lambda () (string-search "no results in sources.json" (buffer-string)))))
        (kill-buffer)))))

(provide 'dbt-dashboard-test)
;;; dbt-dashboard-test.el ends here
