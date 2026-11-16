;;; dbt-dashboard-test.el --- Tests of the dashboard of a project  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What the dashboard shows of the artifacts of a run, as dbt writes
;; them, and what its keys do.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-dashboard)
(require 'dbt-mode)

(defconst dbt-test-freshness-hint
  "  dbt 2 leaves out a check that could not run: see dbt's output or logs/dbt.log."
  "The last line of the section of stale sources.")

(defun dbt-test-write-artifacts (root)
  "Write in ROOT's target the artifacts of a failing build and of freshness.
They have the shape of dbt 2.0.9's: a warned test, a failed one with its
failing rows, an errored model whose message gives its cause second, a
model of a package, a skipped node, one with no message, statuses of
freshness capitalised; and a check that could not run, as dbt 1.x
writes it and dbt 2 does not."
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
                     (message . "Skipped due to upstream failure"))
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
Each heading says which command wrote its file, how many results it
holds and how long ago; statuses read whatever their case; a check that
could not run, as dbt 1.x writes it, shows its error; a missing file
says so; the section of stale sources ends with where to see a check
that dbt 2 leaves out."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-dashboard)
    (with-current-buffer (format "*dbt dashboard: shop*")
      (should (equal (dbt-test-dashboard-lines)
                     `("Failed, warned or skipped in dbt runs: no results in run_results.json"
                       ""
                       "Slowest nodes: no results in run_results.json"
                       ""
                       "Stale sources, of dbt source freshness: no results in sources.json"
                       ,dbt-test-freshness-hint
                       "" "")))
      (dbt-test-write-artifacts root)
      (let ((dbt-dashboard-slowest 2))
        (revert-buffer))
      (let ((lines (dbt-test-dashboard-lines)))
        (should (string-match-p "\\`Failed, warned or skipped in dbt build, from run_results.json, 8 results, written [0-9]+ seconds? ago\\'"
                                (nth 0 lines)))
        (should (equal (seq-take (cdr lines) 7)
                       '("  error   orders  Database Error in model orders (target/run/shop/models/marts/orders.sql): Catalog Error: Table with name nowhere does not exist!"
                         "  error   util  Compilation Error"
                         "  fail    assert_positive_amount  Error, 1 failing rows"
                         "  warn    not_null_stg_orders_order_id.81cfe2fe64  Warn, 2 failing rows"
                         "  skipped customers  Skipped due to upstream failure"
                         "  skipped not_null_customers_customer_id.5c9bf9911d  "
                         "")))
        (should (equal (seq-take (nthcdr 9 lines) 3)
                       '("     1.50 s  orders  error" "     0.75 s  util  error" "")))
        (should (member "  warn    raw.raw_orders  loaded 2018-01-04T00:00:00Z, 3235 days ago" lines))
        (should (member "  runtime error events.page_views  Catalog Error: Table page_views does not exist" lines))
        (should-not (seq-find (lambda (line) (string-search "raw_customers" line)) lines))
        ;; The rows, then the hint, which is no row of its own.
        (should (equal (last lines 4)
                       `("  runtime error events.page_views  Catalog Error: Table page_views does not exist"
                         ,dbt-test-freshness-hint
                         "" ""))))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (search-backward (string-trim dbt-test-freshness-hint))
      (should (eq (get-text-property (point) 'face) 'shadow))
      (should (= (next-single-property-change (point) 'face nil (line-end-position)) (line-end-position)))
      (should (equal (cadr (should-error (execute-kbd-macro (kbd "RET")) :type 'user-error))
                     "No node here"))
      (kill-buffer))))

(ert-deftest dbt-dashboard-tells-no-check-from-every-source-fresh ()
  "A freshness run whose every check could not run is told from one whose every source is fresh.
dbt 2 then writes no result, and the heading says how many results
sources.json holds, one in the singular."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (make-directory (expand-file-name "target" root))
    (pcase-dolist (`(,results ,count)
                   '(([] "0 results")
                     ([((unique_id . "source.shop.raw.raw_orders") (status . "pass"))] "1 result")
                     ([((unique_id . "source.shop.raw.raw_orders") (status . "pass"))
                       ((unique_id . "source.shop.raw.raw_customers") (status . "pass"))]
                      "2 results")))
      (write-region (json-encode `((results . ,results))) nil (expand-file-name "target/sources.json" root))
      (dbt-dashboard)
      (with-current-buffer "*dbt dashboard: shop*"
        (should (string-match-p (format "^Stale sources, of dbt source freshness, from sources.json, %s, written "
                                        count)
                                (buffer-string)))))
    (kill-buffer "*dbt dashboard: shop*")))

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

(ert-deftest dbt-dashboard-live-points-to-the-checks-dbt-leaves-out ()
  "With the real dbt, a check that could not run is left out, and the hint says where to see it.
dbt writes no result for it in sources.json, which a row for raw_missing
would contradict: the hint would then be wrong."
  (unless (getenv "DBT_MODE_TEST_DBT") (ert-skip "no dbt 2 executable"))
  (dbt-test-with-project root
    (write-region (concat "version: 2\n\nsources:\n  - name: raw\n    schema: main\n    tables:\n"
                          "      - name: raw_orders\n        config:\n"
                          "          loaded_at_field: \"cast(order_date as timestamp)\"\n"
                          "          freshness:\n            warn_after: {count: 1, period: day}\n"
                          "      - name: raw_customers\n      - name: raw_missing\n        config:\n"
                          "          loaded_at_field: loaded_at\n"
                          "          freshness:\n            error_after: {count: 1, period: day}\n\n"
                          "  - name: \"events\"\n    tables:\n      - name: page_views\n")
                  nil (expand-file-name "models/staging/_sources.yml" root))
    (let ((default-directory root))
      (should (eq 0 (call-process (getenv "DBT_MODE_TEST_DBT") nil nil nil "seed" "--profiles-dir" root)))
      (call-process (getenv "DBT_MODE_TEST_DBT") nil nil nil "source" "freshness" "--profiles-dir" root)
      (should (string-search "raw_missing does not exist"
                             (with-temp-buffer
                               (insert-file-contents (expand-file-name "logs/dbt.log" root))
                               (buffer-string))))
      (dbt-test-visit root "models/marts/orders.sql")
      (save-window-excursion
        (dbt-dashboard)
        (with-current-buffer "*dbt dashboard: shop*"
          (let ((lines (dbt-test-dashboard-lines)))
            (should (string-prefix-p "  warn    raw.raw_orders  loaded " (car (last lines 4))))
            (should (string-match-p "\\`Stale sources, of dbt source freshness, from sources.json, 1 result, written "
                                    (nth (- (length lines) 5) lines)))
            (should (equal (last lines 3) (list dbt-test-freshness-hint "" "")))
            (should-not (seq-find (lambda (line) (string-search "raw_missing" line)) lines)))
          (kill-buffer))))))

(provide 'dbt-dashboard-test)
;;; dbt-dashboard-test.el ends here
