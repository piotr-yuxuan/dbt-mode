;;; dbt-compiled-test.el --- Tests of the compiled SQL beside its model  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The compiled SQL that C-c C-v shows beside a model: the line it faces,
;; how it follows point and saves, and its diff with another state.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-compiled)

(defun dbt-test-compile (root &optional text)
  "Write TEXT, the orders model by default, as its compiled SQL in ROOT; return the file.
The Jinja of the default is rendered, which makes the config lines go."
  (let ((compiled (expand-file-name "target/compiled/shop/models/marts/orders.sql" root)))
    (make-directory (file-name-directory compiled) t)
    (write-region (or text (concat "with orders as (\n    select * from \"db\".\"main\".\"stg_orders\"\n),\n\n"
                                   "customers as (\n    select * from \"db\".\"main\".\"stg_customers\"\n),\n\n"
                                   "final as (\n    select\n        orders.order_id,\n        orders.customer_id,\n"
                                   "        customers.first_name\n    from orders\n)\n\nselect * from final\n"))
                  nil compiled)
    compiled))

(defun dbt-test-compiled-line (compiled)
  "Return the text of the line at point in the window of the buffer COMPILED."
  (with-current-buffer (get-file-buffer compiled)
    (save-excursion
      (goto-char (window-point (get-buffer-window (current-buffer))))
      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

(ert-deftest dbt-compiled-faces-the-same-line-of-the-same-cte ()
  "Point's line in a CTE faces the same line of that CTE in the compiled SQL.
Outside a CTE, the line at the same share of the buffer."
  (dbt-test-with-project root
    (let ((compiled (find-file-noselect (dbt-test-compile root))))
      (dbt-test-visit root "models/marts/orders.sql")
      (dbt-test-goto "orders.customer_id")
      (should (string-prefix-p "        orders.customer_id"
                               (with-current-buffer compiled
                                 (save-excursion
                                   (goto-char (with-current-buffer (dbt-test-visit root "models/marts/orders.sql")
                                                (dbt-compiled-position compiled)))
                                   (buffer-substring (point) (line-end-position))))))
      ;; A parenthesis open after point's line leaves point in its CTE.
      (with-current-buffer (dbt-test-visit root "models/marts/orders.sql")
        (dbt-test-goto "    from orders\n")
        (insert "    coalesce(\n")
        (dbt-test-goto "orders.customer_id")
        (should (string-prefix-p "        orders.customer_id"
                                 (with-current-buffer compiled
                                   (save-excursion
                                     (goto-char (with-current-buffer (dbt-test-visit root "models/marts/orders.sql")
                                                  (dbt-compiled-position compiled)))
                                     (buffer-substring (point) (line-end-position))))))
        (revert-buffer t t))
      ;; The final select is in no CTE: the line at the same share.
      (dbt-test-goto "select * from final")
      (let ((share (/ (float (1- (line-number-at-pos))) (count-lines (point-min) (point-max))))
            (position (dbt-compiled-position compiled)))
        (with-current-buffer compiled
          (should (= (line-number-at-pos position)
                     (1+ (round (* share (count-lines (point-min) (point-max)))))))
          (should (> (line-number-at-pos position) 1)))))))

(ert-deftest dbt-compiled-faces-the-model-s-cte-not-an-ephemeral-s ()
  "A CTE of an ephemeral model that dbt inlines, named as one of the model's, is not the model's.
The compiled SQL faces the model's own, and the query of a CTE by name
is that of the model's own."
  (dbt-test-with-project root
    (let ((compiled (find-file-noselect
                     (dbt-test-compile root (concat "with __dbt__cte__stg_payments as (\nwith renamed as (\n"
                                                    "    select 1 as id\n)\nselect * from renamed\n),\n\n"
                                                    "renamed as (\n    select 2 as id\n    from x\n)\n\n"
                                                    "select * from renamed\n")))))
      (with-current-buffer (dbt-test-visit root "models/marts/orders.sql")
        (erase-buffer)
        (insert "with renamed as (\n    select 2 as id\n    from {{ ref('x') }}\n)\n\nselect * from renamed\n")
        (dbt-test-goto "from {{")
        (let ((position (dbt-compiled-position compiled)))
          (with-current-buffer compiled
            (goto-char position)
            (should (looking-at "    from x$")))))
      (with-current-buffer compiled
        (dbt-mode)
        (should (string-match-p "select 2 as id\n    from x\n)\nselect \\* from renamed\\'"
                                (cdr (dbt-mode-cte-query "renamed"))))))))

(ert-deftest dbt-compiled-shows-beside-the-model-and-follows-point ()
  "C-c C-v shows the compiled SQL beside the model, point left in it; moving
point moves the compiled SQL along; once its window goes, so does the mode."
  (dbt-test-with-project root
    (let ((compiled (dbt-test-compile root)))
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        (dbt-test-goto "with orders")
        (execute-kbd-macro (kbd "C-c C-v"))
        (should (eq (window-buffer) (get-file-buffer (expand-file-name "models/marts/orders.sql" root))))
        (should (get-buffer-window (get-file-buffer compiled)))
        (should dbt-compiled-follow-mode)
        (with-current-buffer (get-file-buffer compiled)
          (should buffer-read-only)
          (should auto-revert-mode))
        (dbt-test-goto "customers.first_name")
        (execute-kbd-macro (kbd "C-a"))
        (should (equal (dbt-test-compiled-line compiled) "        customers.first_name"))
        (delete-window (get-buffer-window (get-file-buffer compiled)))
        (execute-kbd-macro (kbd "C-e"))
        (should-not dbt-compiled-follow-mode)))))

(ert-deftest dbt-compiled-diffs-with-another-state ()
  "The diff with a state shows how the model compiles there and here."
  (dbt-test-with-project root
    (dbt-test-compile root)
    (let ((state (expand-file-name "prod-state/" root)))
      (make-directory state)
      (write-region (concat "{\"nodes\": {\"model.shop.orders\": {\"compiled_code\": "
                            (json-encode (concat "with orders as (\n    select * from \"db\".\"prod\".\"stg_orders\"\n),"
                                                 "\n\nselect * from orders\n"))
                            "}}}")
                    nil (expand-file-name "manifest.json" state))
      (dbt-test-visit root "models/marts/orders.sql")
      (let ((diff-switches "-u")
            (messages (with-current-buffer (messages-buffer) (point-max-marker))))
        (set-file-times (expand-file-name "manifest.json" state) (encode-time '(0 4 3 2 1 2026 nil nil t)))
        (dbt-compiled-diff state)
        (dbt-test-wait (lambda () (with-current-buffer (messages-buffer)
                                    (save-excursion
                                      (goto-char messages)
                                      (re-search-forward
                                       (concat "Compiled SQL of orders in .*prod-state/?, from a manifest written at "
                                               (regexp-quote (format-time-string "%F %R" (encode-time '(0 4 3 2 1 2026 nil nil t))))
                                               "$")
                                       nil t))))))
      (with-current-buffer (dbt-test-wait (lambda () (and (get-buffer "*Diff*")
                                                          (not (get-buffer-process "*Diff*"))
                                                          (get-buffer "*Diff*"))))
        (should (string-match-p "^-    select \\* from \"db\"\\.\"prod\"\\.\"stg_orders\"$" (buffer-string)))
        (should (string-match-p "^\\+    select \\* from \"db\"\\.\"main\"\\.\"stg_orders\"$" (buffer-string)))
        (kill-buffer))
      (dbt-test-visit root "models/staging/stg_orders.sql")
      (should-error (dbt-compiled-diff state) :type 'user-error))))

(provide 'dbt-compiled-test)
;;; dbt-compiled-test.el ends here
