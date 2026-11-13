;;; dbt-compiled-test.el --- Tests of the compiled SQL beside its model  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The compiled SQL that C-c C-v shows beside a model: the line it faces,
;; how it follows point and saves.

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
                                   (buffer-substring (point) (line-end-position)))))))))

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

(provide 'dbt-compiled-test)
;;; dbt-compiled-test.el ends here
