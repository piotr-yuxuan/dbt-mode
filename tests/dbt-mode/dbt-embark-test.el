;;; dbt-embark-test.el --- Tests of embark's actions on dbt resources  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What embark acts on in dbt-mode, and what its actions do, with the
;; embark of the development shell, given to embark as the README says.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)
(require 'dbt-consult)
(require 'dbt-embark)
(require 'consult)
(require 'embark)

(defmacro dbt-test-with-embark (&rest body)
  "Run BODY with dbt-embark's target finder and keymap given to embark."
  (declare (indent 0) (debug t))
  `(let ((embark-target-finders (cons #'dbt-embark-target embark-target-finders))
         (embark-keymap-alist (cons '(dbt-identifier dbt-embark-identifier-map) embark-keymap-alist))
         (dbt-executable (expand-file-name "bin/dbt" dbt-test-directory)))
     ,@body))

(ert-deftest dbt-embark-acts-on-the-call-at-point ()
  "embark's actions on a ref at point visit its definition and its YAML entry, show its lineage, build and test it.
Each acts on what M-. would go to; outside dbt-mode, no target."
  (dbt-test-with-project root
    (dbt-test-with-embark
      (save-window-excursion
        (pcase-dolist (`(,key ,check)
                       `(("o" ,(lambda () (equal (file-relative-name buffer-file-name root)
                                                 "models/staging/stg_orders.sql")))
                         ("y" ,(lambda () (and (equal (file-relative-name buffer-file-name root)
                                                      "models/staging/_staging.yml")
                                               (looking-at ".*name: stg_orders"))))
                         ("l" ,(lambda () (get-buffer "*dbt lineage: stg_orders*")))
                         ("b" ,(lambda () (dbt-test-ran "build" "^arguments: build --select stg_orders$")))
                         ("t" ,(lambda () (dbt-test-ran "test" "^arguments: test --select stg_orders$")))))
          (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
          (dbt-test-goto "stg_orders')")
          (execute-kbd-macro (kbd (concat "M-x embark-act RET " key)))
          (should (funcall check)))
        (with-temp-buffer
          (insert "{{ ref('stg_orders') }}")
          (goto-char 10)
          (should-not (dbt-embark-target)))))))

(defun dbt-test-ran (command regexp)
  "Return non-nil once the dbt COMMAND of the shop project ran, its output matching REGEXP."
  (with-current-buffer (dbt-test-wait (lambda () (get-buffer (format "*dbt shop: %s*" command))))
    (dbt-test-wait (lambda () (not (get-buffer-process (current-buffer)))))
    (goto-char (point-min))
    (prog1 (re-search-forward regexp nil t)
      (kill-buffer))))

(ert-deftest dbt-embark-acts-on-a-source-entry-and-a-candidate ()
  "embark acts on a source table's YAML entry, and on a candidate of dbt-consult-resource.
The table's relation comes from the manifest; the candidate's lineage
shows, and it builds."
  (dbt-test-with-project root
    (dbt-test-with-embark
      (make-directory (expand-file-name "target" root))
      (write-region (concat "{\"nodes\": {}, \"sources\": {\"source.shop.raw.raw_orders\":"
                            " {\"relation_name\": \"raw.main.raw_orders\"}}}")
                    nil (expand-file-name "target/manifest.json" root))
      (save-window-excursion
        (switch-to-buffer (dbt-test-visit root "models/staging/_sources.yml"))
        (dbt-properties-mode-maybe)
        (dbt-test-goto "raw_orders")
        (execute-kbd-macro (kbd "M-x embark-act RET w"))
        (should (equal (current-kill 0) "raw.main.raw_orders"))
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        ;; The maps of completion inherit this keymap, not the variable.
        (keymap-set minibuffer-local-map "C-c a" #'embark-act)
        ;; Quitting the minibuffer first would end the keyboard macro
        ;; before the action, which then reads a terminal batch has not;
        ;; staying in it, the action would run in consult's preview.
        (let ((embark-quit-after-action nil)
              (consult-preview-key nil))
          (unwind-protect
              (dolist (action '("l" "b"))
                (condition-case nil
                    (execute-kbd-macro (kbd (format "M-x dbt-consult-resource RET ref:stg_orders C-c a %s C-g"
                                                    action)))
                  (quit nil)))
            (keymap-unset minibuffer-local-map "C-c a" t)))
        (should (get-buffer "*dbt lineage: stg_orders*"))
        ;; Built where it is defined, not where point is.
        (should (dbt-test-ran "build" "^arguments: build --select stg_orders$"))))))

(provide 'dbt-embark-test)
;;; dbt-embark-test.el ends here
