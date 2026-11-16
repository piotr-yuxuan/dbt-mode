;;; dbt-consult-test.el --- Tests of the resources read with consult  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What `dbt-consult-resource' offers, previews and visits, with the
;; consult of the development shell.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)
(require 'dbt-consult)
(require 'consult)

(ert-deftest dbt-consult-groups-previews-and-visits-a-resource ()
  "M-x dbt-consult-resource offers each resource under its identifier, grouped by kind, with its package and file.
The current candidate is previewed at its definition; RET visits it
there.  A package's resource comes under its qualified identifier."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "select 1 as id\n" nil (expand-file-name "dbt_packages/shop_utils/models/util.sql" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/customers.sql"))
      (let ((consult-preview-key 'any)
            previewed metadata candidates)
        ;; The maps of completion inherit this keymap, not the variable.
        (keymap-set minibuffer-local-map "C-c t"
                    (lambda ()
                      (interactive)
                      ;; A preview's buffer need not visit its file.
                      (setq previewed (with-current-buffer (window-buffer (minibuffer-selected-window))
                                        (list (buffer-name) (line-number-at-pos)))
                            metadata (completion-metadata "" minibuffer-completion-table
                                                          minibuffer-completion-predicate)
                            candidates (all-completions "" minibuffer-completion-table
                                                        minibuffer-completion-predicate))))
        (unwind-protect
            (execute-kbd-macro (kbd "M-x dbt-consult-resource RET source:raw/raw_customers C-c t RET"))
          (keymap-unset minibuffer-local-map "C-c t" t))
        (should (string-match-p "_sources\\.yml" (car previewed)))
        (should (equal (cadr previewed) 10))
        (should (equal (completion-metadata-get metadata 'category) 'dbt-identifier))
        (should (equal (mapcar (lambda (identifier)
                                 (funcall (completion-metadata-get metadata 'group-function)
                                          (car (member identifier candidates)) nil))
                               '("ref:stg_orders" "ref:shop_utils/util" "source:raw" "source:raw/raw_orders"
                                 "macro:cents_to_dollars" "doc:order_status"))
                       '("Models" "Models" "Sources" "Source tables" "Macros" "Docs")))
        (should (member "ref:shop_utils/util" candidates))
        ;; consult gives the annotation as the suffix of an affixation.
        (should (equal (substring-no-properties
                        (nth 2 (car (funcall (completion-metadata-get metadata 'affixation-function)
                                             (list (car (member "ref:shop_utils/util" candidates)))))))
                       "  shop_utils  dbt_packages/shop_utils/models/util.sql"))
        (should (equal (list (file-relative-name buffer-file-name root) (line-number-at-pos))
                       '("models/staging/_sources.yml" 10)))))))

(provide 'dbt-consult-test)
;;; dbt-consult-test.el ends here
