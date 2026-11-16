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
         (embark-keymap-alist (cons '(dbt-identifier dbt-embark-identifier-map embark-general-map)
                                    embark-keymap-alist))
         (dbt-executable (expand-file-name "bin/dbt" dbt-test-directory)))
     ,@body))

(ert-deftest dbt-embark-acts-on-the-call-at-point ()
  "embark's actions on a ref at point visit its definition and its YAML entry, show its lineage, build, run and test it.
RET goes where M-. goes, the project's model, though a package defines
a namesake, which d lists too.  Each other action acts on what M-.
goes to; a macro is refused by each that does not fit it, and no dbt
runs; outside dbt-mode, or while the region is active, no dbt target."
  (dbt-test-with-project root
    (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
    (write-region "{{ config(alias='shop_utils_stg_orders') }}\nselect 1 as id\n" nil
                  (expand-file-name "dbt_packages/shop_utils/models/stg_orders.sql" root))
    (dbt-test-with-embark
      (save-window-excursion
        (pcase-dolist (`(,key ,check)
                       `(("RET" ,(lambda () (equal (file-relative-name buffer-file-name root)
                                                   "models/staging/stg_orders.sql")))
                         ("d" ,(lambda () (and (equal (buffer-name) "*xref*")
                                               (string-search "shop_utils/models/stg_orders.sql" (buffer-string))
                                               (string-search "models/staging/stg_orders.sql" (buffer-string)))))
                         ("y" ,(lambda () (and (equal (file-relative-name buffer-file-name root)
                                                      "models/staging/_staging.yml")
                                               (looking-at ".*name: stg_orders"))))
                         ("l" ,(lambda () (equal (buffer-local-value 'dbt-lineage--focus
                                                                     (get-buffer "*dbt lineage: stg_orders*"))
                                                 "model.shop.stg_orders")))
                         ("b" ,(lambda () (dbt-test-ran "build" "^arguments: build --select stg_orders$")))
                         ("r" ,(lambda () (dbt-test-ran "run" "^arguments: run --select stg_orders$")))
                         ("t" ,(lambda () (dbt-test-ran "test" "^arguments: test --select stg_orders$")))))
          (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
          (dbt-test-goto "stg_orders')")
          (execute-kbd-macro (kbd (concat "M-x embark-act RET " key)))
          (should (funcall check)))
        (switch-to-buffer (dbt-test-visit root "models/staging/stg_orders.sql"))
        (pcase-dolist (`(,key ,kinds) '(("b" "table, model, seed, snapshot") ("r" "model")
                                        ("t" "table, model, seed, snapshot") ("y" "model, seed, snapshot")
                                        ("l" "model, seed, snapshot") ("w" "table, model, seed, snapshot")))
          (dbt-test-goto "cents_to_dollars")
          (should (equal (cadr (should-error (execute-kbd-macro (kbd (concat "M-x embark-act RET " key)))
                                             :type 'user-error))
                         (concat "Not for a macro, only for a " kinds))))
        (dolist (command '("build" "run" "test"))
          (should-not (get-buffer (format "*dbt shop: %s*" command))))
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        (dbt-test-goto "stg_orders')")
        (set-mark (line-beginning-position))
        (activate-mark)
        (should-not (dbt-embark-target))
        (deactivate-mark)
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
  "embark acts on a source table's YAML entry, and on a candidate of dbt-consult-resource and of C-c C-f.
The table's relation comes from the manifest; the lineage of
dbt-consult-resource's candidate shows, and C-c C-f's candidate builds
and its relation is copied."
  (dbt-test-with-project root
    (dbt-test-with-embark
      (make-directory (expand-file-name "target" root))
      (write-region (concat "{\"nodes\": {\"model.shop.stg_orders\": {\"relation_name\": \"db.main.stg_orders\"}},"
                            " \"sources\": {\"source.shop.raw.raw_orders\":"
                            " {\"relation_name\": \"raw.main.raw_orders\"}}}")
                    nil (expand-file-name "target/manifest.json" root))
      (save-window-excursion
        (switch-to-buffer (dbt-test-visit root "models/staging/_sources.yml"))
        (dbt-properties-mode-maybe)
        (dbt-test-goto "raw_orders")
        (execute-kbd-macro (kbd "M-x embark-act RET w"))
        (should (equal (current-kill 0) "raw.main.raw_orders"))
        (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
        ;; Another test's lineage of stg_orders would pass for this one's.
        (dolist (buffer (buffer-list))
          (when (string-prefix-p "*dbt lineage: " (buffer-name buffer))
            (kill-buffer buffer)))
        ;; The maps of completion inherit this keymap, not the variable.
        (keymap-set minibuffer-local-map "C-c a" #'embark-act)
        ;; Quitting the minibuffer first would end the keyboard macro
        ;; before the action, which then reads a terminal batch has not;
        ;; staying in it, the action would run in consult's preview.
        (let ((embark-quit-after-action nil)
              (consult-preview-key nil))
          (unwind-protect
              (pcase-dolist (`(,command ,action) '(("M-x dbt-consult-resource RET" "l") ("C-c C-f" "b")
                                                   ("C-c C-f" "w")))
                (condition-case nil
                    (execute-kbd-macro (kbd (format "%s ref:stg_orders C-c a %s C-g" command action)))
                  (quit nil)))
            (keymap-unset minibuffer-local-map "C-c a" t)))
        (should (get-buffer "*dbt lineage: stg_orders*"))
        ;; Built and copied for the candidate, not for what point is on.
        (should (dbt-test-ran "build" "^arguments: build --select stg_orders$"))
        (should (equal (current-kill 0) "db.main.stg_orders"))))))

(ert-deftest dbt-embark-acts-from-the-project-on-a-package ()
  "embark builds a model of a local package outside the project from the project, and shows the lineage of the namesake M-. takes.
A package's model whose name the project's model shares is the
package's own from the package's files."
  (dbt-test-with-project root
    (let ((shared (file-name-as-directory (make-temp-file "dbt-shared" t))))
      (unwind-protect
          (dbt-test-with-embark
            (make-directory (expand-file-name "models" shared))
            (make-directory (expand-file-name "dbt_packages/shop_utils/models" root))
            (pcase-dolist (`(,file . ,text)
                           `((,(expand-file-name "dbt_project.yml" shared)
                              . "name: 'shared'\nversion: '1.0.0'\nconfig-version: 2\n")
                             (,(expand-file-name "models/shared_model.sql" shared) . "select 1 as id\n")
                             (,(expand-file-name "packages.yml" root)
                              . ,(format "packages:\n  - local: dbt_packages/shop_utils\n  - local: %s\n" shared))
                             (,(expand-file-name "models/marts/uses_shared.sql" root)
                              . "select * from {{ ref('shared_model') }}\n")
                             (,(expand-file-name "dbt_packages/shop_utils/models/stg_orders.sql" root)
                              . "select 1 as id\n")
                             (,(expand-file-name "dbt_packages/shop_utils/models/pkg_calls.sql" root)
                              . "select * from {{ ref('stg_orders') }}\n")))
              (write-region text nil file))
            (save-window-excursion
              (switch-to-buffer (dbt-test-visit root "models/marts/uses_shared.sql"))
              (dbt-test-goto "shared_model")
              (execute-kbd-macro (kbd "M-x embark-act RET b"))
              (should (dbt-test-ran "build" "^arguments: build --select shared_model$"))
              (switch-to-buffer (dbt-test-visit root "dbt_packages/shop_utils/models/pkg_calls.sql"))
              (dbt-test-goto "stg_orders")
              (execute-kbd-macro (kbd "M-x embark-act RET l"))
              (should (equal (buffer-local-value 'dbt-lineage--focus (get-buffer "*dbt lineage: stg_orders*"))
                             "model.shop_utils.stg_orders"))))
        (dolist (buffer (buffer-list))
          (when (string-prefix-p shared (or (buffer-file-name buffer) ""))
            (kill-buffer buffer)))
        (delete-directory shared t)))))

(provide 'dbt-embark-test)
;;; dbt-embark-test.el ends here
