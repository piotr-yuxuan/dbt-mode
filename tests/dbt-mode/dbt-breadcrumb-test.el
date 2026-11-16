;;; dbt-breadcrumb-test.el --- Tests of the breadcrumbs of dbt-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What `dbt-breadcrumb-mode' shows in the header line without a server.
;; Its names are those of dbt-lsp-plus's outline (its contract A1, and
;; its plan 027 for `if', `for' and `call'), so that a model reads the
;; same with and without the proxy.  With a server, see dbt-lsp-test.el.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)
(require 'ert-x)

(defun dbt-test-idle (seconds)
  "Run the idle timers that Emacs runs once idle for SECONDS.
`ert-run-idle-timers' runs them all, whatever their delay."
  (dolist (timer (copy-sequence timer-idle-list))
    (unless (time-less-p seconds (timer--time timer))
      (timer-event-handler timer))))

(defun dbt-test-breadcrumbs-waiting-p ()
  "Return non-nil when an update of the header line waits for Emacs to be idle."
  (seq-some (lambda (timer) (eq (timer--function timer) #'dbt-breadcrumb--update)) timer-idle-list))

(defun dbt-test-breadcrumbs-at (text)
  "Return the header line once point moves into TEXT and Emacs is idle.
Point goes to the start of TEXT, then `C-f' moves it one character on."
  (dbt-test-goto text)
  (execute-kbd-macro (kbd "C-f"))
  (ert-run-idle-timers)
  (dbt-test-mode-line header-line-format))

(ert-deftest dbt-breadcrumb-names-the-file-then-the-cte-and-blocks-at-point ()
  "dbt-breadcrumb-mode, off by default, names the directories from the root, the file, then what holds point.
The header line follows point once Emacs is idle for half a second, not
before, and a command that moves nothing asks for nothing: in a CTE,
its name, from the start of its first line, where `C-M-a' leaves point;
in an `if' block inside a CTE, both, outermost first; after the CTEs,
the file alone.  Turned off, the header line goes, and nothing follows
the commands."
  (dbt-test-with-project root
    (save-window-excursion
      ;; Each command ends in the selected window's buffer.
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (should-not header-line-format)
      (dbt-test-goto "select * from {{ ref('stg_orders')")
      (execute-kbd-macro (kbd "M-x dbt-breadcrumb-mode RET"))
      (ert-run-idle-timers)
      (should (equal (dbt-test-mode-line header-line-format) " models > marts > orders.sql > orders"))
      (dbt-test-goto "where orders.order_date")
      (execute-kbd-macro (kbd "C-e"))
      (dbt-test-idle 0.4)
      (should (equal (dbt-test-mode-line header-line-format) " models > marts > orders.sql > orders"))
      (dbt-test-idle 0.5)
      (should (equal (dbt-test-mode-line header-line-format) " models > marts > orders.sql > final > if is_incremental()"))
      (execute-kbd-macro (kbd "M-x ignore RET"))
      (should-not (dbt-test-breadcrumbs-waiting-p))
      (execute-kbd-macro (kbd "C-M-a"))
      (should (looking-at "final as ("))
      (ert-run-idle-timers)
      (should (equal (dbt-test-mode-line header-line-format) " models > marts > orders.sql > final"))
      (execute-kbd-macro (kbd "M->"))
      (ert-run-idle-timers)
      (should (equal (dbt-test-mode-line header-line-format) " models > marts > orders.sql"))
      (execute-kbd-macro (kbd "M-x dbt-breadcrumb-mode RET"))
      (should-not header-line-format)
      (execute-kbd-macro (kbd "M-<"))
      (should-not (dbt-test-breadcrumbs-waiting-p)))))

(ert-deftest dbt-breadcrumb-names-a-macro-by-its-name ()
  "In a macro file, the header line names the file then the macro, not its arguments."
  (dbt-test-with-project root
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "macros/cents_to_dollars.sql"))
      (execute-kbd-macro (kbd "M-x dbt-breadcrumb-mode RET"))
      (should (equal (dbt-test-breadcrumbs-at "/ 100")
                     " macros > cents_to_dollars.sql > cents_to_dollars")))))

(ert-deftest dbt-breadcrumb-names-blocks-and-ctes-as-dbt-lsp-plus-does ()
  "Blocks and CTEs are named as in dbt-lsp-plus's outline, nested by extent.
A definition block by the name it defines; an `if', `for' or `call'
block by its statement, from its keyword to the end of its expression,
trim markers dropped and each run of whitespace one space, `%' shown as
written; a CTE by its name, a nested one or one written on one line
included.  A `set' with `=', a `filter' block and a CTE inside a `{# #}'
comment are none."
  (dbt-test-with-project root
    (write-region (concat "{% macro helper(columns) -%}\n"
                          "{% set rows %}\n"
                          "{% filter upper %}\n"
                          "with outer_cte as (\n"
                          "    with inner_cte as (\n"
                          "        select {%- for column in\n"
                          "                   columns -%} {{ column }}{% if loop.index % 2 == 0 %}, 1 AT_IF{% endif %}{% endfor %}\n"
                          "    ), one as (select 1 AT_ONE)\n"
                          "    {% call(user) dbt_utils.my_macro() %} AT_CALL {% endcall %}\n"
                          "    select * from inner_cte\n"
                          ")\n"
                          "{#\n"
                          "commented as (\n"
                          "    AT_COMMENT\n"
                          ")\n"
                          "#}\n"
                          "{% set x = 1 %}\n"
                          "select 1 AT_END\n"
                          "{% endfilter %}\n"
                          "{% endset %}\n"
                          "{%- endmacro %}\n")
                  nil (expand-file-name "macros/helper.sql" root))
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "macros/helper.sql"))
      (execute-kbd-macro (kbd "M-x dbt-breadcrumb-mode RET"))
      (should (equal (dbt-test-breadcrumbs-at "AT_IF")
                     (concat " macros > helper.sql > helper > rows > outer_cte > inner_cte"
                             " > for column in columns > if loop.index % 2 == 0")))
      (should (equal (dbt-test-breadcrumbs-at "AT_ONE") " macros > helper.sql > helper > rows > outer_cte > one"))
      (should (equal (dbt-test-breadcrumbs-at "AT_CALL")
                     " macros > helper.sql > helper > rows > outer_cte > call(user) dbt_utils.my_macro()"))
      (should (equal (dbt-test-breadcrumbs-at "AT_COMMENT") " macros > helper.sql > helper > rows"))
      (should (equal (dbt-test-breadcrumbs-at "AT_END") " macros > helper.sql > helper > rows")))))

(ert-deftest dbt-breadcrumb-leaves-a-killed-buffer-alone ()
  "A buffer killed before Emacs is idle leaves its update nothing to do, and no error.
An error in a timer only shows in *Messages*."
  (dbt-test-with-project root
    (save-window-excursion
      (switch-to-buffer (dbt-test-visit root "models/marts/orders.sql"))
      (execute-kbd-macro (kbd "M-x dbt-breadcrumb-mode RET"))
      (kill-buffer))
    (let ((start (with-current-buffer (messages-buffer) (point-max))))
      (ert-run-idle-timers)
      (should-not (with-current-buffer (messages-buffer)
                    (save-excursion
                      (goto-char start)
                      (search-forward "Error running timer" nil t)))))))

(provide 'dbt-breadcrumb-test)
;;; dbt-breadcrumb-test.el ends here
