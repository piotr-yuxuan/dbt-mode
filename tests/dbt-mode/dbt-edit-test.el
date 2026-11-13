;;; dbt-edit-test.el --- Tests of typing and moving through Jinja and CTEs  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user gets when typing Jinja and moving through a model: pairs
;; of delimiters, closing tags, matching tags, CTEs as defuns, and the
;; regions that expreg selects.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)
(require 'elec-pair)

(defun dbt-test-type (text)
  "Type TEXT at point, one character at a time, as `self-insert-command' does."
  (dolist (char (string-to-list text))
    (let ((last-command-event char))
      (call-interactively #'self-insert-command))))

(defun dbt-test-around-point ()
  "Return the text of the buffer with -!- at point."
  (concat (buffer-substring-no-properties (point-min) (point)) "-!-"
          (buffer-substring-no-properties (point) (point-max))))

(ert-deftest dbt-edit-pairs-jinja-delimiters ()
  "The second character of an opener inserts its closer, point in the middle."
  (dolist (case '(("{{" . "select {{ -!- }}") ("{%" . "select {% -!- %}") ("{#" . "select {# -!- #}")))
    (with-temp-buffer
      (dbt-mode)
      (insert "select ")
      (dbt-test-type (car case))
      (should (equal (dbt-test-around-point) (cdr case))))))

(ert-deftest dbt-edit-pairs-jinja-delimiters-with-electric-pairs ()
  "With `electric-pair-mode', the braces it inserts make way for the closer."
  (dolist (case '(("{{" . "{{ -!- }}") ("{%" . "{% -!- %}")))
    (with-temp-buffer
      (dbt-mode)
      (electric-pair-local-mode 1)
      (dbt-test-type (car case))
      (should (equal (dbt-test-around-point) (cdr case))))))

(ert-deftest dbt-edit-leaves-the-closer-of-a-tag-being-edited ()
  "Before a closer that follows a space, nothing is added; nor when turned off."
  (with-temp-buffer
    (dbt-mode)
    (insert "{ }}")
    (goto-char 2)
    (dbt-test-type "{")
    (should (equal (dbt-test-around-point) "{{-!- }}")))
  (with-temp-buffer
    (dbt-mode)
    (let ((dbt-edit-pair-jinja nil))
      (dbt-test-type "{{"))
    (should (equal (dbt-test-around-point) "{{-!-"))))

(ert-deftest dbt-edit-closes-the-innermost-open-block ()
  "dbt-close-block closes the innermost block, at the column of its opener."
  (with-temp-buffer
    (dbt-mode)
    (insert "{# {% if old %} #}\n{% set n = 3 %}\n{% if a %}\n    {% for x in y %}\n    select 1\n")
    (dbt-close-block)
    (should (string-suffix-p "    select 1\n    {% endfor %}" (buffer-string)))
    (insert "\n")
    (dbt-close-block)
    (should (string-suffix-p "{% endfor %}\n{% endif %}" (buffer-string)))
    (insert "\n")
    (should-error (dbt-close-block) :type 'user-error))
  (with-temp-buffer
    (dbt-mode)
    (insert "select\n    {% set columns %}\n        a, b\n    \n")
    (forward-line -1)
    (end-of-line)
    (dbt-close-block)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "    {% endset %}"))))

(ert-deftest dbt-edit-jumps-between-matching-tags ()
  "From an opening tag to its closing tag and back, nested blocks apart."
  (with-temp-buffer
    (dbt-mode)
    (insert "{% if a %}\n{% if b %}x{% endif %}\n{%- endif %}\n")
    (goto-char (point-min))
    (dbt-jump-to-matching-tag)
    (should (looking-at-p "{%- endif"))
    (forward-char 3)
    (dbt-jump-to-matching-tag)
    (should (bobp))
    (dbt-test-goto "{% if b" 4)
    (dbt-jump-to-matching-tag)
    (should (looking-at-p "{% endif %}\n{%-"))
    (dbt-test-goto "x")
    (should-error (dbt-jump-to-matching-tag) :type 'user-error)))

(ert-deftest dbt-edit-moves-through-ctes-as-defuns ()
  "C-M-a, C-M-e and C-M-h move to and mark the CTE around point."
  (dbt-test-with-project root
    (dbt-test-visit root "models/marts/orders.sql")
    (dbt-test-goto "customers.first_name")
    (beginning-of-defun)
    (should (looking-at-p "final as ("))
    (end-of-defun)
    (should (equal (buffer-substring (line-beginning-position 0) (line-end-position 0)) ")"))
    (dbt-test-goto "customers.first_name")
    (mark-defun)
    ;; As everywhere in Emacs, the region takes in the blank line before.
    (should (string-prefix-p "final as (" (string-trim-left (buffer-substring (region-beginning) (region-end)))))
    (should (string-suffix-p "{% endif %}\n)" (string-trim-right (buffer-substring (region-beginning) (region-end)))))
    (deactivate-mark)
    (dbt-test-goto "ref('stg_customers')")
    (beginning-of-defun)
    (should (looking-at-p "customers as ("))
    (beginning-of-defun)
    (should (looking-at-p "with orders as ("))))

(ert-deftest dbt-edit-gives-expreg-the-tag-the-block-and-the-cte ()
  "expreg can select the tag around point and its contents, its block, its CTE."
  (with-temp-buffer
    (insert "with a as (\n    select 1\n    {% if x %} {{ ref('b') }} {% endif %}\n)\nselect * from a\n")
    (dbt-mode)
    (dbt-test-goto "ref")
    (let ((regions (mapcar (lambda (region) (buffer-substring-no-properties (car region) (cdr region)))
                           (dbt-edit-expreg-regions))))
      (dolist (expected '("{{ ref('b') }}" " ref('b') " "{% if x %} {{ ref('b') }} {% endif %}"
                          " {{ ref('b') }} "))
        (should (member expected regions)))
      (should (seq-some (lambda (region) (string-prefix-p "with a as (" region)) regions))
      (should (seq-some (lambda (region) (string-prefix-p "\n    select 1" region)) regions))))
  (with-temp-buffer
    (fundamental-mode)
    (should-not (dbt-edit-expreg-regions))))

(provide 'dbt-edit-test)
;;; dbt-edit-test.el ends here
