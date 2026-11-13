;;; dbt-edit-test.el --- Tests of typing and moving through Jinja and CTEs  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; What a user gets when typing Jinja and moving through a model: pairs
;; of delimiters, closing tags, matching tags, CTEs as defuns, and the
;; regions that expreg selects.  Keys are pressed in a selected window,
;; so that a lost or shadowed binding fails where the key is pressed.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-mode)
(require 'dbt-edit)
(require 'elec-pair)

(defmacro dbt-test-with-window (text &rest body)
  "Run BODY in a `dbt-mode' buffer of TEXT, point at its end.
The buffer is in the selected window, where keys go."
  (declare (indent 1) (debug (form body)))
  `(save-window-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (dbt-mode)
       (insert ,text)
       ,@body)))

(defun dbt-test-keys (keys)
  "Press KEYS, in the notation of `kbd'."
  (execute-kbd-macro (kbd keys)))

(defun dbt-test-type (text)
  "Type TEXT, one key per character."
  (execute-kbd-macro text))

(defun dbt-test-around-point ()
  "Return the text of the buffer with -!- at point."
  (concat (buffer-substring-no-properties (point-min) (point)) "-!-"
          (buffer-substring-no-properties (point) (point-max))))

(ert-deftest dbt-edit-pairs-jinja-delimiters ()
  "The second character of an opener inserts its closer, point in the middle."
  (dolist (case '(("{{" . "select {{ -!- }}") ("{%" . "select {% -!- %}") ("{#" . "select {# -!- #}")))
    (dbt-test-with-window "select "
      (dbt-test-type (car case))
      (should (equal (dbt-test-around-point) (cdr case))))))

(ert-deftest dbt-edit-pairs-jinja-delimiters-with-electric-pairs ()
  "With `electric-pair-mode', the braces it inserts make way for the closer."
  (dolist (case '(("{{" . "{{ -!- }}") ("{%" . "{% -!- %}")))
    (dbt-test-with-window ""
      (electric-pair-local-mode 1)
      (dbt-test-type (car case))
      (should (equal (dbt-test-around-point) (cdr case))))))

(ert-deftest dbt-edit-leaves-the-closer-of-a-tag-being-edited ()
  "Before a closer that follows a space, nothing is added; nor when turned off."
  (dbt-test-with-window "{ }}"
    (goto-char 2)
    (dbt-test-type "{")
    (should (equal (dbt-test-around-point) "{{-!- }}")))
  (dbt-test-with-window ""
    (let ((dbt-edit-pair-jinja nil))
      (dbt-test-type "{{ x "))
    (should (equal (dbt-test-around-point) "{{ x -!-"))))

(ert-deftest dbt-edit-pairs-nothing-before-text ()
  "An opener typed before text wraps or comments it out: nothing is added."
  (dbt-test-with-window "select * from {{ ref('old') }}"
    (goto-char (point-min))
    (dbt-test-type "{#")
    (should (equal (dbt-test-around-point) "{#-!-select * from {{ ref('old') }}")))
  (dbt-test-with-window "(x)"
    (goto-char 3)
    (dbt-test-type "{{")
    (should (equal (dbt-test-around-point) "(x{{ -!- }})"))))

(ert-deftest dbt-edit-keeps-whitespace-control ()
  "A `-' or `+' typed in a fresh pair sticks to the opener, as Jinja needs."
  (dolist (case '(("{%-" . "{%- -!- %}") ("{{-" . "{{- -!- }}") ("{#+" . "{#+ -!- #}")))
    (dbt-test-with-window ""
      (dbt-test-type (car case))
      (should (equal (dbt-test-around-point) (cdr case)))))
  (dbt-test-with-window ""
    (dbt-test-type "{%- set x = 1 -%}")
    (should (equal (dbt-test-around-point) "{%- set x = 1 -%}-!-"))))

(ert-deftest dbt-edit-tells-a-fresh-pair-after-edits-and-undo ()
  "A pair stays fresh through an undo, and text typed in it is never taken.
An edit before the tag moves nothing that the pairing marked."
  (dbt-test-with-window "abc "
    (dbt-test-type "{{n")
    (goto-char 4)
    (delete-char -1)
    (goto-char (- (point-max) 3))
    (dbt-test-type "+")
    (should (equal (dbt-test-around-point) "ab {{ n+-!- }}")))
  (dbt-test-with-window ""
    (buffer-enable-undo)
    (dolist (keys (list "{{" "-" (kbd "C-/") "-"))
      (execute-kbd-macro keys)
      (undo-boundary))
    (should (equal (dbt-test-around-point) "{{- -!- }}"))))

(ert-deftest dbt-edit-takes-tags-typed-out-of-habit ()
  "A tag typed in full, its spaces and its closer, comes out as typed.
A sign after a typed space is no whitespace control: {{ -1 }} stays so."
  (dolist (text '("select * from {{ ref('x') }}" "{% if x %}" "{# note #}" "{{ {'a': 1} }}" "{{ -1 }}"
                  "{{ +x }}" "{%- if x -%}"))
    (dbt-test-with-window ""
      (dbt-test-type text)
      (should (equal (dbt-test-around-point) (concat text "-!-"))))))

(ert-deftest dbt-edit-deletes-an-empty-pair ()
  "DEL right after a fresh opener deletes the pair; elsewhere, one character."
  (dbt-test-with-window "select "
    (dbt-test-type "{%")
    (dbt-test-keys "DEL")
    (should (equal (dbt-test-around-point) "select -!-"))
    (dbt-test-keys "DEL")
    (should (equal (dbt-test-around-point) "select-!-")))
  (dolist (opener '("{%-" "{%+" "{#-"))
    (dbt-test-with-window ""
      (dbt-test-type opener)
      (dbt-test-keys "DEL")
      (should (equal (dbt-test-around-point) "-!-"))))
  (let ((dbt-edit-pair-jinja nil))
    (dbt-test-with-window "{{  }}"
      (goto-char 4)
      (dbt-test-keys "DEL")
      (should (equal (dbt-test-around-point) "{{-!- }}")))))

(ert-deftest dbt-edit-closes-the-innermost-open-block ()
  "dbt-close-block closes the innermost block, at the column of its opener."
  (dbt-test-with-window "{# {% if old %} #}\n{% set n = 3 %}\n{% if a %}\n    {% for x in y %}\n    select 1\n"
    (dbt-test-keys "C-c /")
    (should (string-suffix-p "    select 1\n    {% endfor %}" (buffer-string)))
    (insert "\n")
    (dbt-test-keys "C-c /")
    (should (string-suffix-p "{% endfor %}\n{% endif %}" (buffer-string)))
    (insert "\n")
    (should-error (dbt-close-block) :type 'user-error))
  (dbt-test-with-window "select\n    {% set columns %}\n        a, b\n    \n"
    (forward-line -1)
    (end-of-line)
    (dbt-test-keys "C-c /")
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "    {% endset %}"))))

(ert-deftest dbt-edit-closes-each-kind-of-block ()
  "Each statement that opens a block in dbt's Jinja is closed by its end tag."
  (dolist (keyword '("if" "for" "macro" "set" "call" "docs" "snapshot" "filter" "raw" "test" "materialization"
                     "block" "with"))
    (dbt-test-with-window (format "{%% %s x %%}\nselect 1\n" keyword)
      (dbt-close-block)
      (should (string-suffix-p (format "{%% end%s %%}" keyword) (buffer-string))))))

(ert-deftest dbt-edit-jumps-between-matching-tags ()
  "From an opening tag to its closing tag and back, nested blocks apart."
  (dbt-test-with-window "{% if a %}\n{% if b %}x{% endif %}\n{%- endif %}\n"
    (goto-char (point-min))
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{%- endif"))
    (forward-char 3)
    (dbt-test-keys "C-c %")
    (should (bobp))
    (dbt-test-goto "{% if b" 4)
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{% endif %}\n{%-"))
    (dbt-test-goto "x")
    (should-error (dbt-jump-to-matching-tag) :type 'user-error)))

(ert-deftest dbt-edit-moves-through-ctes-as-defuns ()
  "C-M-a, C-M-e and C-M-h move to and mark the CTE around point."
  (dbt-test-with-window (with-temp-buffer
                          (insert-file-contents (expand-file-name "models/marts/orders.sql" dbt-test-fixture))
                          (buffer-string))
    (dbt-test-goto "customers.first_name")
    (dbt-test-keys "C-M-a")
    (should (looking-at-p "final as ("))
    (dbt-test-keys "C-M-e")
    ;; As everywhere in Emacs, point goes on to the next line.
    (should (equal (buffer-substring (line-beginning-position 0) (line-end-position 0)) ")"))
    (dbt-test-goto "customers.first_name")
    (dbt-test-keys "C-M-h")
    ;; As everywhere in Emacs, the region takes in the blank line before.
    (should (string-prefix-p "final as (" (string-trim-left (buffer-substring (region-beginning) (region-end)))))
    (should (string-suffix-p "{% endif %}\n)" (string-trim-right (buffer-substring (region-beginning) (region-end)))))
    (deactivate-mark)
    (dbt-test-goto "ref('stg_customers')")
    (dbt-test-keys "C-M-a")
    (should (looking-at-p "customers as ("))
    (dbt-test-keys "C-M-a")
    (should (looking-at-p "with orders as ("))))

(ert-deftest dbt-edit-moves-to-the-first-cte-from-the-header ()
  "From the header of a model, C-M-e and C-M-h reach the first CTE, not the last."
  (dbt-test-with-window (with-temp-buffer
                          (insert-file-contents (expand-file-name "models/marts/orders.sql" dbt-test-fixture))
                          (buffer-string))
    (dbt-test-goto "materialized")
    (dbt-test-keys "C-M-e")
    (should (looking-back "ref('stg_orders') }}\n)" (line-beginning-position 0)))
    (dbt-test-goto "materialized")
    (dbt-test-keys "C-x n d")
    (should (string-search "with orders as (" (buffer-string)))
    (should-not (string-search "customers as (" (buffer-string)))
    (widen))
  (dbt-test-with-window (concat "with\n\nsource as (\n\n    select * from {{ source('raw', 'orders') }}\n\n),\n\n"
                                "renamed as (\n\n    select\n        *\n\n    from source\n\n)\n\n"
                                "select * from renamed\n")
    (goto-char (point-min))
    (dbt-test-keys "C-M-e")
    (should (looking-back "^)" (line-beginning-position)))
    (should (looking-at-p ",\n\nrenamed"))
    (goto-char (point-min))
    (dbt-test-keys "C-M-h")
    (should (string-search "source as (" (buffer-substring (region-beginning) (region-end))))
    (should-not (string-search "renamed as (" (buffer-substring (region-beginning) (region-end))))))

(ert-deftest dbt-edit-gives-expreg-the-tag-the-block-and-the-cte ()
  "expreg gets the tag around point and its contents, its block, its CTE.
Each region is (NAME . (BEG . END)), as expreg reads them."
  (dbt-test-with-window "with a as (\n    select 1\n    {% if x %} {{ ref('b') }} {% endif %}\n)\nselect * from a\n"
    (dbt-test-goto "ref")
    (let ((regions (dbt-edit-expreg-regions)))
      (should (seq-every-p (lambda (region)
                             (and (symbolp (car region)) (integerp (cadr region)) (integerp (cddr region))))
                           regions))
      (let ((texts (mapcar (lambda (region) (buffer-substring-no-properties (cadr region) (cddr region))) regions)))
        (dolist (expected '("{{ ref('b') }}" " ref('b') " "{% if x %} {{ ref('b') }} {% endif %}"
                            " {{ ref('b') }} "))
          (should (member expected texts)))
        (should (seq-some (lambda (text) (string-prefix-p "with a as (" text)) texts))
        (should (seq-some (lambda (text) (string-prefix-p "\n    select 1" text)) texts)))))
  (dbt-test-with-window "with a as (\n    select 1\n"
    (dbt-test-goto "select")
    (should-not (dbt-edit-expreg-regions)))
  (with-temp-buffer
    (fundamental-mode)
    (should-not (dbt-edit-expreg-regions))))

(provide 'dbt-edit-test)
;;; dbt-edit-test.el ends here
