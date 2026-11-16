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

(defun dbt-test-indented (text)
  "Return TEXT as C-x h C-M-\\ indents it in dbt-mode."
  (dbt-test-with-window text
    (dbt-test-keys "C-x h C-M-\\")
    (buffer-string)))

(ert-deftest dbt-edit-indents-ctes-clauses-and-what-they-hold ()
  "A CTE's body is one step in from its line, its ) back there; a clause's lines one step in from it.
A line that a closed parenthesis follows is one step in from the clause
above it, and dbt-mode indents with spaces."
  (should (equal (dbt-test-indented
                  (concat "with orders as (\nselect * from x\n),\nfinal as (\nselect\nid,\ncoalesce(\na,\nb\n) as c,\n"
                          "name\nfrom orders\nwhere a = 1\nand b = 2\n)\nselect * from final\n"))
                 (concat "with orders as (\n    select * from x\n),\nfinal as (\n    select\n        id,\n"
                         "        coalesce(\n            a,\n            b\n        ) as c,\n        name\n"
                         "    from orders\n    where a = 1\n        and b = 2\n)\nselect * from final\n"))))

(ert-deftest dbt-edit-indents-the-jinja-blocks-of-a-model ()
  "A Jinja tag among a clause's lines is at their level, one before a clause at the clauses'; its block one step in.
Its end and else are at its column, as dbt Labs' style guide writes
Jinja."
  (should (equal (dbt-test-indented
                  (concat "select\nid,\n{% if x %}\nname,\n{% else %}\nother,\n{% endif %}\nlast\nfrom t\n"
                          "{% if is_incremental() %}\n-- new rows only\nwhere a > 1\n{% endif %}\n"))
                 (concat "select\n    id,\n    {% if x %}\n        name,\n    {% else %}\n        other,\n"
                         "    {% endif %}\n    last\nfrom t\n{% if is_incremental() %}\n    -- new rows only\n"
                         "    where a > 1\n{% endif %}\n")))
  (should (equal (dbt-test-indented "{% macro cents_to_dollars(col) %}\n({{ col }} / 100)::numeric(16, 2)\n{% endmacro %}\n")
                 "{% macro cents_to_dollars(col) %}\n    ({{ col }} / 100)::numeric(16, 2)\n{% endmacro %}\n"))
  (should (equal (dbt-test-indented "{{\nconfig(\nmaterialized='table'\n)\n}}\n")
                 "{{\n    config(\n        materialized='table'\n    )\n}}\n")))

(ert-deftest dbt-edit-indents-a-line-with-tab ()
  "TAB indents the line it is on; point keeps its place in the text, or goes to the indentation."
  (dbt-test-with-window "select\n    id,\n{% if x %}\n    name,\n        {% endif %}"
    (back-to-indentation)
    (dbt-test-keys "TAB")
    (should (equal (dbt-test-around-point) "select\n    id,\n{% if x %}\n    name,\n-!-{% endif %}"))
    (forward-line -1)
    (end-of-line)
    (insert "  ")
    (dbt-test-keys "TAB")
    (should (string-suffix-p "    name,  -!-\n{% endif %}" (dbt-test-around-point)))))

(ert-deftest dbt-edit-leaves-strings-and-comments-as-they-are ()
  "A line that starts within a string or a comment keeps its indentation, and starts no clause."
  (let ((text "select\n    'a\n  long text' as t,\n    /* a\n      comment */\n    id\nfrom x\n"))
    (should (equal (dbt-test-indented text) text)))
  (let ((text "with x as (\n    select\n        /* note\nfrom the old table\n*/\n        a,\n        b\n    from t\n)\n"))
    (should (equal (dbt-test-indented text) text))))

(ert-deftest dbt-edit-indents-the-fixture-as-it-is-written ()
  "The models of the fixture that follow dbt Labs' style keep their indentation.
orders.sql does not indent the body of its block; dbt-mode does."
  (dolist (file '("models/staging/stg_orders.sql" "models/staging/stg_customers.sql" "models/marts/customers.sql"))
    (let ((text (dbt-test-file-text dbt-test-fixture file)))
      (should (equal (dbt-test-indented text) text))))
  (let ((text (dbt-test-file-text dbt-test-fixture "models/marts/orders.sql")))
    (should (equal (dbt-test-indented text)
                   (string-replace "\n    where orders" "\n        where orders" text)))))

(ert-deftest dbt-edit-indents-a-large-model-at-once ()
  "C-x h C-M-\\ indents a model of 2,000 lines, with a Jinja block in each CTE, in under 2 s."
  (let ((text (with-temp-buffer
                (insert "with ")
                (dotimes (cte 40)
                  (insert (format "%scte_%d as (\nselect\n" (if (> cte 0) ",\n" "") cte))
                  (dotimes (column 40)
                    (insert (format "col_%d,\n" column)))
                  (insert "{% if is_incremental() %}\nupdated_at,\n{% endif %}\nid\nfrom {{ ref('x') }}\n)"))
                (insert "\nselect * from cte_39\n")
                (buffer-string))))
    (dbt-test-with-window text
      (let ((start (float-time)))
        (dbt-test-keys "C-x h C-M-\\")
        (should (< (- (float-time) start) 2.0)))
      (goto-char (point-min))
      (should (search-forward "\n        col_39,\n        {% if is_incremental() %}\n            updated_at,\n" nil t)))))

(ert-deftest dbt-edit-indents-a-wide-select-at-once ()
  "C-x h C-M-\\ indents a select of 1,000 columns, all in one clause, in under 2 s.
Each column's line is placed by the select it belongs to, however far
above."
  (let ((text (with-temp-buffer
                (insert "with renamed as (\nselect\n")
                (dotimes (column 1000)
                  (insert (format "col_%d as column_%d,\n" column column)))
                (insert "id\nfrom source\n)\nselect * from renamed\n")
                (buffer-string))))
    (dbt-test-with-window text
      (let ((start (float-time)))
        (dbt-test-keys "C-x h C-M-\\")
        (should (< (- (float-time) start) 2.0)))
      (goto-char (point-min))
      (should (search-forward "\n        col_999 as column_999,\n        id\n    from source\n)\n" nil t)))))

(ert-deftest dbt-edit-indents-within-a-tag-of-several-lines ()
  "TAB, RET and C-M-\\ indent the lines of a `{% %}' tag of several lines as those of its parenthesis or clause."
  (dbt-test-with-window "{%- set payment_methods = [\n'credit_card',\n'coupon',\n] -%}\nselect 1\n"
    (electric-indent-local-mode 1)
    (dbt-test-goto "'coupon'")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))
    (dbt-test-goto "] -%}")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 0))
    (dbt-test-goto "'coupon',")
    (end-of-line)
    (dbt-test-keys "RET")
    (should (= (current-column) 4))
    ;; A region that ends within the tag.
    (goto-char (point-min))
    (dbt-test-keys "C-SPC")
    (dbt-test-goto "'coupon'")
    (dbt-test-keys "C-M-\\")
    (should (equal (buffer-substring-no-properties (point-min) (point))
                   "{%- set payment_methods = [\n    'credit_card',\n    ")))
  (should (equal (dbt-test-indented "{% macro generate_alias_name(\ncustom_alias_name=none,\nnode=none\n) -%}\nselect 1\n{%- endmacro %}\n")
                 "{% macro generate_alias_name(\n    custom_alias_name=none,\n    node=none\n) -%}\n    select 1\n{%- endmacro %}\n"))
  (dbt-test-with-window "select *\nfrom t\nwhere a = 1\n{% if target.name == 'prod'\nand var('full_refresh', false) %}\n"
    (dbt-test-goto "and var")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4)))
  ;; Of several lines, a tag sits by what it leads to, as on one line.
  (should (equal (dbt-test-indented "select *\nfrom events\n{% if is_incremental()\nand not var('full') %}\nwhere updated_at > 1\n{% endif %}\n")
                 "select *\nfrom events\n{% if is_incremental()\n    and not var('full') %}\n    where updated_at > 1\n{% endif %}\n")))

(ert-deftest dbt-edit-indents-a-comment-as-the-code-after-it ()
  "A line that starts a comment indents as the code after it: before a clause, between CTEs, before the final select.
Its text is left as it is."
  (let ((text (concat "with orders as (\n    select * from x\n    -- only the paid ones\n    where status = 'paid'\n),\n\n"
                      "-- logical CTEs\n{# the final CTE #}\nfinal as (\n    select * from orders\n)\n\n"
                      "-- simple select statement\nselect * from final\n")))
    (should (equal (dbt-test-indented text) text))
    (should (equal (dbt-test-indented (replace-regexp-in-string "^ +" "" text)) text)))
  ;; Before a closer or at the end, a comment is one of the lines above.
  (let ((text "with a as (\n    select\n        id\n        -- more to come\n)\nselect\n    id\n    -- last\n"))
    (should (equal (dbt-test-indented (replace-regexp-in-string "^ +" "" text)) text))))

(ert-deftest dbt-edit-indents-an-else-branch-by-itself ()
  "A line in an else branch is one step in from it, whatever the if branch holds."
  (let ((text (concat "select *\nfrom events\n{% if is_incremental() %}\n    where updated_at > (select max(updated_at) from {{ this }})\n"
                      "{% else %}\n    -- a full refresh reads everything\n    {{ some_filter() }}\n{% endif %}\n")))
    (should (equal (dbt-test-indented (replace-regexp-in-string "^ +" "" text)) text))))

(ert-deftest dbt-edit-indents-a-window-frame-with-its-order-by ()
  "A window's partition and frame are at the level of its order by; a column named range is a column."
  (let ((text (concat "select\n    sum(amount) over (\n        partition by customer_id,\n            region\n        order by order_date\n"
                      "        rows between unbounded preceding and current row\n    ) as running_total,\n    range\n"
                      "from payments\n")))
    (should (equal (dbt-test-indented (replace-regexp-in-string "^ +" "" text)) text))))

(ert-deftest dbt-edit-keeps-left-and-right-among-columns ()
  "Columns that call left() or right() are columns, one step in from select; a join with left or natural is a clause."
  (let ((text (concat "select\n    id,\n    left(name, 3) as initials,\n    RIGHT (code, 2) as suffix,\n    name\n"
                      "from a\nleft outer join b on a.id = b.id\nnatural left join c\n")))
    (should (equal (dbt-test-indented (replace-regexp-in-string "^ +" "" text)) text))))

(ert-deftest dbt-edit-reindents-a-closer-as-it-is-typed ()
  "Typing `{% endif' or `)' at the start of a line moves it to its opener's column at once."
  (dbt-test-with-window "select\n    id,\n{% if x %}\n    name,\n    "
    (electric-indent-local-mode 1)
    (dbt-test-type "{% endif")
    (should (equal (dbt-test-around-point) "select\n    id,\n{% if x %}\n    name,\n{% endif-!- %}")))
  (dbt-test-with-window "with a as (\n    select 1\n    "
    (electric-indent-local-mode 1)
    (dbt-test-type "),")
    (should (equal (dbt-test-around-point) "with a as (\n    select 1\n),-!-")))
  (dbt-test-with-window "{% if x %}\n    select 1\n    "
    (electric-indent-local-mode 1)
    (dbt-test-type "{% else")
    (should (equal (dbt-test-around-point) "{% if x %}\n    select 1\n{% else-!- %}")))
  (dbt-test-with-window "{% set xs = [\n    1,\n    "
    (electric-indent-local-mode 1)
    (dbt-test-type "]")
    (should (equal (dbt-test-around-point) "{% set xs = [\n    1,\n]-!-")))
  ;; At the end of the buffer, a tag leads to what is typed next: a clause.
  (dbt-test-with-window "select *\nfrom t\n"
    (electric-indent-local-mode 1)
    (dbt-test-type "{% if is_incremental() %}")
    (should (equal (dbt-test-around-point) "select *\nfrom t\n{% if is_incremental() %}-!-"))))

(ert-deftest dbt-edit-keeps-the-states-of-its-scan ()
  "The Jinja blocks read from the states kept are those of a scan from the start; a scan near the start keeps the states further on."
  (let ((fresh (lambda (limit) (let ((dbt-edit--scans (list (list 0)))) (dbt-edit-blocks limit)))))
    (with-temp-buffer
      (dbt-mode)
      (insert "{% if a %}\n{% for b in c %}\nselect\n{% endif %}\nx\n")
      (dbt-edit-blocks (point-max))
      (let ((limit (progn (goto-char (point-min)) (search-forward "select") (point))))
        (should (equal (dbt-edit-blocks limit) (funcall fresh limit)))))
    (with-temp-buffer
      (dbt-mode)
      (dotimes (n 50)
        (insert (format "{%% macro m_%d() %%}\nselect %d\n{%% endmacro %%}\n" n n)))
      (dbt-edit-blocks (point-max))
      (let ((reached (caar dbt-edit--scans)))
        (should (> reached (/ (point-max) 2)))
        (dbt-edit-blocks 100)
        (should (= (caar dbt-edit--scans) reached))
        (should (equal (dbt-edit-blocks (point-max)) (funcall fresh (point-max))))))
    ;; A narrowing hides the {# that comments a tag out: its scan is not kept.
    (with-temp-buffer
      (dbt-mode)
      (insert "{# a\n{% if x %}\n#}\n{% for y in z %}\nselect\n")
      (dbt-edit-blocks (point-max))
      (narrow-to-region (progn (goto-char (point-min)) (forward-line 1) (point)) (point-max))
      (dbt-edit-blocks (point-max))
      (widen)
      (should (equal (dbt-edit-blocks (point-max)) (funcall fresh (point-max)))))))

(ert-deftest dbt-edit-indents-a-tag-before-a-long-run-of-lines ()
  "A tag looks past 50 tags, comments and blank lines at most for what it leads to; TAB on it never fails.
Beyond them, or before a tag that stays open, it is one of the lines
above."
  (let ((text (concat "select *\nfrom t\n{% if x %}\n" (apply #'concat (make-list 10 "    -- note\n"))
                      "    where a\n{% endif %}\n")))
    (should (equal (dbt-test-indented text) text)))
  (dbt-test-with-window (concat "select *\nfrom t\n{% if x %}\n" (make-string 100000 ?\n) "where a\n{% endif %}\n")
    (dbt-test-goto "{% if x")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4)))
  (dbt-test-with-window (concat "select *\nfrom t\n{% if x\n" (apply #'concat (make-list 20000 "and y = 1\n")))
    (dbt-test-goto "{% if x")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))))

(ert-deftest dbt-edit-indents-by-its-offset-a-region-alone ()
  "C-M-\\ indents the lines of its region alone, by `dbt-indent-offset'; a stray ) is at the first column."
  (let ((dbt-indent-offset 2))
    (should (equal (dbt-test-indented "with a as (\nselect\nid\nfrom t\n)\nselect * from a\n)\n")
                   "with a as (\n  select\n    id\n  from t\n)\nselect * from a\n)\n")))
  (dbt-test-with-window "  select\nid,\nname\n  from t\n"
    (dbt-test-goto "id,")
    (dbt-test-keys "C-SPC")
    (dbt-test-goto "  from")
    (dbt-test-keys "C-M-\\")
    (should (equal (buffer-string) "  select\n      id,\n      name\n  from t\n"))))

(ert-deftest dbt-edit-indents-the-buffer-as-it-is-now ()
  "TAB reads the Jinja blocks of the text as it is after an edit above it; in a narrowing, the blocks are its own.
TAB indents in the whole buffer, as Emacs widens it; C-c / reads the
narrowing alone."
  (dbt-test-with-window "{% if x %}\nselect\n{% set a = 1 %}\nfrom t\nwhere a\n"
    (dbt-test-goto "where")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))
    (goto-char (point-min))
    (dbt-test-keys "C-k C-k")
    (dbt-test-goto "where")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 0)))
  ;; A tag not closed yet opens no block, until its end is typed.
  (dbt-test-with-window "{% if x\nselect a\n"
    (should-error (dbt-test-keys "C-c /") :type 'user-error)
    (dbt-test-goto "if x" 4)
    (dbt-test-type " %}")
    (goto-char (point-max))
    (dbt-test-keys "C-c /")
    (should (equal (buffer-string) "{% if x %}\nselect a\n{% endif %}")))
  (dbt-test-with-window "{% macro m() %}\nselect\nid\nfrom t\n{% endmacro %}\n"
    (dbt-test-goto "from t")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))
    (narrow-to-region (progn (dbt-test-goto "select") (point)) (progn (dbt-test-goto "{% endmacro") (point)))
    (dbt-test-goto "from t")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))
    (goto-char (point-max))
    (should-error (dbt-test-keys "C-c /") :type 'user-error)
    (widen)
    (should (equal (buffer-string) "{% macro m() %}\nselect\nid\n    from t\n{% endmacro %}\n"))))

(ert-deftest dbt-edit-reads-the-plus-trim-marker ()
  "`{%+' opens, closes and splits blocks as `{%' and `{%-' do, as dbt reads it.
dbt renders `{%+ if false +%} x {%+ endif %}' as nothing; C-c /, C-c %,
C-x h C-M-\\ and typing a closer take its tags as tags."
  (dbt-test-with-window "{%+ if a %}\n    select 1\n"
    (dbt-test-keys "C-c /")
    (should (string-suffix-p "    select 1\n{% endif %}" (buffer-string))))
  (dbt-test-with-window "{%+ if a %}\nx\n{%+ endif +%}\n"
    (goto-char (point-min))
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{%\\+ endif")))
  (should (equal (dbt-test-indented "select\nid,\n{%+ if x %}\nname,\n{%+ else %}\nother,\n{%+ endif %}\nlast\nfrom t\n")
                 (concat "select\n    id,\n    {%+ if x %}\n        name,\n    {%+ else %}\n        other,\n"
                         "    {%+ endif %}\n    last\nfrom t\n")))
  (dbt-test-with-window "select\n    id,\n{%+ if x %}\n    name,\n    "
    (electric-indent-local-mode 1)
    (dbt-test-type "{%+ endif")
    (should (equal (dbt-test-around-point) "select\n    id,\n{%+ if x %}\n    name,\n{%+ endif-!- %}"))))

(ert-deftest dbt-edit-reads-tags-in-sql-comments ()
  "A tag in a SQL comment is a tag, as dbt renders it; only a `{# #}' comment hides one.
A `{#' opens that comment in a SQL comment or string too, up to its
first `#}', and a `{%' in the string of a tag is text."
  (dbt-test-with-window "select 1 as a\n-- {% if a %}\n, 2 as b\n"
    (dbt-test-keys "C-c /")
    ;; At the column of its opening tag, as C-c / places it.
    (should (string-suffix-p ", 2 as b\n   {% endif %}" (buffer-string))))
  (dbt-test-with-window "select 1 as a\n/* {% for x in y %} */\n, 2 as b\n"
    (dbt-test-keys "C-c /")
    (should (string-suffix-p ", 2 as b\n   {% endfor %}" (buffer-string))))
  (dbt-test-with-window "{% if a %}\nselect 1\n-- {% endif %}\n"
    (should-error (dbt-test-keys "C-c /") :type 'user-error)
    (goto-char (point-min))
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{% endif %}\n\\'")))
  (dolist (text '("select 1 as a -- {#\n{% if a %}\n#}\n"
                  "select '{#' as a\n{% if a %}\n#}' as b\n"
                  "select '{{ \"{% if a %}\" }}' as a\n"
                  "{% set x = \"{% if a %}{#\" %}\nselect 1\n"))
    (dbt-test-with-window text
      (should-error (dbt-test-keys "C-c /") :type 'user-error)))
  ;; The where that dbt renders only in an incremental run is in the if.
  (dbt-test-with-window "select *\nfrom t\n-- {% if is_incremental() %}\nwhere a > 1\n-- {% endif %}\norder by 1\n"
    (dbt-test-goto "where")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 4))
    (dbt-test-goto "order")
    (dbt-test-keys "TAB")
    (should (= (current-indentation) 0)))
  ;; A comment's end, deleted, hides what follows from the next command.
  (dbt-test-with-window "{# note #}\n{% if x %}\nselect 1\n"
    (dbt-test-keys "C-c /")
    (should (string-suffix-p "select 1\n{% endif %}" (buffer-string)))
    (dbt-test-goto "#}")
    (dbt-test-keys "C-d C-d")
    (goto-char (point-max))
    (insert "\n")
    (should-error (dbt-test-keys "C-c /") :type 'user-error)))

(ert-deftest dbt-edit-indents-a-tag-in-a-sql-comment-as-the-tag ()
  "A line whose SQL comment holds a tag sits as the tag would: its block, else and end as uncommented.
C-x h C-M-\\ leaves what it indented as it is, as TAB on each line does."
  (dolist (case '(("select *\nfrom t\n-- {% if is_incremental() %}\nwhere a > 1\n-- {% endif %}\norder by 1\n"
                   . "select *\nfrom t\n-- {% if is_incremental() %}\n    where a > 1\n-- {% endif %}\norder by 1\n")
                  ("select\nid\n-- {% if a %}\n, b\n/* {% else %} */\n, c\n-- {% endif %}\nfrom t\n"
                   . "select\n    id\n    -- {% if a %}\n        , b\n    /* {% else %} */\n        , c\n    -- {% endif %}\nfrom t\n")))
    (should (equal (dbt-test-indented (car case)) (cdr case)))
    (should (equal (dbt-test-indented (cdr case)) (cdr case)))
    (dbt-test-with-window (car case)
      (goto-char (point-min))
      (while (not (eobp))
        (dbt-test-keys "TAB")
        (forward-line 1))
      (should (equal (buffer-string) (cdr case)))))
  ;; A `{%' that closes no tag on the line leaves a comment line as one.
  (should (equal (dbt-test-indented "select\n    id\nfrom t\n-- a stray {% here\nwhere a\n")
                 "select\n    id\nfrom t\n-- a stray {% here\nwhere a\n")))

(ert-deftest dbt-edit-keeps-its-states-across-comments-and-raw ()
  "The blocks read from the states kept after a comment that runs past the limit are those of a scan from the start."
  (let ((fresh (lambda (limit) (let ((dbt-edit--scans (list (list 0)))) (dbt-edit-blocks limit)))))
    (with-temp-buffer
      (dbt-mode)
      (insert "{% if a %}\n{# x\n{% for b in c %}\n#}\n{% for d in e %}\nselect\n")
      (dbt-edit-blocks (progn (goto-char (point-min)) (search-forward "{# x") (point)))
      (should (equal (dbt-edit-blocks (point-max)) (funcall fresh (point-max))))
      (should (equal (mapcar #'car (cdr (dbt-edit-blocks (point-max)))) '("for" "if"))))
    ;; Resumed within a raw block, the scan still reads nothing but its end.
    (with-temp-buffer
      (dbt-mode)
      (insert "{% raw %}\n{% if a %}\n{% endraw %}\n{% for b in c %}\nselect\n")
      (dbt-edit-blocks (progn (goto-char (point-min)) (search-forward "{% raw %}\n") (point)))
      (should (equal (dbt-edit-blocks (point-max)) (funcall fresh (point-max))))
      (should (equal (mapcar #'car (cdr (dbt-edit-blocks (point-max)))) '("for"))))))

(ert-deftest dbt-edit-reads-nothing-in-a-raw-block-but-its-end ()
  "Within a raw block, dbt reads no tag but the first `{% endraw %}' as written, whatever the trim markers.
So C-c / closes the raw block, C-c % moves between its tags only, an
`if' within indents nothing after it and expreg offers no block within
it; `{% endraw x %}' ends nothing."
  (dbt-test-with-window "{% raw %}\n{% if a %}\n"
    (dbt-test-keys "C-c /")
    (should (string-suffix-p "{% if a %}\n{% endraw %}" (buffer-string)))
    (insert "\n")
    (should-error (dbt-test-keys "C-c /") :type 'user-error))
  (dbt-test-with-window "{%- raw -%}\n{% if a %}\n{%+ endraw +%}\n"
    (goto-char (point-min))
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{%\\+ endraw"))
    (dbt-test-goto "{% if")
    (should-error (dbt-test-keys "C-c %") :type 'user-error))
  (dbt-test-with-window "{% raw %}a{% endraw x %}{% if a %}{% endraw %}\n"
    (should-error (dbt-test-keys "C-c /") :type 'user-error)
    (goto-char (point-min))
    (dbt-test-keys "C-c %")
    (should (looking-at-p "{% endraw %}\n")))
  (should (equal (dbt-test-indented "{% raw %}{% if a %}{% endraw %}\nselect\nid\n")
                 "{% raw %}{% if a %}{% endraw %}\nselect\n    id\n"))
  (dbt-test-with-window "select 1\n{% raw %}{% if a %}x{% endif %}{% endraw %}\n"
    (dbt-test-goto "x{%")
    (should (equal (mapcar (lambda (region) (buffer-substring-no-properties (cadr region) (cddr region)))
                           (seq-filter (lambda (region) (eq (car region) 'dbt-block)) (dbt-edit-expreg-regions)))
                   '("{% raw %}{% if a %}x{% endif %}{% endraw %}")))))

(provide 'dbt-edit-test)
;;; dbt-edit-test.el ends here
