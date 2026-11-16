;;; dbt-lineage-test.el --- Tests of the lineage of dbt-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; The depths of the upstream and downstream of a node, and their table.

;;; Code:

(require 'dbt-mode-test-helpers)
(require 'dbt-lineage)
(require 'dbt-mode)

(defconst dbt-test-diamond
  '((:id "a" :name "a" :type seed :parents nil)
    (:id "b" :name "b" :type model :parents ("a"))
    (:id "c" :name "c" :type model :parents ("a" "b"))
    (:id "d" :name "d" :type model :parents ("b" "c"))
    (:id "e" :name "e" :type model :parents ("e" "d")))
  "A diamond with a shortcut, and a node that depends on itself.")

(ert-deftest dbt-lineage-reach-is-a-tree-of-shortest-paths ()
  "Each node comes once, at the length of its shortest path, under its nearest parent."
  (should (equal (dbt-lineage-reach dbt-test-diamond "d" 'upstream) '(("b" 1 "d") ("c" 1 "d") ("a" 2 "b"))))
  (should (equal (dbt-lineage-reach dbt-test-diamond "a" 'downstream)
                 '(("b" 1 "a") ("c" 1 "a") ("d" 2 "b") ("e" 3 "d"))))
  (should (equal (dbt-lineage-reach dbt-test-diamond "e" 'downstream) nil)))

(ert-deftest dbt-lineage-shows-thousands-of-nodes-at-once ()
  "The lineage of a project of 4,000 models shows at once, not in seconds.
Each model depends on the one before it and on the one at half its
rank, so that the first is upstream of all the others and the last
downstream of all of them."
  (let ((nodes (mapcar (lambda (rank)
                         (list :id (format "model.p.m%d" rank) :name (format "m%d" rank) :type 'model
                               :parents (and (> rank 0) (list (format "model.p.m%d" (1- rank))
                                                              (format "model.p.m%d" (/ rank 2))))))
                       (number-sequence 0 3999)))
        (start (float-time)))
    (dbt-lineage-show "/" "model.p.m0" nodes "a test" #'ignore)
    (dbt-lineage-show "/" "model.p.m3999" nodes "a test" #'ignore)
    (should (< (- (float-time) start) 1.5))
    (dolist (name '("m0" "m3999"))
      (with-current-buffer (format "*dbt lineage: %s*" name)
        (should (= (length (dbt-test-lineage-rows)) 3999))
        (kill-buffer)))))

(ert-deftest dbt-lineage-reads-the-project-again-on-g-only ()
  "C-c C-g without a server uses the project as last read; g reads it again."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (let ((graph (dbt-project-graph root)))
      (dbt-lineage "stg_orders")
      (should (eq (dbt-project-graph root) graph))
      (with-current-buffer "*dbt lineage: stg_orders*"
        (revert-buffer)
        (should-not (eq (dbt-project-graph root) graph))
        (kill-buffer)))))

(ert-deftest dbt-lineage-shows-the-project-lineage ()
  "C-c C-g without a server shows the upstream and downstream trees.
A node is under the node through which it is nearest."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-lineage "stg_orders")
    (with-current-buffer "*dbt lineage: stg_orders*"
      (should (equal (dbt-test-lineage-rows)
                     '(("upstream" "1" "seed" "raw_orders" "seeds/raw_orders.csv")
                       ("downstream" "1" "analysis" "order_counts" "analyses/order_counts.sql")
                       ("downstream" "1" "model" "orders" "models/marts/orders.sql")
                       ("downstream" "2" "model" "customers" "models/marts/customers.sql")
                       ("downstream" "1" "snapshot" "orders_snapshot" "snapshots/orders_snapshot.sql"))))
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (dbt-lineage-visit)
      (should (equal (file-relative-name buffer-file-name root) "seeds/raw_orders.csv"))
      (kill-buffer "*dbt lineage: stg_orders*"))))

(ert-deftest dbt-lineage-folds-limits-and-exports-the-tree ()
  "TAB folds a node; - and + show one level less and more, point on its node; e writes the graph as dot.
Each node of the graph is named by its unique id and labelled by its
name, so two nodes of one name stay apart."
  (dbt-test-with-project root
    (dbt-test-visit root "models/staging/stg_orders.sql")
    (dbt-lineage "stg_orders")
    (save-window-excursion
      (switch-to-buffer "*dbt lineage: stg_orders*")
      (goto-char (point-min))
      (search-forward "  orders  ")
      (beginning-of-line)
      (execute-kbd-macro (kbd "TAB"))
      (should (save-excursion (search-forward "customers") (outline-invisible-p)))
      (execute-kbd-macro (kbd "-"))
      (should-not (member "customers" (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
      (should (looking-at "  orders  "))
      (execute-kbd-macro (kbd "+"))
      (should (member "customers" (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
      (let ((file (expand-file-name "lineage.dot" root)))
        ;; No dot, whose rendering would outlive the project.
        (let ((exec-path nil))
          (dbt-lineage-export file))
        (let ((dot (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (dolist (line '("\"seed.shop.raw_orders\" -> \"model.shop.stg_orders\";"
                          "\"model.shop.stg_orders\" -> \"model.shop.orders\";"
                          "\"model.shop.orders\" -> \"model.shop.customers\";"
                          "\"model.shop.orders\" [label=\"orders\"];"))
            (should (string-search line dot)))))
      (kill-buffer))))

(ert-deftest dbt-lineage-renders-the-graph-while-emacs-goes-on ()
  "With dot, e renders an SVG while Emacs goes on, and says when it is there, or why not."
  (dbt-test-with-project root
    (let* ((bin (make-temp-file "dot" t))
           (dot (expand-file-name "dot" bin))
           (exec-path (cons bin exec-path))
           (messages (with-current-buffer (messages-buffer) (point-max-marker)))
           (said (lambda (regexp)
                   (dbt-test-wait (lambda () (with-current-buffer (messages-buffer)
                                               (save-excursion
                                                 (goto-char messages)
                                                 (re-search-forward regexp nil t))))))))
      (unwind-protect
          (progn
            ;; dot -Tsvg FILE -o SVG
            (write-region "#!/bin/sh\necho '<svg/>' > \"$4\"\n" nil dot)
            (set-file-modes dot #o755)
            (dbt-test-visit root "models/staging/stg_orders.sql")
            (dbt-lineage "stg_orders")
            (with-current-buffer "*dbt lineage: stg_orders*"
              (dbt-lineage-export (expand-file-name "lineage.dot" root))
              (funcall said "^Wrote .*/lineage\\.svg$")
              (should (file-exists-p (expand-file-name "lineage.svg" root)))
              ;; Emacs goes on while dot renders.
              (write-region "#!/bin/sh\nsleep 1\necho '<svg/>' > \"$4\"\n" nil dot)
              (delete-file (expand-file-name "lineage.svg" root))
              (dbt-lineage-export (expand-file-name "lineage.dot" root))
              (should-not (file-exists-p (expand-file-name "lineage.svg" root)))
              (dbt-test-wait (lambda () (file-exists-p (expand-file-name "lineage.svg" root))))
              (write-region "#!/bin/sh\necho 'syntax error in line 3' >&2\nexit 1\n" nil dot)
              (dbt-lineage-export (expand-file-name "lineage.dot" root))
              (funcall said "^dot failed on .*/lineage\\.dot: syntax error in line 3$")
              (kill-buffer)))
        (delete-directory bin t)))))

;; A graph that a server gives level by level: the focus f depends on a
;; seed s and a macro m, which calls the macro n; a, b and c depend on f,
;; and d on a.
(defconst dbt-test-levels
  '((upstream ("model.p.f" "seed.p.s" "macro.p.m") ("macro.p.m" "macro.p.n"))
    (downstream ("model.p.f" "model.p.a" "model.p.b" "model.p.c") ("model.p.a" "model.p.d")))
  "What each node calls, upstream, and what calls it, downstream.")

(defun dbt-test-level-calls (levels asked &optional pending)
  "Return a function that answers the calls of a level from LEVELS.
LEVELS is as `dbt-test-levels', the answers as `dbt-lineage--calls'
says.  It pushes each level asked, (DIRECTION . IDS), on the car of the
cons ASKED.  It answers at once, or, when PENDING is a cons, pushes on
its car a function that answers."
  (lambda (ids direction callback)
    (push (cons direction ids) (car asked))
    (let ((answer (lambda ()
                    (funcall callback
                             (mapcan (lambda (id)
                                       (mapcar (lambda (other)
                                                 (list (list :id other :name (dbt-lineage-label other)
                                                             :type (intern (car (split-string other "\\."))))
                                                       (if (eq direction 'upstream) id other)
                                                       (if (eq direction 'upstream) other id)))
                                               (cdr (assoc id (alist-get direction levels)))))
                                     ids)))))
      (if pending
          (push answer (car pending))
        (funcall answer)))))

(ert-deftest dbt-lineage-asks-a-server-level-by-level ()
  "A lineage that a server gives level by level opens at depth 2; + asks one level more past the last asked.
- redraws without asking; m shows the macros, hidden at first; past the
last level, + shows every depth.  g asks again, to the depth shown."
  (let* ((asked (list nil))
         (focus (list (list :id "model.p.f" :name "f" :type 'model)))
         (calls (dbt-test-level-calls dbt-test-levels asked)))
    (dbt-lineage-show "/" "model.p.f" focus "a test"
                      (lambda () (dbt-lineage-show "/" "model.p.f" focus "a test" #'ignore calls))
                      calls)
    (save-window-excursion
      (switch-to-buffer "*dbt lineage: f*")
      (should (equal (dbt-test-lineage-rows)
                     '(("upstream" "1" "seed" "s" "")
                       ("downstream" "1" "model" "a" "")
                       ("downstream" "2" "model" "d" "")
                       ("downstream" "1" "model" "b" "")
                       ("downstream" "1" "model" "c" ""))))
      (should (equal header-line-format " Lineage of f, to depth 2"))
      (should (equal (reverse (car asked))
                     '((upstream "model.p.f") (upstream "seed.p.s" "macro.p.m")
                       (downstream "model.p.f") (downstream "model.p.a" "model.p.b" "model.p.c"))))
      (execute-kbd-macro (kbd "m"))
      (should (equal (seq-filter (lambda (row) (equal (car row) "upstream")) (dbt-test-lineage-rows))
                     '(("upstream" "1" "seed" "s" "")
                       ("upstream" "1" "macro" "m" "")
                       ("upstream" "2" "macro" "n" ""))))
      (execute-kbd-macro (kbd "m"))
      (should-not (member "m" (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows))))
      (setcar asked nil)
      (execute-kbd-macro (kbd "-"))
      (should (equal header-line-format " Lineage of f, to depth 1"))
      (should-not (car asked))
      (execute-kbd-macro (kbd "+"))
      (should (equal header-line-format " Lineage of f, to depth 2"))
      (should-not (car asked))
      ;; Level 3: n and d call nothing more, so the lineage ends there.
      (execute-kbd-macro (kbd "+"))
      (should (equal (reverse (car asked)) '((upstream "macro.p.n") (downstream "model.p.d"))))
      (should (equal header-line-format " Lineage of f, to depth 3"))
      (setcar asked nil)
      (execute-kbd-macro (kbd "+"))
      (should (equal header-line-format " Lineage of f, at every depth"))
      (should-not (car asked))
      (execute-kbd-macro (kbd "-"))
      (execute-kbd-macro (kbd "g"))
      (should (equal (length (car asked)) 4))
      (should (equal header-line-format " Lineage of f, to depth 2"))
      (kill-buffer))))

(ert-deftest dbt-lineage-stops-a-level-past-its-limit ()
  "A level of more nodes than `dbt-lineage-level-limit' is the last asked in its direction, as the header line says."
  (let ((asked (list nil))
        (dbt-lineage-level-limit 2))
    (dbt-lineage-show "/" "model.p.f" (list (list :id "model.p.f" :name "f" :type 'model)) "a test" #'ignore
                      (dbt-test-level-calls dbt-test-levels asked))
    (with-current-buffer "*dbt lineage: f*"
      (should (equal (dbt-test-lineage-rows)
                     '(("upstream" "1" "seed" "s" "")
                       ("downstream" "1" "model" "a" "")
                       ("downstream" "1" "model" "b" "")
                       ("downstream" "1" "model" "c" ""))))
      (should (equal header-line-format
                     " Lineage of f, to depth 2; downstream stops at depth 1: its 3 nodes are more than dbt-lineage-level-limit, 2"))
      (should (equal (mapcar #'car (reverse (car asked))) '(upstream upstream downstream)))
      (kill-buffer))))

(defun dbt-test-answer-pending (pending)
  "Give the answers on the car of the cons PENDING, oldest first, until none is left.
The answers that they ask are given in turn."
  (while (car pending)
    (let ((answers (reverse (car pending))))
      (setcar pending nil)
      (mapc #'funcall answers))))

(ert-deftest dbt-lineage-keeps-the-depth-chosen-while-a-level-is-asked ()
  "- while a level is asked keeps its depth once the level comes, though it ends the lineage."
  (let ((asked (list nil))
        (pending (list nil)))
    (dbt-lineage-show "/" "model.p.f" (list (list :id "model.p.f" :name "f" :type 'model)) "a test" #'ignore
                      (dbt-test-level-calls dbt-test-levels asked pending))
    (dbt-test-answer-pending pending)
    (save-window-excursion
      (switch-to-buffer "*dbt lineage: f*")
      (execute-kbd-macro (kbd "+"))
      (execute-kbd-macro (kbd "-"))
      (execute-kbd-macro (kbd "-"))
      (dbt-test-answer-pending pending)
      (should (equal header-line-format " Lineage of f, to depth 1"))
      (kill-buffer))))

(ert-deftest dbt-lineage-asks-no-level-again-while-it-is-asked ()
  "+ while a level is asked asks it no more: its answer asks the next level, as deep as shown."
  (let ((asked (list nil))
        (pending (list nil)))
    (dbt-lineage-show "/" "model.p.f" (list (list :id "model.p.f" :name "f" :type 'model)) "a test" #'ignore
                      (dbt-test-level-calls dbt-test-levels asked pending))
    ;; The first level only, which asks the second.
    (let ((first (reverse (car pending))))
      (setcar pending nil)
      (mapc #'funcall first))
    (save-window-excursion
      (switch-to-buffer "*dbt lineage: f*")
      (should (equal (length (car asked)) 4))
      (execute-kbd-macro (kbd "+"))
      (should (equal (length (car asked)) 4))
      (dbt-test-answer-pending pending)
      (should (equal (reverse (car asked))
                     '((upstream "model.p.f") (downstream "model.p.f")
                       (upstream "seed.p.s" "macro.p.m") (downstream "model.p.a" "model.p.b" "model.p.c")
                       (upstream "macro.p.n") (downstream "model.p.d"))))
      (should (equal header-line-format " Lineage of f, to depth 3"))
      (kill-buffer))))

(ert-deftest dbt-lineage-says-where-plus-stops ()
  "+ that comes to a level past `dbt-lineage-level-limit' says so in the header line."
  (let ((asked (list nil))
        (dbt-lineage-level-limit 2))
    (dbt-lineage-show "/" "model.p.f" (list (list :id "model.p.f" :name "f" :type 'model)) "a test" #'ignore
                      (dbt-test-level-calls '((upstream)
                                              (downstream ("model.p.f" "model.p.a")
                                                          ("model.p.a" "model.p.b" "model.p.c" "model.p.d")))
                                            asked))
    (save-window-excursion
      (switch-to-buffer "*dbt lineage: f*")
      (should (equal header-line-format " Lineage of f, to depth 2"))
      (execute-kbd-macro (kbd "+"))
      (should (equal header-line-format
                     " Lineage of f, to depth 3; downstream stops at depth 2: its 3 nodes are more than dbt-lineage-level-limit, 2"))
      (kill-buffer))))

(ert-deftest dbt-lineage-drops-the-answers-of-a-level-asked-again ()
  "Asked again before its answers come, the lineage keeps the answers of the last asking only.
The first asking would have found a seed old upstream."
  (let ((asked (list nil))
        (pending (list nil))
        (focus (list (list :id "model.p.f" :name "f" :type 'model))))
    (dbt-lineage-show "/" "model.p.f" focus "a test" #'ignore
                      (dbt-test-level-calls '((upstream ("model.p.f" "seed.p.old")) (downstream)) asked pending))
    (let ((stale (car pending)))
      (setcar pending nil)
      (dbt-lineage-show "/" "model.p.f" focus "a test" #'ignore (dbt-test-level-calls dbt-test-levels asked pending))
      ;; The first level of the second asking, which asks the second.
      (let ((first (car pending)))
        (setcar pending nil)
        (mapc #'funcall first))
      (mapc #'funcall stale)
      (mapc #'funcall (car pending)))
    (with-current-buffer "*dbt lineage: f*"
      (should (equal (mapcar (lambda (row) (nth 3 row)) (dbt-test-lineage-rows)) '("s" "a" "d" "b" "c")))
      (should (equal (length (car asked)) 6))
      (kill-buffer))))

(provide 'dbt-lineage-test)
;;; dbt-lineage-test.el ends here
