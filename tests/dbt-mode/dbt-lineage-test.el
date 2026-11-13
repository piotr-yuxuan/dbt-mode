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

(provide 'dbt-lineage-test)
;;; dbt-lineage-test.el ends here
