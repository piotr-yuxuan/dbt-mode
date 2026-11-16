;;; with-scratch-home-test.el --- Tests of scripts/with-scratch-home  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; Where scripts/with-scratch-home makes its scratch directory, and what
;; it leaves there.  The expectations come from its header comment.

;;; Code:

(require 'dbt-mode-test-helpers)

(ert-deftest with-scratch-home-keeps-a-failed-run-without-the-driver-cache ()
  "A failed run keeps its scratch directory, under /tmp whatever TMPDIR says, printed, but for the copy of the driver cache.
So does a run stopped by TERM or HUP, as by a deadline, with status 143
or 129.  A run that succeeds leaves nothing; a copy that fails runs
nothing and keeps nothing.  The suite runs in a scratch home, whose
TMPDIR is not /tmp."
  (let* ((kept nil)
         (tmp (make-temp-file "scratch-test" t))
         (cache (expand-file-name "cache/" tmp))
         (script (expand-file-name "../scripts/with-scratch-home" dbt-test-directory))
         (copied "test -f \"$XDG_CACHE_HOME/com.getdbt/driver\"")
         (run (lambda (&rest command)
                (with-temp-buffer
                  (let ((status (apply #'call-process script nil t nil "--dbt-cache" cache command)))
                    ;; Each directory kept is removed at the end, whatever fails.
                    (goto-char (point-min))
                    (when (re-search-forward "^with-scratch-home: kept \\(/tmp/dbt-mode-scratch\\.[^/\n]+\\)$"
                                             nil t)
                      (push (match-string 1) kept))
                    (list status (buffer-string)))))))
    (unwind-protect
        (progn
          (make-directory cache)
          (write-region "a driver" nil (expand-file-name "driver" cache))
          (pcase-dolist (`(,command ,expected)
                         `((,(concat copied " && exit 3") 3)
                           (,(concat copied " && kill -TERM $PPID && sleep 1") 143)
                           (,(concat copied " && kill -HUP $PPID && sleep 1") 129)))
            (pcase-let ((`(,status ,output) (funcall run "sh" "-c" command)))
              (should (equal status expected))
              (should (string-match-p "^with-scratch-home: kept /tmp/dbt-mode-scratch\\." output))
              (should (file-directory-p (expand-file-name "home" (car kept))))
              (should-not (file-exists-p (expand-file-name "cache/com.getdbt" (car kept))))))
          (should (file-exists-p (expand-file-name "driver" cache)))
          (pcase-let ((`(,status ,output) (funcall run "sh" "-c" (concat copied " && echo \"$HOME\""))))
            (should (= status 0))
            (should (string-match "\\`\\(/tmp/dbt-mode-scratch\\.[^/\n]+\\)/home\n\\'" output))
            (should-not (file-exists-p (match-string 1 output))))
          ;; An unreadable driver: the copy fails.
          (set-file-modes (expand-file-name "driver" cache) 0)
          (pcase-let ((`(,status ,output) (funcall run "sh" "-c" "echo ran")))
            (should (= status 2))
            (should-not (string-search "ran" output))
            (should-not (string-search "kept" output))))
      (dolist (directory kept)
        (delete-directory directory t))
      (delete-directory tmp t))))

(provide 'with-scratch-home-test)
;;; with-scratch-home-test.el ends here
