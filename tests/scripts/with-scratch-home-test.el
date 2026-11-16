;;; with-scratch-home-test.el --- Tests of scripts/with-scratch-home  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

;;; Commentary:

;; Where scripts/with-scratch-home makes its scratch directory, and what
;; it leaves there.  The expectations come from its header comment.

;;; Code:

(require 'dbt-mode-test-helpers)

(ert-deftest with-scratch-home-keeps-a-failed-run-in-tmpdir ()
  "A scratch directory goes under TMPDIR; a failed run keeps it, printed, but for the copy of the driver cache.
A run that succeeds leaves nothing."
  (let* ((tmp (make-temp-file "scratch-test" t))
         (cache (expand-file-name "cache/" tmp))
         (script (expand-file-name "../scripts/with-scratch-home" dbt-test-directory))
         (process-environment (cons (concat "TMPDIR=" (expand-file-name "tmp/" tmp)) process-environment))
         (run (lambda (status)
                (with-temp-buffer
                  (list (call-process script nil t nil "--dbt-cache" cache
                                      "sh" "-c" (format "test -f \"$XDG_CACHE_HOME/com.getdbt/driver\" && exit %d"
                                                        status))
                        (buffer-string))))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "tmp/" tmp))
          (make-directory cache)
          (write-region "a driver" nil (expand-file-name "driver" cache))
          (pcase-let ((`(,status ,output) (funcall run 3)))
            (should (= status 3))
            (should (string-match "^with-scratch-home: kept \\(.+\\)$" output))
            (let ((kept (match-string 1 output)))
              (should (string-prefix-p (expand-file-name "tmp/" tmp) kept))
              (should (file-directory-p (expand-file-name "home" kept)))
              (should-not (file-exists-p (expand-file-name "cache/com.getdbt" kept)))
              (should (file-exists-p (expand-file-name "driver" cache)))
              (delete-directory kept t)))
          (should (equal (funcall run 0) '(0 "")))
          (should-not (directory-files (expand-file-name "tmp/" tmp) nil "\\`[^.]")))
      (delete-directory tmp t))))

(provide 'with-scratch-home-test)
;;; with-scratch-home-test.el ends here
