;;; tests/run-tests.el --- Test runner for rfcview.el test suite

;; Usage (from the project root):
;;   emacs -Q --batch -L . -l tests/run-tests.el

(add-to-list 'load-path ".")
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'ert)

(load (expand-file-name "test-rfcview-core.el"   (file-name-directory load-file-name)))
(load (expand-file-name "test-rfcview-index.el"  (file-name-directory load-file-name)))
(load (expand-file-name "test-rfcview-reader.el" (file-name-directory load-file-name)))

;; ert-run-tests-batch-and-exit has a bug in Emacs 29.x where it exits 0
;; even when tests fail (condition uses `or' instead of `and').  Use
;; ert-run-tests-batch directly so the exit code is reliable on all versions.
(let* ((stats (ert-run-tests-batch t))
       (unexpected (ert-stats-completed-unexpected stats)))
  (kill-emacs (if (zerop unexpected) 0 1)))

;;; run-tests.el ends here