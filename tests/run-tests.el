;;; tests/run-tests.el --- Test runner for rfcview.el test suite

;; Usage (from the project root):
;;   emacs -Q --batch -L . -l tests/run-tests.el

(add-to-list 'load-path ".")
(add-to-list 'load-path (file-name-directory load-file-name))

(require 'ert)

(load (expand-file-name "test-rfcview-core.el"   (file-name-directory load-file-name)))
(load (expand-file-name "test-rfcview-index.el"  (file-name-directory load-file-name)))
(load (expand-file-name "test-rfcview-reader.el" (file-name-directory load-file-name)))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here