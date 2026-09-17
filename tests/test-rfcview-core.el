;;; tests/test-rfcview-core.el --- ERT tests for rfcview-core.el

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(require 'rfcview-core)

;;; rfcview:debug

(ert-deftest rfcview:test-debug-silent-when-disabled ()
  "rfcview:debug produces no output when rfcview:use-debug is nil."
  (let ((rfcview:use-debug nil)
        calls)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args calls))))
      (rfcview:debug "hello %s" "world")
      (should (null calls)))))

(ert-deftest rfcview:test-debug-calls-message-when-enabled ()
  "rfcview:debug forwards format+args to message when rfcview:use-debug is t."
  (let ((rfcview:use-debug t)
        calls)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args calls))))
      (rfcview:debug "test %d" 42)
      (should (= 1 (length calls)))
      (should (equal '("test %d" 42) (car calls))))))

(ert-deftest rfcview:test-debug-multiple-args ()
  "rfcview:debug passes all variadic args through to message."
  (let ((rfcview:use-debug t)
        calls)
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args) (push args calls))))
      (rfcview:debug "%s %s %s" "a" "b" "c")
      (should (equal '("%s %s %s" "a" "b" "c") (car calls))))))

;;; rfcview:load-cache-internal

(ert-deftest rfcview:test-load-cache-internal-returns-nil-for-missing-file ()
  "Returns nil when the cache file does not exist."
  (should (null (rfcview:load-cache-internal "/this/path/does/not/exist/.cache"))))

(ert-deftest rfcview:test-load-cache-internal-reads-sexp-from-file ()
  "Reads and parses a valid sexp from a file."
  (let ((tmp (make-temp-file "rfcview-test-cache-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "(foo bar 42)"))
          (should (equal '(foo bar 42)
                         (rfcview:load-cache-internal tmp))))
      (delete-file tmp))))

(ert-deftest rfcview:test-load-cache-internal-reads-plist ()
  "Reads a plist structure correctly."
  (let ((tmp (make-temp-file "rfcview-test-cache-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (prin1-to-string '(:key "value" :num 7))))
          (should (equal '(:key "value" :num 7)
                         (rfcview:load-cache-internal tmp))))
      (delete-file tmp))))

;;; rfcview:load-cache

(ert-deftest rfcview:test-load-cache-uses-default-when-file-missing ()
  "Sets rfcview:rfc-cache to rfcview:rfc-cache-default when cache file is absent."
  (let ((rfcview:parsed-index-cache-file "/no/such/file")
        rfcview:rfc-cache)
    (cl-letf (((symbol-function 'rfcview:load-cache-internal) (lambda (_) nil)))
      (rfcview:load-cache)
      (should (equal rfcview:rfc-cache rfcview:rfc-cache-default)))))

(ert-deftest rfcview:test-load-cache-uses-file-contents-when-available ()
  "Sets rfcview:rfc-cache to the value read from the cache file
when its :version matches `rfcview:rfc-cache-version'."
  (let* ((expected `(:version ,rfcview:rfc-cache-version
                     :token "etag-12345"
                     :table nil :favorite (3) :recent nil))
         rfcview:rfc-cache)
    (cl-letf (((symbol-function 'rfcview:load-cache-internal)
               (lambda (_) expected)))
      (rfcview:load-cache)
      (should (equal rfcview:rfc-cache expected)))))

(ert-deftest rfcview:test-rfc-cache-default-carries-current-version ()
  "The default cache must advertise the current schema version so a
fresh cache survives a reload without triggering migration."
  (should (equal (plist-get rfcview:rfc-cache-default :version)
                 rfcview:rfc-cache-version)))

(ert-deftest rfcview:test-load-cache-migrates-stale-version ()
  "A cache file whose :version differs from
`rfcview:rfc-cache-version' is run through `rfcview:update-cache':
favorites and recents survive, the table and token are reset so the
next index refresh rebuilds them."
  (let* ((stale `(:version ,(1- rfcview:rfc-cache-version)
                  :token "etag-12345"
                  :table         ,(make-hash-table)
                  :favorite      (3 7 42)
                  :recent        (5 7)))
         rfcview:rfc-cache)
    (cl-letf (((symbol-function 'rfcview:load-cache-internal)
               (lambda (_) stale)))
      (rfcview:load-cache)
      (should (equal (plist-get rfcview:rfc-cache :version)
                     rfcview:rfc-cache-version))
      (should (equal (plist-get rfcview:rfc-cache :favorite) '(3 7 42)))
      (should (equal (plist-get rfcview:rfc-cache :recent)   '(5 7)))
      (should (equal (plist-get rfcview:rfc-cache :token)
                     (plist-get rfcview:rfc-cache-default :token)))
      (should (null (plist-get rfcview:rfc-cache :table))))))

(ert-deftest rfcview:test-load-cache-migrates-pre-versioning-cache ()
  "An on-disk cache from before versioning (no :version key) is
treated as stale and run through `rfcview:update-cache'.  Covers
the upgrade path for users with an existing cache on disk."
  (let ((legacy '(:token "etag-12345"
                  :table         nil
                  :favorite      (1 2)
                  :recent        (9)))
        rfcview:rfc-cache)
    (cl-letf (((symbol-function 'rfcview:load-cache-internal)
               (lambda (_) legacy)))
      (rfcview:load-cache)
      (should (equal (plist-get rfcview:rfc-cache :version)
                     rfcview:rfc-cache-version))
      (should (equal (plist-get rfcview:rfc-cache :favorite) '(1 2)))
      (should (equal (plist-get rfcview:rfc-cache :recent)   '(9))))))

;;; rfcview:update-cache

(ert-deftest rfcview:test-update-cache-preserves-favorite-and-recent ()
  "Direct test on the migration function: favorites and recents in
`rfcview:rfc-cache' are carried forward verbatim."
  (let ((rfcview:rfc-cache '(:version  0
                             :table    (some old shape)
                             :favorite (10 20 30)
                             :recent   (40 50))))
    (rfcview:update-cache 0)
    (should (equal (plist-get rfcview:rfc-cache :favorite) '(10 20 30)))
    (should (equal (plist-get rfcview:rfc-cache :recent)   '(40 50)))))

(ert-deftest rfcview:test-update-cache-resets-non-preserved-slots ()
  "Non-preserved slots (`:table' and other ad-hoc keys) must be
dropped so the next index refresh rebuilds from scratch."
  (let ((rfcview:rfc-cache `(:version       0
                             :token "etag-99999"
                             :table         ,(make-hash-table)
                             :favorite      (1)
                             :recent        (2)
                             :legacy-key    "junk")))
    (rfcview:update-cache 0)
    (should (null (plist-get rfcview:rfc-cache :table)))
    (should (null (plist-get rfcview:rfc-cache :legacy-key)))
    (should (equal (plist-get rfcview:rfc-cache :token)
                   (plist-get rfcview:rfc-cache-default :token)))
    (should (equal (plist-get rfcview:rfc-cache :version)
                   rfcview:rfc-cache-version))))

(ert-deftest rfcview:test-update-cache-handles-nil-old-version ()
  "Pre-versioning caches carry no :version key.  The migration must
accept nil for OLD-VERSION without erroring."
  (let ((rfcview:rfc-cache '(:token "etag-1"
                             :favorite (4)
                             :recent   (5))))
    (rfcview:update-cache nil)
    (should (equal (plist-get rfcview:rfc-cache :favorite) '(4)))
    (should (equal (plist-get rfcview:rfc-cache :recent)   '(5)))))

;;; rfcview:save-cache

(ert-deftest rfcview:test-save-cache-writes-readable-sexp ()
  "rfcview:save-cache writes rfcview:rfc-cache as a readable sexp."
  (let ((tmp (make-temp-file "rfcview-test-cache-"))
        (rfcview:rfc-cache '(:token "etag-0" :table nil :favorite (1 2) :recent (3)))
        (rfcview:parsed-index-cache-file nil))
    (setq rfcview:parsed-index-cache-file tmp)
    (unwind-protect
        (progn
          (rfcview:save-cache)
          (should (file-exists-p tmp))
          (let ((contents (rfcview:load-cache-internal tmp)))
            (should (equal contents rfcview:rfc-cache))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest rfcview:test-save-cache-roundtrip ()
  "Cache data survives a save/load roundtrip intact."
  (let* ((tbl (make-hash-table :test 'equal))
         (cache (list :token "etag-100-200"
                      :table tbl
                      :favorite '(42 793)
                      :recent '(2616)))
         (rfcview:rfc-cache cache)
         (tmp (make-temp-file "rfcview-test-cache-"))
         (rfcview:parsed-index-cache-file tmp))
    (unwind-protect
        (progn
          (rfcview:save-cache)
          (let ((loaded (rfcview:load-cache-internal tmp)))
            (should (equal (plist-get loaded :token)
                           (plist-get cache :token)))
            (should (equal (plist-get loaded :favorite)
                           (plist-get cache :favorite)))
            (should (equal (plist-get loaded :recent)
                           (plist-get cache :recent)))))
      (ignore-errors (delete-file tmp)))))

;;; rfcview:wrap-text-at-word-boundary

(ert-deftest rfcview:test-wrap-short-text-returned-as-is ()
  "Short text that fits within max-width is returned with spaces normalised."
  (let ((result (rfcview:wrap-text-at-word-boundary "Hello world" 6 80)))
    (should (string= "Hello world" result))))

(ert-deftest rfcview:test-wrap-long-text-each-line-within-max-width ()
  "Each line of wrapped output is no longer than max-width."
  (let* ((text "one two three four five six seven eight nine ten eleven twelve")
         (result (rfcview:wrap-text-at-word-boundary text 0 20)))
    (dolist (line (split-string result "\n"))
      (should (<= (length line) 20)))))

(ert-deftest rfcview:test-wrap-preserves-all-words ()
  "No word is lost during wrapping."
  (let* ((words '("alpha" "beta" "gamma" "delta" "epsilon" "zeta"))
         (text (mapconcat #'identity words " "))
         (result (rfcview:wrap-text-at-word-boundary text 0 12)))
    (dolist (w words)
      (should (string-match-p w result)))))

(ert-deftest rfcview:test-wrap-continuation-lines-indented-by-margin ()
  "Continuation lines start with margin-width spaces."
  (let* ((text "one two three four five six seven eight nine ten")
         (margin 4)
         (result (rfcview:wrap-text-at-word-boundary text margin 15)))
    (let ((continuation (cdr (split-string result "\n"))))
      (dolist (line continuation)
        (when (> (length line) 0)
          (should (string-match-p "^    " line)))))))

(ert-deftest rfcview:test-wrap-custom-delimiter ()
  "Wrapping uses the custom delimiter to split words."
  (let* ((text "a'b'c'd'e'f'g'h'i'j'k")
         (result (rfcview:wrap-text-at-word-boundary text 0 5 "'")))
    (dolist (ch '("a" "b" "c"))
      (should (string-match-p ch result)))))

(ert-deftest rfcview:test-wrap-empty-string ()
  "Wrapping an empty string returns an empty string."
  (let ((result (rfcview:wrap-text-at-word-boundary "" 0 80)))
    (should (string= "" result))))

(ert-deftest rfcview:test-wrap-single-word-short ()
  "A single word that fits is returned unchanged."
  (let ((result (rfcview:wrap-text-at-word-boundary "hello" 0 80)))
    (should (string= "hello" result))))

(provide 'test-rfcview-core)
;;; test-rfcview-core.el ends here
