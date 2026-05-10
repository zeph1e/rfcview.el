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

;;; rfcview:http-response-status

(ert-deftest rfcview:test-http-response-status-200 ()
  "Returns 200 for an HTTP/1.1 200 response."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n")
    (should (= 200 (rfcview:http-response-status (current-buffer))))))

(ert-deftest rfcview:test-http-response-status-404 ()
  "Returns 404 for an HTTP/1.0 404 response."
  (with-temp-buffer
    (insert "HTTP/1.0 404 Not Found\r\n\r\n")
    (should (= 404 (rfcview:http-response-status (current-buffer))))))

(ert-deftest rfcview:test-http-response-status-304 ()
  "Returns 304 for an HTTP/1.1 304 response."
  (with-temp-buffer
    (insert "HTTP/1.1 304 Not Modified\r\n\r\n")
    (should (= 304 (rfcview:http-response-status (current-buffer))))))

(ert-deftest rfcview:test-http-response-status-prefers-bound-variable ()
  "Returns url-http-response-status when it is already bound in the buffer."
  (with-temp-buffer
    (setq-local url-http-response-status 200)
    (should (= 200 (rfcview:http-response-status (current-buffer))))))

(ert-deftest rfcview:test-http-response-status-nil-when-no-status ()
  "Returns nil when the buffer contains no HTTP status line."
  (with-temp-buffer
    (insert "Not an HTTP response\n")
    (should (null (rfcview:http-response-status (current-buffer))))))

(ert-deftest rfcview:test-http-response-status-http2 ()
  "Parses status from HTTP/2 response lines."
  (with-temp-buffer
    (insert "HTTP/2 200\r\nContent-Type: text/plain\r\n\r\n")
    (should (= 200 (rfcview:http-response-status (current-buffer))))))

;;; rfcview:retrieve-rfc

(ert-deftest rfcview:test-retrieve-rfc-rejects-non-number ()
  "rfcview:retrieve-rfc signals an error when NUMBER is not numeric."
  (should-error (rfcview:retrieve-rfc "not-a-number")))

(ert-deftest rfcview:test-retrieve-rfc-builds-txt-url ()
  "rfcview:retrieve-rfc calls rfcview:retrieve with rfc<N>.txt URL."
  (let (captured)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (url &optional _method) (setq captured url))))
      (rfcview:retrieve-rfc 793)
      (should (string-match-p "rfc793\\.txt" captured)))))

(ert-deftest rfcview:test-retrieve-rfc-builds-pdf-url ()
  "rfcview:retrieve-rfc builds a .pdf URL when format is 'pdf."
  (let (captured)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (url &optional _method) (setq captured url))))
      (rfcview:retrieve-rfc 793 'pdf)
      (should (string-match-p "rfc793\\.pdf" captured)))))

(ert-deftest rfcview:test-retrieve-rfc-uses-base-url ()
  "rfcview:retrieve-rfc includes rfcview:rfc-base-url in the URL."
  (let ((rfcview:rfc-base-url "http://example.com/rfc/")
        captured)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (url &optional _method) (setq captured url))))
      (rfcview:retrieve-rfc 1)
      (should (string-prefix-p "http://example.com/rfc/" captured)))))

;;; rfcview:retrieve-index

(ert-deftest rfcview:test-retrieve-index-calls-retrieve ()
  "rfcview:retrieve-index calls rfcview:retrieve with the index URL."
  (let (captured-url captured-method)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (url &optional method)
                 (setq captured-url url captured-method method))))
      (rfcview:retrieve-index)
      (should (string= rfcview:rfc-index-url captured-url))
      (should (null captured-method)))))

(ert-deftest rfcview:test-retrieve-index-passes-method ()
  "rfcview:retrieve-index passes the optional METHOD to rfcview:retrieve."
  (let (captured-method)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (_url &optional method) (setq captured-method method))))
      (rfcview:retrieve-index "HEAD")
      (should (string= "HEAD" captured-method)))))

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
  "Sets rfcview:rfc-cache to the value read from the cache file."
  (let ((expected '(:last-modified (12345 0) :table nil :favorite (3) :recent nil))
        rfcview:rfc-cache)
    (cl-letf (((symbol-function 'rfcview:load-cache-internal)
               (lambda (_) expected)))
      (rfcview:load-cache)
      (should (equal rfcview:rfc-cache expected)))))

;;; rfcview:save-cache

(ert-deftest rfcview:test-save-cache-writes-readable-sexp ()
  "rfcview:save-cache writes rfcview:rfc-cache as a readable sexp."
  (let ((tmp (make-temp-file "rfcview-test-cache-"))
        (rfcview:rfc-cache '(:last-modified (0 0) :table nil :favorite (1 2) :recent (3)))
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
         (cache (list :last-modified '(100 200)
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
            (should (equal (plist-get loaded :last-modified)
                           (plist-get cache :last-modified)))
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
