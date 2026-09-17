;;; tests/test-rfcview-transport.el --- ERT tests for rfcview-transport.el -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(require 'rfcview-transport)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun rfcview-test:make-http-response-buffer (status &optional headers body)
  "Return a temp buffer shaped like an HTTP response.
STATUS is the numeric status code.  HEADERS is an alist of
\(NAME . VALUE) extra header lines.  BODY, if given, follows the
blank line separator."
  (let ((buf (generate-new-buffer " *rfcview-transport-test*")))
    (with-current-buffer buf
      (set-buffer-multibyte t)
      (insert (format "HTTP/1.1 %d OK\n" status))
      (dolist (h headers)
        (insert (format "%s: %s\n" (car h) (cdr h))))
      (insert "\n")
      (when body (insert body)))
    buf))

;;; ─── rfcview:transport--http-header ──────────────────────────────────────────

(ert-deftest rfcview:test-transport-http-header-returns-value ()
  (let ((buf (rfcview-test:make-http-response-buffer
              200 '(("ETag" . "\"abc123\"")))))
    (unwind-protect
        (should (string= "\"abc123\""
                         (rfcview:transport--http-header buf "ETag")))
      (kill-buffer buf))))

(ert-deftest rfcview:test-transport-http-header-case-insensitive ()
  (let ((buf (rfcview-test:make-http-response-buffer
              200 '(("etag" . "\"abc123\"")))))
    (unwind-protect
        (should (string= "\"abc123\""
                         (rfcview:transport--http-header buf "ETag")))
      (kill-buffer buf))))

(ert-deftest rfcview:test-transport-http-header-nil-when-absent ()
  (let ((buf (rfcview-test:make-http-response-buffer 200)))
    (unwind-protect
        (should (null (rfcview:transport--http-header buf "ETag")))
      (kill-buffer buf))))

;;; ─── rfcview:transport--http-fetch ───────────────────────────────────────────

(ert-deftest rfcview:test-transport-http-fetch-found-200 ()
  "A 200 GET response is :found t, with :token from ETag and a
body-only :buffer."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (_url &optional _method)
               (rfcview-test:make-http-response-buffer
                200 '(("ETag" . "etag-1")) "body text\n"))))
    (let* ((result (rfcview:transport--http-fetch "http://example.com/x" "GET"))
           (buf (plist-get result :buffer)))
      (unwind-protect
          (progn
            (should (plist-get result :found))
            (should (string= "etag-1" (plist-get result :token)))
            (should (bufferp buf))
            (should (string-match-p "body text"
                                    (with-current-buffer buf (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest rfcview:test-transport-http-fetch-not-found-404 ()
  "A 404 response is :found nil with no buffer or token."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (_url &optional _method)
               (rfcview-test:make-http-response-buffer 404))))
    (let ((result (rfcview:transport--http-fetch "http://example.com/x" "GET")))
      (should-not (plist-get result :found))
      (should (null (plist-get result :buffer)))
      (should (null (plist-get result :token))))))

(ert-deftest rfcview:test-transport-http-fetch-token-prefers-etag-over-last-modified ()
  "When both ETag and Last-Modified are present, ETag wins."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (_url &optional _method)
               (rfcview-test:make-http-response-buffer
                200 '(("ETag" . "etag-1")
                      ("Last-Modified" . "Wed, 01 Jul 2026 12:00:00 GMT"))
                "body\n"))))
    (let* ((result (rfcview:transport--http-fetch "http://example.com/x" "GET"))
           (buf (plist-get result :buffer)))
      (unwind-protect
          (should (string= "etag-1" (plist-get result :token)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest rfcview:test-transport-http-fetch-falls-back-to-last-modified-when-no-etag ()
  "When there is no ETag, :token comes from Last-Modified."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (_url &optional _method)
               (rfcview-test:make-http-response-buffer
                200 '(("Last-Modified" . "Wed, 01 Jul 2026 12:00:00 GMT"))
                "body\n"))))
    (let* ((result (rfcview:transport--http-fetch "http://example.com/x" "GET"))
           (buf (plist-get result :buffer)))
      (unwind-protect
          (should (string= "Wed, 01 Jul 2026 12:00:00 GMT"
                           (plist-get result :token)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest rfcview:test-transport-http-fetch-metadata-only-head-no-buffer ()
  "A successful HEAD has a token but no buffer."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (_url &optional _method)
               (rfcview-test:make-http-response-buffer
                200 '(("ETag" . "etag-1"))))))
    (let ((result (rfcview:transport--http-fetch "http://example.com/x" "HEAD")))
      (should (plist-get result :found))
      (should (null (plist-get result :buffer)))
      (should (string= "etag-1" (plist-get result :token))))))

;;; ─── rfcview:transport--http-backend ─────────────────────────────────────────

(ert-deftest rfcview:test-transport-http-backend-builds-rfc-url ()
  (let (captured-url captured-method
        (rfcview:rfc-base-url "http://example.com/rfc/"))
    (cl-letf (((symbol-function 'rfcview:transport--http-fetch)
               (lambda (url method)
                 (setq captured-url url captured-method method)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport--http-backend (list :kind 'rfc :number 793 :format 'txt))
      (should (string= "http://example.com/rfc/rfc793.txt" captured-url))
      (should (string= "GET" captured-method)))))

(ert-deftest rfcview:test-transport-http-backend-builds-pdf-url ()
  (let (captured-url
        (rfcview:rfc-base-url "http://example.com/rfc/"))
    (cl-letf (((symbol-function 'rfcview:transport--http-fetch)
               (lambda (url _method)
                 (setq captured-url url)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport--http-backend (list :kind 'rfc :number 793 :format 'pdf))
      (should (string= "http://example.com/rfc/rfc793.pdf" captured-url)))))

(ert-deftest rfcview:test-transport-http-backend-index-uses-head-when-metadata-only ()
  (let (captured-method
        (rfcview:rfc-index-url "http://example.com/rfc-index.txt"))
    (cl-letf (((symbol-function 'rfcview:transport--http-fetch)
               (lambda (_url method)
                 (setq captured-method method)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport--http-backend (list :kind 'index :metadata-only t))
      (should (string= "HEAD" captured-method)))))

(ert-deftest rfcview:test-transport-http-backend-index-uses-get-when-full-fetch ()
  (let (captured-url captured-method
        (rfcview:rfc-index-url "http://example.com/rfc-index.txt"))
    (cl-letf (((symbol-function 'rfcview:transport--http-fetch)
               (lambda (url method)
                 (setq captured-url url captured-method method)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport--http-backend (list :kind 'index :metadata-only nil))
      (should (string= "http://example.com/rfc-index.txt" captured-url))
      (should (string= "GET" captured-method)))))

;;; ─── rsync backend ────────────────────────────────────────────────────────────

(defun rfcview-test:mock-call-process (fn)
  "Return a `call-process' replacement delegating to FN.
FN is called as (FN PROGRAM ARGS) and must return a cons (EXIT .
OUTPUT); OUTPUT is inserted into the current buffer, matching real
`call-process' behaviour with DESTINATION t."
  (lambda (program _infile _destination _display &rest args)
    (let* ((result (funcall fn program args))
           (exit (car result))
           (output (cdr result)))
      (when output (insert output))
      exit)))

(ert-deftest rfcview:test-transport-rsync-run-captures-exit-and-output ()
  (cl-letf (((symbol-function 'call-process)
             (rfcview-test:mock-call-process
              (lambda (_program _args) (cons 0 "hello\n")))))
    (let ((result (rfcview:transport--rsync-run '("--foo"))))
      (should (= 0 (car result)))
      (should (string= "hello\n" (cdr result))))))

(ert-deftest rfcview:test-transport-rsync-run-nil-exit-when-binary-missing ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (signal 'file-missing '("no such file")))))
    (let ((result (rfcview:transport--rsync-run '("--foo"))))
      (should (null (car result))))))

(ert-deftest rfcview:test-transport-rsync-classify-found-on-exit-0 ()
  (should (eq 'found (rfcview:transport--rsync-classify 0 ""))))

(ert-deftest rfcview:test-transport-rsync-classify-not-found-on-exit-23 ()
  (should (eq 'not-found
              (rfcview:transport--rsync-classify
               23 "rsync: [sender] link_stat \"/x\" failed: No such file or directory (2)"))))

(ert-deftest rfcview:test-transport-rsync-classify-infra-error-on-other-exit ()
  (should-error
   (rfcview:transport--rsync-classify 10 "rsync: getaddrinfo: Name or service not known")
   :type 'rfcview:transport-infra-error))

(ert-deftest rfcview:test-transport-rsync-classify-infra-error-on-nil-exit ()
  (should-error
   (rfcview:transport--rsync-classify nil nil)
   :type 'rfcview:transport-infra-error))

(ert-deftest rfcview:test-transport-rsync-classify-infra-error-on-exit-23-without-not-found-text ()
  "Exit 23 without the \"No such file\" text is an infra error, not not-found."
  (should-error
   (rfcview:transport--rsync-classify 23 "rsync: some other partial-transfer error")
   :type 'rfcview:transport-infra-error))

(ert-deftest rfcview:test-transport-rsync-parse-listing-extracts-token ()
  (let ((token (rfcview:transport--rsync-parse-listing
                "-r--r--r--         21,088 1997/03/14 00:48:41 rfc1.txt")))
    (should (string= "21088:858268121" token))))

(ert-deftest rfcview:test-transport-rsync-parse-listing-nil-when-unparsable ()
  (should (null (rfcview:transport--rsync-parse-listing "not a listing line"))))

(ert-deftest rfcview:test-transport-rsync-fetch-metadata-only-success ()
  "A metadata-only fetch omits any destination filename argument and
returns the parsed token with no buffer."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (rfcview-test:mock-call-process
                (lambda (_program args)
                  (setq captured-args args)
                  (cons 0 "-r--r--r--  21,088 1997/03/14 00:48:41 rfc1.txt")))))
      (let ((result (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc1.txt" t)))
        (should (plist-get result :found))
        (should (null (plist-get result :buffer)))
        (should (string= "21088:858268121" (plist-get result :token)))
        ;; last arg is the source spec; no local destination path follows it.
        (should (string-match-p "rfcs-text-only/rfc1\\.txt\\'" (car (last captured-args))))))))

(ert-deftest rfcview:test-transport-rsync-fetch-full-success-writes-buffer ()
  "A full fetch writes the transferred bytes into a fresh buffer."
  (cl-letf (((symbol-function 'call-process)
             (rfcview-test:mock-call-process
              (lambda (_program args)
                (let ((dest (car (last args))))
                  (with-temp-file dest (insert "RFC BODY\n")))
                (cons 0 "")))))
    (let* ((result (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc1.txt"))
           (buf (plist-get result :buffer)))
      (unwind-protect
          (progn
            (should (plist-get result :found))
            (should (bufferp buf))
            (should (string= "RFC BODY\n" (with-current-buffer buf (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest rfcview:test-transport-rsync-fetch-full-includes-times-flag ()
  "The full-fetch invocation includes --times so the local file's
mtime can serve as a freshness token."
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (rfcview-test:mock-call-process
                (lambda (_program args)
                  (setq captured-args args)
                  (let ((dest (car (last args))))
                    (with-temp-file dest (insert "x")))
                  (cons 0 "")))))
      (let* ((result (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc1.txt"))
             (buf (plist-get result :buffer)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (should (member "--times" captured-args))))))

(ert-deftest rfcview:test-transport-rsync-fetch-not-found-no-buffer ()
  (cl-letf (((symbol-function 'call-process)
             (rfcview-test:mock-call-process
              (lambda (_program _args)
                (cons 23 "rsync: [sender] link_stat failed: No such file or directory (2)")))))
    (let ((result (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc999999.txt")))
      (should-not (plist-get result :found))
      (should (null (plist-get result :buffer))))))

(ert-deftest rfcview:test-transport-rsync-fetch-infra-error-signals ()
  (cl-letf (((symbol-function 'call-process)
             (rfcview-test:mock-call-process
              (lambda (_program _args)
                (cons 10 "rsync: getaddrinfo: Name or service not known")))))
    (should-error
     (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc1.txt")
     :type 'rfcview:transport-infra-error)))

(ert-deftest rfcview:test-transport-rsync-fetch-uses-text-module-for-txt ()
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (rfcview-test:mock-call-process
                (lambda (_program args)
                  (setq captured-args args)
                  (cons 0 "-r--r--r--  1 1997/03/14 00:48:41 rfc1.txt")))))
      (let* ((result (rfcview:transport--rsync-backend
                     (list :kind 'rfc :number 1 :format 'txt)))
             (buf (plist-get result :buffer)))
        (when (buffer-live-p buf) (kill-buffer buf))
        ;; args end in (... "--times" SOURCE DEST) for a full fetch.
        (should (string-match-p "rfcs-text-only/rfc1\\.txt\\'"
                                (car (last captured-args 2))))))))

(ert-deftest rfcview:test-transport-rsync-fetch-uses-pdf-module-for-pdf ()
  (let (captured-args)
    (cl-letf (((symbol-function 'call-process)
               (rfcview-test:mock-call-process
                (lambda (_program args)
                  (setq captured-args args)
                  (cons 0 "-r--r--r--  1 1997/03/14 00:48:41 rfc1.txt.pdf")))))
      (let* ((result (rfcview:transport--rsync-backend
                     (list :kind 'rfc :number 1 :format 'pdf)))
             (buf (plist-get result :buffer)))
        (when (buffer-live-p buf) (kill-buffer buf))
        (should (string-match-p "rfcs-pdf-only/rfc1\\.txt\\.pdf\\'"
                                (car (last captured-args 2))))))))

(ert-deftest rfcview:test-transport-rsync-fetch-passes-timeout-flags ()
  (let (captured-args
        (rfcview:rsync-timeout 42))
    (cl-letf (((symbol-function 'call-process)
               (rfcview-test:mock-call-process
                (lambda (_program args)
                  (setq captured-args args)
                  (cons 0 "-r--r--r--  1 1997/03/14 00:48:41 rfc1.txt")))))
      (rfcview:transport--rsync-fetch "rfcs-text-only" "rfc1.txt" t)
      (should (member "--timeout=42" captured-args))
      (should (member "--contimeout=42" captured-args)))))

;;; ─── Registry / dispatcher ────────────────────────────────────────────────────

(ert-deftest rfcview:test-transport-dispatch-http-method-never-calls-rsync-backend ()
  (let ((rfcview:transport-method 'http)
        rsync-called)
    (cl-letf (((symbol-function 'rfcview:transport--http-backend)
               (lambda (_request) (list :found t :buffer nil :token nil)))
              ((symbol-function 'rfcview:transport--rsync-backend)
               (lambda (_request) (setq rsync-called t)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport--dispatch (list :kind 'index :metadata-only t))
      (should-not rsync-called))))

(ert-deftest rfcview:test-transport-dispatch-rsync-method-falls-back-on-infra-error ()
  (let ((rfcview:transport-method 'rsync)
        http-called)
    (cl-letf (((symbol-function 'rfcview:transport--rsync-backend)
               (lambda (_request) (signal 'rfcview:transport-infra-error '("boom"))))
              ((symbol-function 'rfcview:transport--http-backend)
               (lambda (_request) (setq http-called t)
                 (list :found t :buffer nil :token "fallback"))))
      (let ((result (rfcview:transport--dispatch (list :kind 'index :metadata-only t))))
        (should http-called)
        (should (string= "fallback" (plist-get result :token)))))))

(ert-deftest rfcview:test-transport-dispatch-rsync-method-no-fallback-on-not-found ()
  "A genuine not-found from rsync is returned as-is -- no HTTP fallback."
  (let ((rfcview:transport-method 'rsync)
        http-called)
    (cl-letf (((symbol-function 'rfcview:transport--rsync-backend)
               (lambda (_request) (list :found nil :buffer nil :token nil)))
              ((symbol-function 'rfcview:transport--http-backend)
               (lambda (_request) (setq http-called t)
                 (list :found t :buffer nil :token "should-not-see-this"))))
      (let ((result (rfcview:transport--dispatch (list :kind 'index :metadata-only t))))
        (should-not http-called)
        (should-not (plist-get result :found))))))

;;; ─── Public entry points ──────────────────────────────────────────────────────

(ert-deftest rfcview:test-transport-fetch-rfc-builds-rfc-request ()
  (let (captured)
    (cl-letf (((symbol-function 'rfcview:transport--dispatch)
               (lambda (request) (setq captured request)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport-fetch-rfc 793 'txt)
      (should (equal '(:kind rfc :number 793 :format txt) captured)))))

(ert-deftest rfcview:test-transport-fetch-index-builds-index-request ()
  (let (captured)
    (cl-letf (((symbol-function 'rfcview:transport--dispatch)
               (lambda (request) (setq captured request)
                 (list :found t :buffer nil :token nil))))
      (rfcview:transport-fetch-index t)
      (should (equal '(:kind index :metadata-only t) captured)))))

(provide 'test-rfcview-transport)
;;; test-rfcview-transport.el ends here
