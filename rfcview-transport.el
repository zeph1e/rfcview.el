;;; rfcview-transport.el --- Network transports for rfcview -*- lexical-binding: t; -*-

;; This file is part of rfcview.el.  It is required by rfcview-core.el and
;; must not require it (or any other rfcview file) back, to avoid a cycle.

;;; Code:

(require 'url)

(defcustom rfcview:rfc-base-url "http://www.ietf.org/rfc/"
  "The base url of RFC"
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rfc-index-url "https://www.rfc-editor.org/rfc-index.txt"
  "The rfc index file url.
This used to be derived from `rfcview:rfc-base-url', but the RFC
Editor moved the index to a different path shape (no `/rfc/' prefix,
`.txt' extension) while individual RFC document URLs under
`rfc-base-url' kept working, so the two are no longer related."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:retrieve-timeout 10
  "The timeout to try retrieve rfc materials from server."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:transport-method 'http
  "Transport used to fetch RFC documents and the rfc-index.
One of \\='http (default) or \\='rsync.  Set once in configuration; this
is not an interactive per-call toggle.  When \\='rsync is selected but
an rsync attempt fails for an infrastructure reason (binary missing,
connection/timeout, protocol error), the call transparently falls
back to HTTP for that request.  A genuine \"file not found\" on the
rsync server is NOT treated as an infrastructure failure and does not
fall back — it behaves like today's HTTP 404 (returns not-found)."
  :type '(choice (const :tag "HTTP (rfc-editor.org)" http)
                 (const :tag "rsync (rsync.rfc-editor.org)" rsync))
  :group 'rfcview)

(defcustom rfcview:rsync-executable "rsync"
  "Path to the rsync executable used when `rfcview:transport-method' is
\\='rsync.  Looked up via `exec-path' if not absolute."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rsync-server-host "rsync.rfc-editor.org"
  "Hostname of the RFC Editor's rsync daemon."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rsync-module-text "rfcs-text-only"
  "rsync daemon module holding plain-text RFCs (rfcNNNN.txt) and
rfc-index.txt at its root."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rsync-module-pdf "rfcs-pdf-only"
  "rsync daemon module holding PDF RFCs (rfcNNNN.txt.pdf) at its root."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rsync-timeout 10
  "The timeout, in seconds, to try retrieve rfc materials via rsync.
Passed to rsync's own `--timeout=' and `--contimeout=' flags, so rsync
itself enforces the bound rather than requiring async polling from
Elisp."
  :type 'integer
  :group 'rfcview)

(define-error 'rfcview:transport-infra-error
  "rfcview transport backend failed for an infrastructure reason")

;; ─── Generic HTTP primitives (used directly by the translation helpers in
;;     rfcview-core.el, and by the HTTP backend below) ──────────────────────

(defun rfcview:retrieve (url &optional method)
  "A wrapper of url-retrieve-synchronously."
  (let ((encoded-url (url-encode-url url))
        (url-request-method (or method "GET")))
    (with-current-buffer (url-retrieve-synchronously
                          encoded-url t nil rfcview:retrieve-timeout)
      (make-local-variable 'url-http-response-status)
      (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
        (if (processp process)
            (unless (process-live-p process)
              (set-process-query-on-exit-flag process nil)
              (delete-process process)
              (error "HTTP error!"))
          (set-buffer-multibyte t)      ; Fix latin chars get broken
          (current-buffer))))))

(defun rfcview:http-response-status (buffer)
  "Return the HTTP status code from BUFFER as an integer, or nil."
  (with-current-buffer buffer
    (if (and (boundp 'url-http-response-status) url-http-response-status)
        url-http-response-status
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (string-to-number (match-string 1)))))))

;; ─── Normalized transport result ────────────────────────────────────────────
;;
;; Both backends, and the two public entry points below, return a plist:
;;   :found   -- t if the resource exists, nil if genuinely absent
;;   :buffer  -- a buffer holding body content ONLY (headers/rsync protocol
;;               stripped), or nil.  The caller is responsible for killing it.
;;   :token   -- an opaque freshness token (HTTP ETag/Last-Modified string, or
;;               an rsync size+mtime token); compared with `equal' only, never
;;               ordered -- an HTTP ETag has no ordering, and there is no
;;               shared ordering between an ETag and an rsync mtime either.

;; ─── HTTP backend ────────────────────────────────────────────────────────────

(defun rfcview:transport--http-header (buffer name)
  "Case-insensitively return header NAME's value string from raw HTTP
BUFFER, or nil if the header is absent."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward (concat "^" (regexp-quote name) ":[ \t]*") nil t)
          (buffer-substring-no-properties (point) (line-end-position)))))))

(defun rfcview:transport--http-fetch (url method)
  "Perform an HTTP request to URL via METHOD (\"GET\" or \"HEAD\").
Returns a normalized transport result plist -- see the section above.
:token prefers the `ETag' header, falling back to `Last-Modified' when
no ETag is present (the current rfc-index URL only sends an ETag;
individual RFC document URLs may still send Last-Modified)."
  (let* ((buf (rfcview:retrieve url method))
         (status (rfcview:http-response-status buf)))
    (cond
     ((and status (= status 404))
      (kill-buffer buf)
      (list :found nil :buffer nil :token nil))
     ((and status (memq status '(200 304)))
      (let ((token (or (rfcview:transport--http-header buf "ETag")
                       (rfcview:transport--http-header buf "Last-Modified"))))
        (if (string-equal method "HEAD")
            (progn (kill-buffer buf)
                   (list :found t :buffer nil :token token))
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (delete-region (point-min) (point))))
          (list :found t :buffer buf :token token))))
     (t
      (kill-buffer buf)
      (list :found nil :buffer nil :token nil)))))

(defun rfcview:transport--http-backend (request)
  "HTTP backend for `rfcview:transport-backends'.
See that variable and `rfcview:transport--dispatch' for the
REQUEST/result contract."
  (pcase (plist-get request :kind)
    ('rfc
     (rfcview:transport--http-fetch
      (format "%srfc%d.%s" rfcview:rfc-base-url
              (plist-get request :number)
              (symbol-name (plist-get request :format)))
      "GET"))
    ('index
     (rfcview:transport--http-fetch
      rfcview:rfc-index-url
      (if (plist-get request :metadata-only) "HEAD" "GET")))))

;; ─── rsync backend ───────────────────────────────────────────────────────────

(defun rfcview:transport--rsync-source (module remote-path)
  "Build the rsync daemon source spec for MODULE/REMOTE-PATH on
`rfcview:rsync-server-host'."
  (format "%s::%s/%s" rfcview:rsync-server-host module remote-path))

(defun rfcview:transport--rsync-run (args)
  "Run `rfcview:rsync-executable' with ARGS.
Returns a cons (EXIT-CODE . OUTPUT-STRING), where OUTPUT-STRING is the
combined stdout+stderr.  EXIT-CODE is nil when the executable itself
could not be invoked (e.g. not installed)."
  (condition-case nil
      (with-temp-buffer
        (let ((exit (apply #'call-process rfcview:rsync-executable
                           nil t nil args)))
          (cons exit (buffer-string))))
    (file-missing (cons nil nil))))

(defun rfcview:transport--rsync-classify (exit output)
  "Classify an rsync invocation's EXIT code and OUTPUT.
Returns \\='found or \\='not-found, or signals
`rfcview:transport-infra-error' for anything else (connection/timeout
errors, protocol errors, a nil EXIT meaning the binary itself could
not be run)."
  (cond
   ((eql exit 0) 'found)
   ((and (memq exit '(23 24))
         (stringp output)
         (string-match-p "No such file or directory" output))
    'not-found)
   (t (signal 'rfcview:transport-infra-error
              (list (format "rsync (exit %s): %s" exit output))))))

(defun rfcview:transport--rsync-token (size mtime-epoch)
  "Build an opaque, equality-only freshness token from SIZE and
MTIME-EPOCH (an integer Unix timestamp)."
  (format "%d:%d" size mtime-epoch))

(defun rfcview:transport--rsync-parse-listing (output)
  "Parse a no-destination rsync listing OUTPUT (one `ls'-style line,
e.g. \"-r--r--r--  21,088 1997/03/14 00:48:41 rfc1.txt\") into a
freshness token via `rfcview:transport--rsync-token', or nil if
OUTPUT does not look like a listing line."
  (when (and (stringp output)
             (string-match
              "\\([0-9][0-9,]*\\)\\s-+\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)\\s-+\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
              output))
    (let ((size  (string-to-number (replace-regexp-in-string
                                    "," "" (match-string 1 output))))
          (year  (string-to-number (match-string 2 output)))
          (month (string-to-number (match-string 3 output)))
          (day   (string-to-number (match-string 4 output)))
          (hour  (string-to-number (match-string 5 output)))
          (min   (string-to-number (match-string 6 output)))
          (sec   (string-to-number (match-string 7 output))))
      (rfcview:transport--rsync-token
       size (time-convert (encode-time sec min hour day month year) 'integer)))))

(defun rfcview:transport--rsync-local-token (path)
  "Build a freshness token from local file PATH's size and mtime.
Only meaningful when PATH was fetched with rsync's `--times' flag, so
its mtime matches the remote file's -- see
`rfcview:transport--rsync-fetch'."
  (let ((attrs (file-attributes path)))
    (rfcview:transport--rsync-token
     (file-attribute-size attrs)
     (time-convert (file-attribute-modification-time attrs) 'integer))))

(defun rfcview:transport--rsync-fetch (module remote-path &optional metadata-only)
  "rsync-fetch REMOTE-PATH from MODULE on `rfcview:rsync-server-host'.
Returns a normalized transport result plist -- see the section above.

When METADATA-ONLY, invokes rsync with no destination argument, which
the daemon answers with a metadata-only listing (no content transfer);
the listing's size+mtime become :token via
`rfcview:transport--rsync-parse-listing', and :buffer is nil.

Otherwise transfers the file to a temp file (with `--times' so the
remote mtime survives locally) and loads it into a fresh buffer as
:buffer; :token comes from that same local file's attributes via
`rfcview:transport--rsync-local-token', which is numerically identical
to what a metadata-only listing of the same remote file would report."
  (let* ((source (rfcview:transport--rsync-source module remote-path))
         (timeout-args (list (format "--timeout=%d" rfcview:rsync-timeout)
                             (format "--contimeout=%d" rfcview:rsync-timeout))))
    (if metadata-only
        (let* ((result (rfcview:transport--rsync-run
                        (append timeout-args (list source))))
               (exit (car result))
               (output (cdr result)))
          (pcase (rfcview:transport--rsync-classify exit output)
            ('found (list :found t :buffer nil
                         :token (rfcview:transport--rsync-parse-listing output)))
            ('not-found (list :found nil :buffer nil :token nil))))
      (let* ((tmp (make-temp-file "rfcview-rsync-")))
        (unwind-protect
            (let* ((result (rfcview:transport--rsync-run
                           (append timeout-args
                                   (list "--times" source tmp))))
                   (exit (car result))
                   (output (cdr result)))
              (pcase (rfcview:transport--rsync-classify exit output)
                ('found
                 (let ((token (rfcview:transport--rsync-local-token tmp))
                       (buf (generate-new-buffer " *rfcview-rsync*")))
                   (with-current-buffer buf
                     (let ((coding-system-for-read 'binary))
                       (insert-file-contents tmp))
                     (set-buffer-multibyte t))
                   (list :found t :buffer buf :token token)))
                ('not-found (list :found nil :buffer nil :token nil))))
          (ignore-errors (delete-file tmp)))))))

(defun rfcview:transport--rsync-backend (request)
  "rsync backend for `rfcview:transport-backends'.
See that variable and `rfcview:transport--dispatch' for the
REQUEST/result contract."
  (pcase (plist-get request :kind)
    ('rfc
     (let* ((pdf (eq (plist-get request :format) 'pdf))
            (module (if pdf rfcview:rsync-module-pdf rfcview:rsync-module-text))
            (remote (format "rfc%d.%s" (plist-get request :number)
                            (if pdf "txt.pdf" "txt"))))
       (rfcview:transport--rsync-fetch module remote)))
    ('index
     (rfcview:transport--rsync-fetch rfcview:rsync-module-text "rfc-index.txt"
                                     (plist-get request :metadata-only)))))

;; ─── Registry and dispatch ───────────────────────────────────────────────────

(defvar rfcview:transport-backends
  '((http  . rfcview:transport--http-backend)
    (rsync . rfcview:transport--rsync-backend))
  "Alist mapping a `rfcview:transport-method' symbol to the backend
function that serves it.  Each backend function takes one request
plist (:kind \\='rfc or \\='index, plus :number/:format for an rfc
request or :metadata-only for an index request) and returns a
normalized result plist (:found :buffer :token; see above).  This is
a `defvar', not a `defcustom' -- it is a code-level extension point
\(push a new (SYMBOL . FN) entry to add a transport), not an end-user
setting.")

(defun rfcview:transport--backend (method)
  "Look up the backend function registered for METHOD.
Signals an error if none is registered."
  (or (alist-get method rfcview:transport-backends)
      (error "No rfcview transport backend registered for `%s'" method)))

(defun rfcview:transport--dispatch (request)
  "Look up the backend for `rfcview:transport-method' and funcall it
with REQUEST.  On `rfcview:transport-infra-error' (only ever signaled
by the rsync backend), retries once against the `http' entry in
`rfcview:transport-backends'."
  (condition-case nil
      (funcall (rfcview:transport--backend rfcview:transport-method) request)
    (rfcview:transport-infra-error
     (funcall (rfcview:transport--backend 'http) request))))

(defun rfcview:transport-fetch-rfc (number format)
  "Fetch RFC NUMBER in FORMAT (\\='txt or \\='pdf) via the configured
transport (`rfcview:transport-method').  Returns a normalized result
plist -- see the section above."
  (rfcview:transport--dispatch (list :kind 'rfc :number number :format format)))

(defun rfcview:transport-fetch-index (&optional metadata-only)
  "Fetch the rfc-index via the configured transport
(`rfcview:transport-method').  When METADATA-ONLY, does the cheapest
\"has this changed\" check and :buffer in the result is nil -- only
:found/:token are meaningful.  Returns a normalized result plist --
see the section above."
  (rfcview:transport--dispatch (list :kind 'index :metadata-only metadata-only)))

(provide 'rfcview-transport)
;;; rfcview-transport.el ends here
