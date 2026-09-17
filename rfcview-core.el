;;; rfcview-core.el --- Shared data, faces, and network for rfcview -*- lexical-binding: t; -*-

;; This file is part of rfcview.el.  It is loaded by rfcview-index.el and
;; rfcview-reader.el and must not require either of them.

;;; Code:

(require 'url)
(require 'rfcview-transport)

(defcustom rfcview:local-directory (concat user-emacs-directory ".RFC/")
  "The location where to store downloaded RFC files."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:parsed-index-cache-file
  (concat rfcview:local-directory ".cache")
  "The location where the parsed rfc index is being stored."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:favorite-symbol ?*
  "The symbol to mark favorite items in index."
  :type 'character
  :group 'rfcview)

(defcustom rfcview:recent-max-count 30
  "The maximum count of recent items to show in filtered index."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:keyword-max-history 10
  "The maximum count of keyword search history."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:preferred-format 'txt
  "Preferred format for reading RFC documents.
One of \\='txt, \\='pdf, or \\='html.

When the rfc-index `(Format: ...)' trailer lists this format, it is
tried first and the remaining listed formats follow in canonical
order as fallback.  When the index does not advertise this format,
it is dropped — the index is authoritative — and the listed supported
formats are tried instead.  If nothing supported is listed (no
`:format' info, or only unsupported tokens like \"PS\"), the RFC
is reported unavailable.

`txt' and `pdf' are downloaded to `rfcview:local-directory' and opened
in Emacs (PDF viewing requires pdf-tools).  `html' is opened in the
user's browser via `browse-url' and is not cached locally."
  :type '(choice (const :tag "Plain text" txt)
                 (const :tag "PDF" pdf)
                 (const :tag "HTML (browser)" html))
  :group 'rfcview)

(defcustom rfcview:use-face t
  "Whether to use text highlighting or not."
  :type 'boolean
  :group 'rfcview)

(defcustom rfcview:index-sort-order 'descending
  "Sort order for the RFC index, by RFC number.
Either \\='descending (default, newest first) or \\='ascending.
Applies to the All and Favorites views only.  Recents preserves
chronological order; Keywords preserves relevance-score order."
  :type '(choice (const :tag "Descending (newest first)" descending)
                 (const :tag "Ascending (oldest first)" ascending))
  :group 'rfcview)

(defcustom rfcview:use-debug nil
  "Whether to use debug output or not."
  :type 'boolean
  :group 'rfcview)

(defcustom rfcview:nav-history-max 100
  "Maximum number of entries kept in the reader navigation history."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:translate-target-language nil
  "Target ISO-639 language code for paragraph translation.
nil means \"ask on first use and persist via Customize\".
Codes are taken from the `iso639-language' property of entries in
`language-info-alist'."
  :type '(choice (const :tag "Ask on first use" nil)
                 (string :tag "ISO 639 code (e.g., ko)"))
  :group 'rfcview)

(defcustom rfcview:translate-source-language "auto"
  "Source language code; \"auto\" lets Google detect it."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:translate-endpoint
  "https://translate.googleapis.com/translate_a/single"
  "Base URL of the Google Translate public endpoint.
The default uses the unofficial gtx client which requires no API
key.  Override only if you need to proxy or self-host."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:translate-languages-endpoint
  "https://translate.googleapis.com/translate_a/l"
  "URL of the Google Translate supported-languages endpoint.
Used by `rfcview:translate-fetch-languages' to populate the
language-pick prompt.  Returns a JSON object whose `tl' key maps
ISO 639 codes to display names."
  :type 'string
  :group 'rfcview)

(defvar rfcview:translate--languages-cache nil
  "Session-cached alist (NAME . CODE) of Google Translate target languages.
Populated lazily on the first call to `rfcview:translate--language-choices'.")

(defvar rfcview:nav-history (cons nil nil)
  "Reader navigation history as a cons cell (BACK . FORWARD).
BACK is a stack of past (RFC-NUMBER . POSITION) records (newest first);
FORWARD is a stack of forward-traversed records.  Cleared whenever an
RFC is opened from the index buffer.")

(defun rfcview:nav-history-clear ()
  "Reset the reader navigation history."
  (setq rfcview:nav-history (cons nil nil)))

(defface rfcview:rfc-number-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "gold"))
    (((class color) (min-colors 88) (background light))
     (:foreground "navy"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight RFC number in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-selected-filter-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "tomato"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark violet"))
    (((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight the active filter button in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-title-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "gainsboro"))
    (((class color) (min-colors 88) (background light))
     (:foreground "grey23"))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold t)))
  "Face used to highlight RFC title in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-authors-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "forest green"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark green"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:bold t)))
  "Face used to highlight RFC authors in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-date-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "dodger blue"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dodger blue"))
    (((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight RFC date in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-traits-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "dark gray"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dim gray"))
    (((class color) (background dark))
     (:foreground "gray"))
    (((class color) (background light))
     (:foreground "gray"))
    (t (:bold nil)))
  "Face used to highlight RFC traits in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:button-face
  '((((class color) (background dark))
     (:foreground "dark turquoise" :underline t))
    (((class color) (background light))
     (:foreground "darkcyan" :underline t))
    (t (:bold t)))
  "Face used to highlight button in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:mouse-face
  '((((class color) (background dark))
     (:foreground "white" :background "blue"))
    (((class color) (background light))
     (:foreground "white" :background "blue"))
    (t (:bold nil)))
  "Face used when mouse pointer is within the region of an entry."
  :group 'rfcview)

(defface rfcview:entry-highlight-face
  '((((class color) (min-colors 88) (background dark))
     (:background "gray20" :extend t))
    (((class color) (min-colors 88) (background light))
     (:background "gray70" :extend t)))
  "Face used to highlight current entry."
  :group 'rfcview)

(define-button-type 'rfcview:rfc-link-button
  'face 'rfcview:button-face
  'mouse-face 'rfcview:mouse-face)

(define-button-type 'rfcview:section-link-button
  'face 'rfcview:button-face
  'mouse-face 'rfcview:mouse-face)

(defvar rfcview:month-name-pattern
  (eval-when-compile (regexp-opt
                      '("January" "February" "March" "April" "May" "June" "July"
                        "August" "September" "October" "November" "December"))))

(defconst rfcview:rfc-cache-version 4
  "Schema version of `rfcview:rfc-cache'.
Bump when the on-disk layout changes incompatibly (new required
keys, value-shape changes, etc.) OR when previously-cached `:table'
data can be wrong due to a parser bug fix — either way, the effect is
the same: the next load discards and rebuilds from scratch.  At load
time, a cache whose `:version' does not match this constant is handed
to `rfcview:update-cache'.

Bumped 2 -> 3 because `rfcview:parse-index-entry''s entry-boundary
regexp silently dropped ranges of RFCs from `:table' on any cache
built before the fix: first RFC >= 10000 (regexp required exactly 4
digits), then, in the fix for that, RFC 1-999 (regexp then required
>= 4 digits, but the real rfc-index never zero-pads).  Also fixed at
the same time: `rfcview:index-updated-p' compared an encoded
(`date-to-time') and a decoded (`parse-time-string') time value, so a
stale cache would almost never be detected as stale via
`Last-Modified' alone — this version bump is what actually forces
already-broken on-disk caches to rebuild.

Bumped 3 -> 4 because `:last-modified' (a comparable time value) was
replaced by `:token' (an opaque, equality-only freshness token) to
support both HTTP ETags and rsync size+mtime tokens, neither of which
shares an ordering with the other or with a plain timestamp.")

(defconst rfcview:rfc-cache-default
  `(:version ,rfcview:rfc-cache-version :token nil))

;; Cache structure
;; (:version 4
;;  :token freshness-token
;;  :table #s(hash-table
;;              size XXXX
;;              data (1 (:number 1
;;                       :title "Host Software."
;;                       :authors ("S. Crocker")
;;                       :format ("TXT")
;;                       :date "April 1969"
;;                       :status nil)
;; ...
;;                   10 (:number 10
;;                       :title "Documentation conventions."
;;                       :authors ("S.D. Crocker")
;;                       :format ("TXT")
;;                       :date "July 1969"
;;                       :obsoletes (RFC0003)
;;                       :obsoleted-by (RFC0016)
;;                       :updated-by (RFC0024 RFC0027 RFC0030)
;;                       :status nil)
;; ...)
;; :favorite (3 66 2039...)
;; :recent (2039 22 44 ...)
;; )
(defvar rfcview:rfc-cache nil
  "A cache of RFCs and their information.")

(defun rfcview:debug (format &rest args)
  (when rfcview:use-debug
    (apply #'message format args)))

(defun rfcview:load-cache-internal (cache-file)
  "Load cache from a file."
  (when (file-exists-p cache-file)
    (with-temp-buffer
      (insert-file-contents cache-file)
      (read (buffer-string)))))

(defun rfcview:update-cache (old-version)
  "Migrate `rfcview:rfc-cache' in place from OLD-VERSION to the current
schema.  Preserves `:favorite' and `:recent' from the existing cache;
all other slots are reset so the next index refresh rebuilds them.

OLD-VERSION is the `:version' read from the loaded cache (or nil for
a pre-versioning cache).  It is kept as an explicit argument so future
per-version migration logic has a dispatch point — today every old
version is migrated identically."
  (ignore old-version)
  (setq rfcview:rfc-cache
        (list :version  rfcview:rfc-cache-version
              :token    (plist-get rfcview:rfc-cache-default :token)
              :favorite (plist-get rfcview:rfc-cache :favorite)
              :recent   (plist-get rfcview:rfc-cache :recent))))

(defun rfcview:load-cache ()
  "Load cache from disk into `rfcview:rfc-cache'.
If the file is missing, fall back to `rfcview:rfc-cache-default'.
If the loaded cache has a stale `:version', hand it to
`rfcview:update-cache' — favorites and recents are carried forward;
everything else is rebuilt by the next index refresh."
  (let ((loaded (rfcview:load-cache-internal rfcview:parsed-index-cache-file)))
    (cond
     ((null loaded)
      (setq rfcview:rfc-cache rfcview:rfc-cache-default))
     ((equal (plist-get loaded :version) rfcview:rfc-cache-version)
      (setq rfcview:rfc-cache loaded))
     (t
      (setq rfcview:rfc-cache loaded)
      (rfcview:update-cache (plist-get loaded :version))))))

(defun rfcview:save-cache ()
  "Save cache into a file."
  (with-temp-buffer
    (insert (prin1-to-string rfcview:rfc-cache))
    (write-file rfcview:parsed-index-cache-file)))

(defun rfcview:wrap-text-at-word-boundary (text margin-width max-width
                                                &optional delimiters)
  "Wrap text at word boundary to fit in given width."
  (setq delimiters (or delimiters " "))
  (let (phrase line offset word-len)
    (if (< (length text) (- max-width margin-width))
        (replace-regexp-in-string delimiters " " text)
      (setq offset margin-width)
      (dolist (word (split-string text delimiters t))
        (setq word-len (length word))
        (if (< word-len (- max-width offset))
            (progn
              (setq offset (+ offset word-len (if line 1 0)))
              (push word line))
          (push line phrase)
          (setq line (list word))
          (setq offset (+ margin-width (length word)))))
      (when line (push line phrase))
      (mapconcat (lambda (l)
                   (mapconcat (lambda (w) w)
                              (reverse l)
                              " "))
                 (reverse phrase)
                 (concat "\n" (make-string margin-width ?\s))))))

;; ─── Translation helpers ────────────────────────────────────────────────────

(defun rfcview:translate-fetch-languages ()
  "Fetch supported target languages from Google Translate.
Returns an alist of (NAME . CODE) sorted by NAME, or nil on failure.
Hits `rfcview:translate-languages-endpoint' synchronously via
`rfcview:retrieve'.  The `auto' pseudo-language and any entries with
non-string display names are filtered out."
  (let* ((url (concat rfcview:translate-languages-endpoint
                      "?client=gtx&hl=en"))
         (buf (rfcview:retrieve url)))
    (unwind-protect
        (let ((status (rfcview:http-response-status buf)))
          (when (and status (= status 200))
            (let* ((body (rfcview:translate--read-body buf))
                   (parsed (and body
                                (condition-case _err
                                    (json-parse-string body
                                                       :object-type 'alist
                                                       :array-type 'array
                                                       :null-object nil)
                                  (error nil))))
                   (tl (and parsed (alist-get 'tl parsed))))
              (when (consp tl)
                (sort
                 (delq nil
                       (mapcar (lambda (pair)
                                 (let ((code (symbol-name (car pair)))
                                       (name (cdr pair)))
                                   (when (and (stringp name)
                                              (not (string= code "auto")))
                                     (cons name code))))
                               tl))
                 (lambda (a b) (string< (car a) (car b))))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun rfcview:translate--language-choices ()
  "Return the supported target-language alist (NAME . CODE).
Fetched from Google Translate's languages API on first call and cached
in `rfcview:translate--languages-cache' for the rest of the session.
Signals an error if the fetch fails — the language picker has no value
without a live list, and the user should be told rather than silently
falling back to a stale baked-in list."
  (or rfcview:translate--languages-cache
      (let ((langs (rfcview:translate-fetch-languages)))
        (unless langs
          (user-error
           "Could not fetch supported languages from Google Translate"))
        (setq rfcview:translate--languages-cache langs))))

(defun rfcview:translate--default-language-name ()
  "Return the Google-Translate name matching `current-language-environment'.
Maps the environment to an ISO 639 code via `language-info-alist''s
`iso639-language' property, then looks that code up in the
`rfcview:translate--language-choices' list (Google's names).
Returns nil when there is no match (e.g., env has no iso639 code, or
Google does not list it as a target language)."
  (let* ((env current-language-environment)
         (raw (get-language-info env 'iso639-language))
         (code (cond ((and raw (symbolp raw)) (symbol-name raw))
                     ((and raw (listp raw) (car raw))
                      (symbol-name (car raw)))
                     (t nil))))
    (when code
      (car (rassoc code (rfcview:translate--language-choices))))))

(defun rfcview:translate--ensure-target-language ()
  "Return the target-language code, prompting and persisting if unset."
  (or rfcview:translate-target-language
      (let* ((choices (rfcview:translate--language-choices))
             (default-name (rfcview:translate--default-language-name))
             (prompt (if default-name
                         (format "Translate to (default %s): " default-name)
                       "Translate to: "))
             (pick (completing-read prompt
                                    (mapcar #'car choices)
                                    nil t nil nil default-name))
             (code (cdr (assoc pick choices))))
        (unless code
          (user-error "No ISO-639 code available for %s" pick))
        (customize-save-variable 'rfcview:translate-target-language code)
        (message "rfcview: target language set to %s (%s)" pick code)
        code)))

(defun rfcview:translate--build-url (text source target)
  "Build the Google Translate request URL for TEXT, SOURCE→TARGET."
  (concat rfcview:translate-endpoint
          "?client=gtx"
          "&sl=" (url-hexify-string source)
          "&tl=" (url-hexify-string target)
          "&dt=t"
          "&q=" (url-hexify-string text)))

(defun rfcview:translate--parse-response (body)
  "Parse Google Translate JSON BODY; return the joined translation string.
Google's response is `[ [ [trans, src, …], … ], … ]'.  Concatenate every
chunk's translation cell from `outer[0]'.  Returns nil on parse failure."
  (condition-case _err
      (let* ((json (json-parse-string body
                                      :array-type 'array
                                      :null-object nil))
             (chunks (and (arrayp json) (> (length json) 0) (aref json 0)))
             (parts (when (arrayp chunks)
                      (let (acc)
                        (dotimes (i (length chunks))
                          (let ((chunk (aref chunks i)))
                            (when (and (arrayp chunk) (> (length chunk) 0))
                              (let ((s (aref chunk 0)))
                                (when (stringp s) (push s acc))))))
                        (nreverse acc)))))
        (when parts (apply #'concat parts)))
    (error nil)))

(defun rfcview:translate--read-body (buffer)
  "Return the response body string from `url-retrieve' BUFFER, or nil.
Decodes UTF-8 when BUFFER is unibyte (raw bytes from url.el); otherwise
returns the body as-is."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward "\r?\n\r?\n" nil t)
        (let ((body (buffer-substring-no-properties (point) (point-max))))
          (if (multibyte-string-p body)
              body
            (decode-coding-string body 'utf-8)))))))

(defun rfcview:translate-fetch (text &optional source target)
  "Synchronously translate TEXT from SOURCE to TARGET via Google Translate.
SOURCE defaults to `rfcview:translate-source-language', TARGET to the
result of `rfcview:translate--ensure-target-language'.  Returns the
translated string or nil on HTTP/parse failure."
  (let* ((src (or source rfcview:translate-source-language))
         (tgt (or target (rfcview:translate--ensure-target-language)))
         (url (rfcview:translate--build-url text src tgt))
         (buf (rfcview:retrieve url)))
    (unwind-protect
        (let ((status (rfcview:http-response-status buf)))
          (when (and status (= status 200))
            (rfcview:translate--parse-response
             (rfcview:translate--read-body buf))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun rfcview:translate-fetch-async (text source target callback)
  "Asynchronously translate TEXT from SOURCE to TARGET.
Calls CALLBACK with the translated string (or nil on error).  Returns
the `url-retrieve' buffer so the caller can abort via
\(delete-process (get-buffer-process BUF)\)."
  (let* ((url (rfcview:translate--build-url text source target))
         (url-request-method "GET"))
    (url-retrieve
     (url-encode-url url)
     (lambda (status)
       (let ((buf (current-buffer))
             translated)
         (unwind-protect
             (unless (plist-get status :error)
               (setq translated
                     (rfcview:translate--parse-response
                      (rfcview:translate--read-body buf))))
           (when (buffer-live-p buf) (kill-buffer buf)))
         (funcall callback translated)))
     nil t t)))

(provide 'rfcview-core)
;;; rfcview-core.el ends here
