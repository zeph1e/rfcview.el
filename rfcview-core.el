;;; rfcview-core.el --- Shared data, faces, and network for rfcview

;; This file is part of rfcview.el.  It is loaded by rfcview-index.el and
;; rfcview-reader.el and must not require either of them.

;;; Code:

(require 'url)

(defcustom rfcview:rfc-base-url "http://www.ietf.org/rfc/"
  "The base url of RFC"
  :type 'string
  :group 'rfcview)

(defcustom rfcview:rfc-index-url (concat rfcview:rfc-base-url "rfc-index")
  "The rfc index file url"
  :type 'string
  :group 'rfcview)

(defcustom rfcview:local-directory (concat user-emacs-directory ".RFC/")
  "The location where to store downloaded RFC files."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:parsed-index-cache-file (concat rfcview:local-directory ".cache")
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
  "The maximum count of keyword seach history."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:retrieve-timeout 10
  "The timeout to try retrieve rfc materials from server."
  :type 'integer
  :group 'rfcview)

(defcustom rfcview:use-face t
  "Whether to use text highlighting or not."
  :type 'boolean
  :group 'rfcview)

(defcustom rfcview:use-debug nil
  "Whether to use debug output or not."
  :type 'boolean
  :group 'rfcview)

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
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight favorite symbol in the *RFC INDEX* buffer."
  :group 'rfcview)

(defface rfcview:rfc-title-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "gainsboro"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark gray"))
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
     (:foreground "cyan" :underline t))
    (((class color) (background light))
     (:foreground "cyan" :underline t))
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
     (:background "gray20"))
    (((class color) (min-colors 88) (background light))
     (:background "gray70")))
  "Face used to highlight current entry."
  :group 'rfcview)

(define-button-type 'rfcview:rfc-link-button
  'face 'rfcview:button-face
  'mouse-face 'rfcview:mouse-face)

(defvar rfcview:month-name-pattern
  (eval-when-compile (regexp-opt
                      '("January" "February" "March" "April" "May" "June" "July"
                        "August" "September" "October" "November" "December"))))

(defconst rfcview:rfc-cache-default '(:last-modified (-33750 55928)))

;; Cache structure
;; (:last-modified lm-date
;;  :table #s(hash-table
;;              size XXXX
;;              data (1 (:number 1
;;                       :title "Host Software."
;;                       :authors ("S. Crocker")
;;                       :date "April 1969")
;; ...
;;                   10 (:number 10
;;                       :title "Documentation conventions."
;;                       :authors ("S.D. Crocker")
;;                       :date "July 1969"
;;                       :obsoletes (RFC0003)
;;                       :obsoleted-by (RFC0016)
;;                       :updated-by (RFC0024 RFC0027 RFC0030))
;; ...)
;; :favorite (3 66 2039...)
;; :recent (2039 22 44 ...)
;; )
(defvar rfcview:rfc-cache nil
  "A cache of RFCs and their information.")

(defun rfcview:debug (format &rest args)
  (when rfcview:use-debug
    (apply #'message format args)))

(defun rfcview:retrieve (url &optional method)
  "A wrapper of url-retrieve-synchronously."
  (let ((encoded-url (url-encode-url url))
        (url-request-method (or method "GET"))
        context)
    (with-current-buffer (url-retrieve-synchronously encoded-url t)
      (make-local-variable 'url-http-response-status)
      (let (process)
        (ignore-errors
          (setq process (get-buffer-process (current-buffer))))
        (if (processp process)
            (progn
              (unless (process-live-p process)
                (process-kill-without-query process)
                (delete-process process)
                (error "HTTP error!")))))
      (current-buffer))))

(defun rfcview:retrieve-rfc (number)
  "Retrieve a RFC document from the server."
  (unless (numberp number)
    (error "NUMBER argument is not numeric."))
  (rfcview:retrieve
   (concat rfcview:rfc-base-url "/rfc" (format "%d" number) ".txt")))

(defun rfcview:retrieve-index (&optional method)
  "Retrieve the RFC index from the server."
  (rfcview:retrieve rfcview:rfc-index-url method))

(defun rfcview:load-cache-internal (cache-file)
  "Load cache from a file."
  (when (file-exists-p cache-file)
    (with-temp-buffer
      (insert-file-contents cache-file)
      (read (buffer-string)))))

(defun rfcview:load-cache ()
  "Load cache from a file or set invalid cache data."
  (setq rfcview:rfc-cache (or (rfcview:load-cache-internal rfcview:parsed-index-cache-file)
                              rfcview:rfc-cache-default)))

(defun rfcview:save-cache ()
  "Save cache into a file."
  (with-temp-buffer
    (insert (prin1-to-string rfcview:rfc-cache))
    (write-file rfcview:parsed-index-cache-file)))

(defun rfcview:wrap-text-at-word-boundary (text margin-width max-width &optional delimiters)
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

(provide 'rfcview-core)
;;; rfcview-core.el ends here
