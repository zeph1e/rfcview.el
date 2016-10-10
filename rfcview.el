;;; rfcview.el --- RFC viewer for Emacs

;; Copyright (C) 2016 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Homepage: http://github.com/zeph1e/rfcview.el
;; Created: 5 Oct 2016
;; Version: 0.1
;; Keywords: docs
;; License: WTFPL (http://sam.zoy.org/wtfpl)
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;;; Change Log:

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

(defcustom rfcview:local-directory (concat user-emacs-directory "RFC/")
  "The location where to store downloaded RFC files."
  :type 'string
  :group 'rfcview)

(defcustom rfcview:parsed-index-cache-file (concat user-emacs-directory ".rfc.cache")
  "The location where the parsed rfc index is being stored."
  :type 'string
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
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan"))
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

(define-button-type 'rfcview:rfc-link-button
  'face 'rfcview:button-face
  'mouse-face 'rfcview:mouse-face)

(defvar rfcview:month-name-pattern
  (eval-when-compile (regexp-opt
                      '("January" "February" "March" "April" "May" "June" "July"
                        "August" "September" "October" "November" "December"))))

(defconst rfcview:rfc-cache-default '(:last-modified (-33750 55928)))

(defvar rfcview:previous-window-width 0)

;; Cache structure
;; (:last-modified lm-date
;;  :table #s(hash-table
;;              size XXXX
;;              data (1 (:title "Host Software."
;;                       :authors ("S. Crocker")
;;                       :date "April 1969")
;;                    2 (:title "Host software."
;;                       :authors ("B. Duvall")
;;                       :date "April 1969")
;; ...
;;                   10 (:title "Documentation conventions."
;;                       :authors ("S.D. Crocker")
;;                       :date "July 1969"
;;                       :obsoletes (RFC0003)
;;                       :obsoleted-by (RFC0016)
;;                       :updated-by (RFC0024 RFC0027 RFC0030))
;; ...
;;                   24 (:title "Documentation conventions."
;;                       :authors ("S.D. Crocker")
;;                       :date "November 1969"
;;                       :obsoletes (RFC0016)
;;                       :updates (RFC0010 RFC0016)
;;                       :updated-by (RFC0027 RFC0030))
;; ...))
(defvar rfcview:rfc-cache nil
  "A cache of RFCs and their information.")

(defun rfcview:initialize (&optional force)
  "Initialize & update RFC index cache."
  (if force
      (setq rfcview:rfc-cache rfcview:rfc-cache-default)
    (when (null rfcview:rfc-cache)
      (rfcview:load-cache)))
  (rfcview:update-index))

(defun rfcview:load-cache ()
  "Load cache from a file or set invalid cache data."
  (setq rfcview:rfc-cache (or (rfcview:load-cache-internal rfcview:parsed-index-cache-file)
                              rfcview:rfc-cache-default)))

(defun rfcview:load-cache-internal (cache-file)
  "Load cache from a file."
  (when (file-exists-p cache-file)
    (with-temp-buffer
      (insert-file-contents cache-file)
      (read (buffer-string)))))

(defun rfcview:save-cache ()
  "Save cache into a file."
  (with-temp-buffer
    (insert (prin1-to-string rfcview:rfc-cache))
    (write-file rfcview:parsed-index-cache-file)))

(defun rfcview:debug (format &rest args)
  (when rfcview:use-debug
    (apply #'message format args)))

(defun rfcview:wrap-text-at-word-boundary (text margin-width max-width)
  "Wrap text at word boundary to fit in given width."
  (let (phrase line offset word-len)
    (if (< (length text) (- max-width margin-width))
        text
      (setq offset margin-width)
      (dolist (word (split-string text " " t))
        (setq word-len (length word))
        (if (< word-len (- max-width offset))
            ;; possible to insert in the end of this line
            (progn
              (setq offset (+ offset word-len (if line 1 0)))
              (push word line))
          ;; need to wrap
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

(defun rfcview:make-entry-line (number title date authors obsoletes obsoleted-by updates updated-by)
  "Make a propertized line containing a single RFC document information."
  ;; line format:
  ;; XXXX  My Amazing RFC document! But this title is to long and should be
  ;;       wrapped.
  ;;       Author Name1, Author Name2                           August XXXX
  ;;       Obsoletes: [RFC00XX] [RFC00YY]
  ;;       Obsoleted by: [RFC0XXX]
  (let* ((body-width (window-body-width))
         (margin-width 6)
         (width (- (window-body-width) 6))
         (str (format "%04d  " number))
         (margin (make-string margin-width ?\s))
         (traits (list (list :var obsoletes :text "Obsoletes")
                       (list :var obsoleted-by :text "Obsoleted by")
                       (list :var updates :text "Updates")
                       (list :var updated-by :text "Updated by")))
         line-beg beg end)
    (setq end (length str))
    (when rfcview:use-face
      (put-text-property 0 end 'face 'rfcview:rfc-number-face str))

    (setq beg end)
    (setq str (concat str (rfcview:wrap-text-at-word-boundary title margin-width body-width)))
    (setq end (length str))
    (setq str (concat str "\n" margin))
    (when rfcview:use-face
      (put-text-property beg end 'face 'rfcview:rfc-title-face str))

    (let ((author-width (- body-width margin-width (length date) 2)))
      (setq beg (length str))
      (setq str (concat str (format (format "%%-%ds  %%s\n" author-width)
                                    (truncate-string-to-width
                                     (mapconcat (lambda (s) s) authors ", ")
                                     author-width)
                                    date)))
      (setq end (length str))
      (when rfcview:use-face
        (put-text-property beg (- end (length date) 1) 'face 'rfcview:rfc-authors-face str)
        (setq beg (- end (length date) 1))
        (put-text-property beg end 'face 'rfcview:rfc-date-face str)))

    (setq beg (length str))
    (dolist (trait traits)
      (let ((var (plist-get trait :var))
            (text (plist-get trait :text)))
        (when (and trait var)
          (setq str (concat str margin (format "%s: " text)
                            (rfcview:wrap-text-at-word-boundary
                             (mapconcat (lambda (s) (format "[%s]" s))
                                        var " ")
                             (+ margin-width (length text) 2)
                             (- body-width 8))
                            "\n")))))
    (setq end (length str))
    (when (< beg end)
      (put-text-property beg end 'face 'rfcview:rfc-traits-face str))
    str))

(defun rfcview:insert-with-text-properties (text number)
  "Insert an entry with text properties."
  (let (plist beg end)
    ;; (when rfcview:use-face
    ;;   (setq plist (plist-put plist 'mouse-face 'rfcview:mouse-face)))

    (setq beg (point))
    (insert text)
    (setq end (point)
          plist (plist-put plist 'rfcview:number number)
          plist (plist-put plist 'read-only t))
    (add-text-properties beg end plist)
    ;;now make buttons
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "\[RFC[0-9]\\{4\\}\]" end 'noerror)
        (let* ((bbtn (- (point) 9))
               (ebtn (point))
               (num (string-to-int (buffer-substring (+ bbtn 4) ebtn)))
               (rfc (gethash num (plist-get rfcview:rfc-cache :table))))
          (make-button bbtn ebtn
                            'number num
                            'type 'rfcview:rfc-link-button
                            'action (lambda (btn) (rfcview:index-goto-number
                                                   (button-get btn 'number)))
                            'help-echo (plist-get rfc :title)))))))

(defvar rfcview:refresh-delay-timer nil)

(defun rfcview:refresh-index (&optional force)
  "Refresh RFC index."
  ;; With helm, window width gets changed too frequently.
  ;; This will delay the refreshing index for every window width update
  ;; and will refresh if it is really required.
  (when rfcview:refresh-delay-timer
    (cancel-timer rfcview:refresh-delay-timer)
    (setq rfcview:refresh-delay-timer nil))
  (if (not force)
      (unless (= (window-body-width) rfcview:previous-window-width)
        (setq rfcview:refresh-delay-timer
              (run-at-time "0.1 sec" nil
                           (lambda (buffer)
                             (with-current-buffer buffer
                               (save-window-excursion
                                 (rfcview:debug "refresh-delay-timer fired!")
                                 (select-window (get-buffer-window (current-buffer)))
                                 (unless (= (window-body-width) rfcview:previous-window-width)
                                   (rfcview:refresh-index t))))) (current-buffer))))

    (with-current-buffer (current-buffer)
      (rfcview:debug "refreshing...")
      (let ((inhibit-read-only t)
            (body-width (window-body-width))
            (saved-point (save-excursion
                           (backward-paragraph)
                           (get-text-property (or (next-single-property-change (point) 'rfcview:number)
                                                  (point-min))
                                              'rfcview:number))))
        (rfcview:debug "body-width=%S saved-point=%S" body-width saved-point)
        (save-excursion
          (erase-buffer)
          (insert (propertize (format (format "RFC INDEX%%%ds\n\n"
                                              (- body-width 9))
                                      (concat "Last Modified: "
                                              (current-time-string
                                               (plist-get rfcview:rfc-cache :last-modified)))
                                      'rfcview:number 0)))
          (when (hash-table-p (plist-get rfcview:rfc-cache :table))
            (maphash (lambda (number data)
                       (rfcview:insert-with-text-properties
                        (rfcview:make-entry-line number
                                                 (plist-get data :title)
                                                 (plist-get data :date)
                                                 (plist-get data :authors)
                                                 (plist-get data :obsoletes)
                                                 (plist-get data :obsoleted-by)
                                                 (plist-get data :updates)
                                                 (plist-get data :updated-by))
                        number)
                       (insert "\n"))
                     (plist-get rfcview:rfc-cache :table))))
        (ignore-errors (rfcview:index-goto-number saved-point)))
      (setq rfcview:previous-window-width (window-body-width)))))

(defun rfcview:parse-index-entry (buffer)
  "Parse an entry from rfc-index file."
  (with-current-buffer buffer
    (when (search-forward-regexp "^[0-9]\\{4\\} " nil t)
      (let ((traits '((obsoletes . "Obsoletes\\s-+")
                      (obeoleted-by . "Obsoleted\\s-+by\\s-+")
                      (updates . "Updates\\s-+")
                      (updated-by . "Updated\\s-+by\\s-+")))
            (beg (- (point) 6))
            end number title authors date trait-begin
            obsoletes obsoleted-by updates updated-by)
        (condition-case e
            (progn
              (setq end (save-excursion (search-forward-regexp "^$" nil t)))
              (setq number (string-to-number (buffer-substring (point-at-bol) (point))))
              (setq title (replace-regexp-in-string
                           "\\s-+" " "
                           (buffer-substring (point) (search-forward-regexp "\\.\\s-+" end t))))
              (setq authors (let (result candidate)
                              (dolist (auth (split-string
                                             (replace-regexp-in-string
                                              "\\s-+" " "
                                              (buffer-substring (progn
                                                                  (skip-chars-forward " \r\n")
                                                                  (point))
                                                                (progn
                                                                  (search-forward-regexp
                                                                   (concat
                                                                    rfcview:month-name-pattern
                                                                    "\\s-+[0-9]\\{4\\}") end t)
                                                                  (search-backward "." nil t)
                                                                  (point)))) ",\\s-+"))
                                (if (and (string= auth "Ed.") (stringp candidate))
                                    (progn
                                      (push (concat candidate ", Ed.") result)
                                      (setq candidate nil))
                                  (if (stringp candidate)
                                      (push candidate result))
                                  (setq candidate auth)))
                              (reverse (if (stringp candidate)
                                           (push candidate result)
                                         result))))
              (setq date (replace-regexp-in-string
                          "\\s-+" " "
                          (buffer-substring (search-forward-regexp "\\s-+" end t)
                                            (search-forward-regexp
                                             (concat rfcview:month-name-pattern
                                                     "\\s-+\\([0-9]\\{4\\}\\)") end t))))
              (setq trait-begin (search-forward-regexp "\\s-+" end t))
              (dolist (trait traits)
                (goto-char trait-begin)
                (set (car trait)
                     (split-string
                      (if (search-forward-regexp (concat "(" (cdr trait)) end t)
                          (replace-regexp-in-string
                           "\\s-+" " "
                           (buffer-substring (point) (1- (search-forward ")" end t))))
                        "")
                      ",\\s-+" t)))
              (list :number number
                    :title title
                    :authors authors
                    :date date
                    :obsoletes obsoletes
                    :obsoleted-by obsoleted-by
                    :updates updates
                    :updated-by updated-by))
          (error (rfcview:debug "parse error in rfc %d %S:\n%S"
                                number (error-message-string e) (buffer-substring beg end))
                 (error "Parse index entry error!")))))))

(defun rfcview:parse-index-buffer (buffer)
  "Parse rfc-index file and create a structured cache."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((last-modified (save-excursion
                           (goto-char (point-min))
                           (unless (eq (point-min) (point-max))
                             (if (search-forward-regexp "^Last-Modified: " nil t)
                                 (date-to-time
                                  (buffer-substring (1+ (point)) (point-at-eol)))))))
          (rfc-table (make-hash-table :test 'equal))
          (continue t)
          entry)
      (while continue
        (ignore-errors
          (setq entry (rfcview:parse-index-entry buffer))
          (if entry
              (puthash (plist-get entry :number) entry rfc-table)
            (setq continue nil))))
      (list :last-modified last-modified :table rfc-table))))

(defun rfcview:index-updated-p ()
  "Check if rfc-index has been updated."
  (with-current-buffer (rfcview:retrieve-index "HEAD")
    (let ((last-modified (progn
                           (goto-char (point-min))
                           (unless (eq (point-min) (point-max))
                             (if (search-forward-regexp "^Last-Modified: " nil t)
                                 (parse-time-string
                                  (buffer-substring (1+ (point)) (point-at-eol))))))))
      ;; (message "server last-modified: %S" last-modified)
      (time-less-p (plist-get rfcview:rfc-cache :last-modified)
                   last-modified))))

(defun rfcview:update-index ()
  "Update RFC index cache if it is required."
  (message "Checking for RFC index update...")
  (when (rfcview:index-updated-p)
    (with-current-buffer (rfcview:retrieve-index)
      (setq rfcview:rfc-cache (rfcview:parse-index-buffer (current-buffer)))
      (rfcview:save-cache)
      (kill-buffer (current-buffer)))))

(defun rfcview:retrieve-rfc (number)
  "Retrieve a RFC document from the server."
  (unless (numberp number)
    (error "NUMBER argument is not numeric."))
  (rfcview:retrieve
   (concat rfcview:rfc-base-url "/rfc" (format "%04d" number) ".txt")))

(defun rfcview:retrieve-index (&optional method)
  "Retrieve the RFC index from the server."
  (rfcview:retrieve rfcview:rfc-index-url method))

(defun rfcview:retrieve (url &optional method)
  "A wrapper of url-retrieve-synchronously."
  (let ((encoded-url (url-encode-url url))
        (url-request-method (or method "GET"))
        context)
    ;; (message "url: %S" encoded-url)
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

(defvar rfcview:read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "a") 'beginning-of-line)
    (define-key map (kbd "b") 'end-of-line)

    ;; vi navigation
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") #'(lambda () (interactive)
                                 (if (bolp) (error "Beginning of line")
                                   (backward-char))))
    (define-key map (kbd "l") #'(lambda () (interactive)
                                  (if (eolp) (error "End of line")
                                    (forward-char))))
    (define-key map (kbd "/") 'isearch-forward)
    (define-key map (kbd "?") 'isearch-backward)

    ;; font scale
    (define-key map (kbd "0") 'text-scale-adjust)
    (define-key map (kbd "-") 'text-scale-adjust)
    (define-key map (kbd "+") 'text-scale-adjust)
    (define-key map (kbd "=") 'text-scale-adjust)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "RFC read mode key map.")

(defun rfcview:read-mode ()
  (kill-all-local-variables)
  (use-local-map rfcview:read-mode-map)
  (setq mode-name "RFC"
        major-mode 'rfcview:read-mode
        buffer-read-only t)
  (run-hooks 'rfcview-read-mode-hook))

(defun rfcview:read-rfc (number)
  (set-buffer (get-buffer-create (format "*RFC %04d*" number)))
  (let ((filename (format "%s/rfc%04d.txt" rfcview:local-directory number)))
    (unless (file-exists-p filename)
      (message "Downloading RFC%04d..." number)
      (with-current-buffer (rfcview:retrieve-rfc number)
        (goto-char (point-min))
        (when (search-forward-regexp "^$" nil t)
          (delete-region (point-min) (point)))
        (write-file filename)
        (kill-buffer)))
    (insert-file-contents filename)
    (rfcview:read-mode)
    (select-window (display-buffer (current-buffer)))))


(defvar rfcview:index-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'rfcview:index-backward-item)
    (define-key map (kbd "n") 'rfcview:index-forward-item)
    (define-key map (kbd "#") 'rfcview:index-goto-number)
    (define-key map (kbd " ") 'rfcview:index-read-item)
    (define-key map (kbd "RET") 'rfcview:index-read-item)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)
    (define-key map (kbd "g") 'rfcview:index-refresh-screen)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "RFC INDEX key map.")

(defun rfcview:index-refresh-screen ()
  (interactive)
  (rfcview:refresh-index t))

(defun rfcview:index-forward-item ()
  (interactive)
  (let* ((at (save-excursion
               (forward-paragraph)
               (point)))
         (target (next-single-property-change at 'rfcview:number)))
    (when target (goto-char target))))

(defun rfcview:index-backward-item ()
  (interactive)
  (let* ((at (save-excursion
               (backward-paragraph)
               (point)))
         (target (previous-single-property-change at 'rfcview:number)))
    (when target (goto-char target))))

(defun rfcview:index-goto-number (number)
  (interactive (values (string-to-int (read-from-minibuffer "RFC number: "))))
  (let* ((at (save-excursion
               (backward-paragraph)
               (point)))
         (target (next-single-property-change at 'rfcview:number)))
    (when (and target
               (unless (and (> (get-text-property target 'rfcview:number) number)
                            (search-backward-regexp (format "^%04d  " number) (point-min) t))
                 (beginning-of-buffer)
                 (search-forward-regexp (format "^%04d  " number) (point-max) t)))
      (beginning-of-line))))

(defun rfcview:index-read-item ()
  (interactive)
  (let* ((at (save-excursion
               (backward-paragraph)
               (point)))
         (target (next-single-property-change at 'rfcview:number)))
    (when target
      (rfcview:read-rfc (get-text-property target 'rfcview:number)))))

(defun rfcview:index-mode ()
  "Major mode to list RFC documents.

Keybindings:
\\{rfcview:index-mode-map}
"
  (kill-all-local-variables)
  (use-local-map rfcview:index-mode-map)
  (setq mode-name "RFC-INDEX"
        major-mode 'rfcview:index-mode
        buffer-read-only t)
  (make-local-variable 'window-configuration-change-hook)
  (add-hook 'window-configuration-change-hook 'rfcview:refresh-index)
  (run-hooks 'rfcview:index-mode-hook))

;;;###autoload
(defun rfcview ()
  "Shows RFC index"
  (interactive)
  (set-buffer (get-buffer-create "*RFC INDEX*"))
  (rfcview:initialize)
  (rfcview:refresh-index t)
  (rfcview:index-mode)
  (select-window (display-buffer (current-buffer))))

(provide 'rfcview)
