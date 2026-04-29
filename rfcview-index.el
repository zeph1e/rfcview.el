;;; rfcview-index.el --- RFC index mode for rfcview

;; This file is part of rfcview.el.  It is loaded by rfcview.el.
;; rfcview:read-rfc (from rfcview-reader.el) is called at runtime and need not
;; be required here; both files are loaded before any interactive command runs.

;;; Code:

(require 'rfcview-core)

(defvar rfcview:index-filter nil
  "The filter function for listing RFC index")

(defvar rfcview:index-current-list-items nil)

(defvar rfcview:suppress-recover-position nil)

(defvar rfcview:filter-keywords-history nil
  "Keywords filter history with previous result cache.")

(defvar rfcview:filter-keyword-current-keyword nil
  "Keyword filter entry for current search.")

(defvar rfcview:filter-keyword-current-result nil)

(defun rfcview:initialize (&optional from-scratch)
  "Initialize & update RFC index cache.
If FROM-SCRATCH is non-nil, discard cached data and
create the cache from scratch."
  (if from-scratch
      (setq rfcview:rfc-cache rfcview:rfc-cache-default)
    (when (null rfcview:rfc-cache)
      (rfcview:load-cache)))
  (rfcview:update-index))

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
              (setq number (string-to-number (buffer-substring (line-beginning-position) (point))))
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
  (rfcview:debug "parsing index buffer %S" buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((last-modified (save-excursion
                           (goto-char (point-min))
                           (unless (eq (point-min) (point-max))
                             (if (search-forward-regexp "^Last-Modified: " nil t)
                                 (date-to-time
                                  (buffer-substring (1+ (point)) (line-end-position)))))))
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
                                  (buffer-substring (1+ (point)) (line-end-position))))))))
      (time-less-p (plist-get rfcview:rfc-cache :last-modified)
                   last-modified))))

(defun rfcview:update-index ()
  "Update RFC index cache if it is required."
  (message "Checking for RFC index update...")
  (when (rfcview:index-updated-p)
    (with-current-buffer (rfcview:retrieve-index)
      (let ((parsed (rfcview:parse-index-buffer (current-buffer))))
        (setq rfcview:rfc-cache
              (plist-put rfcview:rfc-cache :last-modified (plist-get parsed :last-modified)))
        (setq rfcview:rfc-cache
              (plist-put rfcview:rfc-cache :table (plist-get parsed :table))))
      (rfcview:save-cache)
      (kill-buffer (current-buffer)))))

(defun rfcview:make-entry-line (number title date authors obsoletes obsoleted-by
                                       updates updated-by favorite)
  "Make a propertized line containing a single RFC document information."
  (let* ((margin-width 6)
         (margin (make-string margin-width ?\s))
         (num-str (format "%04d%c " number (if favorite rfcview:favorite-symbol ?\s)))
         (traits (list (list :var obsoletes :text "Obsoletes")
                       (list :var obsoleted-by :text "Obsoleted by")
                       (list :var updates :text "Updates")
                       (list :var updated-by :text "Updated by")))
         parts)

    (when (and rfcview:use-debug (eq rfcview:index-filter 'rfcview:index-filter-function-keywords))
      (setq title (concat title (format " (relevance: %d)"
                                        (cdr (assoc number rfcview:filter-keyword-current-result))))))

    (let* ((num-end (length num-str))
           (title-str (concat num-str title "\n")))
      (put-text-property 0 (length title-str) 'wrap-prefix margin title-str)
      (when rfcview:use-face
        (put-text-property 0 num-end 'face 'rfcview:rfc-number-face title-str)
        (put-text-property num-end (1- (length title-str)) 'face 'rfcview:rfc-title-face title-str)
        (when (eq rfcview:index-filter 'rfcview:index-filter-function-keywords)
          (let ((pos num-end))
            (dolist (keyword (split-string rfcview:filter-keyword-current-keyword nil t))
              (setq pos num-end)
              (while (string-match keyword title-str pos)
                (put-text-property (match-beginning 0) (match-end 0)
                                   'face 'rfcview:rfc-selected-filter-face title-str)
                (setq pos (match-end 0)))))))
      (push title-str parts))

    (let* ((author-text (mapconcat #'identity authors ", "))
           (spacer (propertize " " 'display
                               `(space :align-to (- right ,(1+ (length date))))))
           (line (concat margin author-text spacer date "\n")))
      (when rfcview:use-face
        (let ((a-end (+ margin-width (length author-text))))
          (put-text-property margin-width a-end 'face 'rfcview:rfc-authors-face line)
          (put-text-property (1+ a-end) (1- (length line)) 'face 'rfcview:rfc-date-face line)))
      (push line parts))

    (let (trait-parts)
      (dolist (trait traits)
        (let ((var (plist-get trait :var))
              (text (plist-get trait :text)))
          (when var
            (let* ((prefix (concat margin text ": "))
                   (value-str (mapconcat (lambda (s) (format "%s" s)) var " "))
                   (line (concat prefix value-str "\n")))
              (put-text-property 0 (length line) 'wrap-prefix
                                 (make-string (length prefix) ?\s) line)
              (when rfcview:use-face
                (put-text-property 0 (length line) 'face 'rfcview:rfc-traits-face line))
              (push line trait-parts)))))
      (dolist (p (nreverse trait-parts)) (push p parts)))

    (apply #'concat (nreverse parts))))

(defun rfcview:insert-with-text-properties (text number)
  "Insert an entry with text properties."
  (let (plist beg end)
    (when rfcview:use-face
      (setq plist (plist-put plist 'mouse-face 'rfcview:mouse-face)))

    (setq beg (point))
    (insert text)
    (setq end (point)
          plist (plist-put plist 'rfcview:number number)
          plist (plist-put plist 'follow-link
                           (lambda (pos)
                             (let ((number (get-text-property pos 'rfcview:number)))
                               (when number
                                 (rfcview:index-read-item number))))))
    (add-text-properties beg end plist)
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "RFC[0-9]\\{4\\}" end 'noerror)
        (let* ((bbtn (- (point) 7))
               (ebtn (point))
               (num (string-to-number (buffer-substring (+ bbtn 3) ebtn)))
               (rfc (gethash num (plist-get rfcview:rfc-cache :table))))
          (make-button bbtn ebtn
                            'number num
                            'type 'rfcview:rfc-link-button
                            'action 'rfcview:rfc-link-button-action
                            'help-echo (plist-get rfc :title)))))))

(defun rfcview:maphash-with-filter (function table &optional filter)
  (if filter
      (progn
        (setq rfcview:index-current-list-items filter)
        (dolist (key (funcall filter))
          (let ((value (gethash key table)))
            (when value
              (apply function (list key value))))))
    (setq rfcview:index-current-list-items nil)
    (maphash (lambda (key value)
               (push key rfcview:index-current-list-items)
               (apply function (list key value)))
               table)))

(defun rfcview:refresh-header-line ()
  (setq header-line-format
        '("RFC INDEX"
          (:eval (let ((date (concat "Last Modified: "
                                     (current-time-string
                                      (plist-get rfcview:rfc-cache :last-modified)))))
                   (concat (propertize " " 'display
                                       `(space :align-to (- right ,(1+ (length date)))))
                           date))))))

(defun rfcview:get-filter-name (filter)
  (cond ((eq filter 'rfcview:index-filter-function-favorite) "[Favorites]")
        ((eq filter 'rfcview:index-filter-function-recent) "[Recents]")
        ((eq filter 'rfcview:index-filter-function-keywords)
         (format "[%S]" rfcview:filter-keyword-current-keyword))
        (t "[All]")))

(defun rfcview:refresh-filter-line (max-width)
  (let ((beg (point))
        (filter-header "Filters: ")
        (history-header "Search history: ")
        (history-margin 4)
        (filter-name (rfcview:get-filter-name rfcview:index-filter))
        (filters (remove rfcview:index-filter '(nil
                                                rfcview:index-filter-function-favorite
                                                rfcview:index-filter-function-recent)))
        end)
    (insert filter-header
            (rfcview:wrap-text-at-word-boundary
             (concat (propertize filter-name 'face 'rfcview:rfc-selected-filter-face) " "
                     (mapconcat (lambda (f)
                                  (propertize (rfcview:get-filter-name f) 'filter f))
                                filters "'")
                     "'"
                     (propertize "[Search Keywords]" 'filter ""))
             (length filter-header) max-width "'")
            (if (> (- (length rfcview:filter-keywords-history)
                        (if (and (eq rfcview:index-filter 'rfcview:index-filter-function-keywords)
                                 (assoc rfcview:filter-keyword-current-keyword
                                        rfcview:filter-keywords-history)) 1 0)) 0)
              (concat
               "\n" history-header "\n"
               (make-string history-margin ?\s)
               (rfcview:wrap-text-at-word-boundary
                (concat (mapconcat (lambda (history)
                                     (unless (string= rfcview:filter-keyword-current-keyword
                                                      (car history))
                                       (let ((rfcview:filter-keyword-current-keyword (car history)))
                                         (propertize
                                          (rfcview:get-filter-name 'rfcview:index-filter-function-keywords)
                                          'filter (car history)))))
                                   rfcview:filter-keywords-history "'"))
                history-margin max-width "'")) "")
            "\n")
    (setq end (point))
    (save-excursion
      (goto-char beg)
      (when (search-forward-regexp (format "%s\\[[^\]]+\\] " filter-header) end 'noerror)
        (setq beg (point))
        (while (search-forward-regexp "\\[[^\]]+\\]" nil 'noerror)
          (setq end (point))
          (make-button beg end
                       'type 'rfcview:rfc-link-button
                       'action 'rfcview:filter-button-action)
          (search-forward-regexp (concat "\\(\\s-\\|" (regexp-quote history-header) "\\)+") nil 'noerror)
          (setq beg (point)))))))

(defun rfcview:refresh-index ()
  "Refresh RFC index."
  (rfcview:debug "refreshing...")
  (let ((inhibit-read-only t)
        (saved-point (unless rfcview:suppress-recover-position
                       (save-excursion
                         (backward-paragraph)
                         (get-text-property (or (next-single-property-change (point) 'rfcview:number)
                                                (point-min))
                                            'rfcview:number)))))
    (rfcview:debug "saved-point=%S" saved-point)
    (save-excursion
      (erase-buffer)
      (rfcview:refresh-header-line)
      (rfcview:refresh-filter-line (window-body-width))
      (insert (propertize "\n" 'rfcview:number 0))
      (when (hash-table-p (plist-get rfcview:rfc-cache :table))
        (rfcview:maphash-with-filter (lambda (number data)
                                       (rfcview:insert-with-text-properties
                                        (rfcview:make-entry-line number
                                                                 (plist-get data :title)
                                                                 (plist-get data :date)
                                                                 (plist-get data :authors)
                                                                 (plist-get data :obsoletes)
                                                                 (plist-get data :obsoleted-by)
                                                                 (plist-get data :updates)
                                                                 (plist-get data :updated-by)
                                                                 (member number (plist-get rfcview:rfc-cache :favorite)))
                                        number)
                                       (insert "\n"))
                                     (plist-get rfcview:rfc-cache :table)
                                     rfcview:index-filter)))
    (ignore-errors (rfcview:index-goto-number saved-point))))

(defun rfcview:rfc-link-button-action (btn)
  (rfcview:index-goto-number (button-get btn 'number)))

(defun rfcview:filter-button-action (btn)
  (let* ((filter (get-text-property (button-start btn) 'filter))
         (label (button-label btn)))
    (cond ((or (functionp filter) (null filter))
           (setq rfcview:index-filter filter)
           (let ((rfcview:suppress-recover-position t))
             (rfcview:index-refresh-screen)))
          ((stringp filter)
           (rfcview:index-apply-filter-keywords filter)))))

(defvar rfcview:index-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "p") 'rfcview:index-backward-item)
    (define-key map (kbd "n") 'rfcview:index-forward-item)
    (define-key map (kbd "v") 'rfcview:index-toggle-favorite)
    (define-key map (kbd "*") 'rfcview:index-apply-filter-all)
    (define-key map (kbd "A") 'rfcview:index-apply-filter-all)
    (define-key map (kbd "F") 'rfcview:index-apply-filter-favorite)
    (define-key map (kbd "R") 'rfcview:index-apply-filter-recent)
    (define-key map (kbd "K") 'rfcview:index-apply-filter-keywords)
    (define-key map (kbd "?") 'rfcview:index-show-help)
    (define-key map (kbd "f") 'rfcview:index-goto-filter)
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
  (rfcview:refresh-index))

(defun rfcview:index-refresh-entry (number)
  (let ((inhibit-read-only t)
        (cur-number (get-text-property (point) 'rfcview:number))
        (entry (gethash number (plist-get rfcview:rfc-cache :table)))
        beg end)
    (save-excursion
      (setq beg (if (and cur-number (= cur-number number))
                    (progn
                      (backward-paragraph)
                      (next-single-property-change (point) 'rfcview:number))
                  (rfcview:index-goto-number number)
                  (point)))
      (setq end (next-single-property-change beg 'rfcview:number))
      (delete-region beg end)
      (goto-char beg)
      (rfcview:insert-with-text-properties
       (rfcview:make-entry-line number
                                (plist-get entry :title)
                                (plist-get entry :date)
                                (plist-get entry :authors)
                                (plist-get entry :obsoletes)
                                (plist-get entry :obsoleted-by)
                                (plist-get entry :updates)
                                (plist-get entry :updated-by)
                                (member number (plist-get rfcview:rfc-cache :favorite)))
       number))))

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
  (interactive (list (string-to-number (read-from-minibuffer "RFC number: "))))
  (unless (member number rfcview:index-current-list-items)
    (error "No such RFC number in current (filtered) index."))
  (let* ((at (save-excursion
               (backward-paragraph)
               (point)))
         (target (next-single-property-change at 'rfcview:number))
         (moveto (save-excursion
                   (when target
                     (goto-char (point-min))
                     (search-forward-regexp (format "^%04d\\(*\\| \\) " number) (point-max) t)
                     (beginning-of-line)
                     (point)))))
    (when moveto
      (goto-char moveto))))

(defun rfcview:index-goto-filter ()
  (interactive)
  (goto-char (point-min))
  (forward-button 1))

(defun rfcview:move-entry-highlight ()
  (when (boundp 'rfcview:background-highlight-overlay)
    (unless (overlayp rfcview:background-highlight-overlay)
      (setq rfcview:background-highlight-overlay (make-overlay 0 0))
      (overlay-put rfcview:background-highlight-overlay 'face 'rfcview:entry-highlight-face))
    (let* ((at (save-excursion
                 (backward-paragraph)
                 (point)))
           (beg (next-single-property-change at  'rfcview:number))
           (end (next-single-property-change beg 'rfcview:number)))
      (when (or (and (number-or-marker-p beg)
                     (eq 0 (get-text-property beg 'rfcview:number)))
                (and (number-or-marker-p end)
                     (eq 0 (get-text-property end 'rfcview:number))))
        (setq beg (point-min)
              end (point-min)))
      (move-overlay rfcview:background-highlight-overlay beg end))))

(defun rfcview:index-read-item (&optional number)
  (interactive)
  (setq number (or number
                   (let* ((at (save-excursion
                                (backward-paragraph)
                                (point)))
                          (target (next-single-property-change at 'rfcview:number)))
                     (get-text-property target 'rfcview:number))))
  (let ((recent (remove number (plist-get rfcview:rfc-cache :recent))))
    (when (> (length recent) rfcview:recent-max-count)
        (setq recent (remove (last recent) recent)))
    (push number recent)
    (plist-put rfcview:rfc-cache :recent recent)
    (rfcview:read-rfc number)))

(defun rfcview:index-toggle-favorite ()
  (interactive)
  (let* ((number (or (get-text-property (point) 'rfcview:number)
                     (read-from-minibuffer "Enter RFC number to toggle favorite: ")))
         (favorite (plist-get rfcview:rfc-cache :favorite)))
    (if (member number favorite)
        (progn
          (setq favorite (remove number favorite))
          (message "RFC%04d was removed from favorite list." number))
      (push number favorite)
      (message "RFC%04d was added to favorite list." number))
    (plist-put rfcview:rfc-cache :favorite (sort favorite '<))
    (rfcview:index-refresh-entry number)))

(defun rfcview:index-apply-filter-all ()
  (interactive)
  (setq rfcview:index-filter nil)
  (let ((rfcview:suppress-recover-position t))
    (rfcview:index-refresh-screen)))

(defun rfcview:index-apply-filter-favorite ()
  (interactive)
  (setq rfcview:index-filter 'rfcview:index-filter-function-favorite)
  (let ((rfcview:suppress-recover-position t))
    (rfcview:index-refresh-screen)))

(defun rfcview:index-apply-filter-recent ()
  (interactive)
  (setq rfcview:index-filter 'rfcview:index-filter-function-recent)
  (let ((rfcview:suppress-recover-position t))
    (rfcview:index-refresh-screen)))

(defun rfcview:index-apply-filter-keywords (&optional keywords)
  (interactive)
  (setq keywords (or (and (stringp keywords) (> (length keywords) 0) keywords)
                    (mapconcat (lambda (s) s)
                           (split-string (read-from-minibuffer "Keywords: ") "[ \t\n\r\v']")
                           " ")))
  (when rfcview:filter-keyword-current-keyword
      (add-to-list 'rfcview:filter-keywords-history
                   (cons rfcview:filter-keyword-current-keyword
                         rfcview:filter-keyword-current-result))
      (while (> (length rfcview:filter-keywords-history)
                rfcview:keyword-max-history)
        (setq rfcview:filter-keywords-history
              (butlast rfcview:filter-keywords-history))))
  (setq rfcview:index-filter 'rfcview:index-filter-function-keywords
        rfcview:filter-keyword-current-keyword keywords
        rfcview:filter-keyword-current-result nil)
  (let ((rfcview:suppress-recover-position t))
    (rfcview:index-refresh-screen)))

;; (("keywordA" . '((1 . 33) (59 . 92) (205 . 88) (3333 . 87)...) --> (number . score)
;;  ("keywrodB" . '(....))
;; ...)
(defun rfcview:index-filter-function-keywords ()
  (let ((history (assoc rfcview:filter-keyword-current-keyword rfcview:filter-keywords-history)))
    (if history
        (mapcar (lambda (e) (car e)) (cdr history))
      (setq rfcview:filter-keyword-current-result nil)
      (maphash (lambda (key value)
                 (let* ((title (plist-get value :title))
                        (keywords (split-string rfcview:filter-keyword-current-keyword
                                                "\\s-+" t))
                        (seed (if (> (length keywords) 0) (/ 40 (length keywords)) 1))
                        score)
                   (setq score (cond ((string-match
                                       (concat "\\W+"
                                               (mapconcat (lambda (s) s) keywords "\\W+")
                                               "\\W+") title) 100)
                                     ((string-match
                                       (mapconcat (lambda (s) s) keywords "\\W*")  title) 80)
                                     ((string-match
                                       (mapconcat (lambda (s) s) keywords ".+") title) 60)
                                     (t (let ((matched 0))
                                          (dolist (keyword keywords)
                                            (when (string-match (concat "\\W+" keyword "\\W+") title)
                                              (setq matched (1+ matched))))
                                          (* matched seed)))))
                   (when (> score 0)
                     (push (cons key score) rfcview:filter-keyword-current-result))))
               (plist-get rfcview:rfc-cache :table))
      (setq rfcview:filter-keyword-current-result
            (sort rfcview:filter-keyword-current-result (lambda (a b)
                                                          (or (> (cdr a) (cdr b))
                                                              (and (= (cdr a) (cdr b))
                                                                   (< (car a) (car b)))))))
      (mapcar (lambda (e) (car e)) rfcview:filter-keyword-current-result))))

(defun rfcview:index-filter-function-favorite ()
  (plist-get rfcview:rfc-cache :favorite))

(defun rfcview:index-filter-function-recent ()
  (plist-get rfcview:rfc-cache :recent))

(defun rfcview:index-cleanup ()
  (rfcview:save-cache)
  (with-current-buffer (get-buffer "*RFC INDEX*")
    (when (overlayp rfcview:background-highlight-overlay)
      (delete-overlay rfcview:background-highlight-overlay)))
  (setq rfcview:rfc-cache nil))

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
  (setq word-wrap t)
  (set (make-local-variable 'rfcview:background-highlight-overlay) nil)
  (add-hook 'kill-buffer-hook 'rfcview:index-cleanup t t)
  (add-hook 'kill-emacs-hook 'rfcview:index-cleanup)
  (add-hook 'post-command-hook 'rfcview:move-entry-highlight t t)
  (run-hooks 'rfcview:index-mode-hook))

(provide 'rfcview-index)
;;; rfcview-index.el ends here
