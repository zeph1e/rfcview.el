;;; rfcview-reader.el --- RFC document read mode for rfcview

;; This file is part of rfcview.el.  It is loaded by rfcview.el.
;; rfcview:index-goto-number (from rfcview-index.el) is called at runtime and
;; need not be required here; both files are loaded before any interactive
;; command runs.

;;; Code:

(require 'rfcview-core)

(defface rfcview:rfc-section-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "black" :weight bold))
    (t (:bold t)))
  "Face used to highlight section headings in RFC read mode."
  :group 'rfcview)

(defconst rfcview:section-heading-regexp
  "^[0-9]+\\(?:\\.[0-9]+\\)*\\.?[[:space:]]\\{2,\\}[[:upper:]]"
  "Regexp matching numbered section headings in RFC text.")

(defconst rfcview:read-mode-font-lock-keywords
  `((,(concat "^[0-9]+\\(?:\\.[0-9]+\\)*\\.?[[:space:]]\\{2,\\}[[:upper:]][^\n]*")
     . 'rfcview:rfc-section-face))
  "Font-lock keywords for RFC read mode.")

(defconst rfcview:open-rfc-functions '((txt . rfcview:open-rfc-txt)
                                       (pdf . rfcview:open-rfc-pdf)))
(defvar rfcview:read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "a") 'beginning-of-line)

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

    ;; section navigation
    (define-key map (kbd "]") 'rfcview:read-next-section)
    (define-key map (kbd "[") 'rfcview:read-prev-section)

    ;; font scale
    (define-key map (kbd "0") 'text-scale-adjust)
    (define-key map (kbd "-") 'text-scale-adjust)
    (define-key map (kbd "+") 'text-scale-adjust)
    (define-key map (kbd "=") 'text-scale-adjust)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "RFC read mode key map.")

(defun rfcview:read-next-section ()
  "Move to the next numbered section heading."
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (re-search-forward rfcview:section-heading-regexp nil t)
        (beginning-of-line)
      (goto-char orig)
      (message "No next section"))))

(defun rfcview:read-prev-section ()
  "Move to the previous numbered section heading."
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-backward rfcview:section-heading-regexp nil t)
        (beginning-of-line)
      (goto-char orig)
      (message "No previous section"))))

(defun rfcview:read-hide-page-breaks ()
  "Hide RFC page footer/header lines surrounding each form feed."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^\n]*\\[Page [0-9]+\\]\n\f\n[^\n]*\n" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'evaporate t)))))

(defun rfcview:read-buttonize-refs ()
  "Make RFC XXXX cross-references in the buffer clickable."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\bRFC[[:space:]]+\\([0-9]+\\)" nil t)
      (let* ((num (string-to-number (match-string 1)))
             (rfc (and rfcview:rfc-cache
                       (hash-table-p (plist-get rfcview:rfc-cache :table))
                       (gethash num (plist-get rfcview:rfc-cache :table)))))
        (make-button (match-beginning 0) (match-end 0)
                     'type 'rfcview:rfc-link-button
                     'number num
                     'action (lambda (btn)
                               (rfcview:read-rfc (button-get btn 'number)))
                     'help-echo (when rfc (plist-get rfc :title)))))))

(defun rfcview:read-mode ()
  (kill-all-local-variables)
  (use-local-map rfcview:read-mode-map)
  (setq mode-name "RFC"
        major-mode 'rfcview:read-mode
        buffer-read-only t)
  (setq font-lock-defaults '((rfcview:read-mode-font-lock-keywords) t))
  (font-lock-mode 1)
  (rfcview:read-hide-page-breaks)
  (rfcview:read-buttonize-refs)
  (run-hooks 'rfcview-read-mode-hook))

(defun rfcview:open-rfc-txt (number file)
  "Open locally cached txt FILE as RFC NUMBER and return the buffer."
  (let ((buffer (get-buffer-create (format "*RFC %04d*" number))))
    (with-current-buffer buffer
      (insert-file-contents file)
      (set-buffer-modified-p nil)
      (rfcview:read-mode))
    buffer))

(defun rfcview:open-rfc-pdf (number file)
  "Open locally cached PDF FILE as RFC NUMBER in pdf-view-mode and return the buffer.
Signals an error if pdf-tools is not installed."
  (unless (fboundp 'pdf-view-mode)
    (error "pdf-tools is not installed; install it to view RFC %04d (PDF only)" number))
  (let* ((buf-name (format "*RFC %04d*" number))
         (buffer (or (get-buffer buf-name)
                     (let ((b (find-file-noselect file)))
                       (with-current-buffer b
                         (unless (eq major-mode 'pdf-view-mode)
                           (pdf-view-mode))
                         (rename-buffer buf-name t))
                       b))))
    buffer))

(defun rfcview:download-rfc (number format to-file)
  "Download RFC NUMBER, save to TO-FILE, and return the file."
  (message "Downloading RFC%04d (%s)..." number fmt)
  (let ((buf (rfcview:retrieve-rfc number fmt)))
    (when (eql 200 (rfcview:http-response-status buf))
        (with-current-buffer buf
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (if (eq fmt 'pdf)
                (progn (forward-line 1)
                       (let ((coding-system-for-write 'binary))
                         (write-region (point) (point-max) to-file nil 'silent)))
              (delete-region (point-min) (point))
              (write-region (point-min) (point-max) to-file nil 'silent)))
          (kill-buffer buf)
          to-file))))

(defun rfcview:read-rfc (number)
  (let* ((formats (if (eq rfcview:preferred-format 'pdf) '(pdf txt) '(txt pdf)))
         (buffer (or (get-buffer (format "*RFC %04d*" number))
                     (catch 'found
                       (dolist (fmt formats)
                         (let* ((f (format "%srfc%04d.%s"
                                           rfcview:local-directory number
                                           (symbol-name fmt)))
                                (file (if (file-exists-p f) f
                                        (rfcview:download-rfc number fmt f))))
                           (when file
                             (throw 'found
                                    (let ((open-rfc-fn
                                           (cdr (assoc fmt rfcview:open-rfc-functions))))
                                      (message "open-rfc-fn: %S" open-rfc-fn)
                                      (apply open-rfc-fn `(,number ,file)))))))))))
    (if buffer (pop-to-buffer buffer)
      (error "RFC%04d is not available" number))))

(provide 'rfcview-reader)
;;; rfcview-reader.el ends here
