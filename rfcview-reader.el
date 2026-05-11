;;; rfcview-reader.el --- RFC document read mode for rfcview -*- lexical-binding: t; -*-

;; This file is part of rfcview.el.  It is loaded by rfcview.el.
;; rfcview:index-goto-number (from rfcview-index.el) is called at runtime and
;; need not be required here; both files are loaded before any interactive
;; command runs.

;;; Code:

(require 'rfcview-core)

(defvar-local rfcview:read-source-file nil
  "Path to the cached txt file backing the current RFC read buffer.")

(defvar-local rfcview:read-rfc-number 0
  "Number of RFC currently reading.")

(defface rfcview:read-rfc-header-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "dim grey"))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark grey"))
    (t (:bold t)))
  "Face for the RFC header block (network info, category, date)."
  :group 'rfcview)

(defface rfcview:read-rfc-title-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "black" :weight bold))
    (t (:bold t)))
  "Face for the RFC document title."
  :group 'rfcview)

(defface rfcview:read-rfc-section-face
  '((((class color) (min-colors 88) (background dark))
     (:weight bold))
    (((class color) (min-colors 88) (background light))
     (:weight bold))
    (t (:bold t)))
  "Face for section headings in RFC read mode."
  :group 'rfcview)

(defconst rfcview:section-heading-regexp
  (concat
   ;; Numeric headings require a trailing blank line (\n\n) to reject multi-line
   ;; list items that start with a number (e.g. "3.  A HOST has to be...").
   "^\n"
   "\\("
   ;; Numeric with trailing dot: "1.  Title" / "1.1.  Title" / "2.3.10.  Title"
   "[0-9]+\\.\\(?:[0-9]+\\.\\)*[ \t]+"
   ;; Numeric without trailing dot: "1 Title" / "1.1 Title" / "3.7 Media Types"
   "\\|[0-9]+\\(?:\\.[0-9]+\\)*[ \t]+"
   ;; Roman numeral: "I.  Title" / "IV.  Section" / "II. Foo"
   "\\|[IVX]+\\.?[ \t]+"
   "\\)"
   "[A-Z(\"][^,\n]*\n\n"
   ;; Appendix headings are unambiguous so only the first line is matched;
   ;; this handles titles that wrap to a second indented continuation line.
   ;; Appendix (modern): "Appendix A.  Title (possibly wrapped)"
   "\\|^\nAppendix [A-Z]\\.[ \t]+[A-Z][^\n]*\n"
   ;; Appendix (RFC 791 era, all-caps colon): "APPENDIX A:  Title"
   "\\|^\nAPPENDIX [A-Z]:[ \t]+[A-Z][^\n]*\n"
   ;; Appendix subsection: "A.1.  Title" / "B.10 Title"  (1-2 digit number to avoid X.509 false hits)
   "\\|^\n[A-Z]\\.[0-9]\\{1,2\\}\\.?[ \t]+[A-Z][^\n]*\n"
   ;; ALL-CAPS bare-word headings (RFC 854/959/1122 era):
   ;; "INTRODUCTION" / "GENERAL CONSIDERATIONS" / "LINK LAYER REFERENCES"
   "\\|^\n[A-Z][A-Z ]\\{5,\\}[A-Z]\n"
   ;; Mixed-case standalone keyword headings
   "\\|^\nAcknowledgements?[^\n]*\n"
   "\\|^\nAuthors' Addresses?[^\n]*\n"
   "\\|^\nAbstract[^\n]*\n"
   ;; Dash-underline style (RFC 768 era): "Introduction\n------------\n"
   "\\|^\n[A-Z][a-zA-Z ]+\n[[:space:]]*-\\{3,\\}\n")
  "Regexp matching RFC section headings across all eras, preceded by a blank line.")

(defun rfcview:section-heading-search (bound)
  "Font-lock search function for section headings.
After a match that ends with \\n\\n (trailing blank line consumed), backs up
by one so the blank line is available as the leading blank for the next heading.
Without this, adjacent headings like \"8.  References\" / \"8.1.  Normative
References\" cause the second heading to be missed."
  (when (re-search-forward rfcview:section-heading-regexp bound t)
    (when (and (>= (match-end 0) 2)
               (eq (char-before (match-end 0)) ?\n)
               (eq (char-before (1- (match-end 0))) ?\n))
      (goto-char (1- (match-end 0)))
      (let ((md (match-data)))
        (setcar (nthcdr 1 md) (point))
        (set-match-data md)))
    t))

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
    (define-key map (kbd "?") 'rfcview:read-show-help)

    ;; section navigation
    (define-key map (kbd "]") 'rfcview:read-next-section)
    (define-key map (kbd "[") 'rfcview:read-prev-section)

    ;; font scale
    (define-key map (kbd "0") 'text-scale-adjust)
    (define-key map (kbd "-") 'text-scale-adjust)
    (define-key map (kbd "+") 'text-scale-adjust)
    (define-key map (kbd "=") 'text-scale-adjust)
    (define-key map (kbd "RET") 'push-button)
    (define-key map (kbd "o") 'rfcview:read-view-original)
    (define-key map (kbd "q") 'rfcview:read-quit)
    map)
  "RFC read mode key map.")

(defun rfcview:read-next-section ()
  "Move to the next numbered section heading."
  (interactive)
  (let ((orig (point)))
    (end-of-line)
    (if (re-search-forward rfcview:section-heading-regexp nil t)
        (progn (goto-char (match-beginning 0))
               (forward-line 1))
      (goto-char orig)
      (message "No next section"))))

(defun rfcview:read-prev-section ()
  "Move to the previous numbered section heading."
  (interactive)
  (let ((orig (point)))
    (beginning-of-line)
    (if (re-search-backward rfcview:section-heading-regexp nil t)
        (forward-line 1)
      (goto-char orig)
      (message "No previous section"))))

(defun rfcview:read-fontify ()
  "Apply faces to RFC header, title, and section headings via text properties."
  (with-silent-modifications
    (save-excursion
      ;; Header block: from start to the first blank line
      (goto-char (point-min))
      (let ((header-start (if (re-search-forward "^[^\n]+$" nil t)
                              (line-beginning-position)
                            (point-min)))
            (header-end (if (re-search-forward "^[ \t]*$" nil t)
                            (line-beginning-position)
                          (point-max))))
        (put-text-property header-start header-end 'face 'rfcview:read-rfc-header-face))
      ;; Title: first block of indented (centered) non-blank lines after the header gap
      (forward-line 1)
      (while (and (not (eobp)) (looking-at "^[ \t]*$"))
        (forward-line 1))
      (let ((title-start (point))
            (title-end (point)))
        (while (and (not (eobp))
                    (not (looking-at "^[ \t]*$"))
                    (looking-at "^[[:space:]]+[^[:space:]]"))
          (forward-line 1)
          (setq title-end (point)))
        (when (< title-start title-end)
          (put-text-property title-start title-end 'face 'rfcview:read-rfc-title-face)))
      ;; Section headings: apply face to the heading line only (not surrounding blanks)
      (while (re-search-forward rfcview:section-heading-regexp nil t)
        (let* ((line-start (1+ (match-beginning 0)))
               (line-end (save-excursion
                           (goto-char line-start)
                           (line-end-position))))
          (put-text-property line-start line-end 'face 'rfcview:read-rfc-section-face)
          ;; Back up one char when the match consumed a trailing blank line so
          ;; it remains available as the leading blank for the next heading match.
          (when (and (>= (point) 2)
                     (eq (char-before (point)) ?\n)
                     (eq (char-before (1- (point))) ?\n))
            (goto-char (1- (point)))))))))

(defun rfcview:read-hide-page-breaks ()
  "Hide RFC page footers, form feeds, page headers, and surrounding blank lines.
Each page break block is: blank padding lines, footer ([Page N]), form feed,
page header, blank padding lines.  All of that is replaced by nothing.
When the first visible line after the break is a section heading, one blank
line from the top margin is left visible so navigation works correctly."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\f" nil t)
      (let* ((ff-pos (1- (point)))
             (footer-bol (save-excursion
                           (goto-char ff-pos)
                           (forward-line -1)
                           (point))))
        (when (save-excursion
                (goto-char footer-bol)
                (looking-at ".*\\[Page [0-9]+\\]"))
          (let* ((start (save-excursion
                          (goto-char footer-bol)
                          (if (re-search-backward "[^ \t\n]" nil t)
                              (progn (forward-line 1) (point))
                            (point-min))))
                 (end (save-excursion
                        (goto-char ff-pos)
                        (forward-line 2)      ; skip FF line and header line
                        (let (last-blank)
                          (while (and (not (eobp))
                                      (looking-at "^[ \t]*$"))
                            (setq last-blank (point))
                            (forward-line 1))
                          ;; Leave one real blank line visible before a section
                          ;; heading so that line-move navigation works correctly.
                          (when (and last-blank
                                     (not (eobp))
                                     (save-excursion
                                       (goto-char (1- (point)))
                                       (looking-at rfcview:section-heading-regexp)))
                            (goto-char last-blank)))
                        (point))))
            (when (< start end)
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'invisible t)
                (overlay-put ov 'evaporate t)))))))))



(defun rfcview:read-buttonize-refs ()
  "Make RFC XXXX and [RFCXXXX] cross-references in the buffer clickable."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(?:\\[RFC\\([0-9]+\\)\\]\\|\\bRFC[[:space:]]+\\([0-9]+\\)\\)"
            nil t)
      (let* ((num (string-to-number (or (match-string 1) (match-string 2))))
             (rfc (and rfcview:rfc-cache
                       (hash-table-p (plist-get rfcview:rfc-cache :table))
                       (gethash num (plist-get rfcview:rfc-cache :table)))))
        (make-button (match-beginning 0) (match-end 0)
                     'type 'rfcview:rfc-link-button
                     'number num
                     'action (lambda (btn)
                               (rfcview:read-rfc (button-get btn 'number)))
                     'help-echo (when rfc (plist-get rfc :title)))))))

(defun rfcview:read-trim-leading-blanks ()
  "Hide blank lines at the very beginning of the RFC buffer."
  (let ((end (save-excursion
               (goto-char (point-min))
               (while (and (not (eobp)) (looking-at "^[ \t]*$"))
                 (forward-line 1))
               (point))))
    (when (> end (point-min))
      (let ((ov (make-overlay (point-min) end)))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'evaporate t)))))

(defun rfcview:read-quit ()
  "Bury the RFC reader buffer and refocus the index window if visible."
  (interactive)
  (bury-buffer)
  (let ((index-win (get-buffer-window "*RFC INDEX*")))
    (when index-win
      (select-window index-win))))

(defun rfcview:read-show-help ()
  "Show a help buffer listing rfcview read mode keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*RFC Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "rfcview\n\n")
      (insert "  An Emacs tool for browsing, downloading, and reading IETF RFC\n")
      (insert "  documents. Presents an interactive index with filtering by\n")
      (insert "  favorites, recents, or keyword search. RFC documents are\n")
      (insert "  downloaded and cached locally on first access.\n\n")
      (insert "Keybindings\n\n")
      (insert "  Navigation\n")
      (insert "    j / k       next / previous line\n")
      (insert "    h / l       backward / forward char\n")
      (insert "    ] / [       next / previous section\n")
      (insert "    RET         follow link\n\n")
      (insert "  View\n")
      (insert "    + / = / -   increase / reset / decrease text scale\n")
      (insert "    o           view original file\n")
      (insert "    q           quit\n")
      (insert "    ?           this help\n"))
    (view-mode 1)
    (goto-char (point-min)))
  (display-buffer "*RFC Help*"))

(defun rfcview:read-mode ()
  (kill-all-local-variables)
  (use-local-map rfcview:read-mode-map)
  (setq mode-name "RFC"
        major-mode 'rfcview:read-mode
        buffer-read-only t)
  (rfcview:read-fontify)
  (rfcview:read-trim-leading-blanks)
  (rfcview:read-hide-page-breaks)
  (rfcview:read-buttonize-refs)
  (run-hooks 'rfcview-read-mode-hook))

(defun rfcview:read-view-original ()
  "Open the raw cached txt file for this RFC in text-mode."
  (interactive)
  (unless rfcview:read-source-file
    (error "Source file path not recorded for this buffer"))
  (let ((buf (find-file-noselect rfcview:read-source-file)))
    (with-current-buffer buf (text-mode))
    (pop-to-buffer buf)))

(defun rfcview:open-rfc-txt (number file)
  "Open locally cached txt FILE as RFC NUMBER and return the buffer."
  (let ((buffer (get-buffer-create (format "*RFC %04d*" number))))
    (with-current-buffer buffer
      (insert-file-contents file)
      (set-buffer-modified-p nil)
      (rfcview:read-mode)
      ;; Set after rfcview:read-mode: kill-all-local-variables runs first in the mode
      ;; function and would clear any values set before the call.
      (setq rfcview:read-source-file file
            rfcview:read-rfc-number number))
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

(defun rfcview:download-rfc (number fmt to-file)
  "Download RFC NUMBER as FMT format to TO-FILE.  Return TO-FILE on success, nil on 404."
  (message "Downloading RFC%04d (%s)..." number fmt)
  (let ((buf (rfcview:retrieve-rfc number fmt)))
    (if (eql 200 (rfcview:http-response-status buf))
        (progn
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (if (eq fmt 'pdf)
                  (progn (forward-line 1)
                         (let ((coding-system-for-write 'binary))
                           (write-region (point) (point-max) to-file nil 'silent)))
                (delete-region (point-min) (point))
                (write-region (point-min) (point-max) to-file nil 'silent)))
            (kill-buffer buf))
          to-file)
      (kill-buffer buf)
      nil)))

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
                                    (funcall (cdr (assoc fmt rfcview:open-rfc-functions))
                                             number file)))))))))
    (if buffer (pop-to-buffer buffer)
      (error "RFC%04d is not available" number))))

(provide 'rfcview-reader)
;;; rfcview-reader.el ends here
