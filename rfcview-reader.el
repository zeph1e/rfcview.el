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

(defvar-local rfcview:read-section-anchors-by-number nil
  "Hash mapping section-number keys to heading position markers.
Keys look like \"3.1\" or \"A\".  Populated by `rfcview:read-fontify'
and consumed by `rfcview:read-buttonize-toc'.")

(defvar-local rfcview:read-section-anchors-by-title nil
  "Hash mapping normalized heading-title strings to heading position markers.
Fallback lookup for TOC entries without a section number.")

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
  '((t (:bold t)))
  "Face for section headings in RFC read mode."
  :group 'rfcview)

(defface rfcview:read-toc-leader-face
  '((t (:inherit shadow)))
  "Face for the dot-leader and trailing page number in TOC entries."
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
   ;; Roman numeral with dash: "I - INTRODUCTION"
   "\\|[IVX]+[ \t]*-[ \t]+"
   ;; A.1.3
   "\\|[A-Z]\\(?:\\.[0-9]\\{1,2\\}\\)+\\.?[ \t]+"
   "\\)"
   "[A-Z(\"][^,\n]*\n\n"
   ;; Subsection titles (X.Y+ only) with commas in the title — e.g.
   ;; RFC 8698 §6.2 "Method for Delay, Loss, and Marking Ratio Estimation".
   ;; The in-group-1 numeric alt rejects commas to block multi-line list
   ;; items ("3.  A HOST has to be prepared, …").  Multi-segment numbers
   ;; aren't used in RFC list items, and requiring a non-period last char
   ;; rules out the residual "X.Y  Sentence, with, commas." shape.
   "\\|^\n[0-9]+\\(?:\\.[0-9]+\\)+\\.?[ \t]+[A-Z(\"][^\n]*[^.\n]\n\n"
   ;; Wrapped subsection title (X.Y form only, e.g. RFC 8968 §2.6):
   ;; "2.6.  Long title that overflows\n      onto a second line\n\n"
   "\\|^\n[0-9]+\\(?:\\.[0-9]+\\)+\\.?[ \t]+[A-Z(\"][^\n]*\n[ \t]\\{5,\\}[a-zA-Z\"][^\n]*\n\n"
   ;; Appendix headings are unambiguous so both single-line and one-continuation-
   ;; line titles are matched.
   ;; Appendix (modern): "Appendix A.  Title" or wrapped onto a second indented line
   "\\|^\nAppendix [A-Z]\\.[ \t]+[A-Z][^\n]*\\(?:\n[ \t]\\{5,\\}[^\n]+\\)?\n\n"
   ;; Appendix (RFC 791 era, all-caps colon): "APPENDIX A:  Title"
   "\\|^\nAPPENDIX [A-Z]:[ \t]+[A-Z][^\n]*\n\n"
   "\\|^\nAPPENDIX [IVX]+[ \t]+-[ \t]+[A-Z][^\n]*\n\n"
   ;; Appendix subsection: "A.1.  Title" / "B.10 Title" (1-2 digit number to avoid X.509
   ;; false hits), also handles a title that wraps onto one indented continuation line.
   "\\|^\n[A-Z]\\.[0-9]\\{1,2\\}\\.?[ \t]+[A-Z][^\n]*\\(?:\n[ \t]\\{5,\\}[^\n]+\\)?\n\n"
   ;; ALL-CAPS bare-word headings (RFC 854/959/1122 era):
   ;; "INTRODUCTION" / "GENERAL CONSIDERATIONS" / "LINK LAYER REFERENCES"
   "\\|^\n[A-Z][-A-Z() ]\\{,50\\}[A-Z]\n\n"
   ;; Colon follows (RFC 42)
   "\\|^\n[ ]\\{,3\\}[A-Z][A-Z ]+:\n\n"
   ;; Mixed-case standalone keyword headings
   "\\|^\nAcknowledgements?[^\n]*\n\n"
   "\\|^\nAuthor\\(s'\\|'s\\) Address\\(es\\)?[^\n]*\n\n"
   "\\|^\nAbstract[^\n]*\n\n"
   ;; Dash-underline style (RFC 768 era): "Introduction\n------------\n"
   "\\|^\n[ ]*[A-Z][a-zA-Z0-9. ]+\n[ ]*-\\{3,\\}\n\n")
  "Regexp matching RFC section headings across all eras, preceded by a blank line.
Subsection titles (`X.Y' form and deeper) may contain commas (RFC 8698 §6.2)
and may wrap onto an indented continuation line (RFC 8968 §2.6).  Top-level
numbered titles (`X.') keep the strict one-line, no-comma rule, since multi-line
or comma-bearing list items often begin with a single-segment number.")

(defconst rfcview:open-rfc-functions '((txt . rfcview:open-rfc-txt)
                                       (pdf . rfcview:open-rfc-pdf)))

(defvar rfcview:read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)

    ;; section navigation
    (define-key map (kbd "]") 'rfcview:read-next-section)
    (define-key map (kbd "[") 'rfcview:read-prev-section)

    ;; button navigation
    (define-key map (kbd "<tab>") 'forward-button)
    (define-key map (kbd "<backtab>") 'backward-button)

    ;; font scale
    (define-key map (kbd "0") 'text-scale-adjust)
    (define-key map (kbd "-") 'text-scale-adjust)
    (define-key map (kbd "+") 'text-scale-adjust)
    (define-key map (kbd "=") 'text-scale-adjust)

    (define-key map (kbd "RET") 'push-button)

    (define-key map (kbd "o") 'rfcview:read-view-original)

    (define-key map (kbd "?") 'rfcview:read-show-help)
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

(defun rfcview:read--normalize-number (s)
  "Strip leading whitespace and trailing whitespace/dots/dashes from S."
  (replace-regexp-in-string "\\`[ \t]+\\|[ \t.\\-]+\\'" "" s))

(defun rfcview:read--normalize-title (s)
  "Lowercase S, trim it, and collapse internal whitespace runs."
  (downcase (replace-regexp-in-string "[ \t]+" " " (string-trim s))))

(defun rfcview:read--register-anchor (pos heading-line num-prefix)
  "Record a heading at POS in the anchor tables.
HEADING-LINE is the literal heading text without surrounding newlines.
NUM-PREFIX is group 1 of `rfcview:section-heading-regexp', or nil when the
heading matched via an alternative outside group 1 (modern Appendix lines,
ALL-CAPS, Abstract, etc.)."
  (let ((marker (copy-marker pos)))
    (cond
     (num-prefix
      (let ((title (string-trim (substring heading-line (length num-prefix)))))
        (puthash (rfcview:read--normalize-number num-prefix)
                 marker rfcview:read-section-anchors-by-number)
        (when (> (length title) 0)
          (puthash (rfcview:read--normalize-title title)
                   marker rfcview:read-section-anchors-by-title))))
     ((string-match "\\`Appendix \\([A-Z]\\)\\.?[ \t]+\\(.*\\)" heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ((string-match "\\`APPENDIX \\([A-Z]\\|[IVX]+\\)[: \t-]+\\(.*\\)" heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ;; Wrapped subsection ("2.6.  Long Title...") — only line 1 is in heading-line.
     ((string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)+\\)\\.?[ \t]+\\(.*\\)" heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ((string-match "\\`\\([A-Z]\\.[0-9]+\\(?:\\.[0-9]+\\)?\\)\\.?[ \t]+\\(.*\\)"
                    heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     (t
      (puthash (rfcview:read--normalize-title heading-line)
               marker rfcview:read-section-anchors-by-title)))))

(defun rfcview:read-fontify ()
  "Apply faces to RFC header, title, and section headings via text properties.
Also populates `rfcview:read-section-anchors-by-number' and -by-title with
markers pointing at each heading, used later by `rfcview:read-buttonize-toc'."
  (setq rfcview:read-section-anchors-by-number (make-hash-table :test 'equal)
        rfcview:read-section-anchors-by-title  (make-hash-table :test 'equal))
  (with-silent-modifications
    (save-excursion
      ;; Header block: from start to the first blank line
      (goto-char (point-min))
      ;; recent has \ufeff at the very early of document
      (let ((header-start (if (re-search-forward "^[^\ufeff\n]+$" nil t)
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
                    (not (looking-at "^[ \t]*$")))
          (forward-line 1)
          (setq title-end (point)))
        (when (< title-start title-end)
          (put-text-property title-start title-end 'face 'rfcview:read-rfc-title-face)))
      ;; Section headings: apply face to the heading line only (not surrounding blanks)
      (while (re-search-forward rfcview:section-heading-regexp nil t)
        (let* ((line-start (1+ (match-beginning 0)))
               (line-end (save-excursion
                           (goto-char line-start)
                           (line-end-position)))
               (heading-line (buffer-substring-no-properties line-start line-end))
               (num-prefix (and (match-beginning 1) (match-string 1))))
          (put-text-property line-start line-end 'face 'rfcview:read-rfc-section-face)
          (rfcview:read--register-anchor line-start heading-line num-prefix)
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
                           (if (bolp)
                               (forward-line -1)
                             (beginning-of-line))
                           (point))))
        (when (save-excursion
                (goto-char footer-bol)
                (looking-at ".*\\[Page [0-9]+\\]\\|[ \t]+- ?[0-9]+ ?-[ \t]*\\|[ \t]*$"))
          (let* ((start (save-excursion
                          (goto-char footer-bol)
                          (if (re-search-backward "[^ \t\n]" nil t)
                              (progn (forward-line 1) (point))
                            (point-min))))
                 (end (save-excursion
                        (goto-char ff-pos)
                        (forward-line)
                        (when (looking-at (concat
                                           (format ".*RFC.*%d.*"
                                                   rfcview:read-rfc-number)
                                           "\\|[A-Z][A-Z ]+ \\{10,\\}[A-Z ]+"))
                            (forward-line))
                        (apply #'min
                               (delq nil `(,(save-excursion
                                              (when (looking-at "^[ \t]*$")
                                                (re-search-forward "^[^ \t]+$" nil t))
                                              (point))
                                           ,(point-max)))))))
            (when (< start end)
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'invisible t)
                (overlay-put ov 'evaporate t)))))))))



(defun rfcview:read-buttonize-refs ()
  "Make RFC XXXX and [RFCXXXX] cross-references in the buffer clickable.
Skips matches that already lie inside a button — e.g. an `RFC NNNN'
fragment inside a TOC entry's title that `rfcview:read-buttonize-toc'
has already wrapped in a `rfcview:section-link-button'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\\(?:\\[RFC\\([0-9]+\\)\\]\\|\\bRFC[[:space:]]+\\([0-9]+\\)\\)"
            nil t)
      (unless (button-at (match-beginning 0))
        (let* ((num (string-to-number (or (match-string 1) (match-string 2))))
               (rfc (and rfcview:rfc-cache
                         (hash-table-p (plist-get rfcview:rfc-cache :table))
                         (gethash num (plist-get rfcview:rfc-cache :table)))))
          (make-button (match-beginning 0) (match-end 0)
                       'type 'rfcview:rfc-link-button
                       'number num
                       'action (lambda (btn)
                                 (rfcview:read-rfc (button-get btn 'number)))
                       'help-echo (when rfc (plist-get rfc :title))))))))

(defun rfcview:read--make-section-button (beg end target)
  "Wrap [BEG, END) in a section-link button that jumps to marker TARGET."
  (make-button beg end
               'type 'rfcview:section-link-button
               'target target
               'action (lambda (btn)
                         (let ((m (button-get btn 'target)))
                           (when (markerp m)
                             (goto-char m)
                             (when (eq (window-buffer) (current-buffer))
                               (recenter 0)))))
               'help-echo "Jump to section"))

(defun rfcview:read--dim-toc-tail (title-end-on-line-1)
  "Dim everything past TITLE-END-ON-LINE-1 to end of line.
Applies `rfcview:read-toc-leader-face' to any dot leader, trailing page
number, and intervening whitespace.  No-op when the title already ends
at end-of-line (TOCs without leaders, like RFC 9227)."
  (let ((eol (save-excursion (goto-char title-end-on-line-1)
                             (line-end-position))))
    (when (> eol title-end-on-line-1)
      (put-text-property title-end-on-line-1 eol
                         'face 'rfcview:read-toc-leader-face))))

(defun rfcview:read--absorb-toc-continuations (title-beg title-end limit)
  "Extend a TOC title that wraps onto continuation lines.
TITLE-BEG and TITLE-END bracket the title text already matched on the
current line; point must be on that line.  LIMIT bounds the search.
Returns a cons (NEW-TITLE-END . LINES-CONSUMED).  A line counts as a
continuation if it is non-blank, indented to or past TITLE-BEG's column,
and does not start with a section number or \"Appendix\"."
  (let ((title-col (save-excursion (goto-char title-beg) (current-column)))
        (te title-end)
        (extra 0))
    (save-excursion
      (forward-line 1)
      (while (and (< (point) limit)
                  (looking-at "^[ \t]+[^ \t\n]")
                  (not (looking-at "^[ \t]*[0-9]"))
                  (not (looking-at "^[ \t]*Appendix[ \t]"))
                  (let ((c (save-excursion (skip-chars-forward " \t")
                                           (current-column))))
                    (>= c title-col)))
        (setq te (line-end-position))
        (setq extra (1+ extra))
        (forward-line 1)))
    (cons te extra)))

(defun rfcview:read-buttonize-toc ()
  "Make Table of Contents entries clickable buttons that jump to their section.
Looks up each TOC entry in the anchor tables built by `rfcview:read-fontify':
numbered entries match `rfcview:read-section-anchors-by-number', unnumbered
entries fall back to `-by-title'. Lines with no matching anchor are left as
plain text. Does nothing if the buffer has no \"Table of Contents\" heading
or if the anchor tables are empty."
  (when (and (hash-table-p rfcview:read-section-anchors-by-number)
             (hash-table-p rfcview:read-section-anchors-by-title)
             (> (+ (hash-table-count rfcview:read-section-anchors-by-number)
                   (hash-table-count rfcview:read-section-anchors-by-title))
                0))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward
               "^[ \t]*\\(?:[0-9]+\\.[ \t]+\\)?Table of Contents[ \t]*$" nil t)
          (forward-line 1)
          (let ((toc-end (save-excursion
                           (if (re-search-forward
                                rfcview:section-heading-regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
            (with-silent-modifications
              (while (< (point) toc-end)
                (let ((extra 0))
                  (cond
                   ;; Numbered: "   1.2.  Title ............. 7" (title may wrap)
                   ((looking-at
                     "^[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.+?\\)\\(?:[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash num rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te)))
                   ;; Appendix subsection: "   A.1  Foo ........... 30"
                   ((looking-at
                     "^[ \t]*\\([A-Z]\\.[0-9]+\\(?:\\.[0-9]+\\)?\\)\\.?[ \t]+\\(.+?\\)\\(?:[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash num rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te)))
                   ;; Appendix: "   Appendix A.  Title ......... 30"
                   ((looking-at
                     "^[ \t]*Appendix[ \t]+\\([A-Z]\\)\\.?[ \t]+\\(.+?\\)\\(?:[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)?[ \t]*$")
                    (let* ((letter (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash letter rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te)))
                   ;; Unnumbered: "   Acknowledgements ........... 25"
                   ((looking-at
                     "^[ \t]*\\([A-Z][^\n]*?\\)\\(?:[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)?[ \t]*$")
                    (let* ((title (match-string-no-properties 1))
                           (tb (match-beginning 1))
                           (te (match-end 1))
                           (target (gethash (rfcview:read--normalize-title title)
                                            rfcview:read-section-anchors-by-title)))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail te))))
                  (forward-line (1+ extra)))))))))))

(defun rfcview:read-trim-leading-blanks ()
  "Hide blank lines at the very beginning of the RFC buffer."
  (let ((end (save-excursion
               (goto-char (point-min))
               (if (re-search-forward "[^ \t\ufeff\n]" nil t)
                   (1- (point))
                 (point-min)))))
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
      (insert "    TAB / S-TAB next / previous button\n")
      (insert "    RET         follow link\n\n")
      (insert "  View\n")
      (insert "    + / = / -   increase / reset / decrease text scale\n")
      (insert "    o           view original file\n")
      (insert "    q           quit\n")
      (insert "    ?           this help\n"))
    (view-mode 1)
    (goto-char (point-min)))
  (display-buffer "*RFC Help*"))

(defun rfcview:read-mode (number file)
  "Major mode for reading RFC NUMBER from cached FILE.
\\{rfcview:read-mode-map}"
  (kill-all-local-variables)
  (use-local-map rfcview:read-mode-map)
  (setq mode-name "RFC"
        major-mode 'rfcview:read-mode
        buffer-read-only t
        rfcview:read-source-file file
        rfcview:read-rfc-number number)
  (rfcview:read-fontify)
  (rfcview:read-trim-leading-blanks)
  (rfcview:read-hide-page-breaks)
  ;; TOC must run before refs so a TOC entry's section-link covers any
  ;; "RFC NNNN" fragment in the title; refs then skips already-buttoned ranges.
  (rfcview:read-buttonize-toc)
  (rfcview:read-buttonize-refs)

  ;; Load and uniform link button style w/ goto-address-mode
  (make-variable-buffer-local 'goto-address-highlight-keymap)
  (make-variable-buffer-local 'face-remapping-alist)
  (setq face-remapping-alist
        `((link ,(custom-face-attributes-get 'rfcview:button-face nil))))
  (goto-address-mode 1)
  (let ((map goto-address-highlight-keymap))
    (define-key map (kbd "RET") #'goto-address-at-point)
    (define-key map (kbd "<mouse-1>")  #'goto-address-at-point))
  (run-hooks 'rfcview-read-mode-hook))

(defun rfcview:read-view-original ()
  "Open the raw cached txt file for this RFC in text-mode."
  (interactive)
  (unless rfcview:read-source-file
    (error "Source file path not recorded for this buffer"))
  (let ((buf (find-file-noselect rfcview:read-source-file)))
    (with-current-buffer buf (text-mode) (read-only-mode))
    (pop-to-buffer buf)))

(defun rfcview:open-rfc-txt (number file)
  "Open locally cached txt FILE as RFC NUMBER and return the buffer."
  (let ((buffer (get-buffer-create (format "*RFC %04d*" number))))
    (with-current-buffer buffer
      (insert-file-contents file)
      (set-buffer-modified-p nil)
      (rfcview:read-mode number file))
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
