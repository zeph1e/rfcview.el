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
   ;; Top-level (X.) numeric heading with commas in the title — e.g.
   ;; RFC 9959 §2 "Language, Notation, and Terms".  The in-group-1 numeric
   ;; alt rejects commas to block list items; this alt re-admits commas
   ;; under the same non-period-last-char rule used for X.Y+ headings,
   ;; which rejects single-line sentence-shape items like "3.  Foo, bar.".
   "\\|^\n[0-9]+\\.[ \t]+[A-Z(\"][^\n]*[^.\n]\n\n"
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
   ;; Appendix subsection: "A.1.  Title" / "B.10 Title" / "A.4.1. Title" (1-2 digit
   ;; per segment to avoid X.509-style false hits; one or more segments to support
   ;; arbitrary nesting depth), also handles a title that wraps onto one indented
   ;; continuation line.
   "\\|^\n[A-Z]\\(?:\\.[0-9]\\{1,2\\}\\)+\\.?[ \t]+[A-Z][^\n]*\\(?:\n[ \t]\\{5,\\}[^\n]+\\)?\n\n"
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
numbered titles (`X.') may contain commas only when the title does not end
with a period (RFC 9959 §2 \"Language, Notation, and Terms\"); sentence-shape
list items like \"3.  Foo, bar.\" are still rejected.")

(defconst rfcview:supported-formats '(txt pdf html xml)
  "All formats rfcview can route to a viewer.
`txt'/`pdf' are downloaded and opened in Emacs; `html'/`xml' are
handed to `browse-url' and not cached locally.")

(defconst rfcview:open-rfc-functions '((txt . rfcview:open-rfc-txt)
                                       (pdf . rfcview:open-rfc-pdf))
  "Alist mapping a format symbol from `rfcview:supported-formats' to
its open function.  Each handler is called with (NUMBER FILE
&optional SECTION) — the RFC number, a locally-cached file path, and
an optional section key (number or title) to jump to — and must
return the opened buffer (or nil if unavailable).

To wire a new locally-cached format, add an (FMT . FN) entry here and
add FMT to `rfcview:supported-formats'.  Formats listed in
`rfcview:supported-formats' but absent from this alist (e.g. `html',
`xml') fall through to `rfcview:open-rfc-fallback', which opens the
document in the user's browser via `browse-url' and does not cache
it locally.")

(defvar rfcview:read-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "f") 'forward-char)

    ;; section navigation
    (define-key map (kbd "]") 'rfcview:read-next-section)
    (define-key map (kbd "[") 'rfcview:read-prev-section)
    (define-key map (kbd "j") 'rfcview:read-jump-to-section)

    ;; link navigation (buttons + goto-address URL overlays)
    (define-key map (kbd "<tab>") 'rfcview:read-forward-link)
    (define-key map (kbd "<backtab>") 'rfcview:read-backward-link)

    ;; history navigation across button-driven jumps
    (define-key map (kbd "B") 'rfcview:read-history-back)
    (define-key map (kbd "F") 'rfcview:read-history-forward)
    (define-key map (kbd "C-c C-b") 'rfcview:read-history-back)
    (define-key map (kbd "C-c C-f") 'rfcview:read-history-forward)

    ;; font scale
    (define-key map [(?0)] 'text-scale-adjust)
    (define-key map [(?-)] 'text-scale-adjust)
    (define-key map [(?+)] 'text-scale-adjust)
    (define-key map [(?=)] 'text-scale-adjust)

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

(defun rfcview:read-jump-to-section (section &optional inhibit-nav-push)
  "Jump to SECTION in the current RFC reader buffer.
SECTION is a string — a section number (\"3.1\", \"A.1\", \"A\") or a
section title (\"Acknowledgements\").  Looks up
`rfcview:read-section-anchors-by-number' first, then falls back to
`rfcview:read-section-anchors-by-title'.  Pushes the current location
onto the history stack before jumping so `B' returns here, unless
INHIBIT-NAV-PUSH is non-nil — used by `rfcview:open-rfc-txt' when
opening a fresh buffer, where the caller has already pushed the origin
and pushing position 1 of the new buffer would stack the history
twice.  Does nothing when SECTION is nil or blank; messages when no
matching anchor is found.  When called interactively, prompts for
SECTION."
  (interactive "sSection to jump: ")
  (when (and (stringp section) (> (length (string-trim section)) 0))
    (let ((target
           (or (and (hash-table-p rfcview:read-section-anchors-by-number)
                    (gethash (rfcview:read--normalize-number section)
                             rfcview:read-section-anchors-by-number))
               (and (hash-table-p rfcview:read-section-anchors-by-title)
                    (gethash (rfcview:read--normalize-title section)
                             rfcview:read-section-anchors-by-title)))))
      (if (markerp target)
          (progn
            (unless inhibit-nav-push (rfcview:nav-push))
            (goto-char target)
            (when (eq (window-buffer) (current-buffer))
              (recenter 0)))
        (message "Section %s not found" section)))))

(defun rfcview:read--next-goto-address (pos)
  "Position of the next `goto-address' overlay strictly after POS, or nil."
  (let ((p (next-overlay-change pos))
        found)
    (while (and (not found) p (< p (point-max)))
      (if (cl-some (lambda (ov) (overlay-get ov 'goto-address))
                   (overlays-at p))
          (setq found p)
        (setq p (next-overlay-change p))))
    found))

(defun rfcview:read--prev-goto-address (pos)
  "Position of the previous `goto-address' overlay strictly before POS, or nil."
  (let ((p (previous-overlay-change pos))
        found)
    (while (and (not found) p (> p (point-min)))
      (if (cl-some (lambda (ov) (overlay-get ov 'goto-address))
                   (overlays-at p))
          (setq found p)
        (setq p (previous-overlay-change p))))
    found))

(defun rfcview:read--find-link (pos forward)
  "Position of the next link from POS, going FORWARD when non-nil.
Considers both buttons and `goto-address-mode' URL/email overlays."
  (let* ((btn (if forward (next-button pos) (previous-button pos)))
         (btn-pos (and btn (button-start btn)))
         (addr-pos (if forward
                       (rfcview:read--next-goto-address pos)
                     (rfcview:read--prev-goto-address pos))))
    (cond
     ((and btn-pos addr-pos)
      (if forward (min btn-pos addr-pos) (max btn-pos addr-pos)))
     (btn-pos)
     (addr-pos))))

(defun rfcview:read-forward-link (&optional n)
  "Move to the next link after point.
Considers both buttons and URLs highlighted by `goto-address-mode'.
With prefix N, move N links forward; negative N moves backward."
  (interactive "p")
  (setq n (or n 1))
  (unless (zerop n)
    (let ((forward (> n 0))
          (count (abs n))
          target)
      (dotimes (_ count)
        (setq target (rfcview:read--find-link (point) forward))
        (if target
            (goto-char target)
          (user-error (if forward "No next link" "No previous link")))))))

(defun rfcview:read-backward-link (&optional n)
  "Move to the previous link before point.
With prefix N, move N links backward."
  (interactive "p")
  (rfcview:read-forward-link (- (or n 1))))

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
     ;; Numeric heading matched outside group 1 (top-level X. with commas, or
     ;; wrapped/comma-bearing X.Y+ subsection — only line 1 is in heading-line).
     ((string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.*\\)" heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ((string-match "\\`\\([A-Z]\\(?:\\.[0-9]+\\)+\\)\\.?[ \t]+\\(.*\\)"
                    heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     (t
      (puthash (rfcview:read--normalize-title heading-line)
               marker rfcview:read-section-anchors-by-title)))))

(defun rfcview:read--cached-authors ()
  "Return author list for the current RFC from `rfcview:rfc-cache', or nil."
  (let* ((table (and rfcview:rfc-cache (plist-get rfcview:rfc-cache :table)))
         (entry (and (hash-table-p table)
                     (numberp rfcview:read-rfc-number)
                     (> rfcview:read-rfc-number 0)
                     (gethash rfcview:read-rfc-number table))))
    (and entry (plist-get entry :authors))))

(defun rfcview:read--author-regexp (name)
  "Return a regexp matching NAME (cached form) or a plausible expansion.
\"J. Doe\" matches \"J. Doe\" and \"John Doe\".  Non-ASCII Latin
letters (accents, diacritics) are honored via `[[:alpha:]]'.  A
trailing \", Ed.\" suffix in NAME is preserved in the regexp."
  (let* ((ed-re ",[ \t]*Ed\\.?")
         (ed-tail (string-match (concat ed-re "\\'") name))
         (core (if ed-tail (substring name 0 ed-tail) name))
         (tokens (split-string (string-trim core) "[ \t]+" t))
         (surname (car (last tokens)))
         (initials (butlast tokens)))
    (concat (mapconcat (lambda (tok)
                         (concat (regexp-quote (substring tok 0 1))
                                 "[[:alpha:]]*\\.?"))
                       initials "[ \t]+")
            (and initials "[ \t]+")
            (regexp-quote surname)
            (and ed-tail (concat "[ \t]*" ed-re)))))

(defun rfcview:read--authors-regexp (authors)
  "Build a regexp matching any name in AUTHORS, anchored to a full line."
  (when authors
    (concat "\\`[ \t]*\\(?:"
            (mapconcat #'rfcview:read--author-regexp authors "\\|")
            "\\)[ \t]*\\'")))

(defun rfcview:read-fontify ()
  "Apply faces to RFC header, title, and section headings via text properties.
Also populates `rfcview:read-section-anchors-by-number' and -by-title with
markers pointing at each heading, used later by `rfcview:read-buttonize-toc'."
  (setq rfcview:read-section-anchors-by-number (make-hash-table :test 'equal)
        rfcview:read-section-anchors-by-title  (make-hash-table :test 'equal))
  (with-silent-modifications
    (save-excursion
      ;; Header block: from start to the first blank line that does not
      ;; immediately precede a known author line.  RFC 9893 has an internal
      ;; blank line inside the header because one author appears without an
      ;; accompanying organization, and the bare "stop at first blank"
      ;; heuristic would otherwise truncate the header early.
      (goto-char (point-min))
      ;; recent has \ufeff at the very early of document
      (let* ((header-start (if (re-search-forward "^[^\ufeff\n]+$" nil t)
                               (line-beginning-position)
                             (point-min)))
             (author-re (rfcview:read--authors-regexp
                         (rfcview:read--cached-authors))))
        (goto-char header-start)
        (forward-line 1)
        (while (and (not (eobp))
                    (or (not (looking-at "^[ \t]*$"))
                        (and author-re
                             (save-excursion
                               (forward-line 1)
                               (string-match
                                author-re
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))))
          (forward-line 1))
        (put-text-property header-start (point) 'face 'rfcview:read-rfc-header-face))
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
                                 (rfcview:nav-push)
                                 (rfcview:read-rfc (button-get btn 'number)))
                       'help-echo (format "RFC %d : %s" num
                                          (when rfc (plist-get rfc :title))))
          (save-excursion
            (when (looking-back
                   "\\(section[ \n]+\\)\\([0-9A-Z.]+\\)[ \n]+of[ \n]+\\[?RFC ?[0-9]+\\]?" nil)
              (let* ((section (match-string 2)))
                (make-button (match-beginning 1) (match-end 2)
                             'type 'rfcview:rfc-link-button
                             'number num
                             'section section
                             'action (lambda (btn)
                                 (rfcview:nav-push)
                                 (rfcview:read-rfc (button-get btn 'number)
                                                   (button-get btn 'section)))
                             'help-echo (format "Jump to Section %s of RFC %d"
                                                section num))))))))))

(defun rfcview:read--make-section-button (beg end target)
  "Wrap [BEG, END) in a section-link button that jumps to marker TARGET."
  (make-button beg end
               'type 'rfcview:section-link-button
               'target target
               'action (lambda (btn)
                         (let ((m (button-get btn 'target)))
                           (when (markerp m)
                             (rfcview:nav-push)
                             (goto-char m)
                             (when (eq (window-buffer) (current-buffer))
                               (recenter 0)))))
               'help-echo "Jump to section"))

(defun rfcview:read--dim-toc-tail (title-end-on-line-1 &optional entry-end)
  "Dim the dot-leader and trailing page number of a TOC entry.
TITLE-END-ON-LINE-1 is the position of the title's end on the first line.
ENTRY-END, when given, bounds a wrapped entry — dim line 1 from
TITLE-END-ON-LINE-1 to EOL, then walk forward and dim the leader on each
continuation line up to ENTRY-END.  This handles entries where the leader
sits on a continuation line (e.g. RFC 5246 F.1.1.3).  A no-op when there
is nothing to dim (TOCs without leaders, like RFC 9227)."
  (save-excursion
    (goto-char title-end-on-line-1)
    (let ((eol-1 (line-end-position)))
      (when (> eol-1 title-end-on-line-1)
        (put-text-property title-end-on-line-1 eol-1
                           'face 'rfcview:read-toc-leader-face)))
    (when entry-end
      (forward-line 1)
      (while (< (point) entry-end)
        (when (re-search-forward
               "\\([ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)[ \t]*$"
               (line-end-position) t)
          (put-text-property (match-beginning 1) (line-end-position)
                             'face 'rfcview:read-toc-leader-face))
        (forward-line 1)))))

(defun rfcview:read--absorb-toc-continuations (title-beg title-end limit)
  "Extend a TOC title that wraps onto continuation lines.
TITLE-BEG and TITLE-END bracket the title text already matched on the
current line; point must be on that line.  LIMIT bounds the search.
Returns a cons (NEW-TITLE-END . LINES-CONSUMED).  A line counts as a
continuation if it is non-blank, indented to or past TITLE-BEG's column,
and does not start with a section number or \"Appendix\".

On each absorbed line, NEW-TITLE-END is the start of the dot-leader if one
is present (so the section-link button can be shrunk to exclude the
leader, otherwise its overlay face would override the dim).  When the
absorbed line has no leader, NEW-TITLE-END is end-of-line."
  (let ((title-col (save-excursion (goto-char title-beg) (current-column)))
        (te title-end)
        (extra 0))
    (save-excursion
      (forward-line 1)
      (while (and (< (point) limit)
                  (looking-at "^[ \t]+[^ \t\n]")
                  (not (looking-at "^[ \t]*[0-9]"))
                  (not (looking-at "^[ \t]*[A-Z]\\.[0-9]"))
                  (not (looking-at "^[ \t]*Appendix[ \t]"))
                  (let ((c (save-excursion (skip-chars-forward " \t")
                                           (current-column))))
                    (>= c title-col)))
        (setq te (save-excursion
                   (beginning-of-line)
                   (if (re-search-forward
                        "[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+[ \t]*$"
                        (line-end-position) t)
                       (match-beginning 0)
                     (line-end-position))))
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
                      (rfcview:read--dim-toc-tail line1-te te)))
                   ;; Appendix subsection: "   A.1  Foo ......... 30",
                   ;; nesting may go arbitrary depth ("A.4.1", "A.4.1.1", ...).
                   ((looking-at
                     "^[ \t]*\\([A-Z]\\(?:\\.[0-9]+\\)+\\)\\.?[ \t]+\\(.+?\\)\\(?:[ \t]+\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash num rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te te)))
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
                      (rfcview:read--dim-toc-tail line1-te te)))
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

(defun rfcview:nav-push ()
  "Push the current reader location onto the BACK stack and clear FORWARD.
Called by button actions just before they leave the current location."
  (when (and (numberp rfcview:read-rfc-number)
             (> rfcview:read-rfc-number 0))
    (let* ((rec (cons rfcview:read-rfc-number (point)))
           (back (car rfcview:nav-history))
           (new-back (if (equal rec (car back)) back (cons rec back))))
      (when (> (length new-back) rfcview:nav-history-max)
        (setq new-back (butlast new-back)))
      (setq rfcview:nav-history (cons new-back nil)))))

(defun rfcview:nav--restore (rec)
  "Restore navigation record REC: (RFC-NUMBER . POSITION).
Four cases by target buffer status:
- Already in the current buffer: just `goto-char'.
- Visible in another window of this frame: `select-window' it.
- Buffer exists but no window: `switch-to-buffer' in the current window.
- Buffer was killed: re-open via `rfcview:read-rfc' (uses local cache)."
  (let* ((num (car rec))
         (pos (cdr rec))
         (buf-name (format "*RFC %04d*" num))
         (buf (get-buffer buf-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((and buf (eq buf (current-buffer)))
      (goto-char pos))
     (win
      (select-window win)
      (goto-char pos))
     (buf
      (switch-to-buffer buf)
      (goto-char pos))
     (t
      (rfcview:read-rfc num)
      (goto-char pos)))
    (when (eq (window-buffer) (current-buffer))
      (recenter))))

(defun rfcview:read-history-back ()
  "Go back to the previous reader location.
Records the current location onto the forward stack."
  (interactive)
  (let ((back (car rfcview:nav-history))
        (forward (cdr rfcview:nav-history)))
    (unless back (user-error "No earlier location"))
    (let* ((target (car back))
           (cur (cons rfcview:read-rfc-number (point)))
           (new-back (cdr back))
           (new-forward (if (equal cur (car forward)) forward
                          (cons cur forward))))
      (setq rfcview:nav-history (cons new-back new-forward))
      (rfcview:nav--restore target)
      (message "Back to RFC %d" (car target)))))

(defun rfcview:read-history-forward ()
  "Go forward to the next reader location.
Records the current location onto the back stack."
  (interactive)
  (let ((back (car rfcview:nav-history))
        (forward (cdr rfcview:nav-history)))
    (unless forward (user-error "No later location"))
    (let* ((target (car forward))
           (cur (cons rfcview:read-rfc-number (point)))
           (new-back (if (equal cur (car back)) back (cons cur back)))
           (new-forward (cdr forward)))
      (setq rfcview:nav-history (cons new-back new-forward))
      (rfcview:nav--restore target)
      (message "Forward to RFC %d" (car target)))))

(defun rfcview:read--restyle-goto-address-overlays (&optional start end)
  "Sync goto-address overlays in [START, END) with rfcview's button styling.
`goto-address-mode' hardcodes a help-echo referring to its default bindings
and uses `highlight' for `mouse-face'.  Replace both on every URL/mail overlay
so tooltips and hover styling match the rfcview keymap and button look.
Registered with `jit-lock' so overlays created lazily as the user scrolls
are restyled too, not just the ones present at mode-setup time."
  (dolist (ov (overlays-in (or start (point-min)) (or end (point-max))))
    (when (overlay-get ov 'goto-address)
      (overlay-put ov 'help-echo "mouse-1, RET: follow URL")
      (overlay-put ov 'mouse-face 'rfcview:mouse-face))))

(defun rfcview:read-quit ()
  "Bury the RFC reader buffer and return to the RFC index.
If the `*RFC INDEX*' window is visible, select it.  Otherwise, if
the buffer still exists, switch to it in the current window.  If
the index buffer has been killed, just bury the reader."
  (interactive)
  (bury-buffer)
  (let* ((buffer (get-buffer "*RFC INDEX*"))
         (index-win (and buffer (get-buffer-window buffer))))
    (cond
     (index-win (select-window index-win))
     (buffer    (switch-to-buffer buffer)))))

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
      (insert "    n / p       next / previous line\n")
      (insert "    b / f       backward / forward char\n")
      (insert "    ] / [       next / previous section\n")
      (insert "    j           jump to section by number or title\n")
      (insert "    TAB / S-TAB next / previous link (RFC ref, TOC entry, URL)\n")
      (insert "    RET         follow link\n")
      (insert "    B / C-c C-b history back (after following a link)\n")
      (insert "    F / C-c C-f history forward\n\n")
      (insert "  View\n")
      (insert "    + / 0 / -   increase / reset / decrease text scale\n")
      (insert "    o           view original file\n")
      (insert "    q           quit\n")
      (insert "    ?           this help\n"))
    (view-mode 1)
    (goto-char (point-min)))
  (display-buffer "*RFC Help*"))

(defun rfcview:read--init-goto-address ()
  "Enable `goto-address-mode' in the current buffer and sync its
overlay styling with rfcview's buttons.  Restyles overlays already
present, and appends `rfcview:read--restyle-goto-address-overlays'
to `jit-lock-functions' so overlays created lazily (as the user
scrolls into unfontified regions) are restyled too.

The append is critical: `jit-lock-register' would prepend, placing
the restyle BEFORE `goto-address-fontify-region' — it would then
run on regions with no overlays yet and be a silent no-op."
  (goto-address-mode 1)
  (with-eval-after-load 'goto-addr
    (make-variable-buffer-local 'goto-address-highlight-keymap)
    (make-variable-buffer-local 'face-remapping-alist)
    (setq face-remapping-alist
          `((link ,(custom-face-attributes-get 'rfcview:button-face nil))))
    (let ((map goto-address-highlight-keymap))
      (define-key map (kbd "RET") #'goto-address-at-point)
      (define-key map (kbd "<mouse-1>")  #'goto-address-at-point)))
  (rfcview:read--restyle-goto-address-overlays)
  (add-hook 'jit-lock-functions
            #'rfcview:read--restyle-goto-address-overlays t t))

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
  (rfcview:read--init-goto-address)
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

(defun rfcview:open-rfc-fallback (number fmt)
  "Open RFC NUMBER as FMT in the user's browser vina `browse-url'.
Used for formats not rendered in Emacs (html, xml).  The document is
not cached locally."
  (browse-url (format "%srfc%d.%s"
                      rfcview:rfc-base-url number (symbol-name fmt))))

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

(defun rfcview:read--format-order (preferred available)
  "Return the order of formats to try when opening an RFC.
PREFERRED is `rfcview:preferred-format'.  AVAILABLE is the entry's
`:format' list from the rfc-index cache (case-insensitive strings
like \"TXT\"); unsupported tokens are dropped.

The result is the supported formats that appear in AVAILABLE, with
PREFERRED first when it is listed.  When PREFERRED is not listed it
is dropped (the index says it is unavailable).  When nothing supported
is listed, returns nil — the caller treats that as \"unavailable\"."
  (seq-intersection (cons preferred (remove preferred rfcview:supported-formats))
                    (mapcar (lambda (s) (intern (downcase s)))
                            available)))

(defun rfcview:read-rfc (number &optional section)
  "Open RFC NUMBER in the preferred format and pop to its buffer.
Format selection follows `rfcview:read--format-order' against the
cached `:format' for NUMBER, so only formats the rfc-index advertises
are tried.  Each candidate format is dispatched through
`rfcview:open-rfc-functions': a non-nil handler downloads (if needed)
and opens the file in Emacs; a nil entry (or a format missing from the
alist, e.g. `html'/`xml') is handed to `rfcview:open-rfc-fallback',
which opens the document in the user's browser and stops the search.
Signals an error when no candidate format yields a buffer or browser
hand-off."
  (let* ((entry (and (hash-table-p (plist-get rfcview:rfc-cache :table))
                     (gethash number (plist-get rfcview:rfc-cache :table))))
         (formats (rfcview:read--format-order rfcview:preferred-format
                                              (plist-get entry :format)))
         (buffer
          (or (get-buffer (format "*RFC %04d*" number))
              (catch 'found
                (dolist (fmt formats)
                  (let ((fn (cdr (assq fmt rfcview:open-rfc-functions))))
                    (unless fn
                      (rfcview:open-rfc-fallback number fmt section)
                      (throw 'found 'browser))
                    (let* ((f (format "%srfc%04d.%s"
                                      rfcview:local-directory number
                                      (symbol-name fmt)))
                           (file (if (file-exists-p f) f
                                   (rfcview:download-rfc number fmt f))))
                      (when file
                        (throw 'found (funcall fn number file))))))))))
    (cond ((bufferp buffer) (with-current-buffer buffer
                              (when (eq major-mode 'rfcview:read-mode)
                                (pop-to-buffer buffer)
                                (rfcview:read-jump-to-section section t))))
          ((eq buffer 'browser) nil)
          (t (error "RFC%04d is not available" number)))))

(provide 'rfcview-reader)
;;; rfcview-reader.el ends here
