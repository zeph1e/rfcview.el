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

;; ─── Translation state ───

(defvar-local rfcview:read-translation-cache nil
  "Hash table mapping `(BEG . END)' cons keys to translated strings.
Populated by `rfcview:read-translate-at-point' and
`rfcview:read-translate-document'; preserved across show/hide toggles.")

(defvar-local rfcview:read-translation-overlays nil
  "Hash table mapping `(BEG . END)' cons keys to translation overlays.
Value is the live overlay when the translation is showing, nil when it
has been hidden.")

(defvar-local rfcview:read-translation-job nil
  "Plist describing the running whole-document translation job, or nil.
Slots: :url-buffer, :remaining, :total, :done, :cancelled.")

(defvar-local rfcview:read--translation-mode nil
  "Translation mode owner: nil (idle), `single' (via `t'), or `all' (via `T').
`t' and `T' are mutually exclusive — each transition between them either
absorbs the previous overlay set (single→all) or wipes it (all→single).")

(defvar-local rfcview:read--translation-single-key nil
  "When `rfcview:read--translation-mode' is `single', the `(BEG . END)' key
of the currently-translated unit.  Otherwise nil.")

(defvar-local rfcview:read--region-overlays nil
  "Hash table mapping `(BEG . END)' keys to region-translation overlays.
Independent of `rfcview:read--translation-mode' — region translations
are additive (they do not displace the SINGLE/ALL paragraph state) and
take priority on the next `t' press: when any region overlay is visible,
`t' hides them all instead of running the paragraph dispatch.")

(defvar-local rfcview:read--doc-content-width nil
  "Cached widest column reached by any line in the buffer.
Used as a floor for translation overlay wrap width so short paragraphs
do not get rendered narrower than the rest of the document.")

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

(defface rfcview:read-translation-face
  '((((class color) (min-colors 88) (background dark))
     (:foreground "gold"))
    (((class color) (min-colors 88) (background light))
     (:foreground "royal blue"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face for translated paragraph text shown over the original via overlay."
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
   ;; (Regex literal exceeds 80 cols — kept on one line.)
   "\\|^\n[0-9]+\\(?:\\.[0-9]+\\)+\\.?[ \t]+[A-Z(\"][^\n]*\n[ \t]\\{5,\\}[a-zA-Z\"][^\n]*\n\n"
   ;; Appendix headings are unambiguous so both single-line and
   ;; one-continuation-line titles are matched.
   ;; Appendix (modern): "Appendix A.  Title" or wrapped onto a second
   ;; indented line.
   "\\|^\nAppendix [A-Z]\\.[ \t]+[A-Z][^\n]*\\(?:\n[ \t]\\{5,\\}[^\n]+\\)?\n\n"
   ;; Appendix (RFC 791 era, all-caps colon): "APPENDIX A:  Title"
   "\\|^\nAPPENDIX [A-Z]:[ \t]+[A-Z][^\n]*\n\n"
   "\\|^\nAPPENDIX [IVX]+[ \t]+-[ \t]+[A-Z][^\n]*\n\n"
   ;; Appendix subsection: "A.1.  Title" / "B.10 Title" /
   ;; "A.4.1. Title" (1-2 digits per segment to avoid X.509-style
   ;; false hits; one or more segments to support arbitrary nesting
   ;; depth), also handles a title that wraps onto one indented
   ;; continuation line.  Regex literal exceeds 80 cols — kept whole.
   "\\|^\n[A-Z]\\(?:\\.[0-9]\\{1,2\\}\\)+\\.?[ \t]+[A-Z][^\n]*\\(?:\n[ \t]\\{5,\\}[^\n]+\\)?\n\n"
   ;; Dash-form appendix subsection (RFC 1001 style): "A-1.  Title" /
   ;; "B-1.1  Title" / "B-6.1  Title".  1-2 digits per segment for
   ;; symmetry with the dot form.
   "\\|^\n[A-Z]-[0-9]+\\(?:\\.[0-9]\\{1,2\\}\\)*\\.?[ \t]+[A-Z][^\n]*\n\n"
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
  "Regexp matching RFC section headings across all eras.
Each alternative requires a preceding blank line.  Subsection titles
(`X.Y' form and deeper) may contain commas (RFC 8698 §6.2) and may
wrap onto an indented continuation line (RFC 8968 §2.6).  Top-level
numbered titles (`X.') may contain commas only when the title does
not end with a period (RFC 9959 §2 \"Language, Notation, and
Terms\"); sentence-shape list items like \"3.  Foo, bar.\" are still
rejected.")

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
    (define-key map (kbd "<mouse-8>") 'rfcview:read-history-back)
    (define-key map (kbd "<mouse-9>") 'rfcview:read-history-forward)
    (define-key map (kbd "<drag-mouse-8>") 'rfcview:read-history-back)
    (define-key map (kbd "<drag-mouse-9>") 'rfcview:read-history-forward)

    ;; font scale
    (define-key map [(?0)] 'text-scale-adjust)
    (define-key map [(?-)] 'text-scale-adjust)
    (define-key map [(?+)] 'text-scale-adjust)
    (define-key map [(?=)] 'text-scale-adjust)

    (define-key map (kbd "RET") 'push-button)

    (define-key map (kbd "o") 'rfcview:read-view-original)

    ;; translation
    (define-key map (kbd "t") 'rfcview:read-translate-at-point)
    (define-key map (kbd "T") 'rfcview:read-translate-document)
    (define-key map (kbd "l") 'rfcview:read-set-translation-language)

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
     ((string-match "\\`Appendix \\([A-Z]\\)\\.?[ \t]+\\(.*\\)"
                    heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title
                (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ((string-match
       "\\`APPENDIX \\([A-Z]\\|[IVX]+\\)\\(?:[: \t-]+\\(.*\\)\\)?\\'"
       heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (let ((title (match-string 2 heading-line)))
        (puthash (rfcview:read--normalize-title
                  (if (and title (> (length title) 0)) title heading-line))
                 marker rfcview:read-section-anchors-by-title)))
     ;; Dash-form appendix subsection (RFC 1001):
     ;; "A-1.  Title" / "B-1.1  Title".
     ((string-match "\\`\\([A-Z]-[0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.*\\)"
                    heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title
                (match-string 2 heading-line))
               marker rfcview:read-section-anchors-by-title))
     ;; Numeric heading matched outside group 1 (top-level X. with commas, or
     ;; wrapped/comma-bearing X.Y+ subsection — only line 1 is in heading-line).
     ((string-match
       "\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.*\\)"
       heading-line)
      (puthash (match-string 1 heading-line)
               marker rfcview:read-section-anchors-by-number)
      (puthash (rfcview:read--normalize-title
                (match-string 2 heading-line))
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
        (put-text-property header-start (point)
                           'face 'rfcview:read-rfc-header-face))
      ;; Title: first block of indented (centered) non-blank lines after
      ;; the header gap.
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
          (put-text-property title-start title-end
                             'face 'rfcview:read-rfc-title-face)))
      ;; Section headings: apply face to the heading line only (not
      ;; surrounding blanks).
      (while (re-search-forward rfcview:section-heading-regexp nil t)
        (let* ((line-start (1+ (match-beginning 0)))
               (line-end (save-excursion
                           (goto-char line-start)
                           (line-end-position)))
               (heading-line (buffer-substring-no-properties
                              line-start line-end))
               (num-prefix (and (match-beginning 1) (match-string 1))))
          (put-text-property line-start line-end
                             'face 'rfcview:read-rfc-section-face)
          (rfcview:read--register-anchor line-start heading-line num-prefix)
          ;; Back up one char when the match consumed a trailing blank
          ;; line so it remains available as the leading blank for the
          ;; next heading match.
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
                                 (rfcview:read-rfc
                                  (button-get btn 'number)))
                       'help-echo (format "RFC %d : %s" num
                                          (when rfc
                                            (plist-get rfc :title))))
          (save-excursion
            (when (looking-back
                   ;; Section A.1 of RFC 1234
                   ;; Section 2 of RFC1234
                   ;; Sections 1.2.1 and 1.2.3 of [RFC1234] (RFC 9951)
                   ;; Sections AA and BB and CC of RFC 1234
                   (concat "\\(section[s]?[ \n]+\\)\\([0-9A-Z.]+\\)"
                           "\\(\\(?:[ \n]+\\(?:and\\|or\\)[ \n]+[0-9A-Z.]+\\)+\\)?"
                           "[ \n]+of[ \n]+\\[?RFC[ \n]*[0-9]+\\]?")
                   nil)
              (let* ((section (match-string 2))
                     (more-begin (match-beginning 3))
                     (more-end (match-end 3)))
                (make-button (match-beginning 1) (match-end 2)
                             'type 'rfcview:rfc-link-button
                             'number num
                             'section section
                             'action (lambda (btn)
                                 (rfcview:nav-push)
                                 (rfcview:read-rfc (button-get btn 'number)
                                                   (button-get btn 'section)))
                             'help-echo (format "Jump to Section %s of RFC %d"
                                                section num))
                (save-excursion
                  (while (and more-begin
                              (< more-begin more-end)
                              (progn
                                (goto-char more-begin)
                                (looking-at
                                 "[ \n]+\\(and\\|or\\)[ \n]+\\([0-9A-Z.]+\\)")))
                    (let ((more-section (match-string 2)))
                      (make-button (match-beginning 2) (match-end 2)
                                   'type 'rfcview:rfc-link-button
                                   'number num
                                   'section more-section
                                   'action (lambda (btn)
                                             (rfcview:nav-push)
                                             (rfcview:read-rfc
                                              (button-get btn 'number)
                                              (button-get btn 'section)))
                                   'help-echo (format
                                               "Jump to Section %s of RFC %d"
                                               more-section num)))
                    (setq more-begin (match-end 2))))))))))))

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
      ;; `<=' (not `<') so a leader that sits on its own line (RFC 1005
      ;; style: title on lines 1-2, leader+page on line 3) is reached —
      ;; `absorb-toc-continuations' returns the start of that leader line
      ;; as `entry-end' so the section-link button stops there, but the
      ;; leader text itself still needs to be dimmed.
      (while (<= (point) entry-end)
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
          ;; In RFCs whose TOC entries are blank-line-separated (e.g. RFC
          ;; 1001), each entry matches `rfcview:section-heading-regexp', so
          ;; the naive "next heading" would land on the first TOC entry
          ;; itself.  Skip headings whose title line ends with a trailing
          ;; page number (the TOC's right column) — those are TOC entries,
          ;; not real headings.  The stop-regexp also accepts centered
          ;; ALL-CAPS headings with 10+ leading spaces (e.g. RFC 1005's
          ;; "FIGURES" between the TOC and body) — `section-heading-regexp'
          ;; deliberately requires `^[A-Z]' at column 0 for safety, but
          ;; here we need to halt before the figures table whose entries
          ;; otherwise get buttonized as TOC entries.
          (let* ((stop-regexp
                  (concat rfcview:section-heading-regexp
                          "\\|^\n[ \t]\\{10,\\}[A-Z][-A-Z() ]\\{,50\\}[A-Z]\n\n"))
                 (toc-end (save-excursion
                            (let ((found nil))
                              (while (and (not found)
                                          (re-search-forward stop-regexp nil t))
                                (let* ((mb (match-beginning 0))
                                       (str (match-string 0))
                                       (line1
                                        (or (nth 1
                                                 (split-string str "\n"))
                                            "")))
                                  (unless (string-match-p
                                           "[ \t][0-9]+[ \t]*\\'" line1)
                                    (setq found mb))))
                              (or found (point-max))))))
            (with-silent-modifications
              (while (< (point) toc-end)
                (let ((extra 0))
                  ;; Each TOC-format alternative below uses a long single-
                  ;; line regex literal — its compound title/leader/page
                  ;; structure does not split cleanly across lines.
                  (cond
                   ;; Numbered: "   1.2.  Title .......... 7" (may wrap)
                   ((looking-at
                     "^[ \t]*\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.+?\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash
                                    num
                                    rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations
                                  tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te te)))
                   ;; Appendix subsection: "   A.1  Foo ......... 30",
                   ;; nesting may go arbitrary depth ("A.4.1", "A.4.1.1", ...).
                   ((looking-at
                     "^[ \t]*\\([A-Z]\\(?:\\.[0-9]+\\)+\\)\\.?[ \t]+\\(.+?\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash
                                    num
                                    rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations
                                  tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te te)))
                   ;; Dash-form appendix subsection (RFC 1001 style):
                   ;; "   A-1.  Title ............ 61" or
                   ;; "  B-1.1  Title ........ 63".  Placed before the
                   ;; bare-APPENDIX case so the `A`/`B` letter doesn't get
                   ;; swallowed by Appendix's letter group.
                   ((looking-at
                     "^[ \t]*\\([A-Z]-[0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.+?\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((num (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash
                                    num
                                    rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations
                                  tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te te)))
                   ;; Bare APPENDIX (RFC 1001 style): "APPENDIX A    ...    61".
                   ;; The line has no title — just the letter and a page number.
                   ;; The button covers the "APPENDIX X" span.  Must precede the
                   ;; "Appendix [A-Z]. Title" case below, which (under
                   ;; case-fold-search) would otherwise greedily consume
                   ;; the page number as the title.
                   ((looking-at
                     "^[ \t]*\\(APPENDIX[ \t]+[A-Z]\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((tb (match-beginning 1))
                           (te (match-end 1))
                           (letter (substring
                                    (match-string-no-properties 1) -1))
                           (target (gethash
                                    letter
                                    rfcview:read-section-anchors-by-number)))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail te)))
                   ;; Appendix: "   Appendix A.  Title ......... 30"
                   ((looking-at
                     "^[ \t]*Appendix[ \t]+\\([A-Z]\\)\\.?[ \t]+\\(.+?\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((letter (match-string-no-properties 1))
                           (tb (match-beginning 2))
                           (line1-te (match-end 2))
                           (target (gethash
                                    letter
                                    rfcview:read-section-anchors-by-number))
                           (cont (rfcview:read--absorb-toc-continuations
                                  tb line1-te toc-end))
                           (te (car cont)))
                      (setq extra (cdr cont))
                      (when target
                        (rfcview:read--make-section-button tb te target))
                      (rfcview:read--dim-toc-tail line1-te te)))
                   ;; Unnumbered: "   Acknowledgements ........... 25"
                   ((looking-at
                     "^[ \t]*\\([A-Z][^\n]*?\\)\\(?:\\(?:\\(?:[ \t]*\\.\\)\\{2,\\}[ \t]*\\|[ \t]\\{3,\\}\\)[0-9]+\\)?[ \t]*$")
                    (let* ((title (match-string-no-properties 1))
                           (tb (match-beginning 1))
                           (te (match-end 1))
                           (target (gethash
                                    (rfcview:read--normalize-title title)
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

(defun rfcview:read-buffer-name (number)
  "Return the reader buffer name for RFC NUMBER, e.g. \"*RFC 42*\"."
  (format "*RFC %d*" number))

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
         (buf-name (rfcview:read-buffer-name num))
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

;; ─── Translation overlays ──────────────────────────────────────────────────

(defun rfcview:read--init-translation-state ()
  "Initialize buffer-local translation state."
  (setq rfcview:read-translation-cache (make-hash-table :test 'equal)
        rfcview:read-translation-overlays (make-hash-table :test 'equal)
        rfcview:read--region-overlays (make-hash-table :test 'equal)
        rfcview:read-translation-job nil
        rfcview:read--translation-mode nil
        rfcview:read--translation-single-key nil))

(defun rfcview:read--translation-state ()
  "Return the current translation state as a symbol.
One of `idle', `single', `running', `all-shown', `all-hidden'."
  (cond
   (rfcview:read-translation-job 'running)
   ((eq rfcview:read--translation-mode 'single) 'single)
   ((eq rfcview:read--translation-mode 'all)
    (if (rfcview:read--any-overlay-shown-p) 'all-shown 'all-hidden))
   (t 'idle)))

(defun rfcview:read--paragraph-bounds-at (pos)
  "Return (BEG . END) of the paragraph containing POS, or nil if blank.
BEG is the BOL of the paragraph's first non-blank line; END is the BOL
of the next blank line (or point-max)."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (if (looking-at-p "^[ \t]*$")
        nil
      (let (beg end)
        (save-excursion
          (if (re-search-backward "^[ \t]*$" nil t)
              (progn (forward-line 1) (setq beg (point)))
            (setq beg (point-min))))
        (save-excursion
          (forward-line 1)
          (if (re-search-forward "^[ \t]*$" nil t)
              (setq end (line-beginning-position))
            (setq end (point-max))))
        (cons beg end)))))

(defun rfcview:read--all-paragraphs ()
  "Return a list of (BEG . END) for every paragraph in document order."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (when (not (eobp))
          (let ((beg (point)))
            (while (and (not (eobp)) (not (looking-at-p "^[ \t]*$")))
              (forward-line 1))
            (push (cons beg (point)) result)))))
    (nreverse result)))

(defun rfcview:read--paragraphs-visible-first (paragraphs)
  "Reorder PARAGRAPHS so visible ones come first, then after, then before.
The visible region is `(window-start)' to `(window-end nil t)' of the
selected window.  In batch tests where no window is showing the buffer,
returns PARAGRAPHS unchanged."
  (let* ((win (get-buffer-window (current-buffer)))
         (ws (and win (window-start win)))
         (we (and win (window-end win t))))
    (if (not (and ws we))
        paragraphs
      (let (visible after before)
        (dolist (p paragraphs)
          (let ((b (car p))
                (e (cdr p)))
            (cond
             ((and (< b we) (> e ws)) (push p visible))
             ((>= b we)               (push p after))
             (t                       (push p before)))))
        (append (nreverse visible) (nreverse after) (nreverse before))))))

(defun rfcview:read--paragraph-indent (beg)
  "Return the count of leading spaces/tabs of the paragraph starting at BEG."
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \t")
    (- (point) beg)))

(defun rfcview:read--paragraph-target-width (beg end)
  "Return the maximum column reached by any line in the BEG..END paragraph.
Used to match the translated overlay's wrap width to the original's."
  (save-excursion
    (goto-char beg)
    (let ((max-col 0))
      (while (< (point) end)
        (end-of-line)
        (setq max-col (max max-col (current-column)))
        (forward-line 1))
      max-col)))

(defun rfcview:read--doc-content-width ()
  "Return the widest column reached anywhere in the buffer (cached).
Computed once per buffer and reused on every translation render."
  (or rfcview:read--doc-content-width
      (setq rfcview:read--doc-content-width
            (save-excursion
              (goto-char (point-min))
              (let ((mx 0))
                (while (not (eobp))
                  (end-of-line)
                  (setq mx (max mx (current-column)))
                  (forward-line 1))
                mx)))))

(defun rfcview:read--collapse-text (text)
  "Collapse every run of whitespace in TEXT (including newlines) to one space.
Leading and trailing whitespace are trimmed.  RFC text is hard-wrapped
with newlines and per-line indentation; sending it to Google Translate
as-is confuses the segmentation, so this normalisation runs first."
  (string-trim (replace-regexp-in-string "[ \t\n\r\v\f]+" " " text)))

(defun rfcview:read--cjk-no-space-char-p (ch)
  "Return non-nil if CH belongs to a script that wraps without spaces.
Hangul is excluded — Korean uses spaces between words, so its natural
break points are whitespace, same as Latin."
  (or (and (>= ch #x3000) (<= ch #x303F))   ; CJK Symbols and Punctuation
      (and (>= ch #x3040) (<= ch #x309F))   ; Hiragana
      (and (>= ch #x30A0) (<= ch #x30FF))   ; Katakana
      (and (>= ch #x3400) (<= ch #x4DBF))   ; CJK Unified Ideographs Ext A
      (and (>= ch #x4E00) (<= ch #x9FFF))   ; CJK Unified Ideographs
      (and (>= ch #xF900) (<= ch #xFAFF))   ; CJK Compatibility Ideographs
      (and (>= ch #xFF00) (<= ch #xFFEF)))) ; Halfwidth/Fullwidth Forms

(defun rfcview:read--tokenize-for-wrap (text)
  "Split TEXT into wrap tokens.
Each run of non-space, non-CJK characters becomes one token; each
CJK character (Japanese/Chinese, where words have no inter-character
spaces) becomes its own token."
  (let ((tokens nil) (i 0) (len (length text)))
    (while (< i len)
      (let ((ch (aref text i)))
        (cond
         ((memq ch '(?\s ?\t))
          (setq i (1+ i)))
         ((rfcview:read--cjk-no-space-char-p ch)
          (push (char-to-string ch) tokens)
          (setq i (1+ i)))
         (t
          (let ((start i))
            (while (and (< i len)
                        (let ((c (aref text i)))
                          (not (or (memq c '(?\s ?\t))
                                   (rfcview:read--cjk-no-space-char-p c)))))
              (setq i (1+ i)))
            (push (substring text start i) tokens))))))
    (nreverse tokens)))

(defun rfcview:read--wrap-translation-text (text indent max-width)
  "Wrap TEXT to MAX-WIDTH columns, using INDENT spaces as continuation prefix.
Uses `string-width' so CJK characters take their actual display width.
Each CJK character is its own breakable unit so wrapping works for
Japanese and Chinese, which lack inter-word spaces; no space is
inserted between adjacent CJK tokens in the output."
  (let* ((tokens (rfcview:read--tokenize-for-wrap text))
         (sep (concat "\n" (make-string indent ?\s)))
         lines line (col indent) prev-cjk)
    (dolist (tok tokens)
      (let* ((tw (string-width tok))
             (this-cjk (and (> (length tok) 0)
                            (rfcview:read--cjk-no-space-char-p (aref tok 0))))
             (need-space (and line (not (and prev-cjk this-cjk))))
             (added (+ (if need-space 1 0) tw)))
        (cond
         ((null line)
          (setq line (list tok)
                col (+ indent tw)))
         ((<= (+ col added) max-width)
          (when need-space (push " " line))
          (push tok line)
          (setq col (+ col added)))
         (t
          (push (mapconcat #'identity (nreverse line) "") lines)
          (setq line (list tok)
                col (+ indent tw))))
        (setq prev-cjk this-cjk)))
    (when line
      (push (mapconcat #'identity (nreverse line) "") lines))
    (mapconcat #'identity (nreverse lines) sep)))

(defconst rfcview:read--translation-wrap-slack 4
  "Extra columns added to the wrap target for translation overlays.
Word-boundary wrapping ends each line 0..N columns short of the target
where N is the next word's display width.  CJK words are wider than
Latin words on average, so without slack the translated lines visibly
end further left than the original.  4 columns is a heuristic compromise
that makes typical Korean/Japanese output align with the original's
right margin.")

(defun rfcview:read--wrap-translation (beg end text)
  "Wrap TEXT to match the BEG..END paragraph's indent and column width.
The wrap width is the larger of the paragraph's own widest line and the
document's overall widest line, plus `rfcview:read--translation-wrap-slack'
columns of slack so word-boundary wrapping of CJK text lands at the
original's right margin instead of well inside it.  A trailing newline
is appended when the original range ends in one, so the blank-line
separator to the next paragraph is preserved."
  (let* ((indent (rfcview:read--paragraph-indent beg))
         (paragraph (rfcview:read--paragraph-target-width beg end))
         (doc (rfcview:read--doc-content-width))
         (max-width (+ rfcview:read--translation-wrap-slack
                       (max (+ indent 20) paragraph doc)))
         (wrapped (rfcview:read--wrap-translation-text text indent max-width))
         (trailing (if (and (> end beg)
                            (eq (char-before end) ?\n))
                       "\n"
                     "")))
    (concat (make-string indent ?\s) wrapped trailing)))

;; ─── Overlay primitives (shared by paragraph and region tables) ───

(defun rfcview:read--put-overlay (beg end translation table marker)
  "Create the display overlay over BEG..END showing TRANSLATION.
TABLE is the hash-table keyed by (cons BEG END); any existing entry is
deleted first.  MARKER is stored as the overlay's `rfcview:translation'
property to distinguish paragraph (t) from region (\\='region) overlays."
  (let* ((key (cons beg end))
         (existing (gethash key table)))
    (when (overlayp existing) (delete-overlay existing))
    (let* ((display-text (rfcview:read--wrap-translation beg end translation))
           (faced (propertize display-text
                              'face 'rfcview:read-translation-face))
           (ov (make-overlay beg end)))
      (overlay-put ov 'display faced)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'rfcview:translation marker)
      (puthash key ov table)
      ov)))

(defun rfcview:read--drop-overlay (table key)
  "Delete the overlay at KEY in TABLE (if any); set the entry to nil."
  (let ((ov (gethash key table)))
    (when (overlayp ov) (delete-overlay ov))
    (puthash key nil table)))

(defun rfcview:read--overlay-shown-p (table)
  "Return non-nil if TABLE has at least one live overlay."
  (let (found)
    (when (hash-table-p table)
      (maphash (lambda (_k v) (when (overlayp v) (setq found t)))
               table))
    found))

(defun rfcview:read--drop-all-overlays (table)
  "Delete every live overlay in TABLE; cache is untouched."
  (when (hash-table-p table)
    (let (keys)
      (maphash (lambda (k v) (when (overlayp v) (push k keys)))
               table)
      (dolist (k keys) (rfcview:read--drop-overlay table k)))))

;; ─── Paragraph overlay wrappers ───

(defun rfcview:read--show-translation (beg end translation)
  "Create or restore the overlay replacing BEG..END with TRANSLATION."
  (rfcview:read--put-overlay beg end translation
                             rfcview:read-translation-overlays t))

(defun rfcview:read--hide-translation (key)
  "Delete the paragraph overlay registered under KEY."
  (rfcview:read--drop-overlay rfcview:read-translation-overlays key))

(defun rfcview:read--any-overlay-shown-p ()
  "Return non-nil if at least one paragraph translation overlay is shown."
  (rfcview:read--overlay-shown-p rfcview:read-translation-overlays))

(defun rfcview:read--hide-all-translations ()
  "Hide every visible paragraph translation overlay (cache preserved)."
  (rfcview:read--drop-all-overlays rfcview:read-translation-overlays))

;; ─── Region overlay wrappers ───

(defun rfcview:read--show-region-overlay (beg end translation)
  "Create the region overlay that visually replaces BEG..END with TRANSLATION.
Independent of the SINGLE/ALL paragraph overlay table — tracked in
`rfcview:read--region-overlays' so it can be hidden as a set."
  (rfcview:read--put-overlay beg end translation
                             rfcview:read--region-overlays 'region))

(defun rfcview:read--any-region-overlay-p ()
  "Return non-nil if at least one region-translation overlay is showing."
  (rfcview:read--overlay-shown-p rfcview:read--region-overlays))

(defun rfcview:read--hide-all-region-overlays ()
  "Delete every region-translation overlay (cache preserved)."
  (rfcview:read--drop-all-overlays rfcview:read--region-overlays))

(defun rfcview:read--paragraph-chunks-in-range (beg end)
  "Return list of (CBEG . CEND) for each non-blank-line run in BEG..END.
A blank line within the region terminates one chunk and starts the next.
Used to break a multi-paragraph region into per-paragraph translation
units so the original's shape is preserved instead of collapsing into a
single block."
  (save-excursion
    (let (chunks)
      (goto-char beg)
      (while (< (point) end)
        ;; Skip any blank lines at the start of the remaining range
        (while (and (< (point) end)
                    (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (when (< (point) end)
          (let ((cbeg (point)))
            (while (and (< (point) end)
                        (not (looking-at-p "^[ \t]*$")))
              (forward-line 1))
            (let ((cend (min (point) end)))
              (when (> cend cbeg)
                (push (cons cbeg cend) chunks))))))
      (nreverse chunks))))

(defun rfcview:read--translate-and-show-region (beg end)
  "Translate BEG..END as one or more per-paragraph region overlays.
The range is split at blank-line boundaries (`--paragraph-chunks-in-range')
so a region spanning multiple paragraphs becomes multiple independent
overlays — each translated and laid out individually, preserving the
original document's per-paragraph shape.  Reuses the shared translation
cache, keyed by each chunk's bounds.  Does not modify any SINGLE/ALL state."
  (let ((chunks (rfcview:read--paragraph-chunks-in-range beg end)))
    (cond
     ((null chunks)
      (message "Region is empty"))
     (t
      (let ((failures 0))
        (dolist (chunk chunks)
          (let* ((cbeg (car chunk))
                 (cend (cdr chunk))
                 (key (cons cbeg cend))
                 (cached (gethash key rfcview:read-translation-cache)))
            (cond
             (cached
              (rfcview:read--show-region-overlay cbeg cend cached))
             (t
              (let* ((raw (buffer-substring-no-properties cbeg cend))
                     (text (rfcview:read--collapse-text raw))
                     (translated (rfcview:translate-fetch text)))
                (cond
                 ((stringp translated)
                  (puthash key translated rfcview:read-translation-cache)
                  (rfcview:read--show-region-overlay cbeg cend translated))
                 (t
                  (setq failures (1+ failures)))))))))
        (cond
         ((zerop failures)
          (message (if (> (length chunks) 1)
                       "Region translated (%d chunks)"
                     "Region translated")
                   (length chunks)))
         ((= failures (length chunks))
          (message "Translation failed"))
         (t
          (message "%d of %d chunks failed to translate"
                   failures (length chunks)))))))))

(defun rfcview:read-set-translation-language ()
  "Prompt for a target translation language and persist the choice.
Existing cached translations are discarded (they belong to the previous
language); any currently-displayed translation overlays are hidden."
  (interactive)
  (let* ((choices (rfcview:translate--language-choices))
         (default-name (rfcview:translate--default-language-name))
         (current-name
          (or (car (rassoc rfcview:translate-target-language choices))
              default-name))
         (prompt (if current-name
                     (format "Translate to (current %s): " current-name)
                   "Translate to: "))
         (pick (completing-read prompt
                                (mapcar #'car choices)
                                nil t nil nil current-name))
         (code (cdr (assoc pick choices))))
    (unless code
      (user-error "No ISO-639 code available for %s" pick))
    (customize-save-variable 'rfcview:translate-target-language code)
    (when (hash-table-p rfcview:read-translation-overlays)
      (rfcview:read--hide-all-translations))
    (when (hash-table-p rfcview:read--region-overlays)
      (rfcview:read--hide-all-region-overlays))
    (when (hash-table-p rfcview:read-translation-cache)
      (clrhash rfcview:read-translation-cache))
    (setq rfcview:read--translation-mode nil
          rfcview:read--translation-single-key nil)
    (message "rfcview: target language set to %s (%s); cache cleared" pick code)
    code))

(defun rfcview:read--translate-and-show-single (bounds)
  "Translate the paragraph or region BOUNDS, show it, set mode=`single'.
Reuses the cache when present; otherwise fetches synchronously.
Returns non-nil on success, nil on translation failure."
  (let* ((beg (car bounds))
         (end (cdr bounds))
         (key bounds)
         (cached (gethash key rfcview:read-translation-cache)))
    (cond
     (cached
      (rfcview:read--show-translation beg end cached)
      (setq rfcview:read--translation-mode 'single
            rfcview:read--translation-single-key key)
      (message "Translation restored from cache")
      t)
     (t
      (message "Translating…")
      (let* ((raw (buffer-substring-no-properties beg end))
             (text (rfcview:read--collapse-text raw))
             (translated (rfcview:translate-fetch text)))
        (cond
         ((stringp translated)
          (puthash key translated rfcview:read-translation-cache)
          (rfcview:read--show-translation beg end translated)
          (setq rfcview:read--translation-mode 'single
                rfcview:read--translation-single-key key)
          (message "Translated")
          t)
         (t
          (message "Translation failed")
          nil)))))))

(defun rfcview:read-translate-at-point ()
  "Toggle translation of paragraph at point, region, or the region overlay.

Dispatch order:
  1. Any region-translation overlay visible → hide them all.  Pressing
     `t' anywhere clears the additive region overlays without disturbing
     the SINGLE/ALL paragraph state.
  2. Active region (mark) → translate that range as an independent
     region overlay; SINGLE/ALL paragraph state is left alone.
  3. Blank line, no region overlays → if SINGLE is on, hide it.
  4. Otherwise → run the paragraph state machine.

When a new overlay (single or region) is shown, point is moved to the
end of the unit and `window-start' is preserved so the buffer doesn't
scroll under the user."
  (interactive)
  (rfcview:translate--ensure-target-language)
  (cond
   ;; Rule 1: priority — hide region overlays
   ((rfcview:read--any-region-overlay-p)
    (rfcview:read--hide-all-region-overlays)
    (message "Region translation hidden"))
   ;; Rule 2: active region — additive translate (split across paragraphs)
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (win (get-buffer-window (current-buffer)))
           (ws (and win (window-start win))))
      (deactivate-mark)
      (rfcview:read--translate-and-show-region beg end)
      (when (rfcview:read--any-region-overlay-p)
        (goto-char end)
        (when (and win (window-live-p win))
          (set-window-start win ws t)))))
   (t
    ;; Rules 3+4: paragraph dispatch
    (let ((bounds (rfcview:read--paragraph-bounds-at (point))))
      (cond
       ((null bounds)
        (if (eq (rfcview:read--translation-state) 'single)
            (let ((current rfcview:read--translation-single-key))
              (rfcview:read--hide-translation current)
              (setq rfcview:read--translation-mode nil
                    rfcview:read--translation-single-key nil)
              (message "Translation hidden"))
          (message "Not inside a paragraph")))
       (t
        (let* ((win (get-buffer-window (current-buffer)))
               (ws (and win (window-start win)))
               (state (rfcview:read--translation-state))
               (key bounds))
          (cl-case state
            (idle
             (rfcview:read--translate-and-show-single bounds))
            (single
             (let ((current rfcview:read--translation-single-key))
               (cond
                ((equal current key)
                 (rfcview:read--hide-translation key)
                 (setq rfcview:read--translation-mode nil
                       rfcview:read--translation-single-key nil)
                 (message "Translation hidden"))
                (t
                 (when current
                   (rfcview:read--hide-translation current))
                 (rfcview:read--translate-and-show-single bounds)))))
            (running
             (rfcview:read--cancel-job)
             (rfcview:read--translate-and-show-single bounds))
            ((all-shown all-hidden)
             (rfcview:read--hide-all-translations)
             (setq rfcview:read--translation-mode nil
                   rfcview:read--translation-single-key nil)
             (rfcview:read--translate-and-show-single bounds)))
          (when (and (hash-table-p rfcview:read-translation-overlays)
                     (overlayp
                      (gethash bounds rfcview:read-translation-overlays)))
            (goto-char (cdr bounds))
            (when (and win (window-live-p win))
              (set-window-start win ws t))))))))))

(defun rfcview:read--process-next-paragraph (buffer)
  "Pop one paragraph off BUFFER's job queue and start its async fetch.
Called recursively from each fetch callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((job rfcview:read-translation-job))
        (cond
         ((or (null job) (plist-get job :cancelled)) nil)
         ((null (plist-get job :remaining))
          (let ((total (plist-get job :total)))
            (setq rfcview:read-translation-job nil)
            (message "Translation complete (%d paragraph(s))" total)))
         (t
          (let* ((remaining (plist-get job :remaining))
                 (next (car remaining))
                 (beg (car next))
                 (end (cdr next))
                 (raw (buffer-substring-no-properties beg end))
                 (text (rfcview:read--collapse-text raw))
                 (tgt (rfcview:translate--ensure-target-language))
                 (src rfcview:translate-source-language))
            (plist-put job :remaining (cdr remaining))
            (let ((url-buf
                   (rfcview:translate-fetch-async
                    text src tgt
                    (lambda (translated)
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (let ((curjob rfcview:read-translation-job))
                            (when (and curjob
                                       (not (plist-get curjob :cancelled)))
                              (when (stringp translated)
                                (puthash (cons beg end) translated
                                         rfcview:read-translation-cache)
                                (rfcview:read--show-translation
                                 beg end translated))
                              (plist-put curjob :done
                                         (1+ (plist-get curjob :done)))
                              (plist-put curjob :url-buffer nil)
                              (let ((done (plist-get curjob :done))
                                    (total (plist-get curjob :total)))
                                (when (or (= done total)
                                          (zerop (mod done 5)))
                                  (message "Translating %d/%d…" done total)))
                              (rfcview:read--process-next-paragraph
                               buffer)))))))))
              (plist-put job :url-buffer url-buf)))))))))

(defun rfcview:read--cancel-job ()
  "Abort the currently running document-translation job.
Resets mode to idle, hides every visible overlay, and clears single-key."
  (let ((job rfcview:read-translation-job))
    (when job
      (plist-put job :cancelled t)
      (let ((url-buf (plist-get job :url-buffer)))
        (when (buffer-live-p url-buf)
          (let ((proc (get-buffer-process url-buf)))
            (when (processp proc)
              (set-process-query-on-exit-flag proc nil)
              (delete-process proc)))
          (kill-buffer url-buf)))
      (let ((done (plist-get job :done))
            (total (plist-get job :total)))
        (rfcview:read--hide-all-translations)
        (rfcview:read--hide-all-region-overlays)
        (setq rfcview:read-translation-job nil
              rfcview:read--translation-mode nil
              rfcview:read--translation-single-key nil)
        (message "Translation cancelled (%d of %d done; original restored)"
                 done total)))))

(defun rfcview:read--start-or-restore-document ()
  "Show cached paragraph translations and queue uncached ones for fetch.
Sets `rfcview:read--translation-mode' to `all'.  Used by every T
transition that ends in an `all-shown' state."
  (rfcview:translate--ensure-target-language)
  (let* ((paragraphs (rfcview:read--paragraphs-visible-first
                      (rfcview:read--all-paragraphs)))
         cached to-fetch)
    (dolist (p paragraphs)
      (if (gethash p rfcview:read-translation-cache)
          (push p cached)
        (push p to-fetch)))
    (setq cached (nreverse cached)
          to-fetch (nreverse to-fetch))
    (dolist (p cached)
      (rfcview:read--show-translation
       (car p) (cdr p)
       (gethash p rfcview:read-translation-cache)))
    (setq rfcview:read--translation-mode 'all
          rfcview:read--translation-single-key nil)
    (cond
     ((and (null to-fetch) cached)
      (message "Translations restored from cache"))
     ((null to-fetch)
      (message "Nothing to translate"))
     (t
      (setq rfcview:read-translation-job
            (list :url-buffer nil
                  :remaining to-fetch
                  :total (length to-fetch)
                  :done 0
                  :cancelled nil))
      (message "Translating %d paragraph(s) (visible area first)…"
               (length to-fetch))
      (rfcview:read--process-next-paragraph (current-buffer))))))

(defun rfcview:read-translate-document ()
  "Translate every paragraph in the document, visible area first.

State machine:
- IDLE        → start a fresh job; visible-area paragraphs first.
- SINGLE      → absorb the single-`t' overlay into the document set
                and translate the rest; the existing overlay stays visible.
- RUNNING     → cancel the in-flight job, hide all overlays.
- ALL_SHOWN   → hide all overlays (cache preserved).
- ALL_HIDDEN  → re-show every cached translation."
  (interactive)
  (let ((state (rfcview:read--translation-state)))
    (cl-case state
      (running
       (rfcview:read--cancel-job))
      (all-shown
       (rfcview:read--hide-all-translations)
       (message "Translations hidden; original document restored"))
      (all-hidden
       (rfcview:read--start-or-restore-document))
      ;; idle or single — start fresh (single → absorbed into all)
      ((idle single)
       (rfcview:read--start-or-restore-document)))))

(defun rfcview:read-quit ()
  "Bury the RFC reader buffer and return to the RFC index.
If the `*RFC INDEX*' window is visible, select it.  Otherwise, if
the buffer still exists, switch to it in the current window.  If
the index buffer has been killed, just bury the reader."
  (interactive)
  (let* ((buffer (get-buffer "*RFC INDEX*"))
         (index-win (and buffer (get-buffer-window buffer))))
    (cond
     (index-win (bury-buffer) (select-window index-win))
     (buffer    (switch-to-buffer buffer))
     (t (bury-buffer)))))

(defun rfcview:read-show-help ()
  "Show a help buffer listing rfcview read mode keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*RFC Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "rfcview\n\n")
      (insert "  An Emacs tool for browsing, downloading, and reading\n")
      (insert "  IETF RFC documents.  Presents an interactive index with\n")
      (insert "  filtering by favorites, recents, or keyword search.  RFC\n")
      (insert "  documents are downloaded and cached locally on first\n")
      (insert "  access.\n\n")
      (insert "Keybindings\n\n")
      (insert "  Navigation\n")
      (insert "    n / p       next / previous line\n")
      (insert "    b / f       backward / forward char\n")
      (insert "    ] / [       next / previous section\n")
      (insert "    j           jump to section by number/title\n")
      (insert "    TAB / S-TAB next / previous link (RFC, TOC, URL)\n")
      (insert "    RET         follow link\n")
      (insert "    B / C-c C-b history back (after following a link)\n")
      (insert "    F / C-c C-f history forward\n\n")
      (insert "  View\n")
      (insert "    + / 0 / -   increase / reset / decrease text scale\n")
      (insert "    o           view original file\n")
      (insert "    q           quit\n")
      (insert "    ?           this help\n\n")
      (insert "  Translate\n")
      (insert "    t           dispatch:\n")
      (insert "                  - any region-translation overlay shown:\n")
      (insert "                    hide them all (at any point position);\n")
      (insert "                  - active region: translate the marked range\n")
      (insert "                    as an additive overlay (does not disturb\n")
      (insert "                    the paragraph state machine);\n")
      (insert "                  - blank line + SINGLE: hide the SINGLE;\n")
      (insert "                  - in a paragraph: SINGLE state machine\n")
      (insert "                    (different paragraph restores previous,\n")
      (insert "                    same paragraph toggles off)\n")
      (insert "    T           whole-document translate, visible area first,\n")
      (insert "                async serial; press again while running to\n")
      (insert "                cancel, or after completion to hide / re-show\n")
      (insert "                all overlays from cache\n")
      (insert "    l           choose target translation language\n")
      (insert "                (persisted via Customize; clears the cache)\n"))
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
  (rfcview:read--init-translation-state)
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
  (let ((buffer (get-buffer-create (rfcview:read-buffer-name number))))
    (with-current-buffer buffer
      (insert-file-contents file)
      (set-buffer-modified-p nil)
      (rfcview:read-mode number file))
    buffer))

(defun rfcview:open-rfc-pdf (number file)
  "Open cached PDF FILE as RFC NUMBER in `pdf-view-mode'; return the buffer.
Signals an error if pdf-tools is not installed."
  (unless (fboundp 'pdf-view-mode)
    (error
     "pdf-tools is not installed; install it to view RFC %d (PDF only)"
     number))
  (let* ((buf-name (rfcview:read-buffer-name number))
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
  "Download RFC NUMBER as FMT format to TO-FILE.
Return TO-FILE on success, nil on 404."
  (message "Downloading RFC%d (%s)..." number fmt)
  (let ((buf (rfcview:retrieve-rfc number fmt)))
    (if (eql 200 (rfcview:http-response-status buf))
        (progn
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (if (eq fmt 'pdf)
                  (progn (forward-line 1)
                         (let ((coding-system-for-write 'binary))
                           (write-region (point) (point-max)
                                         to-file nil 'silent)))
                (delete-region (point-min) (point))
                (write-region (point-min) (point-max)
                              to-file nil 'silent)))
            (kill-buffer buf))
          to-file)
      (kill-buffer buf)
      nil)))

(defun rfcview:read--local-file-path (number fmt)
  "Return the local cache path for RFC NUMBER in FMT, downloading if needed.
Checks the current (unpadded) filename first.  If that's missing but a
file exists under the legacy zero-padded name (e.g. \"rfc0042.txt\" from
before rfcview dropped the padding), that file is used for this open and
copied forward to the current filename — the legacy file itself is left
in place, not moved, so the original download is never at risk.  Falls
through to `rfcview:download-rfc' when neither is present.  Returns nil
on download failure (404)."
  (let* ((f (format "%srfc%d.%s"
                    rfcview:local-directory number (symbol-name fmt)))
         (legacy-f (format "%srfc%04d.%s"
                           rfcview:local-directory number (symbol-name fmt))))
    (cond
     ((file-exists-p f) f)
     ((and (not (string= f legacy-f)) (file-exists-p legacy-f))
      (copy-file legacy-f f)
      f)
     (t (rfcview:download-rfc number fmt f)))))

(defun rfcview:read--format-order (preferred available)
  "Return the order of formats to try when opening an RFC.
PREFERRED is `rfcview:preferred-format'.  AVAILABLE is the entry's
`:format' list from the rfc-index cache (case-insensitive strings
like \"TXT\"); unsupported tokens are dropped.

The result is the supported formats that appear in AVAILABLE, with
PREFERRED first when it is listed.  When PREFERRED is not listed it
is dropped (the index says it is unavailable).  When nothing supported
is listed, returns nil — the caller treats that as \"unavailable\"."
  (seq-intersection (cons preferred
                          (remove preferred rfcview:supported-formats))
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
          (or (get-buffer (rfcview:read-buffer-name number))
              (catch 'found
                (dolist (fmt formats)
                  (let ((fn (cdr (assq fmt rfcview:open-rfc-functions))))
                    (unless fn
                      (rfcview:open-rfc-fallback number fmt)
                      (throw 'found 'browser))
                    (let ((file (rfcview:read--local-file-path number fmt)))
                      (when file
                        (throw 'found (funcall fn number file))))))))))
    (cond ((bufferp buffer) (with-current-buffer buffer
                              (when (eq major-mode 'rfcview:read-mode)
                                (pop-to-buffer buffer)
                                (rfcview:read-jump-to-section section t))))
          ((eq buffer 'browser) nil)
          (t (error "RFC%d is not available" number)))))

(provide 'rfcview-reader)
;;; rfcview-reader.el ends here
