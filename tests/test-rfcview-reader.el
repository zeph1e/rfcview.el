;;; tests/test-rfcview-reader.el --- ERT tests for rfcview-reader.el

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(require 'rfcview-reader)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun rfcview-test:matches-heading (text)
  "Return non-nil if TEXT matches rfcview:section-heading-regexp."
  (string-match rfcview:section-heading-regexp text))

(defun rfcview-test:invisible-overlays-in (buf)
  "Return invisible overlays in BUF."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'invisible))
                    (with-current-buffer buf
                      (overlays-in (point-min) (point-max)))))

(defun rfcview-test:face-at-string (text)
  "Return the face text property at the first occurrence of TEXT in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (get-text-property (match-beginning 0) 'face))))

(defun rfcview-test:make-structured-rfc ()
  "Return RFC-structured text with a header block, centered title, and sections."
  (concat "Network Working Group                                   J. Author\n"
          "Request for Comments: 9999                           Some Corp.\n"
          "Category: Standards Track                           January 2024\n"
          "\n"
          "\n"
          "                       A Sample Protocol\n"
          "\n"
          "Abstract\n"
          "\n"
          "   This is the abstract.\n"
          "\n"
          "1.  Introduction\n"
          "\n"
          "   This is the introduction.\n"
          "\n"
          "8.  References\n"
          "\n"
          "8.1.  Normative References\n"
          "\n"
          "   [RFC793]\n"))

;;; ─── rfcview:section-heading-regexp ─────────────────────────────────────────

(ert-deftest rfcview:test-section-heading-regexp-numeric-trailing-dot ()
  "Matches '1.  Title\\n\\n' numeric heading with trailing dot."
  (should (rfcview-test:matches-heading "\n1.  Introduction\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-nested-numeric ()
  "Matches '1.1.  Title\\n\\n' nested numeric heading."
  (should (rfcview-test:matches-heading "\n1.1.  Background\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-deeply-nested-numeric ()
  "Matches three-level '2.3.10.  Title\\n\\n' heading."
  (should (rfcview-test:matches-heading "\n2.3.10.  The Details\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-numeric-no-trailing-dot ()
  "Matches '3.7 Media Types\\n\\n' (no trailing dot after sub-number)."
  (should (rfcview-test:matches-heading "\n3.7 Media Types\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-roman-numeral ()
  "Matches 'IV.  Section\\n\\n' Roman numeral heading."
  (should (rfcview-test:matches-heading "\nIV.  Overview\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-modern ()
  "Matches modern 'Appendix A.  Title\\n\\n' heading."
  (should (rfcview-test:matches-heading "\nAppendix A.  Sample Appendix\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-all-caps ()
  "Matches RFC-791-era 'APPENDIX A:  Title\\n\\n' heading."
  (should (rfcview-test:matches-heading "\nAPPENDIX A:  Protocol Specification\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-roman-numeral ()
  "Matches RFC-172-era 'APPENDIX IV - Title\\n\\n' Roman-numeral appendix heading."
  (should (rfcview-test:matches-heading "\nAPPENDIX IV - THE SPECIFICATION\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-subsection ()
  "Matches 'A.1.  Title\\n\\n' appendix subsection heading."
  (should (rfcview-test:matches-heading "\nA.1.  First Appendix Section\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-subsection-multi-level ()
  "Matches multi-level appendix subsection heading 'A.1.3  Title\\n\\n'."
  (should (rfcview-test:matches-heading "\nA.1.3  Detailed Subsection\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-subsection-multi-level-trailing-dot ()
  "Matches multi-level appendix subsection heading with trailing dot 'A.1.3.  Title\\n\\n'."
  (should (rfcview-test:matches-heading "\nA.1.3.  Detailed Subsection\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-wrapped-subsection ()
  "Matches a wrapped subsection heading like RFC 8968 §2.6 / §2.7."
  (should (rfcview-test:matches-heading
           "\n2.6.  Simultaneous Operation of Babel over DTLS and Unprotected Babel on\n      a Node\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-wrapped-subsection-deeper ()
  "Matches a wrapped deeper subsection heading (3-segment number)."
  (should (rfcview-test:matches-heading
           "\n4.3.1.  Long Title That Wraps Onto A Second\n        Continuation Line\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-wrap-not-allowed-for-top-level ()
  "Does not match a wrapped top-level numbered heading (would false-match list items)."
  (should-not (rfcview-test:matches-heading
               "\n3.  A HOST has to be prepared for various\n    failure modes here\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-subsection-with-commas ()
  "Matches a subsection title that contains commas (RFC 8698 §6.2 style)."
  (should (rfcview-test:matches-heading
           "\n6.2.  Method for Delay, Loss, and Marking Ratio Estimation\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-subsection-comma-rejects-sentence ()
  "Does not match a single-line list item that ends with a period (sentence-shape)."
  (should-not (rfcview-test:matches-heading
               "\n6.2.  Foo, bar, and baz.\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-top-level-with-comma-still-rejected ()
  "Top-level (X.) numbered title with a comma is still rejected to block list items."
  (should-not (rfcview-test:matches-heading
               "\n3.  Foo, bar.\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-all-caps-bare-word ()
  "Matches all-caps bare-word headings like 'INTRODUCTION\\n\\n'."
  (should (rfcview-test:matches-heading "\nINTRODUCTION\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-all-caps-multi-word ()
  "Matches multi-word all-caps heading 'GENERAL CONSIDERATIONS\\n\\n'."
  (should (rfcview-test:matches-heading "\nGENERAL CONSIDERATIONS\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-all-caps-hyphenated ()
  "Matches all-caps heading containing a hyphen like 'NON-PRINTING CHARACTERS\\n\\n'."
  (should (rfcview-test:matches-heading "\nNON-PRINTING CHARACTERS\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-acknowledgements ()
  "Matches 'Acknowledgements\\n\\n' heading."
  (should (rfcview-test:matches-heading "\nAcknowledgements\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-acknowledgment-variant ()
  "Matches 'Acknowledgment\\n\\n' (US spelling without 'e')."
  (should (rfcview-test:matches-heading "\nAcknowledgment\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-authors-addresses ()
  "Matches 'Authors\\' Addresses\\n\\n' heading."
  (should (rfcview-test:matches-heading "\nAuthors' Addresses\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-author-singular-address ()
  "Matches 'Author\\'s Address\\n\\n' (singular possessive) heading."
  (should (rfcview-test:matches-heading "\nAuthor's Address\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-abstract ()
  "Matches 'Abstract\\n\\n' heading."
  (should (rfcview-test:matches-heading "\nAbstract\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-dash-underline ()
  "Matches dash-underline style heading."
  (should (rfcview-test:matches-heading "\nIntroduction\n------------\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-rejects-list-item ()
  "Does NOT match a multi-line list item beginning with a number (no trailing \\n\\n)."
  (should-not (rfcview-test:matches-heading
               "\n3.  A HOST has to be prepared to accept incoming")))

(ert-deftest rfcview:test-section-heading-regexp-case-fold-search-behavior ()
  "With case-fold-search nil, [A-Z] enforces an uppercase title start.
In normal use case-fold-search is t (Emacs default), so lowercase titles
also match — this test documents the case-sensitive-only behavior."
  (let ((case-fold-search nil))
    (should-not (rfcview-test:matches-heading "\n1.  lowercase title\n\n"))
    (should (rfcview-test:matches-heading "\n1.  Uppercase Title\n\n"))))


;;; ─── rfcview:read-fontify ────────────────────────────────────────────────────

(ert-deftest rfcview:test-read-fontify-header-gets-traits-face ()
  "The header block (before first blank line) gets rfcview:read-rfc-header-face."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-header-face
                (rfcview-test:face-at-string "Network Working Group")))))

(ert-deftest rfcview:test-read-fontify-traits-face-ends-at-first-blank ()
  "Header face does not extend past the first blank line into the title."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should-not (eq 'rfcview:read-rfc-header-face
                    (rfcview-test:face-at-string "A Sample Protocol")))))

(ert-deftest rfcview:test-read-fontify-title-gets-title-face ()
  "Space-indented lines after the header gap get rfcview:read-rfc-title-face."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-title-face
                (rfcview-test:face-at-string "A Sample Protocol")))))

(ert-deftest rfcview:test-read-fontify-title-modern-single-line ()
  "Left-aligned (non-indented) single-line title gets rfcview:read-rfc-title-face (RFC 9934 style)."
  (with-temp-buffer
    (insert (concat "ISSN: 2070-1721                                        J. Doe\n"
                    "\n"
                    "\n"
                    "Privacy-Enhanced Mail File Format for Encrypted ClientHello\n"
                    "\n"
                    "Abstract\n"
                    "\n"
                    "Content.\n"))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-title-face
                (rfcview-test:face-at-string "Privacy-Enhanced Mail File Format")))))

(ert-deftest rfcview:test-read-fontify-title-modern-wrapped ()
  "Wrapped title whose first line is not indented gets rfcview:read-rfc-title-face on all lines (RFC 9935 style)."
  (with-temp-buffer
    (insert (concat "ISSN: 2070-1721                                        J. Doe\n"
                    "\n"
                    "\n"
                    "Internet X.509 Public Key Infrastructure - Algorithm Identifiers\n"
                    "       for the Module-Lattice-Based Key-Encapsulation Mechanism\n"
                    "\n"
                    "Abstract\n"
                    "\n"
                    "Content.\n"))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-title-face
                (rfcview-test:face-at-string "Internet X.509")))
    (should (eq 'rfcview:read-rfc-title-face
                (rfcview-test:face-at-string "for the Module-Lattice")))))

(ert-deftest rfcview:test-read-fontify-title-when-non-indented ()
  "Non-indented text after the header gap gets rfcview:read-rfc-title-face (modern RFC style)."
  (with-temp-buffer
    (insert (concat "Author: J. Doe\n"
                    "\n"
                    "Non-Indented Title\n"
                    "\n"
                    "Abstract\n"
                    "\n"
                    "Content.\n"))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-title-face
                (rfcview-test:face-at-string "Non-Indented Title")))))

(ert-deftest rfcview:test-read-fontify-numeric-section-gets-section-face ()
  "Numeric section headings get rfcview:read-rfc-section-face."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-section-face
                (rfcview-test:face-at-string "1.  Introduction")))))

(ert-deftest rfcview:test-read-fontify-keyword-heading-gets-section-face ()
  "Keyword headings like Abstract get rfcview:read-rfc-section-face."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-section-face
                (rfcview-test:face-at-string "Abstract")))))

(ert-deftest rfcview:test-read-fontify-all-caps-heading-gets-section-face ()
  "ALL-CAPS bare-word headings get rfcview:read-rfc-section-face."
  (with-temp-buffer
    (insert (concat "Network Working Group\n"
                    "\n"
                    "\n"
                    "                       Old-Style RFC\n"
                    "\n"
                    "\nINTRODUCTION\n\nSome prose.\n"))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-section-face
                (rfcview-test:face-at-string "INTRODUCTION")))))

(ert-deftest rfcview:test-read-fontify-adjacent-sections-both-get-face ()
  "Adjacent headings 8. and 8.1. both get rfcview:read-rfc-section-face."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-section-face
                (rfcview-test:face-at-string "8.  References")))
    (should (eq 'rfcview:read-rfc-section-face
                (rfcview-test:face-at-string "8.1.  Normative References")))))

(ert-deftest rfcview:test-read-fontify-header-with-bom ()
  "Header face is applied when the document begins with a BOM-only line before the header.
Recent RFC documents sometimes begin with a bare U+FEFF on the first line."
  (with-temp-buffer
    (insert (string ?﻿))
    (insert "\n")
    (insert "Network Working Group                                   J. Author\n"
            "Request for Comments: 9999                           Some Corp.\n"
            "\n"
            "\n"
            "                       A Sample Protocol\n"
            "\n"
            "Abstract\n\n   Body.\n")
    (rfcview:read-fontify)
    (should (eq 'rfcview:read-rfc-header-face
                (rfcview-test:face-at-string "Network Working Group")))))

(ert-deftest rfcview:test-read-fontify-body-text-has-no-fontify-face ()
  "Body paragraph text does not get any of the rfcview fontify faces."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (rfcview:read-fontify)
    (let ((face (rfcview-test:face-at-string "This is the introduction")))
      (should-not (memq face '(rfcview:read-rfc-header-face
                               rfcview:read-rfc-title-face
                               rfcview:read-rfc-section-face))))))

(ert-deftest rfcview:test-read-fontify-works-in-read-only-buffer ()
  "rfcview:read-fontify applies text properties even in a read-only buffer."
  (with-temp-buffer
    (insert (rfcview-test:make-structured-rfc))
    (setq buffer-read-only t)
    (unwind-protect
        (should-not (condition-case _
                        (progn (rfcview:read-fontify) nil)
                      (buffer-read-only t)))
      (setq buffer-read-only nil))))

;;; ─── rfcview:read-trim-leading-blanks ────────────────────────────────────────

(ert-deftest rfcview:test-read-trim-leading-blanks-hides-blank-lines ()
  "Creates an invisible overlay over leading blank lines."
  (with-temp-buffer
    (insert "\n\n\nFirst real content.\n")
    (rfcview:read-trim-leading-blanks)
    (should (rfcview-test:invisible-overlays-in (current-buffer)))))

(ert-deftest rfcview:test-read-trim-leading-blanks-no-overlay-when-no-blanks ()
  "Does not create any overlay when there are no leading blank lines."
  (with-temp-buffer
    (insert "Content starts immediately.\n")
    (rfcview:read-trim-leading-blanks)
    (should (null (rfcview-test:invisible-overlays-in (current-buffer))))))

(ert-deftest rfcview:test-read-trim-leading-blanks-overlay-covers-blanks ()
  "The invisible overlay starts at point-min and ends at or before the first non-blank."
  (with-temp-buffer
    (insert "\n\nFirst real line.\n")
    (rfcview:read-trim-leading-blanks)
    (let ((ov (car (rfcview-test:invisible-overlays-in (current-buffer)))))
      (should (= (point-min) (overlay-start ov)))
      ;; overlay end is exclusive: it must not extend PAST the first non-blank
      (should (<= (overlay-end ov)
                  (save-excursion
                    (goto-char (point-min))
                    (skip-chars-forward " \t\n")
                    (point)))))))

(ert-deftest rfcview:test-read-trim-leading-blanks-hides-bom-with-blank-lines ()
  "Creates an invisible overlay when a BOM precedes leading blank lines."
  (with-temp-buffer
    (insert (string ?﻿))
    (insert "\n\nFirst real content.\n")
    (rfcview:read-trim-leading-blanks)
    (should (rfcview-test:invisible-overlays-in (current-buffer)))))

(ert-deftest rfcview:test-read-trim-leading-blanks-bom-overlay-ends-before-content ()
  "The overlay over a BOM+blank-line prefix ends at or before the first real char."
  (with-temp-buffer
    (insert (string ?﻿))
    (insert "\n\nContent.\n")
    (rfcview:read-trim-leading-blanks)
    (let ((ov (car (rfcview-test:invisible-overlays-in (current-buffer)))))
      (should (= (point-min) (overlay-start ov)))
      (should (<= (overlay-end ov)
                  (save-excursion
                    (goto-char (point-min))
                    (skip-chars-forward " \t\n﻿")
                    (point)))))))

;;; ─── rfcview:read-hide-page-breaks ──────────────────────────────────────────

(defun rfcview-test:make-page-break-text ()
  "Sample RFC text containing one page break."
  (concat "Content on page 1.\n"
          "\n"
          "                                                       [Page 1]\n"
          "\f\n"
          "RFC 793              Transmission Control Protocol   September 1981\n"
          "\n"
          "\n"
          "Content on page 2.\n"))

(ert-deftest rfcview:test-read-hide-page-breaks-creates-invisible-overlay ()
  "Creates at least one invisible overlay when form-feed page breaks exist."
  (with-temp-buffer
    (insert (rfcview-test:make-page-break-text))
    (rfcview:read-hide-page-breaks)
    (should (rfcview-test:invisible-overlays-in (current-buffer)))))

(ert-deftest rfcview:test-read-hide-page-breaks-no-overlay-without-formfeed ()
  "Creates no overlay when the buffer has no form-feed characters."
  (with-temp-buffer
    (insert "No page breaks in this text.\nJust two lines.\n")
    (rfcview:read-hide-page-breaks)
    (should (null (rfcview-test:invisible-overlays-in (current-buffer))))))

(ert-deftest rfcview:test-read-hide-page-breaks-no-overlay-without-page-footer ()
  "Does not hide a form-feed that is not preceded by a [Page N] footer line."
  (with-temp-buffer
    (insert "Content.\n\f\nNext page.\n")
    (rfcview:read-hide-page-breaks)
    (should (null (rfcview-test:invisible-overlays-in (current-buffer))))))

(ert-deftest rfcview:test-read-hide-page-breaks-bare-formfeed-no-footer ()
  "Hides a bare form-feed preceded only by a blank line (RFC 729 style).
Content after the break must remain visible."
  (with-temp-buffer
    (setq rfcview:read-rfc-number 729)
    (insert (concat "Content on page 1.\n"
                    "\n"
                    "\f\n"
                    "RFC #729 Telnet Byte Macro Option                    Page 2\n"
                    "\n"
                    "\n"
                    "Content on page 2.\n"))
    (rfcview:read-hide-page-breaks)
    (should (rfcview-test:invisible-overlays-in (current-buffer)))
    (let* ((content-pos (save-excursion
                          (goto-char (point-min))
                          (search-forward "Content on page 2.")
                          (match-beginning 0)))
           (covered-p (cl-some (lambda (ov)
                                 (and (overlay-get ov 'invisible)
                                      (<= (overlay-start ov) content-pos)
                                      (> (overlay-end ov) content-pos)))
                               (overlays-in (point-min) (point-max)))))
      (should-not covered-p))))

(ert-deftest rfcview:test-read-hide-page-breaks-recognizes-dash-number-footer ()
  "Hides page breaks preceded by a dash-number footer (e.g. '  - 3 -')."
  (with-temp-buffer
    (insert (concat "Content on page 3.\n"
                    "\n"
                    "                                  - 3 -\n"
                    "\f\n"
                    "Old RFC Header Line\n"
                    "\n"
                    "Content on page 4.\n"))
    (rfcview:read-hide-page-breaks)
    (should (rfcview-test:invisible-overlays-in (current-buffer)))))

(ert-deftest rfcview:test-read-hide-page-breaks-content-visible-after-break ()
  "Content after the page break is not covered by the overlay.
Overlay end is exclusive (covers [start, end)), so content at overlay-end is visible."
  (with-temp-buffer
    (insert (rfcview-test:make-page-break-text))
    (rfcview:read-hide-page-breaks)
    (let* ((content-pos (save-excursion
                          (goto-char (point-min))
                          (search-forward "Content on page 2.")
                          (match-beginning 0)))
           (covered-p (cl-some (lambda (ov)
                                 (and (overlay-get ov 'invisible)
                                      (<= (overlay-start ov) content-pos)
                                      (> (overlay-end ov) content-pos)))
                               (overlays-in (point-min) (point-max)))))
      (should-not covered-p))))

(ert-deftest rfcview:test-read-hide-page-breaks-leaves-blank-before-section ()
  "When the first visible line after a break is a section heading,
one blank line is left visible (overlay ends before it)."
  (with-temp-buffer
    (insert (concat "Content.\n"
                    "\n"
                    "                            [Page 5]\n"
                    "\f\n"
                    "RFC Title                                         Some Date\n"
                    "\n"
                    "\n"
                    "1.  The First Section\n"
                    "\n"
                    "Body text.\n"))
    (rfcview:read-hide-page-breaks)
    (let* ((section-pos (save-excursion
                          (goto-char (point-min))
                          (search-forward "1.  The First Section")
                          (line-beginning-position)))
           ;; The blank line just before the section heading
           (blank-before (1- section-pos))
           (ovs (overlays-in (point-min) (point-max)))
           (covering-blank-p
            (cl-some (lambda (ov)
                       (and (overlay-get ov 'invisible)
                            (<= (overlay-start ov) blank-before)
                            (>= (overlay-end ov) section-pos)))
                     ovs)))
      ;; The blank line before the section heading should NOT be fully covered
      (should-not covering-blank-p))))

;;; ─── rfcview:read-buttonize-refs ────────────────────────────────────────────

(ert-deftest rfcview:test-read-buttonize-refs-bracketed-notation ()
  "Creates a button for [RFC793] bracket notation."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See [RFC793] for TCP details.\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC793")
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (should (= 793 (button-get btn 'number)))))))

(ert-deftest rfcview:test-read-buttonize-refs-inline-notation ()
  "Creates a button for 'RFC 793' inline notation."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "As defined in RFC 793, TCP provides...\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC 793")
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (should (= 793 (button-get btn 'number)))))))

(ert-deftest rfcview:test-read-buttonize-refs-multiple-references ()
  "Creates buttons for all RFC references in the buffer."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See RFC 791 and RFC 793 and [RFC768].\n")
      (rfcview:read-buttonize-refs)
      (let (nums)
        (goto-char (point-min))
        (while (forward-button 1 nil nil t)
          (push (button-get (button-at (point)) 'number) nums))
        (should (member 791 nums))
        (should (member 793 nums))
        (should (member 768 nums))))))

(ert-deftest rfcview:test-read-buttonize-refs-no-buttons-when-no-refs ()
  "Does not create buttons when there are no RFC references."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "No RFC references in this text at all.\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (should-not (forward-button 1 nil t t)))))

(ert-deftest rfcview:test-read-buttonize-refs-button-number-correct ()
  "The button's 'number property matches the RFC number in the text."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "RFC 2616 is HTTP/1.1.\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC 2616")
      (let ((btn (button-at (match-beginning 0))))
        (should (= 2616 (button-get btn 'number)))))))

(ert-deftest rfcview:test-read-buttonize-refs-uses-cache-for-help-echo ()
  "Button help-echo is set to the RFC title from rfcview:rfc-cache."
  (let* ((tbl (make-hash-table :test 'equal))
         (_ (puthash 793 '(:title "Transmission Control Protocol") tbl))
         (rfcview:rfc-cache (list :last-modified nil :table tbl)))
    (with-temp-buffer
      (insert "See [RFC793].\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC793")
      (let ((btn (button-at (match-beginning 0))))
        (should (string-match-p "Transmission Control Protocol"
                                 (button-get btn 'help-echo)))))))

;;; ─── rfcview:read-buttonize-toc ─────────────────────────────────────────────

(defun rfcview-test:sample-rfc-with-toc ()
  "Return sample RFC text containing a Table of Contents and matching sections."
  (concat "Network Working Group                                   J. Author\n"
          "Request for Comments: 9999                           Some Corp.\n"
          "Category: Standards Track                           January 2024\n"
          "\n"
          "\n"
          "                       A Sample Protocol\n"
          "\n"
          "Abstract\n"
          "\n"
          "   This is the abstract.\n"
          "\n"
          "Table of Contents\n"
          "\n"
          "   1.  Introduction ........................... 2\n"
          "   1.1  Goals .................................. 2\n"
          "   2.  Protocol Details ....................... 3\n"
          "   Acknowledgements ........................... 5\n"
          "\n"
          "1.  Introduction\n"
          "\n"
          "   Intro paragraph.\n"
          "\n"
          "1.1  Goals\n"
          "\n"
          "   Some goals.\n"
          "\n"
          "2.  Protocol Details\n"
          "\n"
          "   Details here.\n"
          "\n"
          "Acknowledgements\n"
          "\n"
          "   Thanks!\n"))

(defun rfcview-test:section-buttons ()
  "Return an alist of (LABEL . TARGET-POS) for all section-link buttons in buffer."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (forward-button 1 nil nil t)
        (let ((b (button-at (point))))
          (when (eq (button-type b) 'rfcview:section-link-button)
            (push (cons (button-label b)
                        (marker-position (button-get b 'target)))
                  result)))))
    (nreverse result)))

(ert-deftest rfcview:test-read-buttonize-toc-numeric-entries ()
  "Numbered TOC entries get section-link buttons pointing at their heading."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((btns (rfcview-test:section-buttons)))
      (should (assoc "Introduction" btns))
      (should (assoc "Protocol Details" btns)))))

(ert-deftest rfcview:test-read-buttonize-toc-multi-level-numeric ()
  "Multi-level TOC entry (1.1) is buttonized and resolves via the by-number map."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (assoc "Goals" (rfcview-test:section-buttons)))))

(ert-deftest rfcview:test-read-buttonize-toc-unnumbered-entry ()
  "Unnumbered TOC entry falls back to title lookup and gets a button."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (assoc "Acknowledgements" (rfcview-test:section-buttons)))))

(ert-deftest rfcview:test-read-buttonize-toc-button-target-is-heading-position ()
  "Button's target marker points at the heading line, not at the TOC entry."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let* ((target (cdr (assoc "Introduction"
                                (rfcview-test:section-buttons))))
           (heading-pos (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^1\\.  Introduction$")
                          (line-beginning-position))))
      (should (= target heading-pos)))))

(ert-deftest rfcview:test-read-buttonize-toc-pushing-button-jumps-to-section ()
  "Activating a TOC button moves point to the section heading."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (goto-char (point-min))
    (re-search-forward "Introduction")
    (let ((btn (button-at (1- (point)))))
      (should btn)
      (push-button (button-start btn))
      (should (looking-at "1\\.  Introduction")))))

(ert-deftest rfcview:test-read-buttonize-toc-no-toc-no-buttons ()
  "No section-link buttons created when buffer lacks a TOC heading."
  (with-temp-buffer
    (insert "Abstract\n\nSome text.\n\n1.  Introduction\n\nIntro.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (null (rfcview-test:section-buttons)))))

(ert-deftest rfcview:test-read-buttonize-toc-missing-anchor-skipped ()
  "TOC entry whose section does not exist in body produces no button."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction ........................... 2\n"
            "   2.  Nonexistent Section .................... 3\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Intro.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "Introduction" labels))
      (should-not (member "Nonexistent Section" labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-absorbs-wrapped-continuation ()
  "A TOC entry that wraps to a continuation line produces one button covering both lines.
The button's label includes the continuation text, and a `match-string'-style
lookup on the wrapped title finds the section."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   4.3.1.  MGM Nonce Format for Transforms Based on the\n"
            "           \"Kuznyechik\" Cipher\n"
            "\n"
            "4.3.1.  MGM Nonce Format for Transforms Based on the \"Kuznyechik\" Cipher\n"
            "\n"
            "   Body text.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar (lambda (e)
                            (replace-regexp-in-string "[ \t\n]+" " " (car e)))
                          (rfcview-test:section-buttons))))
      (should (= 1 (length labels)))
      (should (string-match-p "Kuznyechik" (car labels))))))

(ert-deftest rfcview:test-read-buttonize-toc-wrapped-body-heading-resolves ()
  "A wrapped body subsection heading is detected and its TOC entry links to it."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   2.6.  Simultaneous Operation of Babel over DTLS and Unprotected\n"
            "         Babel on a Node\n"
            "\n"
            "2.6.  Simultaneous Operation of Babel over DTLS and Unprotected Babel on\n"
            "      a Node\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let* ((btns (rfcview-test:section-buttons))
           (heading-pos (save-excursion
                          (goto-char (point-min))
                          (re-search-forward "^2\\.6\\.  Simultaneous")
                          (line-beginning-position))))
      (should (= 1 (length btns)))
      (should (= heading-pos (cdr (car btns)))))))

(ert-deftest rfcview:test-read-buttonize-toc-spaced-dot-leader ()
  "TOC entries with spaced-dot leaders (`. . . . .`) are buttonized cleanly."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction  . . . . . . . . . . . . . . . . . . . . .   3\n"
            "   2.  Definitions . . . . . . . . . . . . . . . . . . . . . .   4\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.  Definitions\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "Introduction" labels))
      (should (member "Definitions" labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-dims-leader-and-page ()
  "Dot-leader and page number on a TOC line carry rfcview:read-toc-leader-face."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction  . . . . . . . . . . . . . . . . . . . . .   3\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (goto-char (point-min))
    (re-search-forward "Introduction")
    (let* ((title-end (point))
           (eol (line-end-position)))
      ;; Title text itself has no leader face.
      (should-not (eq 'rfcview:read-toc-leader-face
                      (get-text-property (1- title-end) 'face)))
      ;; The dot leader and trailing page do.
      (should (eq 'rfcview:read-toc-leader-face
                  (get-text-property (1+ title-end) 'face)))
      (should (eq 'rfcview:read-toc-leader-face
                  (get-text-property (1- eol) 'face))))))

(ert-deftest rfcview:test-read-buttonize-toc-no-leader-no-dimming ()
  "TOC entry without a leader/page (RFC 9227 style) gets no leader face."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (goto-char (point-min))
    (re-search-forward "Introduction")
    ;; Past the title there is nothing on the line; no face to inspect,
    ;; and crucially the title char itself has no leader face.
    (should-not (eq 'rfcview:read-toc-leader-face
                    (get-text-property (1- (point)) 'face)))))

;; --- Tests derived from validation against ~/.emacs.d/.RFC cache ---

(ert-deftest rfcview:test-read-buttonize-toc-title-with-rfc-reference ()
  "TOC entry whose title legitimately contains \"RFC NNNN\" still produces a
section-link button covering the full title (validator false-positive case;
RFC 9720 style \"Changes to RFC 7990\")."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   2.  Changes to RFC 7990\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.  Changes to RFC 7990\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((btns (rfcview-test:section-buttons)))
      (should (assoc "Changes to RFC 7990" btns)))))

(ert-deftest rfcview:test-read-toc-button-wins-over-rfc-ref-in-title ()
  "When a TOC entry's title contains \"RFC NNNN\", the section-link covering
the full title wins; `rfcview:read-buttonize-refs' skips the already-buttoned
range so clicking on the digits jumps to the section, not the cross-RFC."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   2.  Changes to RFC 7990\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.  Changes to RFC 7990\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (rfcview:read-buttonize-refs)
    (goto-char (point-min))
    (re-search-forward "Table of Contents")
    (re-search-forward "RFC 7990")
    (backward-char 2)
    (let ((b (button-at (point))))
      (should b)
      (should (eq (button-type b) 'rfcview:section-link-button)))))

(ert-deftest rfcview:test-read-buttonize-refs-still-runs-outside-toc ()
  "Complement to the TOC-precedence fix: `RFC NNNN' references in body text
(not inside a TOC entry's section-link) still get an `rfc-link-button'."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   See RFC 7990 for details.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (rfcview:read-buttonize-refs)
    (goto-char (point-min))
    (re-search-forward "See RFC 7990")
    (backward-char 4)
    (let ((b (button-at (point))))
      (should b)
      (should (eq (button-type b) 'rfcview:rfc-link-button)))))

(ert-deftest rfcview:test-read-buttonize-toc-short-single-word-continuation ()
  "A wrapped TOC entry whose continuation line is a single trailing word
(RFC 9350 §18.3.1 \"Registry\" style) is absorbed into one button."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "       18.3.1.  IS-IS Sub-TLVs for IS-IS Router CAPABILITY TLV\n"
            "                Registry\n"
            "\n"
            "18.3.1.  IS-IS Sub-TLVs for IS-IS Router CAPABILITY TLV Registry\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((btns (rfcview-test:section-buttons)))
      (should (= 1 (length btns)))
      (should (string-match-p "Registry" (car (car btns)))))))

(ert-deftest rfcview:test-read-buttonize-toc-deeply-nested-numeric ()
  "TOC entry with a three-segment section number (18.4.5) resolves correctly."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "       18.4.5.  Opaque LSA Option Types Registry\n"
            "\n"
            "18.4.5.  Opaque LSA Option Types Registry\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (assoc "Opaque LSA Option Types Registry"
                   (rfcview-test:section-buttons)))))

(ert-deftest rfcview:test-read-buttonize-toc-pre-toc-era-rfc-no-buttons ()
  "Negative: a pre-1980-style RFC without any \"Table of Contents\" heading
creates zero section-link buttons (validator confirmed for RFCs 15–729)."
  (with-temp-buffer
    (insert "Network Working Group                              S. Crocker\n"
            "Request for Comments: 42                            April 1970\n"
            "\n"
            "                       Some Old Memo\n"
            "\n"
            "INTRODUCTION\n"
            "\n"
            "   Some text here.\n"
            "\n"
            "DISCUSSION\n"
            "\n"
            "   More text.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (null (rfcview-test:section-buttons)))))

(ert-deftest rfcview:test-read-buttonize-toc-page-break-text-not-buttonized ()
  "Negative: page-break artifacts (footer, page header) that fall inside the
TOC region are not turned into section-link buttons.  Mirrors what the
validator's \"non-blank lines in TOC region\" count includes but the walker
correctly skips."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction\n"
            "\n\n\n\n\n\n\n\n\n\n\n\n\n"
            "Author, et al.            Informational              [Page 1]\n"
            "\f\n"
            "RFC 9999                  Foo Protocol            January 2024\n"
            "\n\n\n"
            "   2.  Discussion\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.  Discussion\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-hide-page-breaks)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "Introduction" labels))
      (should (member "Discussion" labels))
      (should-not (cl-some (lambda (l)
                             (or (string-match-p "Page" l)
                                 (string-match-p "Foo Protocol" l)
                                 (string-match-p "Author" l)))
                           labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-running-header-not-buttonized ()
  "Negative: a TOC region containing a running RFC header line like
\"RFC 9999  Protocol  January 2024\" does not produce a button for it."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   1.  Introduction\n"
            "RFC 9999                  Foo Protocol            January 2024\n"
            "   2.  Conclusion\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.  Conclusion\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should-not (cl-some (lambda (l) (string-match-p "Foo Protocol" l))
                           labels)))))

(ert-deftest rfcview:test-section-heading-regexp-rejects-prose-paragraph ()
  "Negative: a prose paragraph that happens to be one line and starts with a
capital letter does not match the section heading regexp."
  (should-not (rfcview-test:matches-heading
               "\nThe protocol is designed to be extensible.\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-matches-quoted-title ()
  "Section title that starts with a double-quote — '4.3.1.  \"Kuznyechik\" Foo' —
matches via the `[A-Z(\"]' title-start class."
  (should (rfcview-test:matches-heading
           "\n4.3.1.  \"Kuznyechik\" Cipher Format\n\n")))

(ert-deftest rfcview:test-read-buttonize-toc-handles-all-caps-toc-heading ()
  "TOC line in ALL CAPS (RFC 791 era) is recognized as the TOC heading."
  (with-temp-buffer
    (insert "                            TABLE OF CONTENTS\n"
            "\n"
            "  1.  INTRODUCTION ..........................................    1\n"
            "\n"
            "1.  INTRODUCTION\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (should (assoc "INTRODUCTION" (rfcview-test:section-buttons)))))

;;; ─── rfcview:read-next-section / rfcview:read-prev-section ──────────────────

(defun rfcview-test:sample-multi-section-rfc ()
  "Return sample RFC text with multiple section headings."
  (concat "\nAbstract\n"
          "\nThis is the abstract.\n"
          "\n1.  Introduction\n\n"
          "This is the introduction.\n"
          "\n2.  Protocol Overview\n\n"
          "Overview text.\n"
          "\n3.  Conclusion\n\n"
          "End of document.\n"))

(ert-deftest rfcview:test-read-next-section-moves-forward ()
  "rfcview:read-next-section advances point to the next section heading."
  (with-temp-buffer
    (insert (rfcview-test:sample-multi-section-rfc))
    (goto-char (point-min))
    (let ((start (point)))
      (rfcview:read-next-section)
      (should (> (point) start)))))

(ert-deftest rfcview:test-read-next-section-lands-on-heading-line ()
  "Point lands on the heading text (not the blank line before it)."
  (with-temp-buffer
    (insert (rfcview-test:sample-multi-section-rfc))
    (goto-char (point-min))
    (rfcview:read-next-section)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (should (or (string-match-p "Abstract\\|Introduction\\|Overview\\|Conclusion" line)
                  (string-match-p "^[0-9]" line))))))

(ert-deftest rfcview:test-read-next-section-no-op-when-no-headings ()
  "Stays at current position and messages when no headings exist."
  (with-temp-buffer
    (insert "Just prose.  No sections here.\n")
    (goto-char (point-min))
    (let ((pos (point)))
      (rfcview:read-next-section)
      (should (= pos (point))))))

(ert-deftest rfcview:test-read-prev-section-moves-backward ()
  "rfcview:read-prev-section moves point to a previous section heading."
  (with-temp-buffer
    (insert (rfcview-test:sample-multi-section-rfc))
    (goto-char (point-max))
    (let ((end (point)))
      (rfcview:read-prev-section)
      (should (< (point) end)))))

(ert-deftest rfcview:test-read-prev-section-no-op-when-no-headings ()
  "Stays at current position and messages when no headings exist."
  (with-temp-buffer
    (insert "Just prose without headings.\n")
    (goto-char (point-max))
    (let ((pos (point)))
      (rfcview:read-prev-section)
      (should (= pos (point))))))

(ert-deftest rfcview:test-read-next-then-prev-section-navigates-headings ()
  "Going forward to section 2 then backward lands on section 1's heading."
  (with-temp-buffer
    (insert (rfcview-test:sample-multi-section-rfc))
    (goto-char (point-min))
    ;; Jump to first heading
    (rfcview:read-next-section)
    ;; Jump to second heading
    (rfcview:read-next-section)
    (let ((at-section-2 (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Jump backward — should land on section 1
      (rfcview:read-prev-section)
      (let ((at-section-1 (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
        ;; The two positions should be different headings
        (should (not (string= at-section-1 at-section-2)))))))

;;; ─── rfcview:open-rfc-txt ────────────────────────────────────────────────────

(ert-deftest rfcview:test-open-rfc-txt-creates-named-buffer ()
  "open-rfc-txt creates a buffer named '*RFC XXXX*'."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "RFC content here.\n"))
          (let ((buf (rfcview:open-rfc-txt 793 tmp)))
            (unwind-protect
                (should (string= "*RFC 0793*" (buffer-name buf)))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest rfcview:test-open-rfc-txt-buffer-contains-file-content ()
  "The buffer returned by open-rfc-txt contains the file's text."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Unique marker content XYZ.\n"))
          (let ((buf (rfcview:open-rfc-txt 1 tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should (string-match-p "Unique marker content XYZ"
                                          (buffer-substring-no-properties
                                           (point-min) (point-max)))))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest rfcview:test-open-rfc-txt-sets-read-source-file ()
  "open-rfc-txt stores the file path in rfcview:read-source-file."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Content.\n"))
          (let ((buf (rfcview:open-rfc-txt 2 tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should (string= tmp rfcview:read-source-file)))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest rfcview:test-open-rfc-txt-sets-read-rfc-number ()
  "open-rfc-txt stores the RFC number in rfcview:read-rfc-number."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Content.\n"))
          (let ((buf (rfcview:open-rfc-txt 4 tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should (= 4 rfcview:read-rfc-number)))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest rfcview:test-open-rfc-txt-buffer-not-modified ()
  "The buffer is not marked modified after loading."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Content.\n"))
          (let ((buf (rfcview:open-rfc-txt 3 tmp)))
            (unwind-protect
                (should-not (buffer-modified-p buf))
              (kill-buffer buf))))
      (delete-file tmp))))

(ert-deftest rfcview:test-open-rfc-txt-enables-goto-address-mode ()
  "open-rfc-txt enables goto-address-mode in the RFC buffer."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Content.\n"))
          (let ((buf (rfcview:open-rfc-txt 5 tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should goto-address-mode))
              (kill-buffer buf))))
      (delete-file tmp))))

;;; ─── rfcview:open-rfc-pdf ────────────────────────────────────────────────────

(ert-deftest rfcview:test-open-rfc-pdf-errors-without-pdf-tools ()
  "open-rfc-pdf signals an error when pdf-view-mode is not available."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym)
               (if (eq sym 'pdf-view-mode) nil (fboundp sym)))))
    (should-error (rfcview:open-rfc-pdf 793 "/some/file.pdf"))))

;;; ─── rfcview:read-view-original ─────────────────────────────────────────────

(ert-deftest rfcview:test-read-view-original-errors-without-source-file ()
  "Signals an error when rfcview:read-source-file is nil."
  (with-temp-buffer
    (setq-local rfcview:read-source-file nil)
    (should-error (rfcview:read-view-original))))

(ert-deftest rfcview:test-read-view-original-opens-source-file ()
  "Opens the source file in text-mode in a read-only buffer."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "Raw RFC content.\n"))
          (let ((read-buf (rfcview:open-rfc-txt 999 tmp)))
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
                    (with-current-buffer read-buf
                      (rfcview:read-view-original)))
                  (let ((src-buf (find-buffer-visiting tmp)))
                    (should src-buf)
                    (with-current-buffer src-buf
                      (should (eq major-mode 'text-mode))
                      (should buffer-read-only))
                    (kill-buffer src-buf)))
              (kill-buffer read-buf))))
      (delete-file tmp))))

;;; ─── rfcview:download-rfc ────────────────────────────────────────────────────

(ert-deftest rfcview:test-download-rfc-returns-nil-on-404 ()
  "Returns nil when the server returns a 404 response."
  (cl-letf (((symbol-function 'rfcview:retrieve-rfc)
             (lambda (_num &optional _fmt)
               (with-current-buffer (generate-new-buffer " *test-404*")
                 (insert "HTTP/1.1 404 Not Found\r\n\r\n")
                 (current-buffer))))
            ((symbol-function 'rfcview:http-response-status) (lambda (_) 404))
            ((symbol-function 'kill-buffer) #'ignore))
    (should (null (rfcview:download-rfc 9999 'txt "/tmp/should-not-exist.txt")))))

(ert-deftest rfcview:test-download-rfc-writes-file-and-returns-path-on-200 ()
  "Writes the response body to TO-FILE and returns its path on HTTP 200.
The mock response uses bare LF line endings because rfcview:download-rfc
uses (re-search-forward \"^$\") to find the header/body separator, and
that regex fails to match a line that contains only \\r (from CRLF)."
  (let ((tmp (make-temp-file "rfcview-test-dl-")))
    (unwind-protect
        (cl-letf (((symbol-function 'rfcview:retrieve-rfc)
                   (lambda (_num &optional _fmt)
                     (with-current-buffer (generate-new-buffer " *test-200*")
                       (insert "HTTP/1.1 200 OK\n\n")
                       (insert "RFC body content.\n")
                       (current-buffer))))
                  ((symbol-function 'rfcview:http-response-status) (lambda (_) 200))
                  ((symbol-function 'kill-buffer) #'ignore))
          (let ((result (rfcview:download-rfc 793 'txt tmp)))
            (should (string= tmp result))
            (should (file-exists-p tmp))
            (let ((contents (with-temp-buffer
                              (insert-file-contents tmp)
                              (buffer-string))))
              (should (string-match-p "RFC body content" contents)))))
      (ignore-errors (delete-file tmp)))))

;;; ─── rfcview:read-rfc ────────────────────────────────────────────────────────

(ert-deftest rfcview:test-read-rfc-reuses-existing-buffer ()
  "rfcview:read-rfc pops to an existing *RFC XXXX* buffer without downloading."
  (let ((existing (get-buffer-create "*RFC 0001*")))
    (unwind-protect
        (cl-letf (((symbol-function 'rfcview:download-rfc)
                   (lambda (&rest _) (error "Should not download")))
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (rfcview:read-rfc 1))
      (kill-buffer existing))))

(ert-deftest rfcview:test-read-rfc-errors-when-unavailable ()
  "rfcview:read-rfc signals an error when no cached file and download fails."
  (let ((rfcview:local-directory "/nonexistent/dir/"))
    (cl-letf (((symbol-function 'rfcview:download-rfc) (lambda (&rest _) nil))
              ((symbol-function 'file-exists-p) (lambda (_) nil)))
      (should-error (rfcview:read-rfc 99998)))))

(ert-deftest rfcview:test-read-rfc-falls-back-to-pdf-when-txt-unavailable ()
  "read-rfc falls back to PDF when the txt download returns nil (404)."
  (let ((rfcview:local-directory "/nonexistent/dir/")
        (rfcview:preferred-format 'txt)
        opened-fmt)
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (_number fmt file) (if (eq fmt 'pdf) file nil)))
              ((symbol-function 'rfcview:open-rfc-pdf)
               (lambda (_number _file) (setq opened-fmt 'pdf) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 8)
      (should (eq opened-fmt 'pdf)))))

(ert-deftest rfcview:test-read-rfc-preferred-pdf-falls-back-to-txt ()
  "When preferred-format is pdf but PDF download fails, falls back to txt."
  (let ((rfcview:local-directory "/nonexistent/dir/")
        (rfcview:preferred-format 'pdf)
        opened-fmt)
    (cl-letf (((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (_number fmt file) (if (eq fmt 'txt) file nil)))
              ((symbol-function 'rfcview:open-rfc-txt)
               (lambda (_number _file) (setq opened-fmt 'txt) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 793)
      (should (eq opened-fmt 'txt)))))

(ert-deftest rfcview:test-read-rfc-uses-cached-pdf-without-downloading ()
  "read-rfc uses a locally cached PDF without calling download-rfc."
  (let ((rfcview:local-directory "/fake/dir/")
        (rfcview:preferred-format 'pdf)
        downloaded)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (f) (string-suffix-p ".pdf" f)))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (&rest _) (setq downloaded t) nil))
              ((symbol-function 'rfcview:open-rfc-pdf)
               (lambda (_number _file) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 8)
      (should-not downloaded))))

;;; ─── rfcview:read-show-help ──────────────────────────────────────────────────

(ert-deftest rfcview:test-read-show-help-keys-match-keymap ()
  "Every key listed in the read help buffer is bound as documented in the keymap."
  (let ((m rfcview:read-mode-map))
    (should (eq  (lookup-key m (kbd "j"))         'next-line))
    (should (eq  (lookup-key m (kbd "k"))         'previous-line))
    (should      (lookup-key m (kbd "h")))         ; lambda — just check bound
    (should      (lookup-key m (kbd "l")))         ; lambda — just check bound
    (should (eq  (lookup-key m (kbd "]"))         'rfcview:read-next-section))
    (should (eq  (lookup-key m (kbd "["))         'rfcview:read-prev-section))
    (should (eq  (lookup-key m (kbd "<tab>"))     'forward-button))
    (should (eq  (lookup-key m (kbd "<backtab>")) 'backward-button))
    (should (eq  (lookup-key m (kbd "RET"))       'push-button))
    (should (eq  (lookup-key m (kbd "+"))         'text-scale-adjust))
    (should (eq  (lookup-key m (kbd "="))         'text-scale-adjust))
    (should (eq  (lookup-key m (kbd "-"))         'text-scale-adjust))
    (should (eq  (lookup-key m (kbd "o"))         'rfcview:read-view-original))
    (should (eq  (lookup-key m (kbd "q"))         'rfcview:read-quit))
    (should (eq  (lookup-key m (kbd "?"))         'rfcview:read-show-help))))

;;; ─── rfcview:read-quit ───────────────────────────────────────────────────────

(ert-deftest rfcview:test-read-quit-buries-buffer ()
  "rfcview:read-quit buries the current buffer."
  (let (buried)
    (cl-letf (((symbol-function 'bury-buffer)      (lambda () (setq buried t)))
              ((symbol-function 'get-buffer-window) (lambda (_) nil)))
      (rfcview:read-quit)
      (should buried))))

(ert-deftest rfcview:test-read-quit-selects-index-when-visible ()
  "rfcview:read-quit selects the index window when it is visible."
  (let ((fake-win (make-symbol "win"))
        selected)
    (cl-letf (((symbol-function 'bury-buffer)      #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (_) fake-win))
              ((symbol-function 'select-window)     (lambda (w) (setq selected w))))
      (rfcview:read-quit)
      (should (eq selected fake-win)))))

(ert-deftest rfcview:test-read-quit-no-select-when-index-hidden ()
  "rfcview:read-quit does not select any window when the index is not visible."
  (let (selected)
    (cl-letf (((symbol-function 'bury-buffer)      #'ignore)
              ((symbol-function 'get-buffer-window) (lambda (_) nil))
              ((symbol-function 'select-window)     (lambda (w) (setq selected w))))
      (rfcview:read-quit)
      (should-not selected))))

(provide 'test-rfcview-reader)
;;; test-rfcview-reader.el ends here