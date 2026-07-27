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

(ert-deftest rfcview:test-section-heading-regexp-appendix-modern-wrapped ()
  "Matches Appendix heading whose title wraps to a second indented line (RFC 9950 §A)."
  (should (rfcview-test:matches-heading
           "\nAppendix A.  Example TACACS+ Authentication Configuration with Shared\n             Secret\n\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-subsection-wrapped ()
  "Matches appendix subsection heading whose title wraps to a continuation line (RFC 9950 §B.1)."
  (should (rfcview-test:matches-heading
           "\nB.1.  Example TACACS+ Authentication Configuration with Explicit\n      Certificate Definitions\n\n")))

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

(ert-deftest rfcview:test-section-heading-regexp-top-level-with-commas ()
  "Matches a top-level (X.) numbered heading with commas in the title
\(RFC 9959 §2 style: \"Language, Notation, and Terms\").
The non-period-last-char rule keeps sentence-shape list items rejected."
  (should (rfcview-test:matches-heading
           "\n2.  Language, Notation, and Terms\n\n")))

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

(ert-deftest rfcview:test-read-author-regexp-matches-cached-form ()
  "Cached form (\"J. Doe\") matches itself."
  (let ((re (rfcview:read--author-regexp "J. Doe")))
    (should (string-match-p (concat "\\`" re "\\'") "J. Doe"))))

(ert-deftest rfcview:test-read-author-regexp-matches-expanded-given-name ()
  "Cached initial form matches an expanded given name (\"John Doe\")."
  (let ((re (rfcview:read--author-regexp "J. Doe")))
    (should (string-match-p (concat "\\`" re "\\'") "John Doe"))))

(ert-deftest rfcview:test-read-author-regexp-rejects-different-surname ()
  "Same initial but different surname does not match."
  (let ((re (rfcview:read--author-regexp "J. Doe")))
    (should-not (string-match-p (concat "\\`" re "\\'") "John Smith"))))

(ert-deftest rfcview:test-read-author-regexp-rejects-different-initial ()
  "Same surname but different initial does not match."
  (let ((re (rfcview:read--author-regexp "J. Doe")))
    (should-not (string-match-p (concat "\\`" re "\\'") "K. Doe"))))

(ert-deftest rfcview:test-read-author-regexp-non-ascii-latin ()
  "Non-ASCII Latin letters (accents, diacritics) are honored."
  (let ((re (rfcview:read--author-regexp "Á. García")))
    (should (string-match-p (concat "\\`" re "\\'") "Á. García"))
    (should (string-match-p (concat "\\`" re "\\'") "Álvaro García"))))

(ert-deftest rfcview:test-read-author-regexp-ed-suffix-preserved ()
  "A trailing \", Ed.\" suffix on the cached name is preserved in the regexp."
  (let ((re (rfcview:read--author-regexp "E. Kinzie, Ed.")))
    (should (string-match-p (concat "\\`" re "\\'") "E. Kinzie, Ed."))
    (should (string-match-p (concat "\\`" re "\\'") "Eric Kinzie, Ed."))
    (should-not (string-match-p (concat "\\`" re "\\'") "E. Kinzie"))))

(ert-deftest rfcview:test-read-fontify-header-consumes-internal-blank-before-author ()
  "An internal blank line in the header is consumed when the next line is
a known author (RFC 9893 style: author appears without an organization
on the LHS, leaving the line all-whitespace).  Without this, the header
would be truncated and the author line would be mis-fontified as the
title."
  (let ((rfcview:rfc-cache
         (list :table (let ((h (make-hash-table :test 'equal)))
                        (puthash 9999
                                 (list :authors '("J. Author" "K. Other"))
                                 h)
                        h))))
    (with-temp-buffer
      (setq rfcview:read-rfc-number 9999)
      (insert "Network Working Group                                   J. Author\n"
              "Request for Comments: 9999                            Some Corp.\n"
              "\n"
              "                                                         K. Other\n"
              "                                                     January 2026\n"
              "\n"
              "\n"
              "                       A Sample Protocol\n"
              "\n"
              "Abstract\n\n   Body.\n")
      (rfcview:read-fontify)
      (should (eq 'rfcview:read-rfc-header-face
                  (rfcview-test:face-at-string "K. Other")))
      (should (eq 'rfcview:read-rfc-title-face
                  (rfcview-test:face-at-string "A Sample Protocol"))))))

(ert-deftest rfcview:test-read-fontify-header-author-name-expanded-in-doc ()
  "When the document uses an expanded given name (\"John Doe\") but the
cache stores the initial form (\"J. Doe\"), the author-line peek after
an internal blank still matches and the header is preserved."
  (let ((rfcview:rfc-cache
         (list :table (let ((h (make-hash-table :test 'equal)))
                        (puthash 9999
                                 (list :authors '("J. Author" "J. Doe"))
                                 h)
                        h))))
    (with-temp-buffer
      (setq rfcview:read-rfc-number 9999)
      (insert "Network Working Group                                   J. Author\n"
              "Request for Comments: 9999                            Some Corp.\n"
              "\n"
              "                                                         John Doe\n"
              "                                                     January 2026\n"
              "\n"
              "\n"
              "                       A Sample Protocol\n"
              "\n"
              "Abstract\n\n   Body.\n")
      (rfcview:read-fontify)
      (should (eq 'rfcview:read-rfc-header-face
                  (rfcview-test:face-at-string "John Doe")))
      (should (eq 'rfcview:read-rfc-title-face
                  (rfcview-test:face-at-string "A Sample Protocol"))))))

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

(ert-deftest rfcview:test-read-buttonize-refs-five-digit-inline-notation ()
  "Creates a button for a 5-digit 'RFC 10000' inline reference (RFC >= 10000).
Regression guard: every other fixture in this section tops out at 4 digits,
so nothing here previously exercised the unbounded [0-9]+ in the regexp."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "As defined in RFC 10000, the protocol provides...\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC 10000")
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (should (= 10000 (button-get btn 'number)))))))

(ert-deftest rfcview:test-read-buttonize-refs-five-digit-bracketed-notation ()
  "Creates a button for a 5-digit [RFC10000] bracket notation."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See [RFC10000] for details.\n")
      (rfcview:read-buttonize-refs)
      (goto-char (point-min))
      (search-forward "RFC10000")
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (should (= 10000 (button-get btn 'number)))))))

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

(defun rfcview-test:collect-section-refs ()
  "Return the set of `'section' values from rfc-link-buttons in the buffer."
  (let (sections)
    (save-excursion
      (goto-char (point-min))
      (while (forward-button 1 nil nil t)
        (let ((b (button-at (point))))
          (when (and (eq (button-type b) 'rfcview:rfc-link-button)
                     (button-get b 'section))
            (push (button-get b 'section) sections)))))
    sections))

(ert-deftest rfcview:test-read-buttonize-refs-multi-section-and ()
  "'Sections X and Y of RFC NNNN' creates a separate button per section."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See Sections 1.2 and 3.4 of RFC 9999 for details.\n")
      (rfcview:read-buttonize-refs)
      (let ((sections (rfcview-test:collect-section-refs)))
        (should (member "1.2" sections))
        (should (member "3.4" sections))))))

(ert-deftest rfcview:test-read-buttonize-refs-multi-section-or ()
  "'Sections X or Y of [RFC NNNN]' creates a separate button per section."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "Per Sections 2 or 4 of [RFC 9999].\n")
      (rfcview:read-buttonize-refs)
      (let ((sections (rfcview-test:collect-section-refs)))
        (should (member "2" sections))
        (should (member "4" sections))))))

(ert-deftest rfcview:test-read-buttonize-refs-single-section-still-works ()
  "Single-section form ('Section X of RFC NNNN') still creates a button."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See Section 2.1 of RFC 9999.\n")
      (rfcview:read-buttonize-refs)
      (let ((sections (rfcview-test:collect-section-refs)))
        (should (member "2.1" sections))))))

(ert-deftest rfcview:test-read-buttonize-refs-chained-three-sections ()
  "'Sections AA and BB and CC of RFC 1234' creates a button per section."
  (let ((rfcview:rfc-cache nil))
    (with-temp-buffer
      (insert "See Sections AA and BB and CC of RFC 1234 for details.\n")
      (rfcview:read-buttonize-refs)
      (let ((sections (rfcview-test:collect-section-refs)))
        (should (member "AA" sections))
        (should (member "BB" sections))
        (should (member "CC" sections))))))

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

(ert-deftest rfcview:test-read-buttonize-toc-appendix-wrapped-heading ()
  "Appendix and appendix-subsection headings that wrap to a second line are
detected and their (also wrapped) TOC entries are buttonized (RFC 9950 §A/B.1/B.2)."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   Appendix A.  Example Configuration with\n"
            "           Shared Secret\n"
            "   Appendix B.  TLS Examples\n"
            "     B.1.  Example Configuration with Explicit\n"
            "           Certificate Definitions\n"
            "\n"
            "Appendix A.  Example Configuration with\n"
            "             Shared Secret\n"
            "\n"
            "   Body A.\n"
            "\n"
            "Appendix B.  TLS Examples\n"
            "\n"
            "   Body B.\n"
            "\n"
            "B.1.  Example Configuration with Explicit\n"
            "      Certificate Definitions\n"
            "\n"
            "   Body B.1.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let* ((btns (rfcview-test:section-buttons))
           (targets (mapcar #'cdr btns))
           (heading-a-pos (save-excursion (goto-char (point-min))
                                          (re-search-forward "^Appendix A\\.")
                                          (line-beginning-position)))
           (heading-b-pos (save-excursion (goto-char (point-min))
                                          (re-search-forward "^Appendix B\\.")
                                          (line-beginning-position)))
           (heading-b1-pos (save-excursion (goto-char (point-min))
                                           (re-search-forward "^B\\.1\\.")
                                           (line-beginning-position))))
      (should (= 3 (length btns)))
      (should (member heading-a-pos targets))
      (should (member heading-b-pos targets))
      (should (member heading-b1-pos targets)))))

(ert-deftest rfcview:test-read-buttonize-toc-blank-line-separated-entries ()
  "RFC 1001-style TOCs put a blank line between entries, so each entry matches
`rfcview:section-heading-regexp'.  The naive `next heading' calculation would
land on the first TOC entry and abort the loop; verify the toc-end scan skips
headings whose title line ends with a trailing page number."
  (with-temp-buffer
    (insert "                        TABLE OF CONTENTS\n"
            "\n"
            "\n"
            "1.  STATUS OF THIS MEMO                                             6\n"
            "\n"
            "2.  ACKNOWLEDGEMENTS                                                7\n"
            "\n"
            "3.  INTRODUCTION                                                    8\n"
            "\n"
            "1.  STATUS OF THIS MEMO\n"
            "\n"
            "   Body of status.\n"
            "\n"
            "2.  ACKNOWLEDGEMENTS\n"
            "\n"
            "   Body of ack.\n"
            "\n"
            "3.  INTRODUCTION\n"
            "\n"
            "   Body of intro.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "STATUS OF THIS MEMO" labels))
      (should (member "ACKNOWLEDGEMENTS" labels))
      (should (member "INTRODUCTION" labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-stops-at-centered-figures-heading ()
  "RFC 1005 has a centered `FIGURES' table between the TOC and the body.  Its
entries (`2.1  IP Class A Mapping ... 6') look exactly like TOC entries and
share section numbers with real TOC entries, so without a stop signal the
buttonize loop would buttonize them — pointing the figure entries at the
real sections.  The `toc-end' calculation must halt at the centered ALL-CAPS
heading (`section-heading-regexp' deliberately requires `^[A-Z]' at column 0,
so an indented heading like `FIGURES' would otherwise be missed)."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "        1   INTRODUCTION.......................................... 4\n"
            "        2.1  Real Section ........................................ 6\n"
            "\n"
            "                                  FIGURES\n"
            "\n"
            "        2.1  Some Figure............................................ 6\n"
            "\n"
            "1  INTRODUCTION\n"
            "\n"
            "   Body.\n"
            "\n"
            "2.1  Real Section\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "INTRODUCTION" labels))
      (should (member "Real Section" labels))
      ;; The figure entry "Some Figure" sits after the FIGURES heading; the
      ;; loop must stop before reaching it so it does not get buttonized.
      (should-not (member "Some Figure" labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-adjacent-dot-leader ()
  "RFC 1005-style TOC entries have the dot leader adjacent to the title with
no separating whitespace (`INTRODUCTION.......................... 4').  The
button label must be just the title — the leader and trailing page must dim
and not be swallowed into the title."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "        1   INTRODUCTION.......................................... 4\n"
            "        2   IP ISSUES............................................. 6\n"
            "\n"
            "1  INTRODUCTION\n"
            "\n"
            "   Body.\n"
            "\n"
            "2  IP ISSUES\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let* ((btns (rfcview-test:section-buttons))
           (labels (mapcar #'car btns)))
      (should (member "INTRODUCTION" labels))
      (should (member "IP ISSUES" labels)))
    ;; The dot-leader chars should have the leader face, not the section/title face.
    (goto-char (point-min))
    (re-search-forward "INTRODUCTION\\.\\.")
    (should (eq (get-char-property (match-end 0) 'face)
                'rfcview:read-toc-leader-face))))

(ert-deftest rfcview:test-read-buttonize-toc-leader-only-continuation-line ()
  "RFC 1005-style wrapped TOC entries put the title on lines 1-2 and the dot
leader on a separate (third) line.  The leader on that standalone line must
visually dim (carry `rfcview:read-toc-leader-face')."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "        2.1   Current Interpretation of Class A IP Address\n"
            "              Fields\n"
            "               ................................................... 6\n"
            "\n"
            "2.1  Current Interpretation of Class A IP Address Fields\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (goto-char (point-min))
    (re-search-forward "^[ ]+\\.\\.\\.")
    (let* ((bol (line-beginning-position))
           (eol (line-end-position)))
      (should (eq (get-char-property (+ bol 20) 'face)
                  'rfcview:read-toc-leader-face))
      (should (eq (get-char-property (1- eol) 'face)
                  'rfcview:read-toc-leader-face)))))

(ert-deftest rfcview:test-read-buttonize-toc-bare-appendix-rfc1001 ()
  "RFC 1001-style bare APPENDIX (just the letter, no title on the heading line)
gets a button linking to the body's `APPENDIX A` line, and its dash-form
subsections (A-1, A-2) link to the matching `A-N.  Title` body headings."
  (with-temp-buffer
    (insert "                        TABLE OF CONTENTS\n"
            "\n"
            "\n"
            "APPENDIX A                                                         61\n"
            "   A-1.  ADDITIONAL PROTOCOL REQUIRED IN B AND M NODES              61\n"
            "   A-2.  CONSTRAINTS                                                61\n"
            "\n"
            "APPENDIX A\n"
            "\n"
            "   Body of appendix A.\n"
            "\n"
            "A-1.  ADDITIONAL PROTOCOL REQUIRED IN B AND M NODES\n"
            "\n"
            "   Body of A-1.\n"
            "\n"
            "A-2.  CONSTRAINTS\n"
            "\n"
            "   Body of A-2.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let* ((btns (rfcview-test:section-buttons))
           (labels (mapcar #'car btns)))
      (should (member "APPENDIX A" labels))
      (should (member "ADDITIONAL PROTOCOL REQUIRED IN B AND M NODES" labels))
      (should (member "CONSTRAINTS" labels)))))

(ert-deftest rfcview:test-read-buttonize-toc-dash-form-nested-subsection ()
  "Dash-form sub-subsections (B-1.1, B-6.1) resolve via the by-number map."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "   B-1.  IMPLEMENTATION MODELS                                     62\n"
            "      B-1.1  MODEL INDEPENDENT CONSIDERATIONS                      63\n"
            "      B-1.2  SERVICE OPERATION FOR EACH MODEL                      63\n"
            "\n"
            "B-1.  IMPLEMENTATION MODELS\n"
            "\n"
            "   Body.\n"
            "\n"
            "B-1.1  MODEL INDEPENDENT CONSIDERATIONS\n"
            "\n"
            "   Body.\n"
            "\n"
            "B-1.2  SERVICE OPERATION FOR EACH MODEL\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((labels (mapcar #'car (rfcview-test:section-buttons))))
      (should (member "IMPLEMENTATION MODELS" labels))
      (should (member "MODEL INDEPENDENT CONSIDERATIONS" labels))
      (should (member "SERVICE OPERATION FOR EACH MODEL" labels)))))

(ert-deftest rfcview:test-section-heading-regexp-dash-form-subsection ()
  "Dash-form RFC 1001 subsection headings (A-1., B-1.1) match the section regex
with the title outside group 1."
  (with-temp-buffer
    (insert "\nA-1.  Additional Protocol\n\n"
            "\nB-1.1  Model Independent Considerations\n\n")
    (goto-char (point-min))
    (should (re-search-forward rfcview:section-heading-regexp nil t))
    (should (string-match-p "A-1\\." (match-string 0)))
    (should (re-search-forward rfcview:section-heading-regexp nil t))
    (should (string-match-p "B-1\\.1" (match-string 0)))))

(ert-deftest rfcview:test-read-register-anchor-bare-appendix-by-number ()
  "Body's bare `APPENDIX A` registers under both number key `A` and title
`appendix a' so TOC entries can find it via either lookup."
  (with-temp-buffer
    (insert "Network Working Group\n"
            "Request for Comments: 9999\n"
            "\n"
            "                       Sample\n"
            "\n"
            "Abstract\n"
            "\n"
            "   Some text.\n"
            "\n"
            "1.  Introduction\n"
            "\n"
            "   Body.\n"
            "\n"
            "APPENDIX A\n"
            "\n"
            "   Body of A.\n")
    (rfcview:read-fontify)
    (should (gethash "A" rfcview:read-section-anchors-by-number))
    (should (gethash "appendix a" rfcview:read-section-anchors-by-title))))

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

(ert-deftest rfcview:test-read-buttonize-toc-wrapped-leader-on-continuation ()
  "When a wrapped TOC entry's dot-leader and page number sit on the continuation
line (RFC 5246 F.1.1.3 \"Diffie-Hellman Key Exchange with / Authentication\"),
the continuation-line leader visually dims, and the section-link button stops
at the leader (otherwise the button overlay's face would mask the dim)."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "                  F.1.1.3. Diffie-Hellman Key Exchange with\n"
            "                           Authentication ............................92\n"
            "\n"
            "F.1.1.3. Diffie-Hellman Key Exchange with Authentication\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (goto-char (point-min))
    (re-search-forward "^                           Authentication")
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (dot-pos (save-excursion (re-search-forward " \\.+" eol t)
                                    (match-beginning 0))))
      ;; The button must end at-or-before the dot leader.
      (let ((button (button-at (1+ bol))))
        (should button)
        (should (<= (button-end button) dot-pos)))
      ;; The dot leader and page number must visually show leader-face.
      (should (eq (get-char-property dot-pos 'face)
                  'rfcview:read-toc-leader-face))
      (should (eq (get-char-property (- eol 1) 'face)
                  'rfcview:read-toc-leader-face)))))

(ert-deftest rfcview:test-read-buttonize-toc-deeply-nested-appendix-subsection ()
  "Appendix-subsection TOC entries deeper than two segments (e.g. A.4.1) get
their own buttons and dim, not absorbed as continuations of the parent A.4 entry
(regression for RFC 5246, which has A.4 followed by A.4.1 .. A.4.4)."
  (with-temp-buffer
    (insert "Table of Contents\n"
            "\n"
            "      A.4. Handshake Protocol ........................................70\n"
            "           A.4.1. Hello Messages .....................................71\n"
            "           A.4.2. Server Authentication ..............................72\n"
            "\n"
            "A.4. Handshake Protocol\n"
            "\n"
            "   Body.\n"
            "\n"
            "A.4.1. Hello Messages\n"
            "\n"
            "   Body.\n"
            "\n"
            "A.4.2. Server Authentication\n"
            "\n"
            "   Body.\n")
    (rfcview:read-fontify)
    (rfcview:read-buttonize-toc)
    (let ((btns (rfcview-test:section-buttons)))
      (should (assoc "Handshake Protocol" btns))
      (should (assoc "Hello Messages" btns))
      (should (assoc "Server Authentication" btns)))
    ;; The A.4.1 TOC line should have its leader+page-number dimmed (i.e. NOT
    ;; have been swallowed by A.4's continuation absorption).
    (goto-char (point-min))
    (re-search-forward "^           A\\.4\\.1\\. Hello Messages")
    (let ((bol (line-beginning-position))
          (eol (line-end-position))
          (any-leader nil))
      (let ((p bol))
        (while (< p eol)
          (when (eq (get-text-property p 'face) 'rfcview:read-toc-leader-face)
            (setq any-leader t))
          (setq p (1+ p))))
      (should any-leader))))

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
                (should (string= "*RFC 793*" (buffer-name buf)))
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

(ert-deftest rfcview:test-open-rfc-txt-no-section-leaves-point-at-start ()
  "When SECTION is nil, open-rfc-txt leaves point at buffer start."
  (let ((tmp (make-temp-file "rfcview-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert (rfcview-test:sample-rfc-with-toc)))
          (let ((rfcview:nav-history (cons nil nil))
                (buf (rfcview:open-rfc-txt 8 tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should (= (point) (point-min)))
                  (should (null (car rfcview:nav-history))))
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
  (let ((existing (get-buffer-create "*RFC 1*")))
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
        opened-fmt)
    (cl-letf (((symbol-function 'rfcview:read--format-order)
               (lambda (&rest _) '(txt pdf)))
              ((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (_number fmt file) (if (eq fmt 'pdf) file nil)))
              ((symbol-function 'rfcview:open-rfc-pdf)
               (lambda (_number _file &optional _section) (setq opened-fmt 'pdf) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 8)
      (should (eq opened-fmt 'pdf)))))

(ert-deftest rfcview:test-read-rfc-preferred-pdf-falls-back-to-txt ()
  "When preferred-format is pdf but PDF download fails, falls back to txt."
  (let ((rfcview:local-directory "/nonexistent/dir/")
        opened-fmt)
    (cl-letf (((symbol-function 'rfcview:read--format-order)
               (lambda (&rest _) '(pdf txt)))
              ((symbol-function 'file-exists-p) (lambda (_) nil))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (_number fmt file) (if (eq fmt 'txt) file nil)))
              ((symbol-function 'rfcview:open-rfc-txt)
               (lambda (_number _file &optional _section) (setq opened-fmt 'txt) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 793)
      (should (eq opened-fmt 'txt)))))

(ert-deftest rfcview:test-read-rfc-uses-cached-pdf-without-downloading ()
  "read-rfc uses a locally cached PDF without calling download-rfc."
  (let ((rfcview:local-directory "/fake/dir/")
        downloaded)
    (cl-letf (((symbol-function 'rfcview:read--format-order)
               (lambda (&rest _) '(pdf)))
              ((symbol-function 'file-exists-p)
               (lambda (f) (string-suffix-p ".pdf" f)))
              ((symbol-function 'rfcview:download-rfc)
               (lambda (&rest _) (setq downloaded t) nil))
              ((symbol-function 'rfcview:open-rfc-pdf)
               (lambda (_number _file &optional _section) (current-buffer)))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (rfcview:read-rfc 8)
      (should-not downloaded))))

(ert-deftest rfcview:test-read-rfc-jumps-section-in-reused-buffer ()
  "rfcview:read-rfc with SECTION jumps in an already-open *RFC* buffer."
  (let ((existing (get-buffer-create "*RFC 9999*")))
    (unwind-protect
        (progn
          (with-current-buffer existing
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (rfcview-test:sample-rfc-with-toc)))
            (rfcview:read-mode 9999 nil)
            (goto-char (point-min)))
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (rfcview:read-rfc 9999 "1.1"))
          (with-current-buffer existing
            (should (looking-at "1\\.1  Goals"))))
      (kill-buffer existing))))

(ert-deftest rfcview:test-read-rfc-html-fallback-no-arity-error ()
  "html-only RFC dispatches to open-rfc-fallback without an arity error."
  (let* ((tbl (make-hash-table))
         (rfcview:rfc-cache (progn (puthash 9999 '(:format ("HTML")) tbl)
                                   (list :version 2 :table tbl)))
         (rfcview:preferred-format 'html)
         browsed)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq browsed url))))
      (rfcview:read-rfc 9999 "3.2")
      (should browsed)
      (should (string-match-p "rfc9999\\.html" browsed)))))

;;; ─── rfcview:read-buffer-name ───────────────────────────────────────────────

(ert-deftest rfcview:test-read-buffer-name-unpadded ()
  "rfcview:read-buffer-name does not zero-pad the RFC number."
  (should (string= "*RFC 42*" (rfcview:read-buffer-name 42)))
  (should (string= "*RFC 10000*" (rfcview:read-buffer-name 10000))))

;;; ─── rfcview:read--local-file-path ──────────────────────────────────────────

(ert-deftest rfcview:test-local-file-path-uses-existing-unpadded-file ()
  "Returns the unpadded path directly when it already exists; no legacy check."
  (let* ((dir (file-name-as-directory (make-temp-file "rfcview-test-" t)))
         (rfcview:local-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (concat dir "rfc42.txt") (insert "new content"))
          (cl-letf (((symbol-function 'rfcview:download-rfc)
                     (lambda (&rest _) (error "Should not download"))))
            (should (string= (concat dir "rfc42.txt")
                             (rfcview:read--local-file-path 42 'txt)))))
      (delete-directory dir t))))

(ert-deftest rfcview:test-local-file-path-recovers-legacy-zero-padded-file ()
  "A legacy zero-padded cache file is copied forward to the unpadded name.
The legacy file is left in place (copy, not rename) and no download happens."
  (let* ((dir (file-name-as-directory (make-temp-file "rfcview-test-" t)))
         (rfcview:local-directory dir)
         (legacy (concat dir "rfc0042.txt"))
         (new    (concat dir "rfc42.txt")))
    (unwind-protect
        (progn
          (with-temp-file legacy (insert "legacy content"))
          (cl-letf (((symbol-function 'rfcview:download-rfc)
                     (lambda (&rest _) (error "Should not download"))))
            (should (string= new (rfcview:read--local-file-path 42 'txt))))
          (should (file-exists-p new))
          (should (file-exists-p legacy))
          (with-temp-buffer
            (insert-file-contents new)
            (should (string= "legacy content" (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest rfcview:test-local-file-path-downloads-when-neither-exists ()
  "Falls through to rfcview:download-rfc when neither file is present."
  (let* ((dir (file-name-as-directory (make-temp-file "rfcview-test-" t)))
         (rfcview:local-directory dir)
         downloaded)
    (unwind-protect
        (cl-letf (((symbol-function 'rfcview:download-rfc)
                   (lambda (_number _fmt file) (setq downloaded t) file)))
          (rfcview:read--local-file-path 42 'txt)
          (should downloaded))
      (delete-directory dir t))))

(ert-deftest rfcview:test-local-file-path-no-self-copy-above-999 ()
  "For RFC >= 1000 the legacy and current paths coincide; no copy-file call."
  (let* ((dir (file-name-as-directory (make-temp-file "rfcview-test-" t)))
         (rfcview:local-directory dir)
         copied)
    (unwind-protect
        (progn
          (with-temp-file (concat dir "rfc1234.txt") (insert "content"))
          (cl-letf (((symbol-function 'copy-file)
                     (lambda (&rest _) (setq copied t)))
                    ((symbol-function 'rfcview:download-rfc)
                     (lambda (&rest _) (error "Should not download"))))
            (rfcview:read--local-file-path 1234 'txt)
            (should-not copied)))
      (delete-directory dir t))))

;;; ─── rfcview:nav-history ────────────────────────────────────────────────────

(defmacro rfcview:test--with-fake-reader (rfc-num pos &rest body)
  "Run BODY in a fake reader buffer pretending to be RFC RFC-NUM with point at POS.
Resets the global navigation history before BODY and cleans up the buffer after."
  (declare (indent 2))
  `(let ((buf (generate-new-buffer (rfcview:read-buffer-name ,rfc-num))))
     (rfcview:nav-history-clear)
     (unwind-protect
         (with-current-buffer buf
           (insert (make-string 200 ?x))
           (setq-local rfcview:read-rfc-number ,rfc-num)
           (goto-char ,pos)
           ,@body)
       (kill-buffer buf))))

(ert-deftest rfcview:test-nav-push-records-current-location ()
  "nav-push pushes (RFC . POS) onto BACK and clears FORWARD."
  (rfcview:test--with-fake-reader 100 42
    (setcdr rfcview:nav-history '((999 . 1)))  ; stale forward
    (rfcview:nav-push)
    (should (equal (car rfcview:nav-history) '((100 . 42))))
    (should-not (cdr rfcview:nav-history))))

(ert-deftest rfcview:test-nav-push-deduplicates-adjacent ()
  "Pushing the same record twice in a row keeps only one entry."
  (rfcview:test--with-fake-reader 100 42
    (rfcview:nav-push)
    (rfcview:nav-push)
    (should (equal (car rfcview:nav-history) '((100 . 42))))))

(ert-deftest rfcview:test-nav-push-respects-history-max ()
  "BACK stack length is capped by rfcview:nav-history-max."
  (rfcview:test--with-fake-reader 100 1
    (let ((rfcview:nav-history-max 3))
      (dolist (p '(10 20 30 40 50))
        (goto-char p)
        (rfcview:nav-push))
      (should (= (length (car rfcview:nav-history)) 3))
      ;; newest first; oldest two (10, 20) dropped
      (should (equal (car rfcview:nav-history)
                     '((100 . 50) (100 . 40) (100 . 30)))))))

(ert-deftest rfcview:test-nav-push-skipped-without-rfc-number ()
  "nav-push is a no-op when the buffer has no valid RFC number."
  (rfcview:nav-history-clear)
  (with-temp-buffer
    (setq-local rfcview:read-rfc-number 0)
    (rfcview:nav-push)
    (should-not (car rfcview:nav-history))))

(ert-deftest rfcview:test-history-clear-resets-both-stacks ()
  "nav-history-clear empties BACK and FORWARD."
  (setq rfcview:nav-history (cons '((1 . 1) (2 . 2)) '((3 . 3))))
  (rfcview:nav-history-clear)
  (should-not (car rfcview:nav-history))
  (should-not (cdr rfcview:nav-history)))

(ert-deftest rfcview:test-history-back-restores-same-rfc-position ()
  "C-c C-b on a single-RFC history pops BACK and pushes current to FORWARD."
  (rfcview:test--with-fake-reader 100 42
    (rfcview:nav-push)
    (goto-char 99)
    (rfcview:read-history-back)
    (should (= (point) 42))
    (should-not (car rfcview:nav-history))
    (should (equal (cdr rfcview:nav-history) '((100 . 99))))))

(ert-deftest rfcview:test-history-back-errors-on-empty ()
  "C-c C-b with empty BACK signals user-error."
  (rfcview:test--with-fake-reader 100 1
    (should-error (rfcview:read-history-back) :type 'user-error)))

(ert-deftest rfcview:test-history-forward-errors-on-empty ()
  "C-c C-f with empty FORWARD signals user-error."
  (rfcview:test--with-fake-reader 100 1
    (should-error (rfcview:read-history-forward) :type 'user-error)))

(ert-deftest rfcview:test-history-forward-undoes-back ()
  "After back+forward, point and stacks return to the original state."
  (rfcview:test--with-fake-reader 100 42
    (rfcview:nav-push)
    (goto-char 99)
    (rfcview:read-history-back)
    (should (= (point) 42))
    (rfcview:read-history-forward)
    (should (= (point) 99))
    (should (equal (car rfcview:nav-history) '((100 . 42))))
    (should-not (cdr rfcview:nav-history))))

(ert-deftest rfcview:test-nav-push-clears-forward-after-back ()
  "Making a new jump after going back discards the FORWARD branch."
  (rfcview:test--with-fake-reader 100 42
    (rfcview:nav-push)
    (goto-char 99)
    (rfcview:read-history-back)
    (should (cdr rfcview:nav-history))
    (goto-char 50)
    (rfcview:nav-push)
    (should-not (cdr rfcview:nav-history))))

(ert-deftest rfcview:test-history-back-switches-to-other-rfc-buffer ()
  "C-c C-b across RFCs switches to the source buffer and restores position."
  (let ((buf-a (generate-new-buffer "*RFC 100*"))
        (buf-b (generate-new-buffer "*RFC 200*")))
    (rfcview:nav-history-clear)
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (insert (make-string 200 ?a))
            (setq-local rfcview:read-rfc-number 100))
          (with-current-buffer buf-b
            (insert (make-string 200 ?b))
            (setq-local rfcview:read-rfc-number 200))
          ;; Simulate: jumped from A@42 to B, currently at B@99.
          (setq rfcview:nav-history (cons '((100 . 42)) nil))
          (set-buffer buf-b)
          (goto-char 99)
          (rfcview:read-history-back)
          (should (eq (current-buffer) buf-a))
          (should (= (point) 42))
          (should-not (car rfcview:nav-history))
          (should (equal (cdr rfcview:nav-history) '((200 . 99)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest rfcview:test-jump-to-another-rfc-after-back-clears-forward ()
  "After back, jumping (push) to a new location discards the forward branch."
  (let ((buf-a (generate-new-buffer "*RFC 100*"))
        (buf-b (generate-new-buffer "*RFC 200*")))
    (rfcview:nav-history-clear)
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (insert (make-string 200 ?a))
            (setq-local rfcview:read-rfc-number 100))
          (with-current-buffer buf-b
            (insert (make-string 200 ?b))
            (setq-local rfcview:read-rfc-number 200))
          (setq rfcview:nav-history (cons '((100 . 42)) nil))
          (set-buffer buf-b)
          (goto-char 99)
          (rfcview:read-history-back)            ; now in A@42; forward = ((200 . 99))
          (should (cdr rfcview:nav-history))
          ;; User clicks a link from A@42 to some new place; nav-push records A@42.
          (rfcview:nav-push)
          (should-not (cdr rfcview:nav-history))
          (should (equal (car rfcview:nav-history) '((100 . 42)))))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

;;; ─── rfcview:read-forward-link / rfcview:read-backward-link ─────────────────

(defun rfcview:test--make-link-buffer ()
  "Build a buffer with a button, a goto-address overlay, and another button.
Leading and trailing space ensures point-min/point-max are not inside a link.
Link positions: button at 3..10, URL overlay at 17..24, button at 31..38."
  (let ((buf (generate-new-buffer " *link-test*")))
    (with-current-buffer buf
      (insert "  AAAAAAAA      BBBBBBBB      CCCCCCCC  ")
      (make-text-button 3 11 'type 'rfcview:rfc-link-button)
      (let ((ov (make-overlay 17 25)))
        (overlay-put ov 'goto-address t))
      (make-text-button 31 39 'type 'rfcview:rfc-link-button))
    buf))

(ert-deftest rfcview:test-read-forward-link-single-step-finds-button ()
  "TAB from before the first link jumps to it."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (rfcview:read-forward-link)
          (should (= (point) 3)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-forward-link-single-step-finds-goto-address ()
  "TAB from inside the first button jumps to the goto-address overlay."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char 5)
          (rfcview:read-forward-link)
          (should (= (point) 17)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-forward-link-single-step-skips-from-url-to-next-button ()
  "TAB from inside the URL jumps to the next button."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char 20)
          (rfcview:read-forward-link)
          (should (= (point) 31)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-forward-link-no-next-errors ()
  "TAB past the last link signals a user-error."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (should-error (rfcview:read-forward-link) :type 'user-error))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-backward-link-single-step ()
  "S-TAB from past the last link jumps backward to the last button."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (rfcview:read-backward-link)
          (should (= (point) 31)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-backward-link-finds-goto-address ()
  "S-TAB from inside the last button jumps to the URL overlay."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char 33)
          (rfcview:read-backward-link)
          (should (= (point) 17)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-forward-link-prefix-arg-multi-step ()
  "Prefix arg moves N links forward."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (rfcview:read-forward-link 3)
          (should (= (point) 31)))
      (kill-buffer buf))))

(ert-deftest rfcview:test-read-forward-link-zero-arg-is-noop ()
  "n=0 does not move point."
  (let ((buf (rfcview:test--make-link-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char 13)
          (rfcview:read-forward-link 0)
          (should (= (point) 13)))
      (kill-buffer buf))))

;;; ─── rfcview:read--restyle-goto-address-overlays ────────────────────────────

(ert-deftest rfcview:test-restyle-goto-address-overrides-help-echo ()
  "URL overlays get their stock help-echo replaced with our bindings."
  (with-temp-buffer
    (insert "https://example.com")
    (let ((ov (make-overlay 1 20)))
      (overlay-put ov 'goto-address t)
      (overlay-put ov 'help-echo "mouse-2, C-c RET: follow URL")
      (rfcview:read--restyle-goto-address-overlays)
      (should (equal (overlay-get ov 'help-echo) "mouse-1, RET: follow URL")))))

(ert-deftest rfcview:test-restyle-goto-address-syncs-mouse-face ()
  "URL overlays get `rfcview:mouse-face' instead of `highlight'."
  (with-temp-buffer
    (insert "https://example.com")
    (let ((ov (make-overlay 1 20)))
      (overlay-put ov 'goto-address t)
      (overlay-put ov 'mouse-face 'highlight)
      (rfcview:read--restyle-goto-address-overlays)
      (should (eq (overlay-get ov 'mouse-face) 'rfcview:mouse-face)))))

(ert-deftest rfcview:test-restyle-goto-address-ignores-non-url-overlays ()
  "Overlays without a goto-address property are left alone."
  (with-temp-buffer
    (insert "some text")
    (let ((ov (make-overlay 1 5)))
      (overlay-put ov 'help-echo "unrelated tooltip")
      (overlay-put ov 'mouse-face 'highlight)
      (rfcview:read--restyle-goto-address-overlays)
      (should (equal (overlay-get ov 'help-echo) "unrelated tooltip"))
      (should (eq (overlay-get ov 'mouse-face) 'highlight)))))

(ert-deftest rfcview:test-init-goto-address-registers-restyle-after-fontify ()
  "Calling `rfcview:read--init-goto-address' (the production helper used
by `rfcview:read-mode') must leave the restyle AFTER
`goto-address-fontify-region' in `jit-lock-functions'.  Regression
guard: `jit-lock-register' prepends, which would put the restyle
ahead of goto-address — it would then run on a region with no
overlays yet and be a silent no-op."
  (with-temp-buffer
    (rfcview:read--init-goto-address)
    (let* ((fns (cl-remove-if-not #'symbolp jit-lock-functions))
           (ga  (cl-position 'goto-address-fontify-region fns))
           (rv  (cl-position 'rfcview:read--restyle-goto-address-overlays fns)))
      (should ga)
      (should rv)
      (should (< ga rv)))))

(ert-deftest rfcview:test-init-goto-address-restyles-jit-lock-overlays ()
  "End-to-end via the production helper: after
`rfcview:read--init-goto-address' wires up the buffer and jit-lock
fontifies it, goto-address overlays must end up with our synced
help-echo and mouse-face.  Direct unit tests on the restyle function
pass even when the registration is wrong — this exercises the path
that broke in real read-mode buffers."
  (with-temp-buffer
    (insert "Some text http://example.com/foo more text\n")
    (rfcview:read--init-goto-address)
    ;; Run jit-lock-functions in registered order, skipping the `t' marker.
    (dolist (fn jit-lock-functions)
      (when (functionp fn)
        (funcall fn (point-min) (point-max))))
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'goto-address))
                (overlays-in (point-min) (point-max)))))
      (should (= 1 (length ovs)))
      (let ((ov (car ovs)))
        (should (equal (overlay-get ov 'help-echo) "mouse-1, RET: follow URL"))
        (should (eq    (overlay-get ov 'mouse-face) 'rfcview:mouse-face))))))

;;; ─── rfcview:read-show-help ──────────────────────────────────────────────────

(ert-deftest rfcview:test-read-show-help-keys-match-keymap ()
  "Every key listed in the read help buffer is bound as documented in the keymap."
  (let ((m rfcview:read-mode-map))
    (should (eq  (lookup-key m (kbd "]"))         'rfcview:read-next-section))
    (should (eq  (lookup-key m (kbd "["))         'rfcview:read-prev-section))
    (should (eq  (lookup-key m (kbd "<tab>"))     'rfcview:read-forward-link))
    (should (eq  (lookup-key m (kbd "<backtab>")) 'rfcview:read-backward-link))
    (should (eq  (lookup-key m (kbd "B"))         'rfcview:read-history-back))
    (should (eq  (lookup-key m (kbd "F"))         'rfcview:read-history-forward))
    (should (eq  (lookup-key m (kbd "C-c C-b"))   'rfcview:read-history-back))
    (should (eq  (lookup-key m (kbd "C-c C-f"))   'rfcview:read-history-forward))
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
  (let ((fake-buf (make-symbol "buf"))
        (fake-win (make-symbol "win"))
        selected)
    (cl-letf (((symbol-function 'bury-buffer)      #'ignore)
              ((symbol-function 'get-buffer)        (lambda (_) fake-buf))
              ((symbol-function 'get-buffer-window) (lambda (_) fake-win))
              ((symbol-function 'select-window)     (lambda (w) (setq selected w))))
      (rfcview:read-quit)
      (should (eq selected fake-win)))))

(ert-deftest rfcview:test-read-quit-switches-to-index-when-buffer-exists-but-hidden ()
  "rfcview:read-quit switches to the index buffer when it exists but is not displayed."
  (let ((fake-buf (make-symbol "buf"))
        switched)
    (cl-letf (((symbol-function 'bury-buffer)       #'ignore)
              ((symbol-function 'get-buffer)        (lambda (_) fake-buf))
              ((symbol-function 'get-buffer-window) (lambda (_) nil))
              ((symbol-function 'switch-to-buffer)  (lambda (b) (setq switched b))))
      (rfcview:read-quit)
      (should (eq switched fake-buf)))))

(ert-deftest rfcview:test-read-quit-no-action-when-index-buffer-killed ()
  "rfcview:read-quit does nothing extra when the index buffer has been killed."
  (let (selected switched)
    (cl-letf (((symbol-function 'bury-buffer)       #'ignore)
              ((symbol-function 'get-buffer)        (lambda (_) nil))
              ((symbol-function 'select-window)     (lambda (w) (setq selected w)))
              ((symbol-function 'switch-to-buffer)  (lambda (b) (setq switched b))))
      (rfcview:read-quit)
      (should-not selected)
      (should-not switched))))

(ert-deftest rfcview:test-read-quit-integration-selects-real-index-window ()
  "End-to-end: rfcview:read-quit selects the live `*RFC INDEX*' window."
  (let ((reader (get-buffer-create "*RFC 1*"))
        (index  (get-buffer-create "*RFC INDEX*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer reader)
          (let ((index-win (split-window-below)))
            (set-window-buffer index-win index)
            (select-window (get-buffer-window reader))
            (rfcview:read-quit)
            (should (eq (selected-window) index-win))))
      (kill-buffer reader)
      (kill-buffer index))))

;;; ─── rfcview:read--format-order ────────────────────────────────────────────

(ert-deftest rfcview:test-read-format-order-preferred-leads-when-listed ()
  "Preferred leads the result when it appears in AVAILABLE."
  (should (equal '(txt pdf)  (rfcview:read--format-order 'txt  '("TXT" "PDF"))))
  (should (equal '(pdf txt)  (rfcview:read--format-order 'pdf  '("TXT" "PDF"))))
  (should (equal '(html)     (rfcview:read--format-order 'html '("HTML"))))
  (should (equal '(xml)      (rfcview:read--format-order 'xml  '("XML")))))

(ert-deftest rfcview:test-read-format-order-preferred-dropped-when-not-listed ()
  "Preferred is dropped when the index does not advertise it."
  (should (equal '(txt pdf) (rfcview:read--format-order 'html '("TXT" "PDF"))))
  (should (equal '(html)    (rfcview:read--format-order 'txt  '("HTML"))))
  (should (equal '(html)    (rfcview:read--format-order 'pdf  '("HTML")))))

(ert-deftest rfcview:test-read-format-order-empty-available-returns-nil ()
  "No supported format listed → return nil (caller treats as unavailable)."
  (should (null (rfcview:read--format-order 'txt  nil)))
  (should (null (rfcview:read--format-order 'pdf  nil)))
  (should (null (rfcview:read--format-order 'html nil)))
  (should (null (rfcview:read--format-order 'xml  nil)))
  (should (null (rfcview:read--format-order 'txt  '("PS" "EPUB")))))

(ert-deftest rfcview:test-read-format-order-no-duplicate-preferred ()
  "Preferred is not duplicated when it also appears in AVAILABLE."
  (should (equal '(txt)  (rfcview:read--format-order 'txt  '("TXT"))))
  (should (equal '(html) (rfcview:read--format-order 'html '("HTML")))))

(ert-deftest rfcview:test-read-format-order-case-insensitive ()
  "AVAILABLE strings are matched case-insensitively."
  (should (equal '(txt) (rfcview:read--format-order 'txt '("txt"))))
  (should (equal '(pdf) (rfcview:read--format-order 'pdf '("Pdf"))))
  (should (equal '(txt pdf) (rfcview:read--format-order 'txt '("pdf" "txt")))))

(ert-deftest rfcview:test-read-format-order-unsupported-tokens-filtered ()
  "Tokens outside `rfcview:supported-formats' (PS, EPUB, …) are dropped."
  (should (equal '(txt) (rfcview:read--format-order 'txt '("TXT" "PS" "EPUB")))))

;;; ─── rfcview:read-jump-to-section ───────────────────────────────────────────

(ert-deftest rfcview:test-read-jump-to-section-by-number ()
  "Jumping by a numeric key lands on the matching heading line."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "1.1")
      (should (looking-at "1\\.1  Goals")))))

(ert-deftest rfcview:test-read-jump-to-section-number-trailing-dot ()
  "A trailing dot on the section number is normalized away."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "2.")
      (should (looking-at "2\\.  Protocol Details")))))

(ert-deftest rfcview:test-read-jump-to-section-by-title ()
  "When the input does not match by number, fall back to title lookup."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "Acknowledgements")
      (should (looking-at "Acknowledgements")))))

(ert-deftest rfcview:test-read-jump-to-section-title-case-insensitive ()
  "Title lookup is case- and whitespace-insensitive (normalize-title)."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "  ACKNOWLEDGEMENTS  ")
      (should (looking-at "Acknowledgements")))))

(ert-deftest rfcview:test-read-jump-to-section-nil-is-noop ()
  "Nil input does not move point and does not push nav history."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (setq rfcview:read-rfc-number 9999)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section nil)
      (should (= (point) (point-min)))
      (should (null (car rfcview:nav-history))))))

(ert-deftest rfcview:test-read-jump-to-section-blank-is-noop ()
  "Whitespace-only input does not move point."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "   ")
      (should (= (point) (point-min))))))

(ert-deftest rfcview:test-read-jump-to-section-miss-keeps-point ()
  "Unknown section keeps point and does not push nav history."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (setq rfcview:read-rfc-number 9999)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "42.7")
      (should (= (point) (point-min)))
      (should (null (car rfcview:nav-history))))))

(ert-deftest rfcview:test-read-jump-to-section-pushes-nav ()
  "Successful jump pushes the prior location onto the BACK stack."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (setq rfcview:read-rfc-number 9999)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (let ((origin (point)))
        (rfcview:read-jump-to-section "2")
        (should (looking-at "2\\.  Protocol Details"))
        (should (equal (car (car rfcview:nav-history))
                       (cons 9999 origin)))))))

(ert-deftest rfcview:test-read-jump-to-section-inhibit-nav-push ()
  "INHIBIT-NAV-PUSH suppresses the history push but still moves point.
This is the path used by `rfcview:open-rfc-txt' so a section-button
click does not stack the new buffer's position-1 on top of the origin
already pushed by the button action."
  (with-temp-buffer
    (insert (rfcview-test:sample-rfc-with-toc))
    (rfcview:read-fontify)
    (setq rfcview:read-rfc-number 9999)
    (let ((rfcview:nav-history (cons nil nil)))
      (goto-char (point-min))
      (rfcview:read-jump-to-section "2" t)
      (should (looking-at "2\\.  Protocol Details"))
      (should (null (car rfcview:nav-history))))))

;;; ─── Translation: paragraph bounds ──────────────────────────────────────────

(ert-deftest rfcview:test-paragraph-bounds-at-mid-paragraph ()
  "Returns (BEG . END) when point is inside a non-blank paragraph."
  (with-temp-buffer
    (insert "Line 1\nLine 2\n\nNext para\n")
    (goto-char (+ (point-min) 2))
    (let ((bounds (rfcview:read--paragraph-bounds-at (point))))
      (should bounds)
      (should (= (car bounds) (point-min)))
      (should (= (cdr bounds)
                 (save-excursion (goto-char (point-min))
                                 (forward-line 2)
                                 (point)))))))

(ert-deftest rfcview:test-paragraph-bounds-on-blank-line-returns-nil ()
  "Returns nil when point is on a blank line."
  (with-temp-buffer
    (insert "Line 1\n\nLine 3\n")
    (goto-char (point-min))
    (forward-line 1)
    (should (null (rfcview:read--paragraph-bounds-at (point))))))

(ert-deftest rfcview:test-paragraph-bounds-at-bob ()
  "Returns BEG = point-min when paragraph starts at BOB."
  (with-temp-buffer
    (insert "First para\n\nSecond\n")
    (let ((bounds (rfcview:read--paragraph-bounds-at (point-min))))
      (should bounds)
      (should (= (car bounds) (point-min))))))

(ert-deftest rfcview:test-all-paragraphs-returns-document-order-list ()
  "Returns each paragraph in document order with strictly increasing starts."
  (with-temp-buffer
    (insert "P1 line\n\nP2 line\n\nP3 line\n")
    (let ((paras (rfcview:read--all-paragraphs)))
      (should (= 3 (length paras)))
      (should (< (car (nth 0 paras)) (car (nth 1 paras))))
      (should (< (car (nth 1 paras)) (car (nth 2 paras)))))))

;;; ─── Translation: visible-area first ────────────────────────────────────────

(ert-deftest rfcview:test-paragraphs-visible-first-orders-correctly ()
  "Visible paragraphs come first, then after, then before."
  (let* ((p-before  (cons 1 10))
         (p-vis1    (cons 100 150))
         (p-vis2    (cons 200 250))
         (p-after   (cons 300 400))
         (all (list p-before p-vis1 p-vis2 p-after)))
    (cl-letf (((symbol-function 'get-buffer-window)
               (lambda (&rest _) 'sentinel-win))
              ((symbol-function 'window-start)
               (lambda (&rest _) 90))
              ((symbol-function 'window-end)
               (lambda (&rest _) 260)))
      (should (equal (rfcview:read--paragraphs-visible-first all)
                     (list p-vis1 p-vis2 p-after p-before))))))

;;; ─── Translation: language helpers ──────────────────────────────────────────

(defconst rfcview-test:mock-language-cache
  '(("English" . "en") ("Japanese" . "ja") ("Korean" . "ko"))
  "Stub language-choices list used in tests that bypass network.")

(ert-deftest rfcview:test-fetch-languages-parses-tl-from-google-response ()
  "Fetched languages are built from the `tl' key of Google's JSON response."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (&rest _)
               (rfcview-test:make-http-buffer
                200
                (concat "{\"sl\":{\"auto\":\"Detect language\","
                        "\"ko\":\"Korean\"},"
                        "\"tl\":{\"ko\":\"Korean\","
                        "\"ja\":\"Japanese\","
                        "\"auto\":\"Detect language\"}}")))))
    (let ((langs (rfcview:translate-fetch-languages)))
      (should (member '("Korean" . "ko") langs))
      (should (member '("Japanese" . "ja") langs))
      ;; The `auto' pseudo-language must be filtered out
      (should-not (cl-find-if (lambda (p) (equal (cdr p) "auto")) langs)))))

(ert-deftest rfcview:test-language-choices-fetched-from-google-translate ()
  "Choices come from Google's API, not Emacs's language-info-alist."
  (let ((rfcview:translate--languages-cache nil))
    (cl-letf (((symbol-function 'rfcview:translate-fetch-languages)
               (lambda () '(("Korean" . "ko") ("Japanese" . "ja")))))
      (should (equal (cdr (assoc "Korean"
                                  (rfcview:translate--language-choices)))
                     "ko")))))

(ert-deftest rfcview:test-language-choices-caches-across-calls ()
  "Choices are fetched once and reused for subsequent calls in the session."
  (let ((rfcview:translate--languages-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'rfcview:translate-fetch-languages)
               (lambda () (cl-incf call-count) '(("Korean" . "ko")))))
      (rfcview:translate--language-choices)
      (rfcview:translate--language-choices)
      (should (= call-count 1)))))

(ert-deftest rfcview:test-language-choices-signals-when-fetch-fails ()
  "Signals a user error when the fetch returns nil and there is no cache."
  (let ((rfcview:translate--languages-cache nil))
    (cl-letf (((symbol-function 'rfcview:translate-fetch-languages)
               (lambda () nil)))
      (should-error (rfcview:translate--language-choices)
                    :type 'user-error))))

(ert-deftest rfcview:test-ensure-target-language-skips-prompt-when-set ()
  "Returns the variable's value without prompting when it is non-nil."
  (let ((rfcview:translate-target-language "ko"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Should not prompt"))))
      (should (equal (rfcview:translate--ensure-target-language) "ko")))))

(ert-deftest rfcview:test-ensure-target-language-prompts-when-nil-and-persists ()
  "Prompts, persists via customize-save-variable, and returns the code."
  (let ((rfcview:translate-target-language nil)
        (rfcview:translate--languages-cache rfcview-test:mock-language-cache)
        saved)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Korean"))
              ((symbol-function 'customize-save-variable)
               (lambda (sym val &rest _)
                 (set sym val)
                 (setq saved (cons sym val)))))
      (let ((code (rfcview:translate--ensure-target-language)))
        (should (equal code "ko"))
        (should (equal saved '(rfcview:translate-target-language . "ko")))))))

(ert-deftest rfcview:test-ensure-target-language-offers-environment-as-default ()
  "Passes the Google-API name matching `current-language-environment' as default."
  (let ((rfcview:translate-target-language nil)
        (rfcview:translate--languages-cache rfcview-test:mock-language-cache)
        captured-default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init _hist default)
                 (setq captured-default default)
                 "Korean"))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) nil)))
      (let ((current-language-environment "Korean"))
        (rfcview:translate--ensure-target-language))
      (should (equal captured-default "Korean")))))

;;; ─── Translation: translate-fetch (sync) ────────────────────────────────────

(defun rfcview-test:make-http-buffer (status body)
  "Create a fake post-`rfcview:retrieve' response buffer with STATUS and BODY."
  (let ((buf (generate-new-buffer " *rfcview-test-http*")))
    (with-current-buffer buf
      (set-buffer-multibyte t)
      (insert (format "HTTP/1.1 %d OK\r\nContent-Type: application/json\r\n\r\n%s"
                      status body))
      (make-local-variable 'url-http-response-status)
      (setq url-http-response-status status))
    buf))

(ert-deftest rfcview:test-translate-fetch-url-encodes-query-params ()
  "Built URL includes client=gtx, sl/tl, and a URL-encoded q."
  (let (captured-url)
    (cl-letf (((symbol-function 'rfcview:retrieve)
               (lambda (url &rest _)
                 (setq captured-url url)
                 (rfcview-test:make-http-buffer
                  200 "[[[\"hi\",\"hello\",null,null,1]],null,\"en\"]"))))
      (rfcview:translate-fetch "hello world" "auto" "ko")
      (should captured-url)
      (should (string-match-p "client=gtx" captured-url))
      (should (string-match-p "sl=auto" captured-url))
      (should (string-match-p "tl=ko" captured-url))
      (should (string-match-p "q=hello%20world" captured-url)))))

(ert-deftest rfcview:test-translate-fetch-parses-json-response ()
  "Parses the translation from Google's JSON envelope."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (&rest _)
               (rfcview-test:make-http-buffer
                200 "[[[\"안녕\",\"hi\",null,null,1]],null,\"en\"]"))))
    (should (equal (rfcview:translate-fetch "hi" "auto" "ko") "안녕"))))

(ert-deftest rfcview:test-translate-fetch-multi-chunk-joins-all ()
  "Concatenates every chunk's translation cell from outer[0]."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (&rest _)
               (rfcview-test:make-http-buffer
                200
                "[[[\"안녕\",\"hello\",null,null,1],[\" 세상\",\" world\",null,null,1]],null,\"en\"]"))))
    (should (equal (rfcview:translate-fetch "hello world" "auto" "ko")
                   "안녕 세상"))))

(ert-deftest rfcview:test-translate-fetch-returns-nil-on-http-error ()
  "Returns nil when HTTP status is not 200."
  (cl-letf (((symbol-function 'rfcview:retrieve)
             (lambda (&rest _)
               (rfcview-test:make-http-buffer 404 ""))))
    (should (null (rfcview:translate-fetch "x" "auto" "ko")))))

(ert-deftest rfcview:test-set-translation-language-persists-and-clears-cache ()
  "`l' prompts, persists the new language, and clears cache + overlays."
  (with-temp-buffer
    (insert "Hello world.\n\nGoodbye.\n")
    (let ((rfcview:translate-target-language "ko")
          (rfcview:translate--languages-cache rfcview-test:mock-language-cache)
          saved)
      (rfcview:read--init-translation-state)
      ;; Seed a cached translation + visible overlay
      (puthash (cons (point-min) (1+ (length "Hello world.")))
               "안녕"
               rfcview:read-translation-cache)
      (rfcview:read--show-translation
       (point-min) (1+ (length "Hello world.")) "안녕")
      (should (rfcview:read--any-overlay-shown-p))
      (should (= 1 (hash-table-count rfcview:read-translation-cache)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Japanese"))
                ((symbol-function 'customize-save-variable)
                 (lambda (sym val &rest _)
                   (set sym val)
                   (setq saved (cons sym val)))))
        (rfcview:read-set-translation-language))
      (should (equal rfcview:translate-target-language "ja"))
      (should (equal saved '(rfcview:translate-target-language . "ja")))
      (should (zerop (hash-table-count rfcview:read-translation-cache)))
      (should-not (rfcview:read--any-overlay-shown-p)))))

;;; ─── Translation: t at point ────────────────────────────────────────────────

(ert-deftest rfcview:test-translate-paragraph-creates-overlay-replacing-text ()
  "Creates an overlay with a display property replacing the paragraph."
  (with-temp-buffer
    (insert "Hello world.\nMore text.\n\nNext paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역된 텍스트")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point))
      (should (cl-some (lambda (ov)
                         (and (overlay-get ov 'rfcview:translation)
                              (overlay-get ov 'display)))
                       (overlays-in (point-min) (point-max)))))))

(ert-deftest rfcview:test-translate-paragraph-original-text-still-in-buffer ()
  "`buffer-substring-no-properties' still returns the original text."
  (with-temp-buffer
    (insert "Hello world.\nMore text.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point))
      (should (string-match-p "Hello world"
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))))

(ert-deftest rfcview:test-translate-paragraph-toggle-hides-then-shows-from-cache ()
  "Three presses: fetch + hide + restore-from-cache → exactly one HTTP call."
  (with-temp-buffer
    (insert "Hello world.\n\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) (cl-incf fetch-count) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (rfcview:read-translate-at-point)
        (rfcview:read-translate-at-point)
        (should (= fetch-count 1))))))

(ert-deftest rfcview:test-translate-region-uses-region-bounds-not-paragraph ()
  "When a region is active, the marked range (not the paragraph) is translated."
  (with-temp-buffer
    (insert "Hello world.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "X")))
        (let ((beg 2) (end 6))
          (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                    ((symbol-function 'region-beginning) (lambda () beg))
                    ((symbol-function 'region-end) (lambda () end))
                    ((symbol-function 'deactivate-mark) (lambda (&rest _) nil)))
            (rfcview:read-translate-at-point))
          (should (gethash (cons beg end) rfcview:read-translation-cache)))))))

(ert-deftest rfcview:test-translate-at-point-moves-point-to-paragraph-end ()
  "Point lands at the end of the paragraph (BOL of blank line) after `t'."
  (with-temp-buffer
    (insert "First line.\nSecond line.\nThird line.\n\nNext paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        ;; Point in the middle of the first paragraph
        (goto-char (+ (point-min) 15))
        (let* ((bounds (rfcview:read--paragraph-bounds-at (point)))
               (end (cdr bounds)))
          (rfcview:read-translate-at-point)
          (should (= (point) end)))))))

(ert-deftest rfcview:test-translate-at-point-preserves-window-start ()
  "`window-start' is preserved across `t' so the buffer does not scroll."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (insert (mapconcat #'identity
                         (cl-loop for i from 1 to 60
                                  collect (format "Line %d of content." i))
                         "\n"))
      (insert "\n\nNext paragraph.\n")
      (let ((rfcview:translate-target-language "ko"))
        (rfcview:read--init-translation-state)
        (cl-letf (((symbol-function 'rfcview:translate-fetch)
                   (lambda (&rest _) "번역")))
          (save-window-excursion
            (set-window-buffer (selected-window) buf)
            ;; Scroll so the start of the first paragraph isn't at BOB
            (goto-char (point-min))
            (forward-line 20)
            (set-window-start (selected-window) (point))
            (let ((ws-before (window-start)))
              (forward-line 5)
              (rfcview:read-translate-at-point)
              (should (= (window-start) ws-before)))))))))

(ert-deftest rfcview:test-translate-at-point-on-blank-line-hides-single ()
  "Pressing `t' on a blank line while in SINGLE mode hides the single overlay."
  (with-temp-buffer
    (insert "First line.\n\nSecond paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (should (eq rfcview:read--translation-mode 'single))
        ;; Point was moved to the blank line by the show step
        (should (looking-at-p "^[ \t]*$"))
        (rfcview:read-translate-at-point)
        (should-not rfcview:read--translation-mode)
        (let (vals)
          (maphash (lambda (_k v) (push v vals))
                   rfcview:read-translation-overlays)
          (should (zerop (cl-count-if #'overlayp vals))))))))

(ert-deftest rfcview:test-translate-at-point-on-blank-line-noop-when-idle ()
  "Pressing `t' on a blank line while idle is a no-op (no state change)."
  (with-temp-buffer
    (insert "First line.\n\nSecond.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      ;; Move to the blank line between paragraphs
      (goto-char (point-min))
      (forward-line 1)
      (should (looking-at-p "^[ \t]*$"))
      (rfcview:read-translate-at-point)
      (should-not rfcview:read--translation-mode)
      (should-not rfcview:read--translation-single-key))))

(ert-deftest rfcview:test-translate-toggle-off-keeps-point ()
  "Toggling off (same paragraph) does not relocate point."
  (with-temp-buffer
    (insert "First line.\nSecond line.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point) ; shows; point moved to end
        (let ((pos-after-show (point)))
          (rfcview:read-translate-at-point) ; hides
          ;; Toggle-off leaves point alone — still at the post-show position
          (should (= (point) pos-after-show)))))))

(ert-deftest rfcview:test-t-on-different-paragraph-restores-previous ()
  "Pressing `t' in paragraph B restores paragraph A to English."
  (with-temp-buffer
    (insert "Para A.\n\nPara B.\n\nPara C.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _) (concat "T:" text))))
        ;; Translate A
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (let* ((paras (rfcview:read--all-paragraphs))
               (key-a (nth 0 paras))
               (key-b (nth 1 paras)))
          (should (overlayp (gethash key-a
                                     rfcview:read-translation-overlays)))
          (should (equal rfcview:read--translation-single-key key-a))
          ;; Move to B and translate
          (goto-char (car key-b))
          (rfcview:read-translate-at-point)
          ;; A's overlay hidden, B's shown
          (should-not (overlayp (gethash key-a
                                         rfcview:read-translation-overlays)))
          (should (overlayp (gethash key-b
                                     rfcview:read-translation-overlays)))
          (should (equal rfcview:read--translation-single-key key-b))
          ;; A's translation kept in cache
          (should (gethash key-a rfcview:read-translation-cache)))))))

(ert-deftest rfcview:test-T-after-t-absorbs-single-and-translates-rest ()
  "Pressing `T' after `t' keeps the single overlay visible and translates the rest.
The single-key tracking is cleared (mode becomes `all')."
  (with-temp-buffer
    (insert "Para A.\n\nPara B.\n\nPara C.\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _) (cl-incf fetch-count) (concat "T:" text)))
                ((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (text _src _tgt cb)
                   (cl-incf fetch-count)
                   (funcall cb (concat "T:" text))
                   nil)))
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (should (eq rfcview:read--translation-mode 'single))
        (rfcview:read-translate-document)
        ;; Mode flipped to 'all, single-key cleared
        (should (eq rfcview:read--translation-mode 'all))
        (should (null rfcview:read--translation-single-key))
        ;; All three paragraphs have visible overlays
        (let ((shown 0))
          (maphash (lambda (_k v) (when (overlayp v) (cl-incf shown)))
                   rfcview:read-translation-overlays)
          (should (= shown 3)))
        ;; Exactly one sync call (t) + two async calls (T fetched B and C)
        (should (= fetch-count 3))))))

(ert-deftest rfcview:test-t-during-T-shown-replaces-overlays-with-single ()
  "Pressing `t' while in ALL_SHOWN exits T-mode and shows only point's overlay."
  (with-temp-buffer
    (insert "Para A.\n\nPara B.\n\nPara C.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _) (concat "T:" text)))
                ((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (text _src _tgt cb)
                   (funcall cb (concat "T:" text))
                   nil)))
        (rfcview:read-translate-document)
        (should (eq rfcview:read--translation-mode 'all))
        ;; Move to middle paragraph and press t
        (let* ((paras (rfcview:read--all-paragraphs))
               (key-b (nth 1 paras)))
          (goto-char (car key-b))
          (rfcview:read-translate-at-point)
          (should (eq rfcview:read--translation-mode 'single))
          (should (equal rfcview:read--translation-single-key key-b))
          ;; Only B's overlay is visible
          (let ((shown 0))
            (maphash (lambda (_k v) (when (overlayp v) (cl-incf shown)))
                     rfcview:read-translation-overlays)
            (should (= shown 1))))))))

(ert-deftest rfcview:test-t-during-running-cancels-job-and-shows-point ()
  "Pressing `t' while a `T' job is in flight cancels the job and translates point."
  (with-temp-buffer
    (insert "Para A.\n\nPara B.\n\nPara C.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _) (concat "T:" text)))
                ((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (_text _src _tgt _cb)
                   (generate-new-buffer " *rfcview-test-url*"))))
        (rfcview:read-translate-document)
        (should (eq (rfcview:read--translation-state) 'running))
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (should-not rfcview:read-translation-job)
        (should (eq rfcview:read--translation-mode 'single))
        (let* ((paras (rfcview:read--all-paragraphs))
               (key-a (nth 0 paras)))
          (should (overlayp (gethash key-a
                                     rfcview:read-translation-overlays))))))))

;;; ─── Translation: T whole document ──────────────────────────────────────────

(ert-deftest rfcview:test-translate-document-translates-every-paragraph ()
  "Issues one async fetch per paragraph; populates the cache for all."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n\nPara three.\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (text _src _tgt cb)
                   (cl-incf fetch-count)
                   (funcall cb (concat "T:" (substring text 0 3)))
                   nil)))
        (rfcview:read-translate-document)
        (should (= fetch-count 3))
        (should (= 3 (hash-table-count
                      rfcview:read-translation-cache)))))))

(ert-deftest rfcview:test-translate-document-second-press-after-complete-hides-all ()
  "After completion, T hides every overlay but preserves the cache."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (_text _src _tgt cb)
                   (funcall cb "번역")
                   nil)))
        (rfcview:read-translate-document))
      (should (rfcview:read--any-overlay-shown-p))
      (rfcview:read-translate-document)
      (should-not (rfcview:read--any-overlay-shown-p))
      (should (= 2 (hash-table-count
                    rfcview:read-translation-cache))))))

(ert-deftest rfcview:test-translate-document-third-press-shows-from-cache-no-http ()
  "Show → hide → show: only the first show calls the fetcher."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (_text _src _tgt cb)
                   (cl-incf fetch-count)
                   (funcall cb "번역")
                   nil)))
        (rfcview:read-translate-document)
        (rfcview:read-translate-document)
        (rfcview:read-translate-document))
      (should (= fetch-count 2)))))

(ert-deftest rfcview:test-collapse-text-replaces-newlines-and-runs ()
  "Every whitespace run (including newlines) collapses to one space."
  (should (equal (rfcview:read--collapse-text "   foo\n   bar\t\tbaz\n")
                 "foo bar baz"))
  (should (equal (rfcview:read--collapse-text "  ") "")))

(ert-deftest rfcview:test-doc-content-width-matches-widest-line-in-buffer ()
  "Document content width equals the widest line anywhere in the buffer."
  (with-temp-buffer
    (insert "short.\n\nthis is the widest line in the entire document.\n\ntiny.\n")
    (should (= (rfcview:read--doc-content-width)
               (length "this is the widest line in the entire document.")))))

(ert-deftest rfcview:test-wrap-translation-uses-doc-width-floor-for-short-paragraph ()
  "Short paragraphs wrap to at least the document-wide width."
  (with-temp-buffer
    (insert "Short.\n\nthis is the widest line in the entire document body here.\n")
    (let* ((bounds (rfcview:read--paragraph-bounds-at (point-min)))
           (text (make-string 200 ?x))
           (display (rfcview:read--wrap-translation (car bounds) (cdr bounds) text))
           (lines (split-string display "\n")))
      ;; The wrap target was widened by the document floor, so the longest
      ;; rendered line should reach the document's widest column.
      (should (>= (apply #'max (mapcar #'string-width lines))
                  (rfcview:read--doc-content-width))))))

(ert-deftest rfcview:test-paragraph-target-width-matches-widest-line ()
  "Target width equals the column reached by the longest line."
  (with-temp-buffer
    (insert "   short.\n   somewhat longer line here.\n")
    (should (= (rfcview:read--paragraph-target-width (point-min) (point-max))
               (length "   somewhat longer line here.")))))

(ert-deftest rfcview:test-translate-paragraph-collapses-whitespace-before-fetch ()
  "Multi-line paragraphs are flattened before being sent to the translator."
  (with-temp-buffer
    (insert "   First line of paragraph.\n   Second line follows.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko")
          captured)
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _)
                   (setq captured text)
                   "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point))
      (should captured)
      (should-not (string-match-p "\n" captured))
      (should-not (string-match-p "  " captured))
      (should (equal captured "First line of paragraph. Second line follows.")))))

(ert-deftest rfcview:test-translate-paragraph-display-ends-with-newline-when-paragraph-does ()
  "Display string ends in \\n when the paragraph range ends in \\n.
This preserves the blank-line separator to the next paragraph."
  (with-temp-buffer
    (insert "Hello world.\n\nNext paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point))
      (let ((ov (cl-find-if (lambda (o) (overlay-get o 'rfcview:translation))
                            (overlays-in (point-min) (point-max)))))
        (should ov)
        (should (string-suffix-p "\n" (overlay-get ov 'display)))))))

(ert-deftest rfcview:test-translate-region-no-trailing-newline-when-range-doesnt-end-in-nl ()
  "A marked region not ending in \\n produces a display string without \\n."
  (with-temp-buffer
    (insert "Hello world.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역"))
                ((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 2))
                ((symbol-function 'region-end) (lambda () 6))
                ((symbol-function 'deactivate-mark) (lambda (&rest _) nil)))
        (rfcview:read-translate-at-point))
      (let ((ov (cl-find-if (lambda (o) (overlay-get o 'rfcview:translation))
                            (overlays-in (point-min) (point-max)))))
        (should ov)
        (should-not (string-suffix-p "\n" (overlay-get ov 'display)))))))

;;; ─── Translation: region-overlay dispatch ──────────────────────────────────

(defmacro rfcview-test:with-mocked-region (beg end &rest body)
  "Run BODY with an active region from BEG to END (functions mocked)."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'use-region-p) (lambda () t))
             ((symbol-function 'region-beginning) (lambda () ,beg))
             ((symbol-function 'region-end) (lambda () ,end))
             ((symbol-function 'deactivate-mark) (lambda (&rest _) nil)))
     ,@body))

(ert-deftest rfcview:test-region-translate-creates-region-overlay-table ()
  "Region t puts the overlay into `--region-overlays', not the SINGLE table."
  (with-temp-buffer
    (insert "Hello world.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (rfcview-test:with-mocked-region 2 6
          (rfcview:read-translate-at-point))
        (should (overlayp (gethash (cons 2 6) rfcview:read--region-overlays)))
        (should-not (gethash (cons 2 6) rfcview:read-translation-overlays))))))

(ert-deftest rfcview:test-region-translate-does-not-touch-single-state ()
  "Region t leaves SINGLE mode and its overlay alone."
  (with-temp-buffer
    (insert "First paragraph.\nLine two.\n\nSecond paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        ;; Translate the first paragraph as SINGLE
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (should (eq rfcview:read--translation-mode 'single))
        (let ((single-key rfcview:read--translation-single-key))
          ;; Now translate an unrelated region — SINGLE must stay
          (rfcview-test:with-mocked-region 30 36
            (rfcview:read-translate-at-point))
          (should (eq rfcview:read--translation-mode 'single))
          (should (equal rfcview:read--translation-single-key single-key))
          (should (overlayp
                   (gethash single-key rfcview:read-translation-overlays)))
          (should (overlayp
                   (gethash (cons 30 36) rfcview:read--region-overlays))))))))

(ert-deftest rfcview:test-t-anywhere-hides-region-overlay ()
  "Pressing `t' at any position when a region overlay is visible hides it.
SINGLE state is preserved across the hide."
  (with-temp-buffer
    (insert "First paragraph.\nLine two.\n\nSecond paragraph.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        ;; SINGLE on first paragraph + region overlay
        (goto-char (point-min))
        (rfcview:read-translate-at-point)
        (rfcview-test:with-mocked-region 30 36
          (rfcview:read-translate-at-point))
        (should (rfcview:read--any-region-overlay-p))
        ;; Press t without a region, anywhere — region overlay must hide
        (goto-char (point-max))
        (rfcview:read-translate-at-point)
        (should-not (rfcview:read--any-region-overlay-p))
        ;; SINGLE is still alive
        (should (eq rfcview:read--translation-mode 'single))))))

(ert-deftest rfcview:test-region-translate-reuses-cache-on-second-mark ()
  "Re-marking the same range and pressing `t' restores from cache."
  (with-temp-buffer
    (insert "Hello world.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _)
                   (cl-incf fetch-count)
                   "번역")))
        (rfcview-test:with-mocked-region 2 6
          (rfcview:read-translate-at-point))
        ;; Hide it (t anywhere)
        (rfcview:read-translate-at-point)
        (should-not (rfcview:read--any-region-overlay-p))
        ;; Mark again, press t — must be cached
        (rfcview-test:with-mocked-region 2 6
          (rfcview:read-translate-at-point))
        (should (= fetch-count 1))
        (should (rfcview:read--any-region-overlay-p))))))

(ert-deftest rfcview:test-paragraph-chunks-in-range-single-paragraph ()
  "A range entirely within one paragraph yields one chunk."
  (with-temp-buffer
    (insert "Hello world.\nMore text.\n\nNext.\n")
    (let ((chunks (rfcview:read--paragraph-chunks-in-range 2 12)))
      (should (= 1 (length chunks)))
      (should (equal (car chunks) (cons 2 12))))))

(ert-deftest rfcview:test-paragraph-chunks-in-range-spanning-blank-line ()
  "A range crossing a blank line yields two chunks split at the blank."
  (with-temp-buffer
    (insert "First A.\nSecond A.\n\nFirst B.\nSecond B.\n")
    ;; Range covers the whole buffer
    (let ((chunks (rfcview:read--paragraph-chunks-in-range
                   (point-min) (point-max))))
      (should (= 2 (length chunks)))
      ;; First chunk ends before the blank line
      (let ((first (nth 0 chunks))
            (second (nth 1 chunks)))
        (should (< (cdr first) (car second)))
        ;; The blank line BOL sits between the two chunks
        (should (string-match-p
                 "^[ \t]*$"
                 (buffer-substring-no-properties (cdr first)
                                                 (car second))))))))

(ert-deftest rfcview:test-region-translate-multi-paragraph-creates-multiple-overlays ()
  "Region spanning N paragraphs produces N independent overlays."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n\nPara three.\n")
    (let ((rfcview:translate-target-language "ko")
          (fetch-count 0))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (text &rest _) (cl-incf fetch-count) (concat "T:" text))))
        ;; Select the whole buffer as one region
        (rfcview-test:with-mocked-region (point-min) (point-max)
          (rfcview:read-translate-at-point))
        ;; Three paragraphs → three fetches → three overlays
        (should (= fetch-count 3))
        (let ((live 0))
          (maphash (lambda (_k v) (when (overlayp v) (cl-incf live)))
                   rfcview:read--region-overlays)
          (should (= live 3)))))))

(ert-deftest rfcview:test-region-translate-multi-paragraph-preserves-blank-line ()
  "Blank line between paragraphs is not covered by any region overlay."
  (with-temp-buffer
    (insert "Para A.\n\nPara B.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (rfcview-test:with-mocked-region (point-min) (point-max)
          (rfcview:read-translate-at-point))
        ;; Find the blank line position
        (let ((blank-bol
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^[ \t]*$")
                 (line-beginning-position))))
          (let ((covered nil))
            (maphash (lambda (k _v)
                       (when (and (<= (car k) blank-bol)
                                  (> (cdr k) blank-bol))
                         (setq covered t)))
                     rfcview:read--region-overlays)
            (should-not covered)))))))

(ert-deftest rfcview:test-set-language-clears-region-overlays ()
  "`l' wipes region overlays along with paragraph overlays and cache."
  (with-temp-buffer
    (insert "Hello world.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko")
          (rfcview:translate--languages-cache rfcview-test:mock-language-cache))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (rfcview-test:with-mocked-region 2 6
          (rfcview:read-translate-at-point)))
      (should (rfcview:read--any-region-overlay-p))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Japanese"))
                ((symbol-function 'customize-save-variable)
                 (lambda (&rest _) nil)))
        (rfcview:read-set-translation-language))
      (should-not (rfcview:read--any-region-overlay-p))
      (should (zerop (hash-table-count rfcview:read-translation-cache))))))

(ert-deftest rfcview:test-wrap-translation-applies-slack ()
  "Effective wrap target is the measured width plus the slack constant."
  (with-temp-buffer
    (insert "12345678901234567890\n12345678\n")
    (let* ((text (mapconcat #'identity (make-list 30 "abc") " "))
           (out (rfcview:read--wrap-translation
                 (point-min) (point-max) text))
           (lines (split-string out "\n" t)))
      ;; Without slack, lines would cap at the doc width (20).  The slack
      ;; constant lets at least one line extend beyond that.
      (should (cl-some (lambda (l) (> (string-width l) 20)) lines)))))

(ert-deftest rfcview:test-translate-paragraph-display-preserves-indent ()
  "The first character of the overlay's display string matches the original indent."
  (with-temp-buffer
    (insert "   indented paragraph here.\n   on multiple lines.\n\nNext.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      (cl-letf (((symbol-function 'rfcview:translate-fetch)
                 (lambda (&rest _) "번역")))
        (goto-char (point-min))
        (rfcview:read-translate-at-point))
      (let ((ov (cl-find-if (lambda (o) (overlay-get o 'rfcview:translation))
                            (overlays-in (point-min) (point-max)))))
        (should ov)
        (let ((display (overlay-get ov 'display)))
          (should (stringp display))
          (should (string-match-p "\\`   " display)))))))

(ert-deftest rfcview:test-wrap-translation-text-respects-max-width ()
  "Wrapped lines never exceed MAX-WIDTH columns of display width."
  (let* ((text "one two three four five six seven eight nine ten")
         (out (rfcview:read--wrap-translation-text text 2 12)))
    (dolist (line (split-string out "\n"))
      (should (<= (string-width line) 12)))))

(ert-deftest rfcview:test-wrap-translation-text-wraps-cjk-without-spaces ()
  "Japanese/Chinese text (no inter-word spaces) still wraps to MAX-WIDTH.
Each CJK character is its own breakable unit; no space is inserted
between adjacent CJK characters."
  (let* ((jp "日本語のテキストはスペースなしで折り返される必要があります")
         (out-jp (rfcview:read--wrap-translation-text jp 0 10))
         (lines-jp (split-string out-jp "\n")))
    (should (> (length lines-jp) 1))
    (dolist (line lines-jp)
      (should (<= (string-width line) 10)))
    (should-not (string-match-p " " out-jp)))
  (let* ((zh "中文文本没有空格也需要正确换行处理这是测试")
         (out-zh (rfcview:read--wrap-translation-text zh 0 8))
         (lines-zh (split-string out-zh "\n")))
    (should (> (length lines-zh) 1))
    (dolist (line lines-zh)
      (should (<= (string-width line) 8)))
    (should-not (string-match-p " " out-zh))))

(ert-deftest rfcview:test-wrap-translation-text-mixed-cjk-latin ()
  "Mixed CJK+Latin text wraps with spaces between Latin words and CJK
runs; no spaces are introduced between adjacent CJK characters."
  (let* ((text "Hello 世界 this is a テスト of mixed text")
         (out (rfcview:read--wrap-translation-text text 0 20)))
    (dolist (line (split-string out "\n"))
      (should (<= (string-width line) 20)))
    (should (string-match-p "Hello" out))
    ;; CJK runs survive intact when they fit on one line.
    (should (string-match-p "世界" out))
    (should (string-match-p "テスト" out))
    ;; No space inserted between adjacent CJK characters.
    (should-not (string-match-p "世 界" out))
    (should-not (string-match-p "テ ス" out))))

(ert-deftest rfcview:test-translate-document-cancel-during-job-aborts-and-hides ()
  "Pressing T while a job is in flight clears the job and hides overlays."
  (with-temp-buffer
    (insert "Para one.\n\nPara two.\n\nPara three.\n")
    (let ((rfcview:translate-target-language "ko"))
      (rfcview:read--init-translation-state)
      ;; Mock async fetch to never invoke its callback — job stays in flight.
      (cl-letf (((symbol-function 'rfcview:translate-fetch-async)
                 (lambda (_text _src _tgt _cb)
                   (generate-new-buffer " *rfcview-test-url*"))))
        (rfcview:read-translate-document)
        (should rfcview:read-translation-job)
        (rfcview:read-translate-document)
        (should-not rfcview:read-translation-job)
        (should-not (rfcview:read--any-overlay-shown-p))))))

(provide 'test-rfcview-reader)
;;; test-rfcview-reader.el ends here