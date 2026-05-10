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
  "Matches modern 'Appendix A.  Title\\n' heading."
  (should (rfcview-test:matches-heading "\nAppendix A.  Sample Appendix\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-all-caps ()
  "Matches RFC-791-era 'APPENDIX A:  Title\\n' heading."
  (should (rfcview-test:matches-heading "\nAPPENDIX A:  Protocol Specification\n")))

(ert-deftest rfcview:test-section-heading-regexp-appendix-subsection ()
  "Matches 'A.1.  Title\\n' appendix subsection heading."
  (should (rfcview-test:matches-heading "\nA.1.  First Appendix Section\n")))

(ert-deftest rfcview:test-section-heading-regexp-all-caps-bare-word ()
  "Matches all-caps bare-word headings like 'INTRODUCTION\\n'."
  (should (rfcview-test:matches-heading "\nINTRODUCTION\n")))

(ert-deftest rfcview:test-section-heading-regexp-all-caps-multi-word ()
  "Matches multi-word all-caps heading 'GENERAL CONSIDERATIONS\\n'."
  (should (rfcview-test:matches-heading "\nGENERAL CONSIDERATIONS\n")))

(ert-deftest rfcview:test-section-heading-regexp-acknowledgements ()
  "Matches 'Acknowledgements\\n' heading."
  (should (rfcview-test:matches-heading "\nAcknowledgements\n")))

(ert-deftest rfcview:test-section-heading-regexp-acknowledgment-variant ()
  "Matches 'Acknowledgment\\n' (US spelling without 'e')."
  (should (rfcview-test:matches-heading "\nAcknowledgment\n")))

(ert-deftest rfcview:test-section-heading-regexp-authors-addresses ()
  "Matches 'Authors\\' Addresses\\n' heading."
  (should (rfcview-test:matches-heading "\nAuthors' Addresses\n")))

(ert-deftest rfcview:test-section-heading-regexp-abstract ()
  "Matches 'Abstract\\n' heading."
  (should (rfcview-test:matches-heading "\nAbstract\n")))

(ert-deftest rfcview:test-section-heading-regexp-dash-underline ()
  "Matches dash-underline style heading."
  (should (rfcview-test:matches-heading "\nIntroduction\n------------\n")))

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

;;; ─── rfcview:section-heading-search ─────────────────────────────────────────

(ert-deftest rfcview:test-section-heading-search-finds-first-heading ()
  "section-heading-search finds the first heading in the buffer."
  (with-temp-buffer
    (insert "\nSome prose.\n\n1.  Introduction\n\nDetails here.\n")
    (goto-char (point-min))
    (should (rfcview:section-heading-search nil))))

(ert-deftest rfcview:test-section-heading-search-advances-point ()
  "After a successful match, point is advanced past the heading."
  (with-temp-buffer
    (insert "\n1.  Introduction\n\nContent.\n")
    (goto-char (point-min))
    (rfcview:section-heading-search nil)
    (should (> (point) 1))))

(ert-deftest rfcview:test-section-heading-search-returns-nil-when-none ()
  "Returns nil when the buffer has no section headings."
  (with-temp-buffer
    (insert "No headings here at all.\nJust prose.\n")
    (goto-char (point-min))
    (should-not (rfcview:section-heading-search nil))))

(ert-deftest rfcview:test-section-heading-search-finds-adjacent-headings ()
  "Can find both of two adjacent headings (backs up one newline after double-\\n match)."
  (with-temp-buffer
    (insert "\n8.  References\n\n8.1.  Normative References\n\nSome ref.\n")
    (goto-char (point-min))
    (let ((first  (rfcview:section-heading-search nil))
          (second (rfcview:section-heading-search nil)))
      (should first)
      (should second))))

(ert-deftest rfcview:test-section-heading-search-respects-bound ()
  "Stops at BOUND and returns nil if no match is found before it."
  (with-temp-buffer
    (insert "\n1.  Section One\n\nContent after bound.\n\n2.  Section Two\n\n")
    (goto-char (point-min))
    (let* ((bound (save-excursion
                    (search-forward "Content after bound")
                    (point)))
           (first  (rfcview:section-heading-search bound))
           (second (rfcview:section-heading-search bound)))
      (should first)
      (should-not second))))

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

(ert-deftest rfcview:test-read-fontify-no-title-when-non-indented ()
  "Non-indented text after the header gap does not get rfcview:read-rfc-title-face."
  (with-temp-buffer
    (insert (concat "Author: J. Doe\n"
                    "\n"
                    "Non-indented line\n"
                    "\n"
                    "1.  Body\n"
                    "\n"
                    "Content.\n"))
    (rfcview:read-fontify)
    (should-not (eq 'rfcview:read-rfc-title-face
                    (rfcview-test:face-at-string "Non-indented line")))))

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
  "Opens the source file in text-mode in a new buffer."
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
                  ;; The source file should now be visiting in a buffer
                  (let ((src-buf (find-buffer-visiting tmp)))
                    (should src-buf)
                    (with-current-buffer src-buf
                      (should (eq major-mode 'text-mode)))
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

(provide 'test-rfcview-reader)
;;; test-rfcview-reader.el ends here