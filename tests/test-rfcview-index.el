;;; tests/test-rfcview-index.el --- ERT tests for rfcview-index.el

(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name buffer-file-name))))
(require 'rfcview-index)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun rfcview-test:make-cache (&optional table favorite recent)
  "Return a minimal rfcview:rfc-cache plist."
  (list :last-modified '(0 0)
        :table (or table (make-hash-table :test 'equal))
        :favorite favorite
        :recent recent))

(defun rfcview-test:make-table (&rest entries)
  "Build a hash-table from (NUMBER . PLIST) pairs in ENTRIES."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e entries tbl)
      (puthash (car e) (cdr e) tbl))))

(defun rfcview-test:build-index-buffer (rfc-alist)
  "Return a temp buffer with a minimal RFC index representation.
RFC-ALIST is a list of (NUMBER . DATA-PLIST) pairs."
  (let ((buf (generate-new-buffer " *rfcview-test-index*"))
        (rfcview:rfc-cache (rfcview-test:make-cache
                            (apply #'rfcview-test:make-table rfc-alist)))
        (rfcview:index-filter nil)
        (rfcview:index-current-list-items nil)
        (rfcview:use-face nil)
        (rfcview:use-debug nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "\n" 'rfcview:number 0))
        (dolist (e rfc-alist)
          (let ((number (car e))
                (data   (cdr e)))
            (rfcview:insert-with-text-properties
             (rfcview:make-entry-line number
                                      (plist-get data :title)
                                      (plist-get data :date)
                                      (plist-get data :authors)
                                      (plist-get data :obsoletes)
                                      (plist-get data :obsoleted-by)
                                      (plist-get data :updates)
                                      (plist-get data :updated-by)
                                      nil)
             number)
            (insert "\n")
            (push number rfcview:index-current-list-items)))
        (goto-char (point-min))))
    buf))

;;; ─── rfcview:parse-index-entry ──────────────────────────────────────────────

(defconst rfcview-test:entry-rfc1
  "0001 Host Software. S. Crocker. April 1969. (Format: TXT=19529 bytes) (Status: UNKNOWN)\n\n")

(defconst rfcview-test:entry-rfc10
  (concat "0010 Documentation Conventions. S.D. Crocker. July 1969. "
          "(Format: TXT=9529 bytes) (Obsoletes RFC0003) "
          "(Updated by RFC0024, RFC0027, RFC0030) (Status: UNKNOWN)\n\n"))

(defconst rfcview-test:entry-rfc793
  (concat "0793 Transmission Control Protocol. J. Postel. September 1981. "
          "(Format: TXT=172710 bytes) (Status: INTERNET STANDARD)\n\n"))

(defconst rfcview-test:entry-with-editor
  (concat "2026 The IMAP Mailbox Attribute List. B. Leiba, Ed. March 1996. "
          "(Format: TXT=10000 bytes) (Status: PROPOSED STANDARD)\n\n"))

(ert-deftest rfcview:test-parse-index-entry-number ()
  "Parses RFC number from index entry."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc1)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (= 1 (plist-get entry :number))))))

(ert-deftest rfcview:test-parse-index-entry-title ()
  "Parses RFC title from index entry."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc1)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (string-match-p "Host Software" (plist-get entry :title))))))

(ert-deftest rfcview:test-parse-index-entry-authors ()
  "Parses authors from index entry."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc1)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (member "S. Crocker" (plist-get entry :authors))))))

(ert-deftest rfcview:test-parse-index-entry-date ()
  "Parses date from index entry."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc1)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (string-match-p "April 1969" (plist-get entry :date))))))

(ert-deftest rfcview:test-parse-index-entry-obsoletes ()
  "Parses the Obsoletes trait from an index entry."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc10)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (member "RFC0003" (plist-get entry :obsoletes))))))

(ert-deftest rfcview:test-parse-index-entry-updated-by-multiple ()
  "Parses multiple RFC numbers from Updated-by trait."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc10)
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (let ((updated-by (plist-get entry :updated-by)))
        (should (member "RFC0024" updated-by))
        (should (member "RFC0027" updated-by))
        (should (member "RFC0030" updated-by))))))

(ert-deftest rfcview:test-parse-index-entry-obsoleted-by ()
  "Parses :obsoleted-by field correctly."
  (with-temp-buffer
    (insert (concat "0003 Documentation. S.D. Crocker. April 1969. "
                    "(Obsoleted by RFC0010) (Status: UNKNOWN)\n\n"))
    (goto-char (point-min))
    (let ((entry (rfcview:parse-index-entry (current-buffer))))
      (should (member "RFC0010" (plist-get entry :obsoleted-by))))))

(ert-deftest rfcview:test-parse-index-entry-returns-nil-at-end-of-buffer ()
  "Returns nil when there are no more entries in the buffer."
  (with-temp-buffer
    (insert "No RFC entries here.\n\n")
    (goto-char (point-min))
    (should (null (rfcview:parse-index-entry (current-buffer))))))

(ert-deftest rfcview:test-parse-index-entry-advances-point ()
  "Advances point past the parsed entry so sequential calls work."
  (with-temp-buffer
    (insert rfcview-test:entry-rfc1)
    (insert rfcview-test:entry-rfc793)
    (goto-char (point-min))
    (rfcview:parse-index-entry (current-buffer))
    (let ((entry2 (rfcview:parse-index-entry (current-buffer))))
      (should (= 793 (plist-get entry2 :number))))))

;;; ─── rfcview:parse-index-buffer ─────────────────────────────────────────────

(defconst rfcview-test:small-index-text
  (concat "HTTP/1.1 200 OK\r\nLast-Modified: Tue, 01 Jan 2019 12:00:00 GMT\r\n\r\n"
          "\n\n"
          rfcview-test:entry-rfc1
          rfcview-test:entry-rfc793))

(ert-deftest rfcview:test-parse-index-buffer-returns-hash-table ()
  "Returns a plist with a :table that is a hash-table."
  (with-temp-buffer
    (insert rfcview-test:small-index-text)
    (goto-char (point-min))
    (let ((result (rfcview:parse-index-buffer (current-buffer))))
      (should (hash-table-p (plist-get result :table))))))

(ert-deftest rfcview:test-parse-index-buffer-populates-entries ()
  "Populates the hash-table with all entries found."
  (with-temp-buffer
    (insert rfcview-test:small-index-text)
    (goto-char (point-min))
    (let* ((result (rfcview:parse-index-buffer (current-buffer)))
           (tbl    (plist-get result :table)))
      (should (gethash 1 tbl))
      (should (gethash 793 tbl)))))

(ert-deftest rfcview:test-parse-index-buffer-entry-fields ()
  "Each entry in the parsed table has the expected field values."
  (with-temp-buffer
    (insert rfcview-test:small-index-text)
    (goto-char (point-min))
    (let* ((result (rfcview:parse-index-buffer (current-buffer)))
           (rfc793 (gethash 793 (plist-get result :table))))
      (should rfc793)
      (should (= 793 (plist-get rfc793 :number)))
      (should (string-match-p "Transmission Control Protocol"
                               (plist-get rfc793 :title))))))

;;; ─── rfcview:get-filter-name ─────────────────────────────────────────────────

(ert-deftest rfcview:test-get-filter-name-all ()
  "Returns \"[All]\" for the nil (no-filter) case."
  (should (string= "[All]" (rfcview:get-filter-name nil))))

(ert-deftest rfcview:test-get-filter-name-favorite ()
  "Returns \"[Favorites]\" for the favorite filter."
  (should (string= "[Favorites]"
                   (rfcview:get-filter-name 'rfcview:index-filter-function-favorite))))

(ert-deftest rfcview:test-get-filter-name-recent ()
  "Returns \"[Recents]\" for the recent filter."
  (should (string= "[Recents]"
                   (rfcview:get-filter-name 'rfcview:index-filter-function-recent))))

(ert-deftest rfcview:test-get-filter-name-keywords ()
  "Returns the current keyword wrapped in brackets."
  (let ((rfcview:filter-keyword-current-keyword "tcp"))
    (let ((name (rfcview:get-filter-name 'rfcview:index-filter-function-keywords)))
      (should (string-match-p "tcp" name)))))

;;; ─── rfcview:maphash-with-filter ─────────────────────────────────────────────

(ert-deftest rfcview:test-maphash-with-filter-no-filter-visits-all ()
  "Without a filter, visits every entry in the table."
  (let ((tbl (rfcview-test:make-table
              '(1 . (:title "T1")) '(2 . (:title "T2")) '(3 . (:title "T3"))))
        (rfcview:index-current-list-items nil)
        collected)
    (rfcview:maphash-with-filter
     (lambda (k _v) (push k collected))
     tbl)
    (should (= 3 (length collected)))
    (should (member 1 collected))
    (should (member 2 collected))
    (should (member 3 collected))))

(ert-deftest rfcview:test-maphash-with-filter-no-filter-populates-list-items ()
  "Without a filter, rfcview:index-current-list-items is set to all keys."
  (let ((tbl (rfcview-test:make-table '(10 . (:title "T10")) '(20 . (:title "T20"))))
        (rfcview:index-current-list-items nil))
    (rfcview:maphash-with-filter (lambda (_k _v)) tbl)
    (should (member 10 rfcview:index-current-list-items))
    (should (member 20 rfcview:index-current-list-items))))

(ert-deftest rfcview:test-maphash-with-filter-with-filter-restricts-keys ()
  "With a filter function, only keys returned by the filter are visited."
  (let ((tbl (rfcview-test:make-table
              '(1 . (:title "T1")) '(2 . (:title "T2")) '(3 . (:title "T3"))))
        (rfcview:index-current-list-items nil)
        collected)
    (rfcview:maphash-with-filter
     (lambda (k _v) (push k collected))
     tbl
     (lambda () '(1 3)))
    (should (= 2 (length collected)))
    (should (member 1 collected))
    (should (member 3 collected))
    (should-not (member 2 collected))))

(ert-deftest rfcview:test-maphash-with-filter-skips-missing-keys ()
  "Keys returned by the filter but absent from the table are silently skipped."
  (let ((tbl (rfcview-test:make-table '(1 . (:title "T1"))))
        (rfcview:index-current-list-items nil)
        collected)
    (rfcview:maphash-with-filter
     (lambda (k _v) (push k collected))
     tbl
     (lambda () '(1 999)))
    (should (equal '(1) collected))))

;;; ─── rfcview:index-filter-function-favorite ──────────────────────────────────

(ert-deftest rfcview:test-index-filter-function-favorite-returns-list ()
  "Returns the :favorite list from rfcview:rfc-cache."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache nil '(3 5 7) nil)))
    (should (equal '(3 5 7) (rfcview:index-filter-function-favorite)))))

(ert-deftest rfcview:test-index-filter-function-favorite-nil-when-empty ()
  "Returns nil when the favorite list is empty."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache nil nil nil)))
    (should (null (rfcview:index-filter-function-favorite)))))

;;; ─── rfcview:index-filter-function-recent ────────────────────────────────────

(ert-deftest rfcview:test-index-filter-function-recent-returns-list ()
  "Returns the :recent list from rfcview:rfc-cache."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache nil nil '(42 793 2616))))
    (should (equal '(42 793 2616) (rfcview:index-filter-function-recent)))))

(ert-deftest rfcview:test-index-filter-function-recent-nil-when-empty ()
  "Returns nil when the recent list is empty."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache nil nil nil)))
    (should (null (rfcview:index-filter-function-recent)))))

;;; ─── rfcview:index-filter-function-keywords ─────────────────────────────────

(defmacro rfcview-test:with-keyword-filter (keyword &rest body)
  "Run BODY with rfcview:rfc-cache set up for keyword filtering."
  (declare (indent 1))
  `(let* ((rfcview:filter-keyword-current-keyword ,keyword)
          (rfcview:filter-keyword-current-result nil)
          (rfcview:filter-keywords-history nil))
     ,@body))

(ert-deftest rfcview:test-keywords-filter-includes-matching-title ()
  "An RFC whose title contains the keyword is included in results."
  (let* ((tbl (rfcview-test:make-table
               '(793 . (:title "Transmission Control Protocol"))
               '(768 . (:title "User Datagram Protocol"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl)))
    (rfcview-test:with-keyword-filter "Transmission"
      (should (member 793 (rfcview:index-filter-function-keywords))))))

(ert-deftest rfcview:test-keywords-filter-excludes-non-matching-title ()
  "An RFC whose title does not contain the keyword is excluded."
  (let* ((tbl (rfcview-test:make-table
               '(793 . (:title "Transmission Control Protocol"))
               '(768 . (:title "User Datagram Protocol"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl)))
    (rfcview-test:with-keyword-filter "Transmission"
      (should-not (member 768 (rfcview:index-filter-function-keywords))))))

(ert-deftest rfcview:test-keywords-filter-returns-nil-for-no-match ()
  "Returns nil when no RFC title matches the keyword."
  (let* ((tbl (rfcview-test:make-table
               '(793 . (:title "Transmission Control Protocol"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl)))
    (rfcview-test:with-keyword-filter "XYZZY_NONEXISTENT"
      (should (null (rfcview:index-filter-function-keywords))))))

(ert-deftest rfcview:test-keywords-filter-high-score-entry-comes-first ()
  "The entry with a higher match score appears before a lower-scoring one."
  (let* ((tbl (rfcview-test:make-table
               '(793 . (:title "Transmission Control Protocol"))
               '(768 . (:title "User Protocol"))
               '(791 . (:title "Internet Protocol"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl)))
    (rfcview-test:with-keyword-filter "Transmission"
      (let ((result (rfcview:index-filter-function-keywords)))
        (should (= (car result) 793))))))

(ert-deftest rfcview:test-keywords-filter-uses-history-cache ()
  "Returns cached results when keyword matches an entry in history."
  (let* ((tbl (rfcview-test:make-table '(793 . (:title "TCP"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl))
         (rfcview:filter-keyword-current-keyword "tcp")
         (rfcview:filter-keyword-current-result nil)
         (rfcview:filter-keywords-history
          (list (cons "tcp" '((793 . 80) (100 . 40))))))
    (let ((result (rfcview:index-filter-function-keywords)))
      (should (member 793 result))
      (should (member 100 result)))))

(ert-deftest rfcview:test-keywords-filter-multi-word-matches-both ()
  "A two-word keyword matches titles that contain both words."
  (let* ((tbl (rfcview-test:make-table
               '(793  . (:title "Transmission Control Protocol"))
               '(2616 . (:title "Hypertext Transfer Protocol"))
               '(768  . (:title "User Datagram Protocol"))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl)))
    (rfcview-test:with-keyword-filter "Transmission Control"
      (let ((result (rfcview:index-filter-function-keywords)))
        (should (member 793 result))
        (should-not (member 768 result))))))

;;; ─── rfcview:make-entry-line ─────────────────────────────────────────────────

(ert-deftest rfcview:test-make-entry-line-contains-rfc-number ()
  "Entry line contains the zero-padded RFC number."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981" '("J. Postel")
                                         nil nil nil nil nil)))
      (should (string-match-p "0793" line)))))

(ert-deftest rfcview:test-make-entry-line-contains-title ()
  "Entry line contains the RFC title."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793
                                         "Transmission Control Protocol"
                                         "September 1981"
                                         '("J. Postel")
                                         nil nil nil nil nil)))
      (should (string-match-p "Transmission Control Protocol" line)))))

(ert-deftest rfcview:test-make-entry-line-contains-authors ()
  "Entry line contains the author names."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981"
                                         '("J. Postel" "R. Braden")
                                         nil nil nil nil nil)))
      (should (string-match-p "J. Postel" line))
      (should (string-match-p "R. Braden" line)))))

(ert-deftest rfcview:test-make-entry-line-contains-date ()
  "Entry line contains the publication date."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793 "TCP" "September 1981"
                                         '("J. Postel")
                                         nil nil nil nil nil)))
      (should (string-match-p "September 1981" line)))))

(ert-deftest rfcview:test-make-entry-line-marks-favorite ()
  "Entry line includes the favorite symbol when favorite is non-nil."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil)
        (rfcview:favorite-symbol ?*))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981" '("J. Postel")
                                         nil nil nil nil t)))
      (should (string-match-p "\\*" line)))))

(ert-deftest rfcview:test-make-entry-line-no-favorite-marker-when-not-set ()
  "Entry line uses a space (not the favorite symbol) when not favorite."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil)
        (rfcview:favorite-symbol ?*))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981" '("J. Postel")
                                         nil nil nil nil nil)))
      (should (string-match-p "0793 " line)))))

(ert-deftest rfcview:test-make-entry-line-shows-obsoletes-trait ()
  "Entry line includes 'Obsoletes' when the obsoletes list is non-nil."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 10 "Doc Conventions" "July 1969"
                                         '("S.D. Crocker")
                                         '("RFC0003") nil nil nil nil)))
      (should (string-match-p "Obsoletes" line))
      (should (string-match-p "RFC0003" line)))))

(ert-deftest rfcview:test-make-entry-line-shows-updated-by-trait ()
  "Entry line includes 'Updated by' when updated-by list is non-nil."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981" '("J. Postel")
                                         nil nil nil '("RFC2581" "RFC3168") nil)))
      (should (string-match-p "Updated by" line)))))

(ert-deftest rfcview:test-make-entry-line-no-traits-when-all-nil ()
  "Entry line has no trait section when all trait lists are nil."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((line (rfcview:make-entry-line 793 "TCP" "1981" '("J. Postel")
                                         nil nil nil nil nil)))
      (should-not (string-match-p "Obsoletes" line))
      (should-not (string-match-p "Updated by" line)))))

;;; ─── rfcview:insert-with-text-properties ─────────────────────────────────────

(ert-deftest rfcview:test-insert-with-text-properties-sets-number-property ()
  "The inserted region has the rfcview:number text property set to NUMBER."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache))
        (rfcview:use-face nil))
    (with-temp-buffer
      (rfcview:insert-with-text-properties "0793 TCP\n" 793)
      (let ((num (get-text-property 1 'rfcview:number)))
        (should (= 793 num))))))

(ert-deftest rfcview:test-insert-with-text-properties-creates-buttons-for-refs ()
  "RFC reference strings like RFC0003 within the text become clickable buttons."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache))
        (rfcview:use-face nil))
    (with-temp-buffer
      (rfcview:insert-with-text-properties
       "See RFC0003 and RFC0768 for details.\n"
       1)
      (goto-char (point-min))
      (search-forward "RFC0003")
      (let ((btn (button-at (match-beginning 0))))
        (should btn)
        (should (= 3 (button-get btn 'number)))))))

;;; ─── rfcview:index-toggle-favorite ──────────────────────────────────────────

(ert-deftest rfcview:test-index-toggle-favorite-adds-to-list ()
  "Toggling an RFC not in favorites adds it to the :favorite list."
  (let* ((rfcview:rfc-cache (rfcview-test:make-cache nil nil nil))
         (buf (rfcview-test:build-index-buffer
               `((793 . (:title "TCP" :date "1981" :authors ("J. Postel")))))))
    (unwind-protect
        (with-current-buffer buf
          (setq rfcview:rfc-cache (rfcview-test:make-cache
                                   (rfcview-test:make-table
                                    '(793 . (:title "TCP" :date "1981" :authors ("J. Postel"))))
                                   nil nil))
          (goto-char (point-min))
          (search-forward "0793")
          (rfcview:index-toggle-favorite)
          (should (member 793 (plist-get rfcview:rfc-cache :favorite))))
      (kill-buffer buf))))

(ert-deftest rfcview:test-index-toggle-favorite-removes-from-list ()
  "Toggling an RFC that is already a favorite removes it."
  (let* ((tbl (rfcview-test:make-table
               '(793 . (:title "TCP" :date "1981" :authors ("J. Postel")))))
         (rfcview:rfc-cache (rfcview-test:make-cache tbl '(793) nil))
         (buf (rfcview-test:build-index-buffer
               `((793 . (:title "TCP" :date "1981" :authors ("J. Postel")))))))
    (unwind-protect
        (with-current-buffer buf
          (setq rfcview:rfc-cache
                (rfcview-test:make-cache tbl '(793) nil))
          (goto-char (point-min))
          (search-forward "0793")
          (rfcview:index-toggle-favorite)
          (should-not (member 793 (plist-get rfcview:rfc-cache :favorite))))
      (kill-buffer buf))))

;;; ─── rfcview:index-apply-filter-* ───────────────────────────────────────────

(defmacro rfcview-test:with-index-buf (entries &rest body)
  "Evaluate BODY in a test index buffer containing ENTRIES.
Mocks rfcview:refresh-index so no window operations occur."
  (declare (indent 1))
  `(let* ((rfcview:index-filter nil)
          (rfcview:index-current-list-items nil)
          (rfcview:suppress-recover-position t)
          (rfcview:filter-keyword-current-keyword nil)
          (rfcview:filter-keyword-current-result nil)
          (rfcview:filter-keywords-history nil)
          (refresh-calls 0))
     (cl-letf (((symbol-function 'rfcview:index-refresh-screen)
                (lambda () (setq refresh-calls (1+ refresh-calls)))))
       ,@body)))

(ert-deftest rfcview:test-index-apply-filter-all-clears-filter ()
  "rfcview:index-apply-filter-all sets rfcview:index-filter to nil."
  (rfcview-test:with-index-buf ()
    (setq rfcview:index-filter 'rfcview:index-filter-function-favorite)
    (rfcview:index-apply-filter-all)
    (should (null rfcview:index-filter))))

(ert-deftest rfcview:test-index-apply-filter-favorite-sets-filter ()
  "rfcview:index-apply-filter-favorite sets the favorite filter."
  (rfcview-test:with-index-buf ()
    (rfcview:index-apply-filter-favorite)
    (should (eq rfcview:index-filter 'rfcview:index-filter-function-favorite))))

(ert-deftest rfcview:test-index-apply-filter-recent-sets-filter ()
  "rfcview:index-apply-filter-recent sets the recent filter."
  (rfcview-test:with-index-buf ()
    (rfcview:index-apply-filter-recent)
    (should (eq rfcview:index-filter 'rfcview:index-filter-function-recent))))

(ert-deftest rfcview:test-index-apply-filter-keywords-sets-filter-and-keyword ()
  "rfcview:index-apply-filter-keywords sets both the filter and keyword."
  (rfcview-test:with-index-buf ()
    (rfcview:index-apply-filter-keywords "tcp")
    (should (eq rfcview:index-filter 'rfcview:index-filter-function-keywords))
    (should (string= "tcp" rfcview:filter-keyword-current-keyword))))

(ert-deftest rfcview:test-index-apply-filter-keywords-saves-history ()
  "Applying a new keyword filter saves the previous one in history."
  (rfcview-test:with-index-buf ()
    (setq rfcview:filter-keyword-current-keyword "old-keyword"
          rfcview:filter-keyword-current-result '((1 . 80)))
    (rfcview:index-apply-filter-keywords "new-keyword")
    (should (assoc "old-keyword" rfcview:filter-keywords-history))))

(ert-deftest rfcview:test-index-apply-filter-keywords-truncates-history ()
  "History is trimmed to rfcview:keyword-max-history entries."
  (let ((rfcview:keyword-max-history 2))
    (rfcview-test:with-index-buf ()
      (setq rfcview:filter-keywords-history
            '(("k1" . nil) ("k2" . nil) ("k3" . nil)))
      (setq rfcview:filter-keyword-current-keyword "k-prev"
            rfcview:filter-keyword-current-result nil)
      (rfcview:index-apply-filter-keywords "k-new")
      (should (<= (length rfcview:filter-keywords-history)
                  rfcview:keyword-max-history)))))

;;; ─── Navigation (forward / backward / goto) ─────────────────────────────────

(ert-deftest rfcview:test-index-forward-item-moves-to-next-entry ()
  "rfcview:index-forward-item moves point to the next RFC entry."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil)
        (rfcview:rfc-cache
         (rfcview-test:make-cache
          (rfcview-test:make-table
           '(1   . (:title "RFC 1" :date "1969" :authors ("A")))
           '(793 . (:title "TCP"   :date "1981" :authors ("B")))))))
    (let ((buf (rfcview-test:build-index-buffer
                '((1   . (:title "RFC 1" :date "1969" :authors ("A")))
                  (793 . (:title "TCP"   :date "1981" :authors ("B")))))))
      (unwind-protect
          (with-current-buffer buf
            (setq rfcview:rfc-cache
                  (rfcview-test:make-cache
                   (rfcview-test:make-table
                    '(1   . (:title "RFC 1" :date "1969" :authors ("A")))
                    '(793 . (:title "TCP"   :date "1981" :authors ("B"))))))
            (goto-char (point-min))
            (let ((start (point)))
              (rfcview:index-forward-item)
              (should (> (point) start))))
        (kill-buffer buf)))))

(ert-deftest rfcview:test-index-goto-number-moves-to-correct-entry ()
  "rfcview:index-goto-number moves point to the line of the specified RFC."
  (let ((rfcview:use-face nil)
        (rfcview:use-debug nil)
        (rfcview:index-filter nil))
    (let ((buf (rfcview-test:build-index-buffer
                '((1   . (:title "RFC 1" :date "1969" :authors ("A")))
                  (793 . (:title "TCP"   :date "1981" :authors ("B")))))))
      (unwind-protect
          (with-current-buffer buf
            (setq rfcview:index-current-list-items '(1 793))
            (goto-char (point-min))
            (rfcview:index-goto-number 793)
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (should (string-match-p "0793" line))))
        (kill-buffer buf)))))

(ert-deftest rfcview:test-index-goto-number-errors-for-missing-rfc ()
  "rfcview:index-goto-number signals an error when NUMBER is not in the index."
  (let ((rfcview:index-current-list-items '(1 2 3)))
    (with-temp-buffer
      (should-error (rfcview:index-goto-number 9999)))))

;;; ─── rfcview:initialize ──────────────────────────────────────────────────────

(ert-deftest rfcview:test-initialize-resets-cache-when-from-scratch ()
  "rfcview:initialize resets rfcview:rfc-cache when from-scratch is non-nil."
  (let ((rfcview:rfc-cache '(:some :old :data)))
    (cl-letf (((symbol-function 'rfcview:update-index) (lambda () nil)))
      (rfcview:initialize t)
      (should (equal rfcview:rfc-cache rfcview:rfc-cache-default)))))

(ert-deftest rfcview:test-initialize-loads-cache-when-nil ()
  "rfcview:initialize calls rfcview:load-cache when rfcview:rfc-cache is nil."
  (let ((rfcview:rfc-cache nil)
        load-called)
    (cl-letf (((symbol-function 'rfcview:load-cache)
               (lambda () (setq load-called t)))
              ((symbol-function 'rfcview:update-index) (lambda () nil)))
      (rfcview:initialize)
      (should load-called))))

(ert-deftest rfcview:test-initialize-skips-load-when-cache-populated ()
  "rfcview:initialize does not call rfcview:load-cache when cache is already set."
  (let ((rfcview:rfc-cache rfcview:rfc-cache-default)
        load-called)
    (cl-letf (((symbol-function 'rfcview:load-cache)
               (lambda () (setq load-called t)))
              ((symbol-function 'rfcview:update-index) (lambda () nil)))
      (rfcview:initialize)
      (should-not load-called))))

;;; ─── rfcview:refresh-header-line ─────────────────────────────────────────────

(ert-deftest rfcview:test-refresh-header-line-sets-header-line-format ()
  "rfcview:refresh-header-line sets header-line-format in the buffer."
  (let ((rfcview:rfc-cache (rfcview-test:make-cache)))
    (with-temp-buffer
      (rfcview:refresh-header-line)
      (should (not (null header-line-format))))))

(provide 'test-rfcview-index)
;;; test-rfcview-index.el ends here
