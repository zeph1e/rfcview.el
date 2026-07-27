;;; rfcview.el --- RFC viewer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Yunsik Jang

;; Author: Yunsik Jang <doomsday@kldp.org>
;; Homepage: http://github.com/zeph1e/rfcview.el
;; Created: 5 Oct 2016
;; Version: 2.0
;; Keywords: docs
;; License: WTFPL (http://sam.zoy.org/wtfpl)
;;
;; This file is not part of GNU Emacs.
;;

;;; Commentary:

;;; Change Log:
;;
;;  2.0 - 28 Jul 2026 - Google Translate overlays, RFC-number & cache fixes
;;
;;   - inline Google Translate overlays in the reader: `t' translates
;;     the paragraph or active region at point, `T' translates the
;;     whole document (async, visible area first), `l' picks the
;;     target language; results are shown as `display' overlays so
;;     the original English text is never modified; CJK (Japanese /
;;     Chinese) wraps correctly without inter-word spaces.
;;   - RFC-number and cache correctness overhaul: the real rfc-index
;;     never zero-pads RFC numbers and has crossed into 5 digits;
;;     fixed entry parsing that silently dropped RFC >= 10000 and
;;     RFC 1-999, cross-reference buttons that truncated 5+ digit
;;     numbers, and a cache-freshness check that never detected a
;;     newer index; on-disk cache schema bumped so any cache broken
;;     by these bugs rebuilds automatically.
;;   - Table of Contents buttonization now covers RFC-1001/1005-era
;;     formatting: blank-line-separated entries, bare `APPENDIX A'
;;     headings, dash-form subsections, dot leaders with no
;;     separating space.
;;   - format/status-driven dispatch: `(Format: ...)' / `(Status: ...)'
;;     read from the rfc-index per entry; format selection is now
;;     index-authoritative instead of a hard-coded txt/pdf pair;
;;     `html' / `xml' open via the browser.
;;   - reader: `j' jumps to a section by number or title; "Sections
;;     X, Y, and Z of RFC NNNN" buttonizes every listed section.
;;   - index UI: entry highlight extends correctly into wrapped /
;;     continuation margins; keyword search scoring rewritten to be
;;     case-insensitive, additive, and multi-field (title, authors,
;;     status, RFC number).
;;   - misc. fixes: section-jump landing when the target buffer is
;;     already open, buttonizing an `RFC NNNN' reference that line-
;;     wraps mid-match, header fontification truncating early when
;;     an author has no organization line, `html'/`xml' fallback
;;     arity bug.
;;
;;  1.1 - 16 May 2026 - TOC buttons, link / history navigation, polished UI
;;
;;   - Table of Contents entries become buttons that jump to the
;;     matching section heading; handles wrapped TOC titles,
;;     appendix subsections, ALL-CAPS TOCs, and titles that contain
;;     `RFC NNNN' fragments.
;;   - reader link navigation: TAB / S-TAB next / prev link
;;     (RFC cross-references and bare URLs via `goto-address-mode'),
;;     `B' / `F' (or C-c C-b / C-c C-f) for history back / forward
;;     across RFCs, `]' / `[' next / prev section.
;;   - help buffer for both modes (`?'), keymap-verified.
;;   - index UI: RFC number in the left margin, date in the right
;;     margin, right fringe moved outside the margin, `s' toggles
;;     sort order, dark / light theme adjustments.
;;   - versioned on-disk cache; stale caches migrate automatically
;;     and keep favorites and recents.
;;   - section heading regexp covers all RFC eras: deeply-nested
;;     numeric, RFC 791-era APPENDIX, wrapped subsection titles,
;;     and top-level titles with commas (RFC 9959 §2 style).
;;   - misc. fixes: modern left-aligned title fontification,
;;     latin-character index parsing, footerless page breaks
;;     (RFC 729), `describe-mode' crash, light-mode face contrast,
;;     font-scale key bindings.
;;
;;  1.0 - 11 May 2026 - reader mode, fontification, four-file split
;;
;;   - new `rfcview-reader.el': open RFCs in a dedicated read-mode
;;     buffer with header / title / section-heading fontification,
;;     hidden page-break blocks, leading-blank trim, and a local
;;     cache under `~/.emacs.d/.RFC/'.
;;   - support .txt and .pdf via `rfcview:preferred-format'; the
;;     other format is tried as a fallback when one is unavailable.
;;   - `RFC NNNN' / `[RFCNNNN]' cross-references become clickable
;;     buttons that open that RFC.
;;   - `o' opens the raw cached text file in a read-only buffer.
;;   - highlight the current entry in the RFC index.
;;   - split the package into `rfcview-core', `rfcview-index',
;;     `rfcview-reader', and the `rfcview.el' entry point.
;;   - ERT unit test suite and a `run-tests.sh' runner.
;;   - misc. compatibility fixes for recent Emacs versions.
;;
;;  0.2 - 12 Oct 2016 - family link & filters
;;
;;   - add links of family RFC, which succeed or precede the RFC,
;;     into the bottom of RFC entries in index.
;;   - supports filters: recent, favorite, keywords search,
;;   - misc. fixes & refactorings on listing & line-wrapping.
;;
;;; Code:

(require 'rfcview-index)
(require 'rfcview-reader)

;;;###autoload
(defun rfcview ()
  "Shows RFC index"
  (interactive)
  (let ((buffer (get-buffer "*RFC INDEX*")))
    (unless buffer
      (setq buffer (set-buffer (get-buffer-create "*RFC INDEX*")))
      (buffer-disable-undo)
      (rfcview:initialize)
      (rfcview:index-mode))
    (select-window (display-buffer buffer))
    (rfcview:refresh-index)))

(provide 'rfcview)
;;; rfcview.el ends here
