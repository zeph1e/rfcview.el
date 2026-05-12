# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`rfcview.el` is a four-file Emacs package (Elisp) for browsing, downloading, and reading IETF RFC documents inside Emacs. There are no build steps and no external dependencies beyond Emacs's built-in `url` library. An ERT-based test suite lives under `tests/`.

## Development

Byte-compile all files to catch errors and warnings:
```
emacs -Q --batch -L . -f batch-byte-compile rfcview-core.el rfcview-index.el rfcview-reader.el rfcview.el
```

Load interactively for testing (all four files must be on the load path):
```emacs-lisp
(add-to-list 'load-path "/path/to/rfcview.el")
(require 'rfcview)
```

Entry point: `M-x rfcview` opens `*RFC INDEX*`. Toggle `rfcview:use-debug` to `t` to enable debug messages via `rfcview:debug`.

## Architecture

The package is split across four files with a strict one-way dependency chain:

```
rfcview-core.el  ←  rfcview-index.el  ←┐
      ↑                                  rfcview.el (entry point)
      └──────────  rfcview-reader.el  ←┘
```

`rfcview-index.el` and `rfcview-reader.el` each `require` only `rfcview-core`. They call each other's functions at runtime (`rfcview:index-goto-number` from reader, `rfcview:read-rfc` from index), but those calls are resolved dynamically — by the time any interactive command runs, `rfcview.el` has already loaded both files.

### rfcview-core.el — shared data, faces, and network

All `defcustom` declarations (group `rfcview`) and most `defface` definitions live here, along with the shared `rfcview:rfc-link-button` button type. Core also holds the cache variable `rfcview:rfc-cache` (a plist with `:last-modified`, `:table` hash-table keyed by RFC number, `:favorite`, and `:recent`), network functions (`rfcview:retrieve`, `rfcview:retrieve-rfc`, `rfcview:retrieve-index`), cache persistence (`rfcview:load-cache` / `rfcview:save-cache`), and the text-wrapping utility `rfcview:wrap-text-at-word-boundary`.

Note: `rfcview:read-rfc-header-face`, `rfcview:read-rfc-title-face`, and `rfcview:read-rfc-section-face` are defined in `rfcview-reader.el`, not here — they are reader-specific and not needed in the index.

`rfcview:preferred-format` (`'txt` or `'pdf`) controls which format is tried first when opening an RFC. Both the local cache lookup and the download fallback respect this order.

`rfcview:wrap-text-at-word-boundary` is used only for the filter line in the index (which has mixed propertized strings). Entry text is laid out using Emacs's `word-wrap` mode with `wrap-prefix` text properties instead.

### rfcview-index.el — index mode

**Parsing** — `rfcview:parse-index-entry` / `rfcview:parse-index-buffer` parse the raw IETF rfc-index text into the hash-table stored in `rfcview:rfc-cache`. `rfcview:update-index` orchestrates update: it sends a HEAD request via `rfcview:index-updated-p` and only fetches the full index when `Last-Modified` has advanced.

**Index mode** (`*RFC INDEX*` buffer, `rfcview:index-mode`) — a read-only buffer rendered by `rfcview:refresh-index`. Each entry is built by `rfcview:make-entry-line` and inserted by `rfcview:insert-with-text-properties`, which also creates clickable buttons for RFC cross-references within traits. Entry navigation uses `rfcview:number` text properties and `paragraph` boundaries. A filter line at the top provides buttons for All / Favorites / Recents / keyword search. The current entry is highlighted with `rfcview:background-highlight-overlay`. `?` opens `rfcview:index-show-help`.

Index layout uses Emacs's `word-wrap` mode with `wrap-prefix` text properties (no literal newlines in entry strings). Each entry line begins with a non-breaking-space carrier character (U+00A0, `\xa0`) whose `display` property places the RFC number string in the left margin: `'((margin left-margin) NUM-STR)`. The left margin width (`left-margin-width`) is computed in `rfcview:refresh-index` as `(1+ (length (number-to-string max-rfc-number)))` — one wider than the digit count of the largest RFC number in the cache — and applied immediately with `(set-window-margins nil left-margin-width right-margin-width)`. The date on each entry is placed in the right margin (`right-margin-width` = 15) via a second carrier character on the author line, right-aligned with `format "%15s"`. `rfcview:refresh-index` calls `(set-window-fringes nil nil nil t)` (OUTSIDE-MARGINS = t) so the right fringe is placed after the right margin rather than between the text area and the margin, eliminating the visible separator while keeping the outer fringe intact.

`rfcview:index-goto-number` locates an entry by scanning text properties with `next-single-property-change` rather than regexp search, since RFC numbers are embedded in display properties (not literal text).

Cache is saved to disk via `rfcview:index-cleanup`, which is registered on both `kill-buffer-hook` (buffer-local) and `kill-emacs-hook` so favorites and recents are persisted. Two overlays track the selected entry: `rfcview:background-highlight-overlay` spans the full entry text (face `rfcview:entry-highlight-face` with `:extend t`), and `rfcview:margin-highlight-overlays` is a list of overlays — one pair per visual line — that extend the highlight into both margins. Right-margin overlays are always zero-width with an `after-string`. Left-margin overlays differ by line: the first visual line of each entry creates a non-zero-width overlay spanning the carrier character whose `display` property replaces the carrier's own margin display with the RFC number rendered in both `rfcview:rfc-number-face` and `rfcview:entry-highlight-face` — using `before-string` here would concatenate with the carrier's display and push the number beyond the margin width; subsequent (wrapped) visual lines use a zero-width overlay with a `before-string` of highlighted spaces since they have no carrier character. Both are buffer-local variables initialized to `nil` in `rfcview:index-mode` and deleted in `rfcview:index-cleanup`.

**Filters** — `rfcview:index-filter` holds a function symbol (or `nil` for all). The three built-in filter functions (`rfcview:index-filter-function-favorite`, `rfcview:index-filter-function-recent`, `rfcview:index-filter-function-keywords`) return ordered lists of RFC numbers. Keyword search scores titles by regex matches and sorts by score descending, RFC number ascending.

### rfcview-reader.el — RFC document read mode

**Read mode** (`*RFC XXXX*` buffer, `rfcview:read-mode`) — RFC files are downloaded once and cached locally under `rfcview:local-directory` (`~/.emacs.d/.RFC/`). Format selection (txt vs pdf) follows `rfcview:preferred-format`; the other format is tried as a fallback. `rfcview:download-rfc` handles a single format and returns `nil` on 404; `rfcview:read-rfc` drives the preference-ordered loop via the `rfcview:open-rfc-functions` alist (`txt → rfcview:open-rfc-txt`, `pdf → rfcview:open-rfc-pdf`).

On load of a text RFC, four post-processing steps run:

- `rfcview:read-fontify` — applies faces via `put-text-property`: `rfcview:read-rfc-header-face` to the header block (network info, category, date), `rfcview:read-rfc-title-face` to the centered title, and `rfcview:read-rfc-section-face` to section headings: numeric (`1.2.`), alphabetic appendix (`A.1.`), Roman numeral (`II.`), ALL-CAPS, and keyword headings.
- `rfcview:read-hide-page-breaks` — hides the full page-break block around each form feed: blank bottom-margin lines, the `[Page N]` footer, the form feed, the running page header, and blank top-margin lines.  When the first visible line after the break is a section heading, a blank line is prepended via a `before-string` overlay for visual separation.
- `rfcview:read-buttonize-refs` — turns every `RFC XXXX` and `[RFCXXXX]` occurrence into a clickable button that opens that RFC document.
- `rfcview:read-trim-leading-blanks` — hides blank lines at the very start of the buffer.

Navigation keys: vi-style line/char movement (`h`/`j`/`k`/`l`), `]`/`[` for next/previous section heading, `RET` to activate a button, `+`/`-`/`=`/`0` for `text-scale-adjust`, `o` to view the raw cached file, `?` opens `rfcview:read-show-help`. `q` runs `rfcview:read-quit`, which buries the buffer and selects the `*RFC INDEX*` window if it is visible in the current frame.

### rfcview.el — entry point

Requires `rfcview-index` and `rfcview-reader`, then defines the single `;;;###autoload` entry point `rfcview`, which creates the `*RFC INDEX*` buffer, calls `rfcview:initialize` and `rfcview:index-mode`, and displays it.

## Conventions

- All symbols are prefixed `rfcview:` (colon separator, not hyphen).
- User-facing customizations are `defcustom` in the `rfcview` group.
- Faces are `defface` with support for dark/light backgrounds and degraded color displays.
- `rfcview:use-debug` / `rfcview:debug` gate any debug output.
- **Known naming inconsistency**: the index mode hook is `rfcview:index-mode-hook` (colon) but the read mode hook is `rfcview-read-mode-hook` (hyphen). Do not "fix" this without checking existing user configs.

## Known issues

See `TODO.md` for tracked items. Notable open items: TOC entry section jumping not implemented.

### Section heading regexp notes

`rfcview:section-heading-regexp` requires `^\n` (blank line before) and a trailing `\n\n` (blank line after) for the first alternative group.  The trailing blank line is consumed as part of the match — this is intentional to avoid false positives on multi-line list items that begin with a number (`3.  A HOST has to be prepared...`).  Emacs's regexp engine does not support lookahead assertions (`\(?=...\)`), so the blank-line constraint must be expressed by consumption.  Callers that need the heading line itself should use `(nth 1 (split-string (match-string 0) "\n"))` or check `(match-beginning 1)` (group 1 captures the section-number prefix).
