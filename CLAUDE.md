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

### Testing

Run the full suite (byte-compile + all ERT tests) the same way CI does:
```
bash run-tests.sh
```

Run the tests only, without re-byte-compiling:
```
emacs -Q --batch -L . -l tests/run-tests.el
```

Run a single test (or a regex-matched subset):
```
emacs -Q --batch -L . -L tests \
  -l tests/test-rfcview-reader.el \
  --eval "(ert-run-tests-batch-and-exit \"rfcview:test-parse-index-entry-title\")"
```
Tests are named `rfcview:test-…`. `tests/run-tests.el` uses `ert-run-tests-batch` (not `…-and-exit`) and computes its own exit code — Emacs 29.x has a bug where `ert-run-tests-batch-and-exit` returns 0 even on failure (the condition uses `or` instead of `and`). Preserve this if touching the runner; CI relies on a reliable exit code to file issues / PR comments.

### CI

`.github/workflows/test.yml` runs `run-tests.sh` on push and PR. On failure it: parses `FAILED` lines from the output, posts (or updates, by `<!-- rfcview-ci-failure -->` marker) a PR comment for PRs, and opens a labeled `bug` issue for pushes to `master`/`main`.

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

`rfcview:preferred-format` (one of `'txt`, `'pdf`, `'html`, `'xml`) is the user's preferred format for opening an RFC. The actual try-order is computed by `rfcview:read--format-order` from the cached `:format` list of each entry — only formats the rfc-index advertises are tried, and if `preferred-format` is not listed it is dropped (index is authoritative). `txt`/`pdf` are downloaded and opened in Emacs; `html`/`xml` are handed to `browse-url` and are not cached locally.

`rfcview:wrap-text-at-word-boundary` is used only for the filter line in the index (which has mixed propertized strings). Entry text is laid out using Emacs's `word-wrap` mode with `wrap-prefix` text properties instead.

### rfcview-index.el — index mode

**Parsing** — `rfcview:parse-index-entry` / `rfcview:parse-index-buffer` parse the raw IETF rfc-index text into the hash-table stored in `rfcview:rfc-cache`. `rfcview:update-index` orchestrates update: it sends a HEAD request via `rfcview:index-updated-p` and only fetches the full index when `Last-Modified` has advanced.

**Index mode** (`*RFC INDEX*` buffer, `rfcview:index-mode`) — a read-only buffer rendered by `rfcview:refresh-index`. Each entry is built by `rfcview:make-entry-line` and inserted by `rfcview:insert-with-text-properties`, which also creates clickable buttons for RFC cross-references within traits. Entry navigation uses `rfcview:number` text properties and `paragraph` boundaries. A filter line at the top provides buttons for All / Favorites / Recents / keyword search. The current entry is highlighted with `rfcview:background-highlight-overlay`. `?` opens `rfcview:index-show-help`.

Index layout uses Emacs's `word-wrap` mode with `wrap-prefix` text properties (no literal newlines in entry strings). Each entry line begins with a non-breaking-space carrier character (U+00A0, `\xa0`) whose `display` property places the RFC number string in the left margin: `'((margin left-margin) NUM-STR)`. The left margin width (`left-margin-width`) is computed in `rfcview:refresh-index` as `(1+ (length (number-to-string max-rfc-number)))` — one wider than the digit count of the largest RFC number in the cache — and applied immediately with `(set-window-margins nil left-margin-width right-margin-width)`. The date on each entry is placed in the right margin (`right-margin-width` = 15) via a second carrier character on the author line, right-aligned with `format "%15s"`. `rfcview:refresh-index` calls `(set-window-fringes nil nil nil t)` (OUTSIDE-MARGINS = t) so the right fringe is placed after the right margin rather than between the text area and the margin, eliminating the visible separator while keeping the outer fringe intact.

`rfcview:index-goto-number` locates an entry by scanning text properties with `next-single-property-change` rather than regexp search, since RFC numbers are embedded in display properties (not literal text).

Cache is saved to disk via `rfcview:index-cleanup`, which is registered on both `kill-buffer-hook` (buffer-local) and `kill-emacs-hook` so favorites and recents are persisted. Two overlays track the selected entry: `rfcview:background-highlight-overlay` spans the full entry text (face `rfcview:entry-highlight-face` with `:extend t`), and `rfcview:margin-highlight-overlays` is a list of overlays — one pair per visual line — that extend the highlight into both margins. Right-margin overlays are always zero-width with an `after-string`. Left-margin overlays differ by line: the first visual line of each entry creates a non-zero-width overlay spanning the carrier character whose `display` property replaces the carrier's own margin display with the RFC number rendered in both `rfcview:rfc-number-face` and `rfcview:entry-highlight-face` — using `before-string` here would concatenate with the carrier's display and push the number beyond the margin width; subsequent (wrapped) visual lines use a zero-width overlay with a `before-string` of highlighted spaces since they have no carrier character. Both are buffer-local variables initialized to `nil` in `rfcview:index-mode` and deleted in `rfcview:index-cleanup`.

**Filters** — `rfcview:index-filter` holds a function symbol (or `nil` for all). The three built-in filter functions (`rfcview:index-filter-function-favorite`, `rfcview:index-filter-function-recent`, `rfcview:index-filter-function-keywords`) return ordered lists of RFC numbers. Keyword search scores titles by regex matches and sorts by score descending, RFC number ascending.

### rfcview-reader.el — RFC document read mode

**Read mode** (`*RFC XXXX*` buffer, `rfcview:read-mode`) — `txt`/`pdf` RFCs are downloaded once and cached locally under `rfcview:local-directory` (`~/.emacs.d/.RFC/`); `html`/`xml` are handed to `browse-url` and not cached. Format selection is driven by `rfcview:read--format-order`, which returns the intersection of `rfcview:supported-formats` and the entry's cached `:format` list (preferred first when listed; preferred dropped when not listed; nil when nothing supported is listed). `rfcview:download-rfc` handles a single locally-cached format and returns `nil` on 404. `rfcview:read-rfc` dispatches each candidate via the `rfcview:open-rfc-functions` alist (`txt → rfcview:open-rfc-txt`, `pdf → rfcview:open-rfc-pdf`); a missing/nil entry falls through to `rfcview:open-rfc-fallback`, which calls `browse-url` and ends the search.

On load of a text RFC, five post-processing steps run (in call order):

- `rfcview:read-fontify` — applies faces via `put-text-property`: `rfcview:read-rfc-header-face` to the header block (network info, category, date), `rfcview:read-rfc-title-face` to the centered title, and `rfcview:read-rfc-section-face` to section headings: numeric (`1.2.`), alphabetic appendix (`A.1.`), Roman numeral (`II.`), ALL-CAPS, and keyword headings. Header detection walks line-by-line and consults the rfc-index cache: a blank line ends the header *unless* the next non-blank line matches one of the cached `:authors` names — RFC 9893 has an author without an organization on the LHS, which leaves an all-whitespace line inside the header, and the naive "stop at first blank" heuristic would otherwise truncate it. Matching uses a single `\|`-joined regexp built by `rfcview:read--authors-regexp` / `rfcview:read--author-regexp`, where each cached given-name token (`J.`) becomes `J[[:alpha:]]*\.?` so it matches expansions in the document (`John`, `Jane`, accented forms like `Álvaro`) and the surname is `regexp-quote`d. A trailing `, Ed.` in the cached name is preserved. When the cache lookup fails (entry missing or `rfcview:read-rfc-number` unset), the regexp is `nil` and behavior collapses to the original "stop at first blank".
- `rfcview:read-trim-leading-blanks` — hides blank lines at the very start of the buffer.
- `rfcview:read-hide-page-breaks` — hides the full page-break block around each form feed: blank bottom-margin lines, the footer (`[Page N]` or the older `- N -` dash format), the form feed, the running page header, and blank top-margin lines.  When the first visible line after the break is a section heading, a blank line is prepended via a `before-string` overlay for visual separation.  The footer is detected at `footer-bol`; if the form feed is at the start of a line (`bolp`), `footer-bol` steps back one line, otherwise it uses `beginning-of-line`.
- `rfcview:read-buttonize-toc` — locates the `Table of Contents` heading and wraps each TOC entry in a `rfcview:section-link-button` whose action jumps to that section. Anchors are built piggyback on `rfcview:read-fontify`'s heading scan: two buffer-local hash tables (`rfcview:read-section-anchors-by-number` keyed by `"3.1"`, `"A"`, etc.; `rfcview:read-section-anchors-by-title` keyed by lowercased title) map keys to markers at heading line starts. TOC line parsing tries, in order: numeric (`1.2.`), appendix subsection (`A.1`, arbitrarily nested — `A.4.1`, `A.4.1.1`, …), `Appendix X.`, then unnumbered title. Multi-line TOC entries (RFC 9227-style, where the title wraps to one or more indented continuation lines) are joined by `rfcview:read--absorb-toc-continuations`, which walks forward while lines are non-blank, indented to or past the title's column, and don't begin with a section number, `[A-Z].[0-9]` (appendix-subsection sibling), or `Appendix` — the button range is extended to cover all absorbed lines, and the outer loop advances past them. Lines that find no anchor are left as plain text. The dot leader and trailing page number are dimmed via `rfcview:read--dim-toc-tail` (face `rfcview:read-toc-leader-face`, inherits from `shadow`); for wrapped entries where the leader sits on a continuation line (RFC 5246 F.1.1.3), the function walks each absorbed line and dims the leader there too. Crucially, `--absorb-toc-continuations` clips its returned `te` at the leader-start on each absorbed line so the section-link button (an overlay, whose face beats text-property faces) does not span the leader and override the dim.
- `rfcview:read-buttonize-refs` — turns every `RFC XXXX` and `[RFCXXXX]` occurrence into a clickable button that opens that RFC document. Skips any match whose position is already covered by a button — this is how the TOC-precedence rule is enforced when a TOC title contains an `RFC NNNN` fragment (e.g. RFC 9720's "Changes to RFC 7990"). Runs *after* `rfcview:read-buttonize-toc` for this reason.

After these steps, `goto-address-mode` is enabled to make bare URLs in the document clickable.

Navigation keys: `n`/`p` for next/previous line, `]`/`[` for next/previous section heading, `j` to jump to a section by number or title (see below), `TAB`/`S-TAB` for next/previous button (RFC cross-reference), `RET` to activate a button, `B`/`F` (or `C-c C-b`/`C-c C-f`) for history back/forward (see below), `+`/`-`/`=`/`0` for `text-scale-adjust`, `o` to view the raw cached file, `?` opens `rfcview:read-show-help`. `q` runs `rfcview:read-quit`, which buries the buffer and selects the `*RFC INDEX*` window if it is visible in the current frame.

**Section jump (`j`)** — `rfcview:read-jump-to-section` reads a section string from the minibuffer (or accepts one as an argument when called from elisp, e.g. `rfcview:open-rfc-txt` passes the `section` from `rfcview:read-rfc`) and looks it up against the same anchor tables `rfcview:read-buttonize-toc` consumes: number key first (`rfcview:read-section-anchors-by-number`, after `rfcview:read--normalize-number`), title key as a fallback (`rfcview:read-section-anchors-by-title`, after `rfcview:read--normalize-title`). On a hit it calls `rfcview:nav-push` and `goto-char`+`recenter` (so `B` returns to where the user jumped from); on a miss it just `message`s. Nil/blank input is a silent no-op so the `rfcview:open-rfc-txt` call path is harmless when no section is requested. The optional `INHIBIT-NAV-PUSH` second arg suppresses the push — `rfcview:open-rfc-txt` passes `t` because the calling button action (e.g. the "section XX of [RFC NNNN]" link from `rfcview:read-buttonize-refs`) has already pushed the origin; without the inhibit, the new buffer's position 1 would stack on top of the origin, requiring two `B` presses to return.

**Link navigation (`TAB`/`S-TAB`)** — `rfcview:read-forward-link` and `rfcview:read-backward-link` use Emacs's built-in `next-button`/`previous-button` for buttons and a short `next-overlay-change` walk for `goto-address-mode` URL overlays (helpers `rfcview:read--next-goto-address` / `rfcview:read--prev-goto-address`). `rfcview:read--find-link` returns whichever of the two candidates is closer in the requested direction. Cost per press is proportional to the distance to the nearest link, not the buffer size — no buffer-wide sweep, no sort, no dedupe. The user-facing functions accept a prefix arg `N` (negative goes the other way); `rfcview:read-backward-link` simply calls forward with negated `N`.

**History navigation (`B`/`F`, or `C-c C-b`/`C-c C-f`)** — `rfcview:nav-history` (in `rfcview-core.el`) is a single cons cell `(BACK . FORWARD)` holding two stacks of `(RFC-NUMBER . POSITION)` records. Both `rfc-link-button` and `section-link-button` actions call `rfcview:nav-push` *before* jumping, which pushes the current location onto `BACK`, deduplicates against the previous entry, caps at `rfcview:nav-history-max` (100 by default), and clears `FORWARD`. `rfcview:read-history-back` pops `BACK`, pushes the current location onto `FORWARD`, then calls `rfcview:nav--restore` to replay the target — which dispatches on target-buffer status: (1) target is already the current buffer → `goto-char` only, (2) buffer exists and is visible in another window → `select-window` it, (3) buffer exists but no window shows it → `switch-to-buffer` in the current window, (4) buffer was killed → re-render via `rfcview:read-rfc` from local cache. `rfcview:read-history-forward` is the mirror. `rfcview:index-read-item` calls `rfcview:nav-history-clear` before opening an RFC from the index, so each new index-driven open starts a fresh history. Position storage is plain integers (RFCs are read-only, so markers buy nothing and would die when buffers are killed). Recenter after restoration is guarded by `(eq (window-buffer) (current-buffer))` because batch-mode tests run with the current buffer not displayed in the selected window.

### rfcview.el — entry point

Requires `rfcview-index` and `rfcview-reader`, then defines the single `;;;###autoload` entry point `rfcview`, which creates the `*RFC INDEX*` buffer, calls `rfcview:initialize` and `rfcview:index-mode`, and displays it.

## Conventions

- All symbols are prefixed `rfcview:` (colon separator, not hyphen).
- User-facing customizations are `defcustom` in the `rfcview` group.
- Faces are `defface` with support for dark/light backgrounds and degraded color displays.
- `rfcview:use-debug` / `rfcview:debug` gate any debug output.
- **Known naming inconsistency**: the index mode hook is `rfcview:index-mode-hook` (colon) but the read mode hook is `rfcview-read-mode-hook` (hyphen). Do not "fix" this without checking existing user configs.

## Known issues

See `TODO.md` for tracked items.

### Section heading regexp notes

`rfcview:section-heading-regexp` requires `^\n` (blank line before) for all alternatives.  The trailing blank-line constraint (`\n\n`) is applied universally — all alternatives consume a trailing blank line as part of the match.  This is intentional: consuming the blank line avoids false positives on multi-line list items that begin with a number (`3.  A HOST has to be prepared...`) or on single-word lines that are not headings.  Emacs's regexp engine does not support lookahead assertions (`\(?=...\)`), so the blank-line constraint must be expressed by consumption.  Callers that need the heading line itself should use `(nth 1 (split-string (match-string 0) "\n"))` or check `(match-beginning 1)` (group 1 captures the section-number prefix).

Three alternatives loosen the in-group-1 numeric rule outside group 1: (a) single-segment titles with commas (e.g. RFC 9959 §2 — "2.  Language, Notation, and Terms"), (b) `X.Y`+ subsection titles with commas (e.g. RFC 8698 §6.2 — "6.2.  Method for Delay, Loss, and Marking Ratio Estimation"), and (c) wrapped `X.Y`+ subsection titles (e.g. RFC 8968 §2.6 — "2.6.  Long title that wraps\n      onto a second line\n\n"). All three require the last title char to be non-period, which rejects sentence-shape list items like "3.  Foo, bar." and "X.Y  Sentence, with, commas.". Since they sit outside group 1, `rfcview:read--register-anchor` keys them by scanning the first line of `heading-line` against `\\`\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\.?[ \t]+\\(.*\\)` — the `*` (vs `+`) is what admits single-segment numbers like "2".

The appendix-subsection alts (both the in-group-1 single-line form `[A-Z]\(?:\.[0-9]{1,2}\)+\.?` and the wrapped form outside the group) support arbitrary nesting depth (`A.1`, `A.4.1`, `A.4.1.1`, …, per RFC 5246 F.1.1.x). Per segment the digit count is capped at 1-2 to avoid X.509-style false hits in body prose.
