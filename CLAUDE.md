# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`rfcview.el` is a two-file Emacs package (Elisp) for browsing, downloading, and reading IETF RFC documents inside Emacs. There are no build steps, no test suite, and no external dependencies beyond Emacs's built-in `url` library.

## Installation / Loading

Manual load (no package manager needed — all four `.el` files must be on the load path):
```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/rfcview")
(autoload 'rfcview "rfcview" t t)
```

Via `el-get`: copy `rfcview.rcp` into a directory on `el-get-recipe-path`, then `(el-get-bundle rfcview)`.

Entry point: `M-x rfcview` opens `*RFC INDEX*`.

## Architecture

The package is split across four files with a strict one-way dependency chain:

```
rfcview-core.el  ←  rfcview-index.el  ←┐
      ↑                                  rfcview.el (entry point)
      └──────────  rfcview-reader.el  ←┘
```

`rfcview-index.el` and `rfcview-reader.el` each `require` only `rfcview-core`. They call each other's functions at runtime (`rfcview:index-goto-number` from reader, `rfcview:read-rfc` from index), but those calls are resolved dynamically — by the time any interactive command runs, `rfcview.el` has already loaded both files.

### rfcview-core.el — shared data, faces, and network

All `defcustom` declarations (group `rfcview`), all `defface` definitions, and the shared `rfcview:rfc-link-button` button type live here. Core also holds the cache variable `rfcview:rfc-cache` (a plist with `:last-modified`, `:table` hash-table keyed by RFC number, `:favorite`, and `:recent`), network functions (`rfcview:retrieve`, `rfcview:retrieve-rfc`, `rfcview:retrieve-index`), cache persistence (`rfcview:load-cache` / `rfcview:save-cache`), and the text-wrapping utility `rfcview:wrap-text-at-word-boundary`.

### rfcview-index.el — index mode

**Parsing** — `rfcview:parse-index-entry` / `rfcview:parse-index-buffer` parse the raw IETF rfc-index text into the hash-table stored in `rfcview:rfc-cache`. `rfcview:update-index` orchestrates update: it sends a HEAD request via `rfcview:index-updated-p` and only fetches the full index when `Last-Modified` has advanced.

**Index mode** (`*RFC INDEX*` buffer, `rfcview:index-mode`) — a read-only buffer rendered by `rfcview:refresh-index`. Each entry is built by `rfcview:make-entry-line` and inserted by `rfcview:insert-with-text-properties`, which also creates clickable buttons for RFC cross-references within traits. Entry navigation uses `rfcview:number` text properties and `paragraph` boundaries. A filter line at the top provides buttons for All / Favorites / Recents / keyword search. The current entry is highlighted with `rfcview:background-highlight-overlay`.

Index layout uses Emacs's `word-wrap` mode with `wrap-prefix` text properties (no literal newlines in entry strings). The date on each entry is right-aligned using a `display (space :align-to ...)` property so layout never depends on window width — `rfcview:refresh-index` is only called when content changes, not on window resize.

**Filters** — `rfcview:index-filter` holds a function symbol (or `nil` for all). The three built-in filter functions (`rfcview:index-filter-function-favorite`, `rfcview:index-filter-function-recent`, `rfcview:index-filter-function-keywords`) return ordered lists of RFC numbers. Keyword search scores titles by regex matches and sorts by score descending, RFC number ascending.

### rfcview-reader.el — RFC document read mode

**Read mode** (`*RFC XXXX*` buffer, `rfcview:read-mode`) — RFC text files are downloaded once and cached locally under `rfcview:local-directory` (`~/.emacs.d/.RFC/`). On load, three post-processing steps run:

- `rfcview:read-hide-page-breaks` — overlays page footer/formfeed/header blocks as invisible so pages flow continuously.
- `rfcview:read-buttonize-refs` — turns every `RFC XXXX` occurrence into a clickable button that jumps the index to that entry (calls `rfcview:index-goto-number` at runtime).
- Font-lock highlights numbered section headings (`rfcview:rfc-section-face`).

Navigation keys: vi-style line/char movement, `/`/`?` for isearch, `]`/`[` for next/previous section heading, `q` to bury.

### rfcview.el — entry point

Requires `rfcview-index` and `rfcview-reader`, then defines the single `;;;###autoload` entry point `rfcview`, which creates the `*RFC INDEX*` buffer, calls `rfcview:initialize` and `rfcview:index-mode`, and displays it.

## Conventions

- All symbols are prefixed `rfcview:` (colon separator, not hyphen).
- User-facing customizations are `defcustom` in the `rfcview` group.
- Faces are `defface` with support for dark/light backgrounds and degraded color displays.
- `rfcview:use-debug` / `rfcview:debug` gate any debug output.
