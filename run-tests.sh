#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

emacs -Q --batch -L . -f batch-byte-compile \
  rfcview-core.el rfcview-index.el rfcview-reader.el rfcview.el

exec emacs -Q --batch -L . -l tests/run-tests.el