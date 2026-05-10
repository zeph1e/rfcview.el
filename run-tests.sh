#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

exec emacs -Q --batch -L . -l tests/run-tests.el