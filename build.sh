#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SJASMPLUS_BIN="${SJASMPLUS_BIN:-sjasmplus}"

cd "${ROOT_DIR}"
"${SJASMPLUS_BIN}" --lst=ted.lst --lstlab textedit.asm
