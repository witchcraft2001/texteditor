#!/usr/bin/env bash
set -euo pipefail

if ! command -v mcopy >/dev/null 2>&1 || ! command -v mmd >/dev/null 2>&1; then
  echo "Error: mtools is required (mcopy and mmd were not found)." >&2
  exit 1
fi

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
IMAGE_TEMPLATE="$ROOT_DIR/image/dss_image.img"
IMAGE_PATH="${1:-$ROOT_DIR/build/editor.img}"

if [ ! -f "$IMAGE_TEMPLATE" ]; then
  echo "Error: missing template image: $IMAGE_TEMPLATE" >&2
  exit 1
fi

"$ROOT_DIR/build.sh"

for file in ted.exe ted.txt; do
  if [ ! -f "$ROOT_DIR/$file" ]; then
    echo "Error: missing file: $ROOT_DIR/$file" >&2
    exit 1
  fi
done

mkdir -p "$(dirname "$IMAGE_PATH")"
cp "$IMAGE_TEMPLATE" "$IMAGE_PATH"

mmd -i "$IMAGE_PATH" ::/EDITOR 2>/dev/null || true
mcopy -i "$IMAGE_PATH" -o "$ROOT_DIR/ted.exe" ::/EDITOR/TED.EXE
mcopy -i "$IMAGE_PATH" -o "$ROOT_DIR/ted.txt" ::/EDITOR/TED.TXT

echo "Created floppy image: $IMAGE_PATH"
echo "Copied files: TED.EXE, TED.TXT -> /EDITOR"
