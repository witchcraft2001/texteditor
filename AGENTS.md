# Agent Notes

## Scope and target
- This repo is a single Z80 assembly program for **Sprinter/ZX** (`textedit.asm`), not a multi-package project.
- The build target is a DOS-style executable image written by `savebin "ted.exe"` in `textedit.asm`.

## Canonical build commands
- `make.bat` is the minimal local build: `C:\asm\sjasm\sjasmplus.exe --lst=ted.lst --lstlab textedit.asm`.
- `make_image.bat` does full image packaging: assemble -> copy `image\dss_image.img` to `build\editor.img` -> mount with `osfmount.com` as `X:` -> copy `TED.EXE` into `X:\EDITOR` -> unmount -> copy image to `%SPRINTER_EMULATOR%`.
- On macOS, run these after each iteration, in this exact order: `./build.sh` then `./create_floppy_image.sh`.

## Environment assumptions (easy to miss)
- Build scripts are Windows batch scripts and assume Windows tools/paths (`C:\asm\sjasm\sjasmplus.exe`, `osfmount.com`, drive `X:`).
- `make_image.bat` depends on `%SPRINTER_EMULATOR%` being set.
- There is no repo-local CI/test/lint/typecheck config; do not invent verification steps that are not present.

## Source layout that matters
- `textedit.asm` is the real entrypoint and composition root; it includes `routines.asm`, `editor.asm`, and `menu.asm` in that order.
- `routines.asm` contains startup/exit flow (`START`), memory/page setup, and command-line file-open handling.
- `editor.asm` contains text buffer/editing core operations.
- `menu.asm` contains menu/UI flows and file operations (New/Load/Save/etc.).

## Artifacts and hygiene
- Common generated artifacts: `ted.lst` and `build/` (both ignored).
- `ted.exe` is produced by the assembler and is currently tracked in the repo; be explicit about whether a change intentionally updates this binary.

## External reference sources
- You may consult the following local sibling repositories/directories for answers, platform details, and implementation ideas:
  - `/Users/dmitry/dev/zx/sprinter/sprinter_bios`
  - `/Users/dmitry/dev/zx/sprinter/sprinter_dss`
  - `/Users/dmitry/dev/zx/sprinter/sprinter_ai_doc/manual`
  - `/Users/dmitry/dev/zx/sprinter/sources/tasm_071/TASM`
  - `/Users/dmitry/dev/zx/sprinter/sources/fformat/src/fformat_v113`
  - `/Users/dmitry/dev/zx/sprinter/sources/fm/FM-SRC/FM`
- Treat them as reference material only; this repository remains the source of truth for changes you make here.
