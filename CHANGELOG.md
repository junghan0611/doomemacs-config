# Changelog

All notable changes to this project will be documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). The repo uses CalVer
(`vYYYY.M.D`).

## Unreleased

## v2026.7.7 — ghostel 공식 :term 모듈 이관 + telega 리치 메시지 + export refs 브리지

### Added

- Migrated the in-Emacs terminal to Doom's official `:term ghostel` module with
  the `+everywhere` flag: comint/compile/eshell integration, solaire/persp
  handling, and `evil-ghostel` now come from the module, so `lisp/term-config.el`
  shrinks to an override-only layer (`doom-real-buffer-modes`, `ghostel-shell`,
  `ghostel-eval-cmds`, OSC 9;4 spinner, `ghostel-ime`, `evil-ghostel-escape`).
- Replaced the retired vterm toggle/here with ghostel equivalents
  (`my/ghostel-toggle`/`my/ghostel-here`), a popup rule, and the
  `my/pi-ghostel-start`/`my/zmx-launch` agent launchers — kept over the module's
  `+ghostel/toggle`, which references `buffer-name` as a variable and wires
  neither evil-insert nor the Korean IME.
- telega `messageRichMessage` renderer: serializes TDLib 1.8.64+ rich content
  (headings, lists, tables, code blocks, superscript/subscript, marked text) to
  markdown source so OpenClaw/Gemini bot messages render instead of showing
  `<TODO: messageRichMessage>`.  Pure serializer extracted to
  `lisp/telega-rich-md.el` with a `tests/fixtures/rich/` Txx matrix + ERT gate.
- Export-side `#+reference:` → md frontmatter refs bridge — vanilla core
  (`lisp/denote-export-refs.el`: split/dedupe, resolve) with garden-independent
  fixtures and ERT (foundation; export glue still pending).
- `jinx` spell checking config.
- `f10 O`/`f10 S` scratch bindings for org/markdown buffers.

### Changed

- ghostel Korean Lisp-IME read-only compose fix merged upstream
  ([dakra/ghostel PR #510](https://github.com/dakra/ghostel/pull/510), shipped as
  the documented `ghostel-ime` extension).  Retired the `junghan0611/ghostel`
  fork; `packages.el` now tracks `dakra/main` via `(unpin! ghostel evil-ghostel)`
  — the official module pins a stale SHA (Doom's reproducibility policy) and
  ghostel iterates faster, so we version-manage it ourselves.
- Unpinned telega to track upstream, required for the rich-message TDLib schema.

### Fixed

- agent-server: `org-agenda-skip-unavailable-files t` stops the headless daemon
  from hanging on a missing-agenda-file prompt.
- agent-server: pin git cwd for denote rename and read files fresh (issue #9).
- telega: desurrogate rich-message output so emoji render.
- korean-input: guard `set-fontset-font` for `emacs-nox` builds.
- keys: disable the alice-keyboard backtick → `toggle-input-method` bind.
- init: drop the dead `+smartparens` flag (moved to `:doom` compat).
- term-config: use the `pit` command for the pi launcher.

## v2026.6.15 — zmx 라이브 세션 + 데몬/export 보강

### Added

- Persistent terminal sessions via `term-sessions`
  (ArthurHeymans/emacs-term-sessions), with zmx as the backend: Emacs is the
  client, zmx owns session lifecycle/PTYs/history. The ghostel frontend keeps
  Korean IME working; `SPC j z` opens the consult session picker (the Emacs
  analog of the `zx` fzf picker) and `my/zmx-launch` (`SPC j a`) ports the
  `zcc`/`zcx`/`zagy`/`zpi` harness launchers as per-project
  `<prefix>.<project>` sessions.
- `run.sh doom-pull` command.
- Keep an empty `modules/` directory (`modules/.gitkeep`) so Doom's post-v3
  `doctor` path check succeeds and future local Doom modules have a stable
  landing zone.

### Fixed

- Restored `server-start` for non-daemon GUI instances; the daemon-only guard
  had prevented the `user` GUI socket from starting.
- denote-export dblock save now skips writing when the rendered content is
  unchanged, preserving the source file's mtime.

### Documentation

- Clarified in `NEXT.md` that `lisp/` remains the personal concern-split layer;
  only stable, opt-in/out units should graduate to `$DOOMDIR/modules`.

## v2026.6.9

### Removed

- Superseded parallel-export wrapper `bin/denote-export.sh` and its bash-trap
  test `tests/test_bash_cleanup.sh`. The live path is
  `run.sh export → bin/denote-export-parallel.py` (run.sh calls the Python
  orchestrator directly); the wrapper's SIGINT/SIGTERM daemon cleanup is
  already duplicated by run.sh's own trap.
- `bin/gh-starred-to-bib.sh` (GitHub stars → BibTeX) — migrated to
  zotero-config.
- `tests/test_daemon_cleanup.py` — it tested a string-embedded
  reimplementation of the cleanup logic (written by its own
  `create_testable_module()` into `bin/`), not the live
  `bin/denote-export-parallel.py`, so it covered no real code and littered
  `bin/` on each run. Genuine coverage would require making parallel.py
  importable first.
- `DENOTE-EXPORT-ISSUES.md` resolved-issue log — folded into this changelog
  (the entries below).

### Added

- Test harness: `tests/run-tests.sh` auto-discovers `test-*.el` (ERT). Tier
  A/B/C testing model documented in `tests/TESTING-GUIDELINES.org`;
  `tests/test-andenken.el` characterization tests added.
- Export pipeline Understanding pinned in `NEXT.md` and the garden guide note:
  live path, legacy/manual surface, stale references, and Tier C test boundary.
- `CHANGELOG.md` as the release/change-history SSOT; completed NEXT entries now
  move here so NEXT stays focused on remaining work.

### Fixed (denote-export daemon hardening, 2026-03-12)

- `after!` → `with-eval-after-load` in the export config so the `--quick`
  standalone daemon (no Doom macros) loads `oc` advice correctly.
- dblock mode missing GC counter: `denote-dblock-update-file` now shares the
  export GC-interval logic.
- Daemon crash root cause: `debug-on-error t` raised the debugger inside a
  frameless headless daemon on any dblock error → process crash. Set
  `debug-on-error nil`, scope the dblock scan to `^#\+BEGIN: denote`, and
  revert-buffer on error.
- meta NBSP filename cleanup: 344 files carried U+00A0 in titles; normalized
  to a plain space then bulk `denote-rename-file-using-front-matter` (wrapped
  with `denote-rename-confirmations nil` / `denote-save-buffers nil`).

### Changed

- Coding conventions: `my/` namespace, env-based Termux detection (no `uname`
  subprocess at init), Doom-v3-aware compat-shim avoidance, vanilla-first logic,
  and characterization-test gate. See AGENTS.md and the public convention note.
- `NEXT.md` trimmed back to pending work only; completed cleanup/test/export
  history lives in this changelog.
