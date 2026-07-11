# Changelog

All notable changes to this project will be documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). The repo uses CalVer
(`vYYYY.M.D`).

## Unreleased

## v2026.7.12 — 키바인딩은 얹는다, 덮지 않는다

### Fixed

- `map!`에서 prefix에 이름을 달지 않는다. `./run.sh G`로 Doom을 당긴 뒤 leader가
  무너졌다 — `SPC f`가 `doom-leader-file-map` 19키를, `SPC h`가 표준 `help-map`
  62키를, `SPC o`가 `doom-leader-open-map` 15키를 잃었고, `SPC j`를 나눠 쓰던 다섯
  파일이 서로를 지워 1키만 남았다. 원인은 upstream
  [`de2a3364a`](https://github.com/doomemacs/doomemacs/commit/de2a3364a): which-key가
  Emacs 30.1에 내장되자 Doom이 `map!`에서 general.el을 걷어내고 라벨을 전역 regexp
  테이블에서 네이티브 cons cell로 옮겼다. 그 결과 설명 붙은 `:prefix`가 **라벨만 다는
  no-op에서 새 `make-sparse-keymap`을 바인딩하는 동작으로** 바뀌었다. 문법이 그대로라
  조용히 깨졌다.

  규칙은 한 줄이 됐다 — 키는 `(:prefix "f" …)`로만 밀어 넣는다. 이 형태는 그 키에 이미
  있는 keymap 안으로 걸어 들어가므로 Doom의 바인딩이 살아남고 우리 것이 그 위에 얹힌다.
  충돌 시엔 `config.el`이 나중에 로드되니 우리가 이기되, 그룹을 통째로 데려가지 않는다.
  `:prefix-map`도 배제했다 — 그 키에 맵을 바인딩하는 건 똑같아서, Doom이 언젠가 그 키를
  가져가는 날 같은 사고가 난다.

  실측 복구: `SPC f`는 다시 `doom-leader-file-map` 본체(`eq`), `SPC h`는 `help-map`
  본체, Doom 자체 라벨(file/notes/help/open/project/buffer) 보존, `SPC j`에 다섯 파일
  바인딩 전부 누적, `SPC f y`는 여전히 Doom 기본을 이김.

### Added

- leader 그룹 라벨 SSOT (`lisp/keybindings-config.el`). 빌드인 which-key의
  `which-key-add-keymap-based-replacements`는 기존 바인딩을 **제자리에서 이름만 바꾼다**
  (`(106 keymap …)` → `(106 "pi-agent" keymap …)`). 맵을 교체하지 않고, 아직 없는
  그룹에도 라벨을 심어둔다 — 비파괴적이면서 로드 순서와 무관하다. 그래서 11개 그룹
  라벨을 파일마다 흩지 않고 한 블록에 모았다. leader 구성이 한 화면에 읽힌다.
- `tests/test-keybinding-lint.el` — `lisp/`·`autoload/`를 스캔해 이름 붙은 prefix
  두 형태를 모두 `file:line`으로 잡는 Tier A 게이트. 위반 파일을 넣어 실제로 잡히는
  것까지 확인했다.

### Changed

- edge-tts leader 바인딩 비활성, `SPC -`를 whisper에 온전히 반환. 두 모듈이 `SPC - w`를
  두고 다퉜고(`whisper-run` vs TTS org 워크플로우) 나중에 로드되는 TTS가 이겨 whisper의
  녹음 키가 닿지 않았다 — TTS 맵이 whisper prefix를 통째로 덮고 있어 가려져 있던
  충돌이다. TTS는 요즘 거의 안 쓰고 whisper는 매일 쓰니 `SPC -`를 넘겼다. 지우지 않고
  주석 처리했으며, 주석 안의 형태는 이미 비파괴형이라 되살릴 때 빈 leader 자리만 고르면
  된다.
- 문서: `AGENTS.md § map! prefix 규약`(금지형/사용형/배경/게이트), `README § Keybindings
  — layer, never replace`, "Things to Watch"에 진단 한 줄 —
  `(lookup-key doom-leader-map "f")`가 `doom-leader-file-map`과 `eq`인지 보면 즉시 갈린다.

- 가든 meta 태그 풀을 `meta/`가 바뀔 때 다시 만든다 (`4c7b77d`). 풀이 프로세스 수명 동안
  캐시돼, meta 노트를 새로 쓴 뒤 export하면 새 자석이 반영되지 않았다.

## v2026.7.9 — 가든 태그를 meta 자석으로 제한

### Added

- Garden tags are now restricted to the tag pool that `meta/*.org` headers
  define (`lisp/denote-export-config.el` § 1.7).  A tag is a magnet —
  `/tags/emacs` links to `meta/†-이맥스` — so a tag no meta note defines is a
  dead URL.  Org sources may still accumulate stray tags; the filter drops them
  at export time and never touches the org files.  No allowlist and no special
  case: to publish a tag, write its meta note; to retire one, remove the
  filetag.  Control lives in the notes, not in the export code.
  Measured on the live garden: 2,437 unique tags → 1,213, nothing surviving
  outside the pool.
- Fail-closed guard: a pool below 500 tags aborts the export with a
  `user-error`.  The headless daemon loads no Doom modules, so a wrong
  `denote-directory` has happened before; silently wiping every tag in the
  garden would otherwise look like a successful run.
- `run.sh` asks before the lychee pass in `verify`/`fix` `[4/4]`.  Answering `n`
  skips only the network check — the local content checks (host alias, internal
  path, private endpoint, credential) still run in ~0.5s, so the step keeps its
  value instead of being abandoned with C-c.
- `jinx` spell checking config, `f10 O`/`f10 S` scratch bindings.

### Changed

- `org-export-with-tags` is now nil.  Heading tags carry Org bookkeeping
  (`ARCHIVE`, `IMPORTANT`), render as `<span class="tag">` in the body, and never
  create a `/tags/` page — tag curation belongs in `#+filetags:`.  This also
  keeps `org-hugo-tag-processing-functions` from running on heading tags
  (ox-hugo.el:2128), so the § 1.7 filter needs no guard against them.
- `evil-disable-insert-state-bindings` enabled via `setopt` so the `:set`
  handler rebuilds `evil-insert-state-map` live and evil-markdown skips its
  `M-*`/`M-b`/`M-i` insert bindings.

### Fixed

- Five `test-denote-export.el` tests had never run.  `:expected-result` was
  evaluated at load time, when `my/denote-link-ol-export` was still unbound,
  freezing the result as `:failed`, while the matching `skip-unless` is
  evaluated at run time.  Dropping the marker exposed two real defects:
  `no-wrong-type-argument-error` asserted `(should-not (should-error ...))`,
  which cannot pass because `should-error` itself fails the test when no error
  is raised; and `normal-link` depended on `org-hugo-base-dir`, which only the
  export daemon sets.  This settles the NEXT item "denote-export test dead-path
  Tier 결정" in favour of Tier C — straight build on `load-path`, guarded by
  `skip-unless`, inside the existing runner (`tests/test-hugo-tag-filter.el`).

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
