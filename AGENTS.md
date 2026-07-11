# AGENTS.md — doomemacs-config Agent Guide

You are the **담당자** (agent-in-charge) for this repository.
This is not a toy dotfile. Read this before touching anything.

## What This Repo Is

A 16K-line Doom Emacs configuration that serves as the **frontend for a human-agent collaborative ecosystem**. Emacs here is not just a text editor — it is the harness where:

- **GLG (힣)** writes, thinks, and manages knowledge in org-mode
- **Agents** (Entwurf, secretaries, 힣봇s) read/write the same org files, stamp agenda entries, publish to the digital garden
- **Both sides share a single `org-agenda` timeline** via `workflow-shared.el`

The agent server (`bin/agent-server.el`) exposes elisp APIs over socket `"server"`. The human uses socket `"user"`. They operate on the same `~/org/` corpus.

## Architecture at a Glance

```
init.el                 # Doom modules + single-instance guard
config.el               # Loader only — requires lisp/*.el
├── lisp/ (39 files)    # One concern = one file
├── bin/                # Standalone scripts (no Doom dependency)
├── autoload/           # ;;;###autoload lazy functions
├── run.sh              # Unified CLI/TUI: sync, export, agent, unstable
└── flake.nix           # Emacs 31 preview channel (Savannah emacs-31) via nix
```

## Code Organization Rules

### config.el is a loader

Only `(require ...)` and minimal glue. All logic lives in `lisp/`.

### One concern = one file

| Domain | File(s) |
|--------|---------|
| AI tools | `lisp/ai-*.el` (6 files) |
| Org-mode | `lisp/org-config.el`, `org-functions.el` |
| Denote | `lisp/denote-*.el` (4 files) |
| Export pipeline | `lisp/denote-export-config.el` + `bin/denote-export*.{el,py,sh}` |
| Korean input / fonts | `lisp/korean-input-config.el` |
| Unicode (NBSP, ZWS) | `lisp/unicode-config.el` |
| TTY (term-keys, kitty-graphics, clipboard) | `lisp/tty-config.el` |
| Evil | `lisp/evil-config.el` |
| Editing | `lisp/editing-config.el` |
| UI / theme | `lisp/ui-config.el` |
| Completion | `lisp/completion-config.el` |
| Search | `lisp/search-config.el` |
| Key bindings | `lisp/keybindings-config.el`, `keybindings-denote-config.el` |
| tmux / Zellij | `lisp/tmux-config.el`, `zellij-config.el` |
| RSS | `lisp/elfeed-config.el` |
| Programming | `lisp/prog-mode-config.el` |
| Termux/Android | `lisp/termux-config.el` |
| Tab bar | `lisp/tab-bar-config.el` |
| Presentation | `lisp/present-config.el` |
| Human-Agent shared | `lisp/workflow-shared.el` |

### File header standard

```elisp
;;; $DOOMDIR/lisp/module-name.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Description of what this module does.

;;; Code:

;;;; Section 1

;;;; Section 2

(provide 'module-name)
;;; module-name.el ends here
```

### Outline structure (outli.el)

Every `.el` file uses outline headings:

```elisp
;;; Level 1
;;;; Level 2
```

### Function placement

- Needs `;;;###autoload`? → `autoload/junghan.el`
- Internal to a module? → Inside that `lisp/*.el`

### Elisp coding conventions

This repo is a long-lived personal Lisp system. New agent-written Elisp should
follow a small, repeatable style so later agents can extend or remove features
without inventing a new idiom each time.

- **Doom style baseline**: follow Doom's current `docs/contributing.org` unless
  this section narrows it: bbatsov Elisp style, no hanging parentheses, and use
  `DEPRECATED` only for code that will actually be removed. Prefer `mapc` over
  `seq-do` when iterating only for side effects.
- **Namespace**: custom public variables/functions use `my/...` (`my/termux-p`,
  `my/org-download-image-dir`). Interactive commands may use `my/name` in Doom's
  command style. Private helpers should still start with `my/` and include the
  concern name; use `--` for genuinely internal helpers inside a larger module,
  e.g. `my/termux--decode-arrow-key`.
- **Vanilla-first logic**: functions with clear input→output behavior should
  run under `emacs -Q` whenever possible. Keep Doom macros (`map!`, `after!`,
  `use-package!`) in glue, keybinding, or package-configuration layers, not in
  reusable logic.
- **Test gate before refactor**: for Tier A logic, write characterization tests
  before changing behavior or structure. `tests/run-tests.sh` is the vanilla
  `emacs -Q` ERT gate; package-dependent paths belong in a separate Tier C
  integration runner or should have their pure branch extracted first.
- **Avoid Doom v2 compat shims**: do not add new `IS-*`, `EMACS29+`, `setq!`,
  `featurep!`, `appendq!`, `pushnew!`, etc. Prefer `(featurep :system 'macos)`,
  `(>= emacs-major-version 31)`, `setopt`/`setq`, `cl-callf`, `add-to-list`, or
  a local `my/...` predicate when grep-ability matters.
- **Use Emacs libraries before hand-rolled loops**: prefer `seq.el` for lists,
  `map.el` for alists, `subr-x` for string helpers, and existing Doom/Emacs
  primitives before writing long bespoke functions.
- **Data shape**: for small structured state, prefer alists with `:kebab-case`
  keyword keys. Use plists/hash-tables only when there is a clear reason.
- **Accessors**: prefer `map-elt` and `map-nested-elt` for alists; avoid repeated
  nested `assoc`/`cdr` boilerplate.
- **cl-lib surface**: `cl-defun` with `&key` is welcome for self-documenting
  call sites. Reach for other `cl-lib` forms only when they simplify the code.
- **Flatten control flow**: prefer `when`, `unless`, `when-let*`, and guard
  bindings over deeply nested `let`/`if` blocks. Use `let*` only when bindings
  depend on earlier bindings.
- **No premature API**: avoid new `defcustom` until the option has settled.
  Start with an internal `defvar`/`defconst` and promote later if needed.
- **Comments/docstrings**: code comments should be in English and explain intent
  or invariants, not restate the obvious. Korean operational notes are fine in
  higher-level docs, but generated code should keep comments concise.
- **Consistency first**: before adding a feature, search for the closest existing
  pattern in `lisp/` and mirror it unless there is a strong reason not to.

### map! prefix 규약

**우리 원칙**: Doom이 선언한 키바인딩은 그대로 쓴다. 정말 바꿔야 할 키만 그 위에
얹는다. 겹치면 우리 것이 이기되, **Doom 것을 죽이지 않는다**. `config.el`은 Doom 모듈
뒤에 로드되므로, 같은 키를 다시 `define-key` 하면 그 키만 우리 것으로 덮인다 — 이게
정상 동작이고, 나머지 Doom 키는 살아있어야 한다.

**규칙은 한 줄**: `map!`에서 prefix에 **이름을 달지 않는다**. 키는 비파괴적으로 밀어
넣고, 라벨은 keymap을 건드리지 않는 경로로 따로 단다.

```elisp
;; 금지 — 둘 다 그 키에 keymap 을 "바인딩"한다
(map! :leader (:prefix     ("f" . "files")   "y" #'foo))
(map! :leader (:prefix-map ("j" . "pi-agent") "a" #'bar))

;; 사용 — 키만 얹는다. 기존 맵이 있으면 그 안으로 들어가고, 없으면 만들어진다
(map! :leader (:prefix "f" "y" #'foo))
(map! :leader (:prefix "j" "a" #'bar))
```

라벨은 `lisp/keybindings-config.el`의 **`Leader prefix labels — SSOT`** 블록 한곳에서
`which-key-add-keymap-based-replacements`로 단다. 이건 pseudo-key만 심어서 기존
바인딩을 절대 덮지 않고, 아직 없는 prefix는 만들어 두기만 한다 — 어느 파일이 먼저
로드되든 결과가 같다. Doom 소유 prefix(`f` `n` `h` `o` `p` `b` …)는 Doom이 이미 이름을
달아뒀으니 넣지 않는다.

**왜 그렇게 됐나.** Doom이 `map!`에서 general.el을 걷어내면서(upstream `de2a3364a`,
2026-07) 의미가 바뀌었다. which-key가 Emacs 30.1에 내장되자 Henrik이 퇴출 결정을 뒤집고
core로 재통합했고, 라벨을 general의 전역 regexp 테이블에서 **네이티브 cons cell**
(`("label" . command)`)로 옮겼다.

| | 이전 (general) | 지금 (네이티브) |
|---|---|---|
| `:desc`가 만드는 것 | `(:ignore t :which-key "desc")` | `(cons "desc" def)` |
| prefix 선언 시 | `:ignore` → **define-key 건너뜀** (라벨만) | `(cons "desc" (make-sparse-keymap))` → **새 빈 맵을 바인딩** |

즉 desc 붙은 `:prefix`는 이제 **그 자리의 기존 prefix 맵을 통째로 파괴한다**. Doom의
`doom-leader-file-map`, 표준 `help-map`, 다른 파일이 선언한 prefix가 전부 날아간다.
문법은 그대로라 조용히 깨진다. `:prefix-map`은 `defvar` 덕에 우리 파일끼리는 맵을
재사용하지만, 그 키에 맵을 바인딩하는 건 똑같아서 Doom이 나중에 그 키를 가져가면 같은
사고가 난다. 그래서 둘 다 쓰지 않는다.

**게이트**: `tests/test-keybinding-lint.el`이 `lisp/`·`autoload/`를 스캔해 두 형태를
모두 file:line으로 잡는다. `tests/run-tests.sh`에 자동 포함.

**upstream 대응 원칙 — 우리가 맞춘다.** 이 건으로 Doom에 이슈나 PR을 올리지 않는다.
GLG 결정(2026-07-12): 메인테이너는 바쁘고, upstream이 어떻게 움직이든 **닷파일이
따라가는 게 우리 일**이다. 상류가 바뀌면 대응 코드로 버틴다 — 상류를 우리 쪽으로 끌지
않는다. 이건 이 한 건의 판단이 아니라 이 리포가 upstream을 대하는 기본 자세다. `./run.sh G`
뒤에 뭔가 깨지면, 먼저 우리 코드를 바꿀 자리를 찾는다.

**주의**: Doom은 `SPC h`에 표준 `help-map`을 직접 바인딩한다
(`:config default` `+evil-bindings.el:343`). 그래서 `(:prefix "h" ...)` 아래 정의는
전역 `help-map`을 변형한다 — 이 리포에서 `C-h t`는 `help-with-tutorial`이 아니라
테마 맵이다 (의도된 것).

## Emacs Server Sockets

| Socket | Purpose | How it starts |
|--------|---------|---------------|
| `"user"` | GLG's GUI Emacs | `doom run` (Doom auto-calls `server-start` for GUI) |
| `"server"` | Agent daemon | `run.sh agent start` (separate `--init-directory`) |
| `"doom-unstable"` | Emacs preview channel | `bin/emacs-unstable.sh` (or `run.sh unstable`) |

The **single-instance guard** in `init.el` only blocks duplicate daemons. Non-daemon instances (`emacs -nw`, `doom run`) run independently.

## Key Workflows

### Adding a new config module

1. Create `lisp/my-feature-config.el` with header + `(provide 'my-feature-config)`
2. Add `(require 'my-feature-config)` to `config.el`
3. Maintain outline structure

### Editing bin/ scripts

- `bin/denote-export.el` and `lisp/denote-export-config.el` share logic
- Advice/settings go in `denote-export-config.el` so both sides pick them up
- New export folders → update `get-org-hugo-section-from-path` in `bin/denote-export.el`

### Agent server

```bash
./run.sh agent start    # Start (checks stale socket)
./run.sh agent status   # Health check
./run.sh agent restart  # Stop + start
./run.sh agent eval     # Interactive eval
```

### workflow-shared.el — the contract

This file defines rules that **all three context** — user Emacs (GUI), agent-server,
**denote-export 데몬** — must agree on. 데몬은 Doom 모듈을 로드하지 않으므로,
GUI 에서만 자동으로 잡히는 설정은 여기에 SSOT로 두고 명시 적용해야 한다.

| Setting | Why |
|---------|-----|
| `org-tag-re` | Tags allow only `[[:alnum:]@#%]+` — matches Denote filetags |
| `org-agenda-files` | Dynamic: `_aprj` tagged files + `botlog/agenda/` + current journal |
| `org-todo-keywords` | TODO/NEXT/DONE/DONT(o) — agent-server가 DONT skip 하려면 데몬도 인식 필요 |
| `my/org-download-image-dir` | `[[download:foo.png]]` 해석 (`~/screenshot/`). GUI는 Doom org-download `:config`, 데몬은 SSOT applier |
| `my/org-attach-id-dir` | `[[attachment:foo.png]]` 해석 (`~/org/.attach/`). GUI는 Doom lang/org 모듈, 데몬은 SSOT applier |
| Journal entry format | Active timestamps so entries appear in agenda |

**Rule**: UI/theme/keybindings can differ. **Data read/write rules must be identical.**

**SSOT 적용 패턴**:
```elisp
;; lisp/workflow-shared.el
(defvar my/X "...")
(defun my/apply-X () (setq-default X my/X) (setq X my/X))

;; lisp/org-config.el (GUI)         → (require 'workflow-shared) + (my/apply-X)
;; bin/denote-export.el (데몬)      → workflow-shared 로드 직후 (my/apply-X)
;; bin/agent-server.el (데몬)        → workflow-shared 로드 직후 (my/apply-X)
```

같은 패턴으로 누락된 buffer-local 변수가 더 발견되면 같은 자리에 추가.

### Garden Verify / Fix — `./run.sh verify` and `./run.sh fix`

가든 export 후 콘텐츠 품질 검증 / 정정 흐름. 두 키로 통합, 4단계:

| 단계 | verify ([N/4]) | fix ([N/4]) |
|------|----------------|-------------|
| **relref** | `verify-relref.py --summary` (link 카테고리 카운트) | `--fix --apply` (DEAD/REWRITE/MALFORMED → plain text 또는 정정) |
| **anchors** | (verify에 포함) | `--fix-anchors --apply` (ox-hugo가 link 내장 헤딩에 흘려넣은 `{#title--relref-section-id-dot-md}` 누출 제거 → `{#title}`) |
| **description** | `check-description.sh` (botlog 우선, 누락만 경고) | (verify-only) |
| **figures** | `verify-figures.py` (figure shortcode + markdown `![]()` 둘 다) | `--fix --apply` (REWRITE → 소스 → static/images 복사 + src 치환) |
| **content** | `verify-content.py --summary --lychee` (host alias / 내부 경로 / 사설 endpoint / URL credential / GITHUB_404) | `--fix --lychee --apply` (자동 정정 카테고리만 plain text 또는 alias 치환) |

**figure 검증 카테고리**:
- `ALIVE` — `/images/...` / `https://...` (정상)
- `REWRITE` — broken 패턴이지만 SEARCH_DIRS에서 basename 매칭 → 자동 정정 가능
- `AMBIGUOUS` — 여러 매치, sha 다름 → 수동 확인
- `DEAD` — 알려진 broken 패턴(`/home/...`, `~/...` 등), 매치 0
- `UNKNOWN` — 비표준 패턴(`assets/`, `img/`, 상대경로 등), 매치 0

**SEARCH_DIRS** (verify-figures.py): `notes/static/images/` → `~/screenshot/` → `~/org/.attach/`. 이전 export에서 이미 옮겨진 파일이 우선 잡혀 file copy 없이 src 치환만 일어남.

**content 검증 카테고리** (verify-content.py, `site-policy.el` SSOT):
- `HOST_ALIAS` — 사라진 자기 서브도메인 (예: geworfen.junghanacs.com → agent.junghanacs.com) → 자동 alias 치환
- `INTERNAL_PATH` — `~/repos/`, `/home/junghan/`, `file://` 누출 → plain text
- `PRIVATE_ENDPOINT` — `localhost`, `127.0.0.1`, 사설 IPv4 (보고만)
- `URL_CRED` — `https://user:pass@host` basic auth 누출 (보고/보안 alert)
- `GITHUB_404` — lychee가 잡은 `github.com/USER/*` 404/410 → plain text
- `ORPHAN` — `[desc]` 끝에 target 없음. **현재 비활성** (정교화 follow-up — code fence/inline/shortcode 제외 필요)

**lychee 운영 노트**:
- `GITHUB_TOKEN` 또는 `GITHUB_PERSONAL_ACCESS_TOKEN` 필요. 없으면 결과는 **참고용 (advisory)** — secondary abuse rate-limit으로 다수가 false positive (실측: token 없음 84 broken vs token+튜닝 1 broken).
- `~/.env.local`을 `load_env_local` 헬퍼가 verify/fix [4/4]와 `fix-org --check` 진입 시 자동 source.
- `.lycheecache` (.gitignored) 1d 캐시. `cache-exclude-status: "404"` 안전망 — agenda-stamp가 박는 SHA URL은 push 사이클로 404→200 변하므로 캐시 시 false positive 지속.
- `max-concurrency: 16` (기본 128은 abuse detection 걸림).

### Org Hygiene — `./run.sh fix-org`

~/org 원본 link 정정 (Stage 1). 가든 export hook 옆에 두는 SSOT 변환:

| 케이스 | 변환 |
|--------|------|
| `[[file:~/repos/gh/REPO]]` | `[[https://github.com/junghan0611/REPO]]` |
| `[[file:~/repos/gh/REPO/path/file.el]]` | `[[https://github.com/junghan0611/REPO/blob/main/path/file.el]]` |
| `[[file:~/repos/gh/REPO/file.el::N]]` | `[[https://github.com/junghan0611/REPO/blob/main/file.el#LN]]` (라인 anchor 보존) |
| `[[https://OLD.junghanacs.com][...]]` | site-policy.el `host-aliases` 사전 치환 |

**보호 영역 (절대 건드리지 않음)**:
- `[[denote:UUID]]` — denote system invariant
- `[[file:~/screenshot/...]]`, `[[file:~/org/.attach/...]]` — figure 파이프라인 영역
- `[[file:~/org/...]]` — denote 내부 cross-ref
- code / verbatim / src-block 내부 (org-element 기반이라 자동)

**흐름**:
- `O` / `./run.sh fix-org` — dry-run → `y/N` → apply. 로컬 부재 ⚠ 표시
- `./run.sh fix-org --apply` — prompt 없이 직접 적용
- `./run.sh fix-org --check` — 변환 안 함. `~/org`의 `[[https://github.com/USER/...]]` URL을 lychee로 검증, broken을 file:line으로 보고 (read-only — 사용자가 ~/org에서 직접 분류)

**SSOT**: `bin/site-policy.el`. `host-aliases` 한 줄 추가하면 `fix-org` / `verify-content` / `verify-org-links` 세 도구가 동시에 인지.

### Garden Deploy Workflow

전체 export 후 표준 운영 흐름. ~/org 변경부터 가든 publish까지:

```
Phase           Command                                  Purpose
─────           ───────                                  ───────
1. Org 위생     O ) ./run.sh fix-org                     ~/repos/gh/ file: → GitHub URL
   (선택)         CLI: ./run.sh fix-org --check          broken GitHub URL 검출 (token 필요)

2. Export       7 ) ./run.sh export <dir>                증분
                8 ) ./run.sh export <dir> --force        전체 재구축
                9 ) submenu                              폴더별 증분/전체

3. 가든 검증    V ) ./run.sh verify                      [1/4] relref / [2/4] description /
                                                          [3/4] figures / [4/4] content+lychee

4. 가든 정정    F ) ./run.sh fix                         단계별 y/N. 자동 가능만 적용

5. 사용자 분류  (emacs에서 ~/org 직접)                   verify 출력의 → ~/org 매핑 따라
                                                          케이스별 결정 (private 표기 / plain
                                                          text / rename)

6. Deploy       cd ~/repos/gh/notes && git push          가든 배포
                cd ~/sync/org && git push                ~/org 변경 같이
```

**도구 잡는 자리 vs 사용자 잡는 자리**:

| 자리 | 도구 자동 ([1/4]~[4/4] fix) | 사용자 수동 |
|------|------------------------------|-------------|
| relref DEAD / REWRITE | ✓ | |
| heading anchor 누출 | ✓ | |
| figure src REWRITE | ✓ | |
| host alias 잔재 | ✓ | |
| 내부 경로 노출 | ✓ (plain text) | |
| GitHub 404 (md link 형태) | ✓ (plain text) | |
| 외부 URL 404 (GitHub 외) | | ✓ (도구 빈자리, follow-up) |
| ~/org GitHub URL 분류 | | ✓ (`fix-org --check` → ~/org 결정) |
| 의도된 private repo | | ✓ (org에서 표기 결정) |
| ox-hugo silent transform | UNKNOWN 보고 | ✓ |

**운영 노트**:
- 전체 export: 3,300 org → 2,200 md, 병렬 8 worker로 분 단위. 증분이 일반적.
- D 사례 (메타 list item silently drop) — 재export로 회복되는 경우 있음. 회귀 의심 시 export 한 번 더.
- token 없는 verify는 advisory. 실제 ~/org 분류 작업은 token 있는 conclusive run 기준.
- `fix-org --apply` 후 `~/sync/org` modified는 정상 (사용자 commit 자리).

## Commit Messages

```
feat: add tty-config — term-keys, kitty-graphics, clipboard unified

- daemon guard now daemon-only (emacs -nw allowed)
- config.el terminal block → tty-config.el (45 lines removed)
```

**Never** include "Generated with Claude" or "Co-Authored-By".

## Things to Watch

- `doom sync` needed after `init.el` module changes, NOT for `config.el`/`lisp/` edits
- `per-machine.el` is git-ignored — font/theme overrides go there
- Emacs 31 preview channel coexists with system stable via separate `EMACSDIR` (`~/doomemacs-unstable`) and `server-name` (`doom-unstable`). flake output name is still `emacs-unstable` for launcher compatibility, but it pins Savannah `emacs-31` release branch while Emacs 31 is pre-release. overlay#emacs-unstable follows latest stable release tag and may remain at 30.2 until 31.1 is tagged; overlay#emacs-git tracks master and may already be 32.0.50.
- Korean input edge cases: NFD→NFC, Evil state auto-switch, TTY clipboard
- WezTerm + terminal Emacs + built-in Korean input is a custom path; if minibuffer/search prompt spacing breaks, inspect TTY width drift first — especially hardcoded Unicode ellipsis (`…`) in Consult prompt/path truncation before blaming Hangul input
- **헤드리스 데몬은 Doom 모듈을 로드하지 않는다** (`bin/denote-export.el`, `bin/agent-server.el`). buffer-local org 변수(`org-attach-id-dir`, `org-download-image-dir` 등)가 GUI에서만 자동으로 잡히는 경우 가든에 broken figure가 누적된다. `workflow-shared.el`에 SSOT applier로 두고 양쪽에서 호출 — 회귀 시 첫 의심 지점. (사례: 2026-05-10 commit b348898 / d8b977a)
- **키바인딩이 통째로 사라지면 `map!` prefix부터 의심한다**. `./run.sh G`로 Doom을 당긴 뒤 `SPC f s`·`SPC h d` 같은 Doom 기본키가 `undefined`가 되면, 십중팔구 desc 붙은 `:prefix`가 기존 prefix 맵을 덮은 것이다. 진단은 `emacsclient -s user`로 `(lookup-key doom-leader-map "f")`가 `doom-leader-file-map`과 `eq`인지 보면 즉시 갈린다. 규약과 배경은 § map! prefix 규약, 게이트는 `tests/test-keybinding-lint.el`. (사례: 2026-07-12, upstream `de2a3364a`)
- 가든 broken은 빌드를 깨지 않는다. `./run.sh verify` → `./run.sh fix` 흐름으로 주기적 청소
- **export 직후에는 항상 `./run.sh fix`를 같이**: ox-hugo가 link 내장 헤딩 anchor에 `{#title--relref-section-id-dot-md}` 노이즈를 흘리는 회귀가 살아있다. fix 단계 [2/3] `--fix-anchors`가 안전망 — 안 돌리면 export 직후 짧게 노출됨. "버그 새로 생긴 것 같다" 착각의 단골 원인.
