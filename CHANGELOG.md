# Changelog

All notable changes to this project will be documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). The repo uses CalVer
(`vYYYY.M.D`).

## Unreleased

## v2026.8.23-forge.1 — Magit Forge as the local issue inbox

### Added

- **Magit Forge inbox** (`init.el`, `+user-info.el`, `lisp/project-config.el`).
  `(magit +forge)` is on and the local SQLite db is seeded, so GitHub issues
  are readable with the network down — bodies, comments, labels and assignees
  all land locally. Measured on adoption: 19 repos, 65 open issues, 295 issue
  comments, 2.3 MB db, ~12 s for a full pull of every tracked repo.
  - `my/forge-seed-repositories` asks `gh` which owned repos currently carry an
    open issue, so the seed list cannot go stale in config.
  - Archived repos are kept out on the way in (`--archived=false`) and pruned
    on the way out by `my/forge-prune-archived-repositories`, since forge has
    no notion of archived.
  - `SPC g i` / `SPC g I` open the inbox. Refresh is stock: `forge-pull`
    resolves the repository at point, so `SPC g ' f f` on an inbox line pulls
    that repo and `f t` pulls the single topic.
- **Workspace and tab navigation keys** (`lisp/keybindings-config.el`).
  `M-[` / `M-]` switch workspaces, reclaimed on `dired-mode-map` after
  `casual-dired-setup` rebinds them; `gb` / `gB` move between tabs.

### Changed

- **consult-gh scoped to discovery.** `consult-gh-favorite-orgs-list` now
  holds only the two accounts in use. It is deliberately *not* wired to
  `consult-gh-forge`: that mode inserts every selected search hit's repo into
  the forge db and overrides `ghub--token`/`--username`/`--host` globally.
  Global search and the local inbox are different axes.
- **tab-bar config** trimmed; dead centaur-tabs face handling removed.

## v2026.8.23 — Korean writing hygiene SSOT, gptel Copilot, smaller UX fixes

### Added

- **GitHub Copilot backend on gptel** for the trial subscription rail
  (`lisp/ai-gptel.el`).
- **`:lang lean` and `:lang ledger`** enabled in `init.el`.
- **`korean/nfc-normalize-buffer`** alias (was documented, missing as a command).

### Changed

- **Korean writing hygiene SSOT** in `lisp/korean-input-config.el`.
  Interactive cleanup that used to live in `functions.el`
  (`my/clear-nbsp-and-ascii-punctuations`, `my/insert-nbsp-simple-all`,
  `my/fix-markdown-bold-to-org`) now sits with input/NFC under one agent-facing
  catalog. Export-time CJK emphasis and batch `fix-bold` stay on the export
  path (daemon-readable). Docs point here: `AGENTS.md`, `README.md`,
  `bin/fix-org-mdbold.el`.
- **casual**: drop the per-mode `<f12>` table; use `casual-init` instead.
- **agent-shell**: remove unused `meta-agent-shell` / `agent-shell-attention`
  wiring.
- **keycast** re-enabled in `packages.el`.
- **org-element cache** turned off for a trial (`org-element-use-cache` /
  `org-element-cache-persistent` nil) after intermittent org cache pain.
- AGENTS note: run `./run.sh fix` right after export while the ox-hugo anchor
  leak still needs the safety net.

### Fixed

- **elfeed-show `q`** → `kill-buffer-and-window`. evil-collection bound
  `kill-current-buffer`, which left an empty window after reading an entry.
- **Pi start key** moved off the sticky `SPC j j` chord to `SPC j M-j` so it
  stops stealing the double-tap.
- **Bibliography paths** follow `.bib` files under `resources/bib/`
  (`+user-info.el`).
- **TTS / transcript** output directory paths updated (`ai-tts-edge.el`,
  `denote-functions.el`).
- **emacs-unstable launcher** uses Doom's emacs command for launches.
- **pimacs** requires its package before reading `pimacs-flags`.

## v2026.8.11 — measured model tiers, tightened docs

From here on, new entries are written in English: this is a public repo and the
changelog ships as release notes. Older sections are left as they were written.

### Fixed

- **gptel summarize/translate failing as "요청 실패 — HTTP/2 200".** The
  subscription rail answers a congested request with HTTP 200 and a payload-level
  `server_is_overloaded`, so reporting `:status` alone hid the real cause and made
  remote congestion look like a local regression. `my/gptel-error-message` now reads
  `:error` first, and all eight failure sites in `ai-gptel.el` and `elfeed-config.el`
  go through it.
- **agent-server never applied the workflow-shared org contract.** It loaded
  `workflow-shared.el` but called neither `my/apply-org-download-image-dir` nor
  `my/apply-org-attach-id-dir`, so a headless daemon — which loads no Doom modules —
  resolved `[[attachment:…]]` and `[[download:…]]` against org defaults. AGENTS.md had
  documented the applier contract as if all three contexts honored it. Found by
  cross-review; verified after the fix that the daemon now reports the SSOT paths.
- **ELFEED_LINK (Stage 3 F).** ox-hugo leaks org `elfeed:` links as
  `[desc](host#https://article…)` dead targets. Export is left alone; verify-content
  rewrites them to the embedded https URL, handled in one `F` pass.

### Changed

- **`my/gptel-model-fast` moved from `gpt-5.6-luna` to `gpt-5.6-terra`.** Interleaved
  sequential probes on 2026-08-11 measured luna at 11/36 successes (~31%, 5–9s even
  when it answered) against terra at 15/15 (100%, median 2s) — prompt length, system
  prompt, and the `Originator` header were all ruled out, leaving tier as the only
  variable. A congested cheap tier is the slot that fails most and answers slowest.
  This one constant also fixes `gptel-quick` and magit commit messages. End-to-end
  elfeed summarize went from 19–32s with retries to a flat 8s.
- **AGENTS.md rewritten and cut from 468 to ~330 lines, now in English.** It keeps
  structure, coding conventions, contracts, and recurring traps; frequently-changing
  operational detail (verify/fix categories, deploy stages, org-hygiene tables,
  lychee tuning, neomacs measurements) now lives only in `README.md` and in code SSOT,
  with pointers left behind. A precedence table at the top settles conflicts: the code
  wins, and a doc that contradicts it is a bug.
- **Documentation corrected against the code** after cross-review by a peer agent:
  the neomacs profile is builtin-only *except* for an on-disk pure-Elisp Denote
  checkout; `--gnu` cannot be combined with `--probe` (tracked in `NEXT.md`); the
  socket table counts five, not four; the retry's model fallback is inert while fast
  and default are the same model; and `packages.el` also requires `doom sync`.
- Config no longer loads `andenken-config`.

### Added

- **`my/gptel-request-retry`** — exponential backoff for transient overload codes,
  with the final attempt falling back to a less contended model. This is what a Codex
  CLI does internally; without it a single first failure was all the user ever saw.
- **Pimacs** — one-buffer Pi client that picks its model at start.
- Keybinding lint now also catches a command bound where another file opens a prefix.
- User and assistant callout templates.

### Fixed (UI)

- Vertico renders completion icons in GUI only.

### Added

- **`./run.sh fix-bold` (B)** — 가든 퍼블리시 면(`notes/bib/meta/botlog`)에서
  마크다운 `**bold**` → org `*bold*`. **journal 제외.** org-element로
  src/drawer/code 보호. dry-run → y/N → apply. 구현: `bin/fix-org-mdbold.el`.

### Fixed

- **INTERNAL_PATH: `~/` 틸드 홈 전체.** Stage 3가 `~/repos/`만 봐서 ox-hugo가
  내보내는 `~/sync/...`, `~/claude-memory/...` dead md 링크를 놓쳤다. SSOT
  `site-policy.el` 패턴을 `^~/`로 넓혀 F(content fix)가 plain text로 접게 한다.
- **elfeed → remember Org 안전.** 요약의 `* item` / `**bold**` / `# heading`이
  remember.org 헤딩을 박살 내던 문제. (1) 요약·번역 프롬프트를 hyphen-list only로
  고정 (2) 삽입 전 `+elfeed--org-safe-summary` sanitize (3) 요약을 `#+begin_quote`
  `[!note]`로 감쌈 (4) `remember-text-format-function`으로 `* [stamp] title` 헤딩.
- **consult-gh transient 메뉴.** `consult-gh-transient`는 sibling 파일이라
  `:commands` autoload가 깨졌다. `my/consult-gh-menu` 래퍼로 `SPC g h h` 복구.

## v2026.8.2 — 인간 검색면만 얇게, 작업면은 나눈다

### Added

- **consult-gh — human GitHub search surface.** 예전 omni/forge 묶음으로 지저분해져
  걷어냈던 패키지를 **MELPA core만** 다시 들인다. 역할은 하나: 브라우저 대신
  Emacs 안에서 repo/code/issue를 찾고, 타깃을 에이전트에 넘긴다. 이슈·PR 본작업은
  에이전트 `ghcli` / Magit 쪽 자리로 둔다.

  | 키 | 명령 |
  |---|---|
  | `SPC g h s` | `consult-gh-search-repos` |
  | `SPC g h c` | `consult-gh-search-code` |
  | `SPC g h i` | `consult-gh-search-issues` |
  | `SPC g h a` | `consult-gh-auth-switch` (멀티 계정) |
  | `SPC g h h` | `consult-gh-transient` (필요할 때만) |

  clone 기본 경로 `~/repos/3rd/`, 선택 액션은 브라우저가 아니라 파일 브라우즈.
  embark / forge / pr-review / consult-omni / default view-mode 키는 **안 켬**.
  `ox-gfm`은 hard dep이라 같이 선언. 문서: README § GitHub work surfaces,
  설정: `lisp/project-config.el`.

### Changed

- **Emacs preview channel → 31.0.91 (pretest 2).** 핀을 브랜치 중간 커밋에서
  태그 `emacs-31.0.91`(`57581b8b`)로 옮겼다. 다음 pretest도 태그 자리에서만 올린다.
  빌드·GC root까지 완료. 첫 `./run.sh unstable` 기동(straight/eln 재빌드)만 GLG 자리.

### Docs

- NEXT 상단을 **NOW** 한 블록으로 압축. 완료된 31.0.91 빌드 체크리스트는 여기로 이전.
- README에 GitHub 작업면 분담 표 (consult-gh / magit-gh / git-link / ghcli).
- telega `telega-server-libs-prefix` 해시순 정렬 취약점 관찰 기록 (NEXT, 미해결).

## v2026.7.22 — 표면을 줄이고, 상류를 따라간다

### Changed

- **gptel: 백엔드 하나, 모델 셋.** 모델은 닷파일이 따라갈 수 없는 속도로 나온다. 새
  릴리즈가 나올 때마다 백엔드가 하나씩, 손으로 베낀 스펙 블록이 하나씩,
  `my/gptel-switch-to-*` 명령이 하나씩 늘었고 — 안 쓰게 된 것들은 그대로 남았다.
  표면을 **OpenAI-sub** (ChatGPT 구독 OAuth, Codex Responses endpoint) 하나로 자르고
  모델을 셋으로 줄였다: `gpt-5.6-terra`(기본), `gpt-5.6-sol`(무거운 것),
  `gpt-5.6-luna`(빠른 것 — quick / magit 커밋 메시지 / 인라인 번역 / elfeed).
  `my/gptel-models` 한 줄이 SSOT다.

  `:models`를 명시하는 게 중요한 자리였다 — 안 넘기면 backend가 upstream 기본
  목록(9개, `gpt-5.2`까지)을 그대로 광고해서 메뉴가 안 쓰는 모델로 찬다.

  스펙은 다시 적지 않는다. `gptel--process-models`는 모델이 **cons cell로 올 때만**
  심볼 plist를 붙인다 — 맨 심볼은 빈 plist로 착지해서 메뉴가 컨텍스트·비용·capabilities를
  잃는다. `my/gptel--model-specs`가 upstream `gptel--openai-models`에서 스펙을 끌어오므로
  수치가 gptel을 따라가고, 모르는 모델은 에러 대신 맨 심볼로 degrade한다.

  검증: `gpt-5.6-{terra,sol,luna}` 모두 ctx 1050k / caps `(media tool-use json url
  responses-api)` 실측 확인. GLG 실사용 — elfeed 요약, magit 커밋 메시지.

### Removed

- gptel 레거시 백엔드 일체: DeepSeek, OpenRouter(하드코딩된 Gemini 4종 + `gpt-5.1-chat`
  스펙 75줄), Claude-Code docker wrapper(`docker/claude-wrapper/` + `enable_tools`
  advice + health check), CLIProxy(`lisp/ai-gptel-local-proxy.el`, `config.el`에서 이미
  주석 처리돼 있던 것). elfeed 6모델 번역 벤치마크(77줄)도 함께 — 백엔드가 하나면 비교할
  것이 없다. `my/gptel-switch-to-*` 4개는 `my/gptel-switch-model` 하나로 접었다.
  Gemini **이미지 생성**(`my/gemini-generate-image`)은 남겼다 — 채팅 모델이 아니다.

### Added

- **Neomacs 바닐라 프로파일 + K-review 프로브** (`neomacs/`, `bin/neomacs.sh`).
  Emacs 코어(~300K줄 C)를 Rust로 재작성한 [Neomacs](https://github.com/eval-exec/neomacs)
  위의 빌트인 전용 프로파일. Doom과 완전 분리 — 별도 `--init-directory`, 별도 server
  name, 공유 상태 없음. **Neomacs와 stock GNU Emacs 양쪽에서 동일하게 돌아야 한다**는
  게 핵심 규약이라, 갈라짐이 나왔을 때 `--gnu` 한 번으로 런타임 탓인지 우리 설정 탓인지
  갈린다. 프로브는 파일마다 별도 프로세스로 돌아 런타임을 죽이는 버그가 나머지 보고를
  막지 않는다 — 크래시 자체가 산출물이다.

  실측 (Neomacs 0.0.13 vs GNU Emacs 31.0.50): 61 OK / 4 FAIL, 실제 837노트 코퍼스는
  0 FAIL. **GUI는 아직 못 쓴다** (메뉴 흔들림) — 그래서 데일리 드라이버가 아니라 2주
  관찰 레인이 붙은 기록으로 들어간다. 갈라짐 2건은 재현 케이스로 프로브에 박아뒀다:
  #121의 뿌리는 핸드셰이크가 아니라 `:nowait` (blocking connect는 TLS를 협상하는데 async는
  안 해서 url-http가 평문으로 폴백 → ELPA 빈 archive), 그리고 `org-table-align`이 링크를
  표시 폭이 아니라 원시 대괄호 형태로 재서 링크 든 표를 부풀리는 것. upstream 이슈·PR은
  내지 않았다.
- `my/org-link-to-headline` — 같은 버퍼의 다른 헤딩으로 fuzzy `[[*Heading][desc]]` 링크.
  저널의 시각 헤딩들이 앞선 생각과 한 타임라인 위에서 엮인다. 이전 텍스트를 고쳐 쓰지
  않고, 소스 파일을 받은 에이전트는 이미 타깃 전체를 들고 있어 추가 fetch가 없다.
  `SPC m l h` (org localleader) / `SPC n M-l` (denote leader).
- Denote ID로 가든 짝 이동 — Org 소스 ↔ Hugo export ↔ 가든 미러 페이지 토글. prefix
  인자로 미러 선택, 모든 경로 ERT 커버.
- `tests/test-agent-denote-link.el` — 정규식을 소스에서 직접 읽어 게이트가 실제 코드와
  드리프트할 수 없게 한다.
- `tests/` 를 README 구조 트리에 노출. 키바인딩 절이 `test-keybinding-lint.el`을
  참조하는데 정작 트리에 없었다.

### Fixed

- **`evil-collection`이 껐던 RET 전송을 조용히 되살려놨다.** 커밋 `c9d9217`
  ("Prevent `RET` from sending message in gptel")에서 끈 동작이 되돌아와 있었다. upstream이
  gptel 전용 옵션 `evil-collection-gptel-want-ret-to-send`를 **삭제하고** REPL 공통
  `repl-submit` 추상 바인딩으로 옮겼는데, `evil-collection-repl-submit-state` 기본값이
  `normal`이라 normal state RET이 다시 `gptel-send`로 갔다. 죽은 `setq`는 아무 신호도
  주지 않아서 방패처럼 보였다. 지금은 `evil-collection-binding-overrides`의 per-map
  `:enabled` 람다로 gptel에서만 끈다 — cider/eshell/vterm은 그대로. 전송은 `C-c RET`
  (gptel이 minor mode 키맵에 직접 박아둔 기본), `M-RET`, `S-RET`(menu).
- 행(hang)에 빠진 agent/pi 데몬 복구. `stop`이 5s `timeout`으로 묶이고, 안 죽으면 PID
  강제 종료(`-9`까지 에스컬레이션) + stale socket 정리 후 `start`. 데몬 PID는
  start-anchored emacs cmdline으로 매칭해서 토큰을 언급한 셸 명령이 잡히는 일이 없다.
- `agent-denote-add-link`가 링크를 엉뚱한 섹션에 넣던 것. 표준 heading은 **붙여쓴
  `* 관련노트`**(코퍼스 442건)인데 옛 regex가 이걸 못 잡고 `관련메타`·`관련링크`·
  `관련 레퍼런스` 같은 형제 섹션을 오매칭했다. heading 전체를 앵커하는 regex로 교체.
- `rename-by-front-matter`가 새 경로 대신 rename 함수의 dired-buffer 반환값을 보고하던 것
  (`old -> old`으로 찍혔다). `denote-get-path-by-id`로 실제 경로를 보고한다.
- `denote-dired-mode` hook 복원. `e200ee2`에서 denote 3.x `-extras` require 마이그레이션과
  함께 지워진 뒤 돌아오지 않았고, 손으로 토글하지 않으면 Dired가 포맷되지 않았다.
  scoped 변종(`denote-dired-mode-in-directories`) 대신 무조건 hook을 쓴다 — scoped 쪽은
  `denote-dired-directories` 기본값이 `~/org/` 정확히라 정작 노트가 사는 하위
  디렉터리(`notes/`, `journal/`, `llmlog/`)를 빼고, `denote-silo-discover-repo-docs`가
  런타임에 찾는 silo도 못 따라간다. 비용은 무시할 만하다 — font-lock은 JIT이라 보이는
  영역만 칠한다. 71,696줄 `/nix/store` Dired 실측: 화면당 1ms, 줄당 ~18us, 앵커 매처는
  Denote 아닌 파일에서 첫 이름 검사에 빠진다.
- README의 은퇴한 `junghanacs` org 링크 2곳. 가든 리포는 `junghan0611/garden`이고 옛
  URL은 이미 `oldorg` remote로 밀려나 있었다.

### Docs

- README `One backend, three models` 절 — 왜 표면을 잘랐는지, terra/sol/luna 역할,
  `my/gptel-models` SSOT, 스펙을 손으로 안 베끼는 이유.
- 실측 수치 갱신: ~20K 라인, 3,500+ 노트, 2,200+ 게시, `lisp/` 43파일.
- `AGENTS.md` § Things to Watch 두 항목 — gptel 백엔드/모델 표면을 넓히지 않는다,
  그리고 evil-collection이 gptel 키를 가져가면 죽은 옵션부터 의심한다.
- upstream 대응 자세를 리포의 기본 자세로 명문화 — 상류가 움직이면 따라가는 게 우리
  일이다. `map!` prefix 재편은 감시 레인으로 열어뒀다 (Henrik이 `de2a3364a`에서
  doomemacs/modules 후속을 예고했고, 그러면 우리가 얹는 leader 그룹이 또 움직인다).

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
