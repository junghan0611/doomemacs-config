# NEXT.md — doomemacs-config

> 시작할 때 무엇을 할지 몰라서 발생하는 진행 정체를 막는다.
> 일정은 의미 없다. 적은 만큼 할 수 있는 만큼만 — 진행은 진행된다.

운영 baseline은 [AGENTS.md](AGENTS.md). 후속 작업 / 미완 검증은 여기에.

---

## TOP — ghostel + 한글 IME race condition (2026-05-26, in-flight)

ghostel은 우리 담당. fork branch `fix/korean-ime-commit` 운용 중.
upstream refactor 시 회귀 대비 — **이 NEXT.md + commit msg + 코드 주석**
세 자리에 진단/근거가 박혀 있어야 함.

ghostel repo: `~/repos/gh/ghostel`, current branch `fix/korean-ime-commit`
(2 patch commits: `8d3320d`, `d9c5b11`). doomemacs straight build:
`~/.emacs.d/.local/straight/build-30.2/ghostel/ghostel.elc`.

### 증상

wezterm → emacs GUI → ghostel buffer → pi-coding-agent 환경에서:

- **첫 턴**: 한글 입력 정상 ("테스트 입니다" + SPC + 마침표 모두 OK)
- **에이전트 응답 중 / 직후**: SPC 누르면 음절 사라짐, 또는 카오스 음절
  39바이트 한 번에 PTY 전송 (사례: "자갈ㅓㅏㅓㅏㅏㅏㅓㅏㅓㅏㅓ")

### race window 진단 (분신 검수 통과)

1. **hangul2-input-method**는 우리 IME wrapper(`ghostel--ime-wrap-input-method`)
   안에서 자체 `read-key-sequence` 루프 + `hangul-insert-character` →
   `self-insert-command`로 ghostel buffer를 **임시 작업장**처럼 사용.
2. 동시에 pi의 토큰 stream이 PTY로 도착 → ghostel filter의
   **immediate-redraw path** (`ghostel.el:5606`, output ≤ 256B + 직전 send
   ≤ 50ms전 조건) 가 `ghostel--delayed-redraw`를 **sync로** 호출.
3. `--delayed-redraw` 본체 (`ghostel.el:6628`) 가 native module
   `ghostel--redraw`로 buffer를 erase+reinsert. `quail-overlay` marker
   invalidated. `(inhibit-modification-hooks t)` 라 quail은 본인 overlay
   깨진 것도 모름.
4. wrapper의 `(buffer-substring before-point after-point)` 캡쳐가 stale
   buffer state를 잡아서 garbage를 PTY로 forward.

### 기존 한계 (왜 ghostel이 지금까지 못 잡았나)

`ghostel--active-preedit-overlay` (`ghostel.el:6366`) 는
`'(x-preedit-overlay pgtk-preedit-overlay)` 두 GUI native IME만 추적.
`quail-overlay` (emacs lisp IME — hangul, anthy 등 다 포함) 는 ghostel
설계 단계에서 시야 밖. `--capture-preedit-state`/`--restore-preedit-state`
도 동일.

### Invariant (이 patch가 박는 contract)

> **Lisp IME composition 중에는 terminal buffer rewrite 금지.
> PTY output은 libghostty pending으로 누적, composition 끝나면 처리.**

### Fix 디자인 v2 (검수 통과, 적용 대기)

3-commit, 분신 정정점 4개 반영 (no timeout, buffer identity dynamic
bind, GUI IME 분리, nil-safe overlay 검사):

**commit 1** — `ghostel--ime-lisp-composing-p` helper + dynamic flag.
behavior change 없음. wrapper에 `(let ((ghostel--ime-lisp-composition-buffer (current-buffer))) ...)` 추가.

**commit 2** — `ghostel--filter-output` immediate-redraw path
(`ghostel.el:5606`) 조건에 `(not (ghostel--ime-lisp-composing-p))` 추가.
sync race 차단.

**commit 3** — `ghostel--delayed-redraw` 본체 진입 (`ghostel.el:6574`
직후) 에 composition 가드 + reschedule. **timeout 없음** (correctness >
liveness — 1.5초 read-loop 사례 있어서 watchdog은 의미 약함).

각 commit msg에 분신 권고 6항목 박을 것: **증상 / race / 기존 한계 /
invariant / 위치 근거 / 경계**. upstream refactor 시 후속 에이전트가
주석만 보고 race window 진단을 복원 가능해야 함.

### Fix A (auto-reattach via post-command-hook) — 보류

분신 의견: root 잡힌 후 wrapper 탈락 snapshot이 재현되면 그때 별도
safety commit. 같은 묶음에는 비추천. "self-heal, not root fix"라 명시
필요.

### 현재 진행 상태

- ✅ debug instrumentation v1+v2 발사: `/tmp/ghostel-ime-debug.el`,
  `/tmp/ghostel-ime-debug-v2.el` (advice + variable-watcher + auto-reattach
  prototype). pi emacs (`/run/user/.../emacs/pi`) 에 살아있음.
- ✅ snapshot 캡쳐 완료 (broken state: `wrapped=nil`, `orig=hangul2-input-method`,
  `hangul-queue=[9 0 35 0 0 0]`).
- ✅ 분신 1차 (`task 66c9dee1`, $1.21) — 가설 절반 맞음 판정 + hangul2
  read-loop race 메커니즘 확정.
- ✅ 분신 2차 (resume `66c9dee1`, $0.56) — design v2 검수 통과 + 4개
  정정점 반영.
- ⏸ 코드 작성 안 함. GLG 결정 대기.

### 다음 한 걸음

1. `ghostel` repo `fix/korean-ime-commit` 브랜치에 3-commit으로 patch
   적용. 주석 깊이는 분신 권고 6항목 그대로.
2. `git push junghan0611 fix/korean-ime-commit` (origin = 자기 fork).
3. doomemacs straight rebuild — `doom sync` 또는 `M-x straight-rebuild-package ghostel`.
4. pi에서 한글 입력 재검증 — 에이전트 streaming 중 SPC + 한글 합성 깨지지
   않는지. wrapper 탈락 (`wrapped=nil`) 재현되면 Fix A 별도 commit 검토.
5. 검증 OK면 `/tmp/ghostel-ime-debug*.el` instrumentation 제거 (또는
   archive).

### upstream 회귀 시 어디 봐야 하나

- 5606 immediate-redraw condition이 refactor되면 → commit 2의 가드
  자리가 사라짐. 새 immediate path를 찾아 같은 predicate로 가드.
- `ghostel--delayed-redraw` 본체가 refactor되면 → commit 3의 진입부
  guard 위치 재조정. 핵심은 `(ghostel--redraw ghostel--term ...)`
  native module 호출 **전**에 가드가 박혀야 함.
- `ghostel--active-preedit-overlay`에 quail-overlay가 upstream에서
  공식 추가되면 → 우리 patch는 폐기 가능. predicate 통합 검토.

### 관련 자리

- 분석 세션: 2026-05-26 GLG-Claude (이 NEXT 항목)
- 분신 task id: `66c9dee1` (gpt-5.5, sync, resumable)
- ghostel repo: `~/repos/gh/ghostel`
- whimsical 스피너 자리: `~/repos/gh/agent-config/pi-extensions/whimsical.ts`
  (별도 fix 완료 — `setWorkingIndicator({frames: ["●"]})`)

---

## gptel / gptel-agent 모니터링 (2026-05-26 정렬)

OAuth Codex backend 활용 자리. 우회 advice 4종 + 설정이 `lisp/ai-gptel.el`
(`use-package! gptel-agent :config`) 에 영구 박힘.

상세 작업기록 + issue 답변 draft:
[[file:~/org/llmlog/20260526T140435--gptel-oauth-codex-활용-검증-작업기록__codex_gptel_gptelagent_llmlog_oauth.org][20260526T140435]]

### upstream 변경 시 advice 재검토 자리

| advice | 불필요해질 조건 | tracking |
|--------|-----------------|----------|
| `+gptel--codex-stream-advice` | `gptel` 의 `gptel-request` wrapping point 또는 stream 자리 설계 결정 | [gptel #1432](https://github.com/karthink/gptel/issues/1432), [gptel-agent #107](https://github.com/karthink/gptel-agent/issues/107) |
| `+gptel--codex-clear-max-tokens-advice` | `gptel-agent.el:690` 의 `gptel-max-tokens 8192` 무조건 박는 자리에 OAuth backend 가드 추가될 때 | [gptel-agent #108](https://github.com/karthink/gptel-agent/issues/108) |
| `+gptel--disable-tool-confirmations` | `gptel-agent` 의 16 default tool 의 `:confirm t` 자리 변경 시 (영향 가능) | — |
| `+gptel-agent--inject-user-prompt` | `gptel-agent` 의 `agents/*.md` frontmatter format 변경 시 (영향 가능) | — |

### 답변 push 완료 (2026-05-26)

- [gptel #1432 follow-up](https://github.com/karthink/gptel/issues/1432#issuecomment-4540436258) — #2 retract + #1 확정 데이터 + #107 cross-link + gptel-agent side note
- [gptel-agent #107 코멘트](https://github.com/karthink/gptel-agent/issues/107#issuecomment-4540436517) — ekattsim 자리 인사 + 우리 advice 가 subagent flow 도 자동 우회 (추정)
- [gptel-agent #108 신규](https://github.com/karthink/gptel-agent/issues/108) — `gptel-agent.el:690` `gptel-max-tokens` 8192 default 가 OAuth Codex 와 충돌

karthink 답변 대기. 미해결 회귀 (subagent flow nil propagation, ekattsim
second issue buffer lifecycle) 는 별개 issue 후보로 llmlog 안에 기록 — 데이터
더 모은 후.

---

## 가든 missing link 후속 검증 확장 (2026-05-19 발견, 안정 단계)

샘플: `/home/junghan/sync/org/botlog/20260517T211731--§lifetract...org` →
`~/repos/gh/notes/content/botlog/20260517T211731.md`

`run.sh verify`/`fix`가 relref만 본다. 다음 4종은 못 잡는다 — SEO에 직타.

| 패턴 | 예시 (md 라인) | 잡을 자리 |
|------|----------------|-----------|
| **A. `[label](~/repos/gh/xxx)`** 홈경로 markdown URL | `[lifetract](~/repos/gh/lifetract)` L137, `[nixos-config](~/repos/gh/nixos-config)` L135 | (1) Org 단 `[[file:~/repos/gh/REPO]]` → `[[https://github.com/junghan0611/REPO]]` rewrite hook (ox-hugo before-processing), 또는 (2) verify-relref.py 확장: standard md link regex `\]\((~\|/home/junghan\|file:)` 카테고리 추가 |
| **B. 사라진 알리아스 호스트** (`*.junghanacs.com`) | `https://geworfen.junghanacs.com` L113 → 실제 `agent.junghanacs.com` | host alias map (yaml/json) + verifier. Org 본문도 같이 점검할 수 있게 — denote-export hook로 사전 변환이 깔끔 |
| **C. dangling bracket** (`[desc]` 뒤에 `(`/`{` 없음) | L19, L32, L44 — 비공개 llmlog denote 링크가 가든에 export 안 돼 description만 남음 | verify-relref.py에 카테고리 `ORPHAN` 추가. `[!abstract]` 같은 callout/list marker는 제외. fix는 plain text 치환 (`[X]` → `X`) |
| **D. 메타 줄 silently drop** | Org L28-31 4개 메타(#건강/#워크플로우/#데이터로그/#데이터베이스) → md에 자취 없음. 대상 메타 md 4개는 가든에 **존재** | export 파이프라인 회귀. `denote-export.el` + ox-hugo 단에서 어떤 단계가 list item을 통째로 삼키는지 추적. 우선 1개 repro부터 |

### 작업 순서 제안

1. **D 먼저** — 회귀라 콘텐츠 손실. repro는 메타 8줄짜리 `20260517T211731.org` 하나 export → 4줄 사라지는 단계 식별. ox-hugo `--verbose` 또는 `org-export-before-processing-hook` 뒤 buffer dump.
2. **A — Org-side rewrite hook**: `lisp/denote-export-config.el` Section 2.3 (`my/org-rewrite-download-links`) 옆에 `my/org-rewrite-repo-file-links` 추가. `[[file:~/repos/gh/REPO/...]]` → `[[https://github.com/junghan0611/REPO/tree/main/...]]`. 단, `[[file:~/screenshot/]]` / `[[file:~/org/.attach/]]` 같은 첨부는 건드리지 말 것 (이미 figure 파이프라인이 잡음).
3. **B — host alias yaml**: `bin/host-alias.yaml` SSOT 한 파일. `geworfen.junghanacs.com: agent.junghanacs.com` 같이. verify-relref.py가 외부 URL도 한 번 훑어보게 확장. fix-mode는 자동 치환.
4. **C — ORPHAN 카테고리**: verify-relref.py에 dangling bracket scanner 추가. 일반 list item `- text` 와 `[!abstract]` callout, footnote `[^1]`, image alt `![alt]` 는 negative-lookahead로 제외.

### 안 잡을 부분 (의식적 제외)

- 외부 URL의 진짜 404 (geworfen은 alias라 잡지만, 살아있는 외부 도메인의 404는 별도 link checker 영역).
- Org-mode 안의 `[[denote:UUID]]` 자체 무결성 (denotecli 영역).

---

## 가든 위생 통합 검증 (보안 + SEO + link rot, 안정 단계)

링크 위생은 SEO만의 문제가 아니다. **내부 경로 노출 / 사라진 서브도메인 / URL 내 credential**은 보안 카테고리. notes 리포에 `gitleaks`(`.gitleaks.toml`)는 이미 있지만 secret 한정 — link 위생은 빈자리.

### 응집 원칙 — 검증은 doomemacs-config에서 끝낸다

notes 리포는 가든 빌더(Quartz/Hugo/...)가 바뀔 수 있다. doomemacs-config는 작성/내보내기 인프라라 안정적이다. 따라서:

- **검증 도구 의존성**: `flake.nix` devshell ← 여기서 lychee/python3+pyyaml 핀.
- **검증 진입점**: `./run.sh verify` / `./run.sh fix` 단 하나. 외부 cron / 별도 CI 없음.
- **검증 코드**: `bin/verify-*.py` ← 이미 자리 잡힌 패턴 확장.
- **정책 SSOT**: `bin/site-policy.yaml` ← Org-side hook과 md verifier가 같은 파일을 읽음.
- **notes 리포 의무**: `lint.sh` gitleaks 유지(secret은 콘텐츠 위생). link 검증은 가져오지 않는다.

### 도구 지도 (참고용 — 채택 도구만 응집)

| 도구 | 영역 | 채택 | 자리 |
|------|------|------|------|
| `gitleaks` (이미 있음, `notes/lint.sh`) | secrets | 그대로 | notes 측 |
| **lychee** (Rust) | 외부 link rot, redirect/deprecated 자동 추적, fragment까지 | ✓ | `flake.nix` devshell → `run.sh` 호출 |
| **자체 `verify-content.py`** (신규) | 가든 특화 패턴 매칭 | ✓ | `bin/` |
| `htmltest` / `markdown-link-check` | (lychee와 영역 중복) | ✗ | — |

### 도구로 못 잡는 — 가든 특화 패턴 (`verify-content.py` 영역)

1. **내부 경로 노출** — `~/repos/`, `/home/junghan/`, `file:///` (호스트 정보 누출 = 보안)
2. **사설 endpoint** — `localhost:`, `127.0.0.1`, `192.168.`, `10.\d`, `172.(1[6-9]\|2\d\|3[01])\.` (사설망 노출 = 보안)
3. **사라진 자기 서브도메인** — `geworfen.junghanacs.com` 같은 alias rot (subdomain takeover 가능성 = 보안)
4. **URL 내 credential** — `https?://[^/]+:[^@/]+@` (basic auth 누출 = 보안)
5. **dangling bracket** — `[desc]` 뒤 target 없음 (도구는 못 잡음. 깨진 링크조차 아닌 텍스트 잔재)
6. **deprecated 자기 도메인** — `*.junghanacs.com` 의 옛 alias map (자체 운영 SSOT만 안다)

### 세 자리 — Org 원본 + Org export hook + md 검증

> 핵심: **Org 원본 파일도 깨졌다.** export hook은 temp buffer만 변환 → GUI Emacs / denotecli / semantic-memory가 원본 읽을 때 여전히 깨진 링크. 따라서 **원본 일괄 정정이 먼저** 필요. export hook은 신규 작성 노트 안전망.

**Stage 1 — Org 원본 일괄 정정** (`bin/fix-org-links.el` 신규)
- emacs batch 실행. denote-export.el daemon 인프라 재활용.
- `bin/site-policy.yaml` SSOT 읽음 (Stage 2/3과 공유).
- 변환 규칙 (확정):
  - `[[file:~/repos/gh/REPO]]` → `[[https://github.com/junghan0611/REPO]]`
  - `[[file:~/repos/gh/REPO/path/to/file.el]]` → `[[https://github.com/junghan0611/REPO/blob/main/path/to/file.el]]`
  - `[[file:~/screenshot/...]]`, `[[file:~/org/.attach/...]]`, `[[file:~/org/...denote-id...]]` 는 **제외** (figure / attach / denote 내부 링크 보호)
  - `https://geworfen.junghanacs.com` → `https://agent.junghanacs.com` 등 host alias 적용
- 동작 방식 (확정): **dry-run 기본 + `--apply` 옵션**. `verify-relref.py --fix` 패턴 답습.
  - `./run.sh fix-org` → 변경 후보 모두 출력 → `y/N` → `./run.sh fix-org --apply`
- 안전:
  - `[[denote:UUID]]` 링크는 절대 건드리지 않음
  - 본문 code/verbatim/src block 내부는 제외 (Org parser 사용, 단순 regex 아님)
  - dry-run 출력에 파일별 before/after diff
- 호출 빈도: yaml에 alias 한 줄 추가될 때마다 일괄 재실행 (idempotent).

**Stage 2 — Org export-time hook** (신규 작성 노트 안전망)
- `lisp/denote-export-config.el` Section 2.3 옆에 hook 두 개:
  - `my/org-rewrite-repo-file-links` — Stage 1과 동일 변환 규칙, temp buffer에 적용
  - `my/org-rewrite-host-alias` — `site-policy.yaml` 의 host_aliases 사전 치환
- 같은 SSOT 읽음. Stage 1이 빠진 사이에 작성된 신규 노트도 export 시점에 보호.
- yaml 로드는 `defvar` 캐시 (worker당 1회).

**Stage 3 — md-side 검증** (가든 회귀 잡이)
- `bin/verify-content.py` 신설. `bin/site-policy.yaml` SSOT 읽음:
  ```yaml
  internal_paths: ["^~/repos/", "^/home/junghan/", "^file://"]
  private_endpoints: ["localhost", "127\\.0\\.0\\.1", "192\\.168\\.", "10\\.", "172\\.(1[6-9]|2\\d|3[01])\\."]
  host_aliases:
    "geworfen.junghanacs.com": "agent.junghanacs.com"
  deprecated_hosts: []          # 완전히 사라진 도메인 — 추후 확장
  credential_in_url: "://[^/]+:[^@/]+@"
  orphan_bracket: true          # dangling [desc] 검출 토글
  lychee:
    skip: ["mailto:", "tel:"]
    max_redirects: 5
  ```
- `./run.sh verify` 흐름 (확장):
  - [1/4] relref (`verify-relref.py --summary`) — 기존
  - [2/4] description (`check-description.sh`) — 기존
  - [3/4] figures (`verify-figures.py`) — 기존
  - **[4/4] content + lychee (`verify-content.py`)** — 신규. 패턴 6종 + 외부 link rot 통합.
- `./run.sh fix` 흐름 (확장):
  - [1/4] relref → [2/4] anchors → [3/4] figures (기존)
  - **[4/4] content** — host alias 자동 치환, dangling bracket → plain text. lychee 결과는 보고만(자동 외부 URL 수정은 안 함).

### SSOT 한 줄 추가 → 세 자리 즉시 반영

`bin/site-policy.yaml` 에 alias 한 줄 추가 →
- **Stage 1** (`fix-org-links.el --apply`) 재실행 → 원본 ~/org 일괄 정정
- **Stage 2** (export hook) → 다음 export부터 신규 노트 자동 보호
- **Stage 3** (`verify-content.py`) → 가든 md 회귀 잡음 + 자동 정정

이 SSOT가 핵심. 시간이 갈수록 alias map은 자라는 자산.

### 진행 상태 (2026-05-19)

**Stage 1a 완료**
- `bin/site-policy.el` — SSOT 초안 (host-aliases, github-user/branch, internal-paths, private-endpoints, credential-in-url, orphan-bracket, lychee)
- `bin/fix-org-links.el` — emacs batch fixer. org-element 기반 (denote/code/verbatim/src-block 자동 보호). dry-run + `--apply` + 로컬 부재 ⚠ 표시. `::N` → `#LN` 라인 anchor 보존.
- `run.sh fix-org` / TUI `O` — 실제 호출, dry-run 후 `y/N` prompt
- `flake.nix` — lychee + python3-pyyaml devshell 추가
- **`~/sync/org` 일괄 변환 적용 완료**: 17 files, 59 changes, ⚠ 6건 로컬 부재 (orgmode-skills, family-config 등 미동기화 repo — Stage 3가 가든 측에서 떨어뜨림)
- **doomemacs-config 커밋**: `476c774` feat(hygiene): add fix-org Stage 1a
- 잔재 검수: `~/sync/org` 전수에서 `[[file:~/repos/gh/...]]` 패턴 **0건**

### 설계 결정 — 검증 자리 분리 (2026-05-19)

~/org 원본과 가든 출력물은 다른 자리다:

| 자리 | 목적 | 깨진 GitHub URL 정책 |
|------|------|----------------------|
| **~/org 원본** | 사람이 보는 자리 / 도구 입력 | **일관성 우선**. file: → GitHub URL로 통일. push 안 됐어도 그대로 둠. 나중에 push되면 자동 회복. |
| **가든 (`notes/content/`)** | 외부 공개 / SEO / 보안 | **퍼블리시 차단 우선**. 404는 plain text로 떨어뜨림 (description만 남김) |

따라서 Stage 1b (별도 verify-org-links 도구)는 **만들지 않음**. 같은 lychee 검증을 가든 측 Stage 3 (`verify-content.py`)에서 한 번에 잡으면 구현 줄어들고 응집도 맞다. ~/org 원본은 깨끗하게 일관성만 유지.

**다음 작업 자리**
1. **`~/sync/org` 커밋** (별도 repo, 사용자가 직접) — 17 파일 변환 결과 영구화
2. **Stage 3 — `bin/verify-content.py`** 신규. `site-policy.el` SSOT 읽음. 패턴 6종 + **lychee로 가든 md 내 GitHub URL 200 OK 검증**. `--fix`는 404 → plain text 정정. `./run.sh verify` / `fix` 단계 [4/4] 추가.
3. 영향받은 노트 재export → 가든 deploy → Stage 3가 broken 6건 정리
4. **Stage 2 — `lisp/denote-export-config.el` export hook**. Stage 1 변환 함수 재사용 (신규 노트도 export 시 자동 일관성). lychee 호출은 안 함 (Stage 3 영역).
5. **D — 메타 4줄 silently drop** (TOP 회귀). 콘텐츠 손실이라 별도 작업.


### 도구 관할 + 빈자리 (2026-05-19 리뷰)

> 정체성: 링크 위생 문제를 **해결**한 것이 아니라 **다룰 수 있는 운영 표면을 세운 것**. Stage 1~3 tooling landed.

> **신뢰 조건 — `GITHUB_TOKEN`**: verify-org-links / verify-content의 lychee 기반 GitHub URL 검증은 token 없으면 **참고용 (advisory)** 이다. token 없이 나온 broken은 secondary abuse rate-limit으로 잘못 분류된 false positive를 다수 포함한다 (실측: ~/org 1420 URL 검증 시 token 없음 411 OK / 84 broken vs token+튜닝 1247 OK / 81 broken — broken 23 → 81로 늘었지만 의미는 정반대로, 이전 84 중 다수가 false positive였다는 뜻). **실제 ~/org 분류 작업은 token 있는 conclusive run 기준으로 한다.**

각 도구의 jurisdiction과 silent 회귀 위험 빈자리:

| 도구 | 자리 | 잡는 것 | 못 잡음 (회귀 위험) |
|------|------|---------|---------------------|
| fix-org-links.el | ~/org 정정 | `~/repos/gh/REPO[/path][::N]` → GitHub URL, host alias, `#LN` | 그 외 file: 경로, denote 깨짐, image, ORPHAN |
| verify-org-links.py | ~/org 검증 (read-only) | `[[github.com/USER/...]]` 404/410 | bare URL, ref-style, image, file:, GitHub 외 도메인 |
| verify-relref.py | 가든 relref | `{{< relref >}}` 6 카테고리, anchor 누출 | relref 외 md link, 외부 URL, 이미지 |
| verify-figures.py | 가든 figure | figure shortcode + `![](src)` 5 카테고리 | ox-hugo silent transform (query-string, NFD/NFC), 외부 이미지 404 |
| check-description.sh | 가든 frontmatter | description/abstract 누락 | 내용 품질 |
| verify-content.py | 가든 콘텐츠 위생 | HOST_ALIAS, INTERNAL_PATH, PRIVATE_ENDPOINT, URL_CRED, GITHUB_404 (md link만) | ORPHAN(비활성), bare URL, ref-style, image, GitHub 외 외부 URL, rate-limit inconclusive |

**빈자리 — 어느 도구도 잡지 못하는 회귀** (다음 follow-up 우선순위):

1. **bare URL 404** (md link 형태 아닌 평문 `https://...`) — verify-content/verify-org-links 둘 다 미검출. lychee scan 확장 필요.
2. **reference-style link** (`[text][ref]` + `[ref]: url`) — 패턴 자체 미인식.
3. **GitHub 외 외부 URL 404** (notion.so, slack archive, 블로그 등) — lychee `--include` 좁힘이라 미검사. 우선순위는 낮음 (대량 외부 검사는 rate-limit/timeout 영향).
4. **외부 이미지 404** (`![alt](https://example.com/img.png)`) — figure pipeline 영역인데 외부 검증 안 함.
5. **frontmatter 안의 URL** — yaml 안은 어느 도구도 안 봄.
6. **ox-hugo silent transform** — figure src에 query-string 추가, 한글 NFD/NFC 깨짐 같은 silent 회귀. UNKNOWN으로 잡힐 수도 ALIVE로 잘못 잡힐 수도.

### Follow-up 우선순위

**높음 (운영 직타)**
- **GitHub token 활용** — `GITHUB_TOKEN` env 또는 site-policy.el로. rate-limit으로 877 inconclusive였던 ~/org 검증을 완전판으로. 안 하면 매 호출 신뢰도 떨어짐.
- **bare URL + reference-style 검출 확장** — verify-content.py에 패턴 추가. 회귀 빈자리 #1 #2 즉시 메움.
- **ORPHAN 정교화** — 현재 비활성. 정교화 후 켜야 비공개 denote 잔재 잡힘. 조건:
  - 코드 펜스 ``` ... ``` 안 제외
  - inline code `...` 안 제외
  - Hugo shortcode `{{< ... >}}` 안 제외
  - description signature (denote-id, 화살표 →, 한글 어미 등)

**중간 (가든 위생 강화)**
- **외부 URL 404 옵션** — lychee `--include` 제한 풀고 전 URL 검사 모드. cron/주기 실행으로 분리.
- **외부 이미지 404** — verify-figures.py 확장 또는 verify-content.py에 image 카테고리.
- **ox-hugo silent transform regression test** — figure src 변환 사례 모음 + 비교 스크립트.

**낮음 (응집 정리)**
- **verify-org-links.py를 Stage 1.5로 명시** — 응집 미학 흐려짐 인정. 도구 헤더/메뉴 주석 갱신.
- **Stage 2 export-time hook** — Stage 1 함수 재사용. 신규 노트가 자동으로 일관성 유지.

### Stage 순서 — 첫 가동 시 (현황)

1. ✓ Stage 1a — PoC 1파일 + ~/org 전체 dry-run 완료
2. ✓ Stage 1a `--apply` — ~/sync/org 일관 변환 + 커밋 (476c774)
3. ✓ Stage 3 PoC — `verify-content.py` 패턴 매칭 + run.sh [4/4] 통합 (2cf5878)
4. ✓ Stage 3 lychee — GITHUB_404 카테고리 통합 (d092647)
5. ✓ 가든 사용자 수동 정리 + 재export — verify 0건. notes repo 11 파일 미커밋 (사용자 작업)
6. ✓ ~/org 매핑 + verify-org-links — 1418 URL → 84 broken, 877 inconclusive (d71b3c5)

---

## ghostel — 0.29.0 머지 후 닷파일 follow-up (2026-05-24)

upstream `dakra/ghostel` 0.27 → 0.29 (40 커밋, 3 릴리스) 우리 fork `fix/korean-ime-commit` 위에 머지 완료 (`junghan0611/ghostel` `505bdbe`). IME 패치 (`8d3320d` + `d9c5b11`) 충돌 0 — `lisp/ghostel.el` 라인만 1838 → 2304로 자연 이동. doomemacs-config 측 활용은 다음 자리.

### Step 0 — sync + 빌드 (user Emacs 자리)

`packages.el` L93~99이 `fix/korean-ime-commit` 픽업 중이라 자동 따라옴.

```bash
~/.config/emacs/bin/doom sync     # straight가 새 커밋 fetch
```
- user Emacs에서 `M-x doom/reload`
- `M-x ghostel` 첫 호출 → `ghostel-module-auto-install 'ask` 빌드 prompt → 0.29.0 zig 모듈 빌드 (libghostty C API → Zig API 전환됨)
- 빌드 후 기본 동작 확인: Pi 세션 띄우기 + 한글 IME 입력 + `evil-ghostel-escape 'terminal` 동작

### Step 1 — 자동 적용 변화 체감 (수정 0)

닷파일 변경 없이 활성화. 체감 후 회귀 있으면 자리에 기록:

- **`ghostel-query-before-killing` default `'auto`** — Pi/Claude/Codex 실행 중 buffer kill 사고 방지. OSC 133 C/D markers로 prompt 자리에선 quiet
- **`ghostel--filter-soft-wraps` O(n²) → O(n)** — Pi 긴 출력 copy mode `M-w` freeze 사라짐
- **CRLF normalization on alt screen 스킵** — tmux/vim/less 레이아웃 깨짐 해소 (DECSET 47/1047/1049 감지)
- **TTY auto-composition 비활성화** — 이모지 + VS-16 (🗂️) 컬럼 드리프트. Pi/Claude/Codex 출력에 직접
- **Linux framebuffer black-on-black** — `unspecified-fg/bg` 인식. laptop/nuc에서 `emacs -nw` 살아남
- **Bash OSC 7 hostname → `gethostname(2)`** — Toolbox/container에서 TRAMP 매 `cd`마다 발동 안 함 (#276)
- **Semi-char mode `M-<digit/punct/uppercase>/SPC` 셸 포워딩** — `M-.`, `M-1`, `M-/` 같은 키가 Pi/셸까지 닿음 (이전엔 `M-a..z`만, 나머지는 Emacs global commands가 가로챘음)

### Step 2 — 추가 후보 (하나씩 켜고 테스트)

세 자리 모두 `lisp/term-config.el`. 한 번에 다 켜지 말고 a → b → c 순서로 하나씩.

**a. `ghostel-glyph-scale-floor 1.0`** — CJK 자연 크기 (#298)
```elisp
;; lisp/term-config.el `(use-package! ghostel ... :config` 안
(setq ghostel-glyph-scale-floor 1.0)
```
- 0 (default) = strict-grid 유지. 1 = 자연 크기 (row 약간 높아짐 trade-off)
- buffer-local이라 ghostel 버퍼만. 다른 버퍼 영향 0
- 테스트: ghostel 세션 두 개 띄워서 0 vs 1 비교 — 한글/이모지 잘림 확인

**b. `global-ghostel-comint-mode 1`** — comint VT parser (0.29 큰 신규)
```elisp
;; lisp/term-config.el 끝에 (사용자가 직접 켜기, 자동화 X)
;; (global-ghostel-comint-mode 1)
```
- `M-x compile`, `M-x shell`, eshell, run.sh export/verify/fix 버퍼에서 truecolor + OSC 8 hyperlink + OSC 7 cwd
- **테스트 전 주의**: font-lock과의 상호작용. 켜기 전 `M-x compile RET ls --color=always RET` 같은 PoC로 색감/faint/inverse 깨지지 않는지 확인
- 깨지면 buffer-local enable로 좁힘 (`ghostel-comint-mode`만)

**c. Multi-terminal navigation leader binding 풀기** (0.28 신규)
```elisp
;; lisp/term-config.el L124~127 주석 자리 풀기 (조건부 — 빈도 누적 후)
(map! :leader
      :desc "Pi (ghostel)"   "j SPC" #'my/pi-ghostel-start
      :desc "Ghostel list"   "j l"   #'ghostel-project-list-buffers
      :desc "Ghostel next"   "j n"   #'ghostel-project-next
      :desc "Ghostel prev"   "j p"   #'ghostel-project-previous)
```
- 닷파일에 "leader binding parked while experimental" 의식적 명시. Pi 세션 동시 N개 띄우는 빈도 누적된 후 결정
- vterm이 daily driver 자리는 그대로 — `j SPC`는 Pi 전용 우회로

### Step 3 — 추가 검토 자리 (낮은 우선순위)

- **`ghostel-prompt-regexp`** — OSC 133 없는 셸 fallback. 기본값이 `$ # % > >>> λ ❯ ➜ →` 이미 인식. oracle/nuc/laptop SSH 첫 진입 자리에서 prompt detect 안 되면 사용자 prompt 패턴 추가
- **macOS `ghostel-macos-login-shell`** — 우리 자리 NixOS라 무관

### Step 4 — `fix/korean-ime-commit` upstream PR 자리 (보류)

`8d3320d Forward IME-committed text to PTY for Emacs input methods` + `d9c5b11 Restrict IME commit forwarding to PTY-forwarding input modes` 두 커밋은 commit message + commentary 잘 정리됨 (Hangul IME `(self-insert-command 1)` 직접 호출 → `[remap]` 우회 → PTY 미전송 → 다음 redraw에 erased 문제, gating 포함). dakra/ghostel에 PR 던질 자리는 있음. 단:

- 이전 PR 시도/토의 있었는지 먼저 확인: `gh pr list -R dakra/ghostel --search "korean ime author:junghan0611"` / `gh issue list -R dakra/ghostel --search "korean ime"`
- 일반 IME (non-Korean quail) 회귀 테스트 자리 정리 필요
- **보류 사유**: 한글 IME 패치는 우리 fork 자리에서 안정. dakra가 받지 않아도 fork로 충분. 우선순위 낮음 — 다른 작업과 트레이드오프

---

## gptel — 흡수됨 (2026-05-24)

이 섹션은 **TOP — gptel OAuth Codex 검증 트랙**으로 흡수. 본문은 그쪽 참조.

운영 노트만 남김:
- PoC 등록한 `get_current_time` tool은 user emacs 메모리 한정 — 재시작 / `doom/reload` 시 사라짐. 파일 수정 0건.
- `+user-info.el` "제안/대안/다음 단계 안내 하지 마세요" 한 줄 (commit `eea14bd`). gpt-5.4 후행 제안 톤 차단용.
- upstream issue: [karthink/gptel#1432](https://github.com/karthink/gptel/issues/1432).
- 우리 advice: `lisp/ai-gptel.el` `+gptel--codex-stream-advice`.

---

## README / AGENTS.md 현행화 (2026-05-21 큐)

`README.md`에 한국어/영어 혼용 자리 누적. 외부 가시성 큰 자리(L211~258 § Garden verify / fix + § Org hygiene 운영 문서)는 한 챕터 분량의 한국어로 작성되어 있음. L11 identity 표기, L67 bin/ 코멘트도 한국어.

### 자리 정리 — 전체 rewrite

부분 patch 아니라 **AGENTS.md와 현행화하면서 전체 다시 정리**.

- AGENTS.md의 영어/한국어 혼용 정책과 일치시킴
- Garden Verify/Fix · Org Hygiene · Garden Deploy Workflow 세 섹션은 AGENTS.md에 이미 영어 골격이 있으니 그것을 SSOT로
- README는 외부 first-touch 자리라 일관성 영어 톤
- 한국어 정체성 표기(GLG/힣)는 유지

### 범위

- `README.md` 전체 (235줄)
- 필요 시 `AGENTS.md` 의 가든 운영 섹션과 cross-reference 갱신
- `snippets/README.md`, `tests/README.org`는 별개 — 외부 가시성 낮아 후순위

### 절차

1. README와 AGENTS.md 같이 읽고 자리/구조 결정
2. 섹션 단위 rewrite (예: § Garden verify / fix를 영어로 + AGENTS.md와 일관)
3. dry-read 한 번 (외부 first-touch 시각)
4. commit + push

이런 정리는 한 번 하면 다음 현행화 사이클까지 안정. 다음 세션 초반 자리.
7. → **다음**: GitHub token 활용 또는 bare URL 확장 (회귀 빈자리 메움)

---

## Google Chat OAuth 토큰 만료 (2026-05-24)

`agenda-stamp.sh` 옆 chat notify가 `invalid_grant "Token has been expired or revoked"`. 한 줄 자리:

```bash
gog auth login    # 재인증
```

- **영향**: 커밋/머지 시 Google Chat 알림 누락. agenda 도장은 정상 작동.
- **회복**: 한 번 인증하면 자동. 다른 자리 영향 없음.
- **발견 자리**: ghostel 0.29.0 머지 푸시 후 chat notify 호출 (`505bdbe`).
