# NEXT.md — doomemacs-config

> 시작할 때 무엇을 할지 몰라서 발생하는 진행 정체를 막는다.
> 일정은 의미 없다. 적은 만큼 할 수 있는 만큼만 — 진행은 진행된다.

운영 baseline은 [AGENTS.md](AGENTS.md). 후속 작업 / 미완 검증은 여기에.

---

## TOP — 가든 missing link 후속 검증 확장 (2026-05-19 발견)

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

## TOP-2 — 가든 위생 통합 검증 (보안 + SEO + link rot)

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

## gptel — Codex 구독 백엔드 후속 (2026-05-21)

upstream issue [karthink/gptel#1432](https://github.com/karthink/gptel/issues/1432) 등록. karthink 답변 확인 후 두 자리 갈라짐:

### A. `+gptel--codex-stream-advice` 거취

- 현재: `lisp/ai-gptel.el` 안 `:around` advice로 `gptel-request :callback` 흐름에서 Codex backend 감지 시 `:stream t` 강제 + chunk 누적 → callback semantic 보존
- 이유: `gptel-request`의 `:stream` keyword default = `nil` (gptel-request.el:1962) + Codex endpoint stream=true 필수 → quick/magit/elfeed/embark 통합이 400으로 깨짐
- **다음 자리**: karthink 답변 따라
  - upstream fix 들어가면 → advice 제거 + commit
  - 답변에 "그건 이래야 한다" 설계 자리면 → advice 유지 + 코멘트 갱신
  - 답변 없으면 → 그대로 유지 (회귀 안전망)

### B. tool_use 무한 turn 자리

- 현재: gpt-5.4 + `get_current_time` PoC tool 등록 시 모델이 결과 받고도 final text 못 내고 같은 tool 무한 호출. `gptel-abort`로만 중단.
- 검증된 자리:
  - tool 정의/실행/회수는 정상 (`#+begin_tool` 블록에 결과 잘 들어감)
  - `gptel-use-tools` t/force 둘 다 무한
  - system message 단순화도 효과 없음 (`+user-info.el` 프롬프트도 단순 "call once then stop"도 동일)
  - `+gptel--codex-stream-advice`는 chat 흐름에서 미통과 (gate가 `:callback` 필요)
  - gptel에 `max-tool-cycles` / iteration cap **없음** (소스 0건)
- 원인 후보 (미확정 — karthink 답변 자리):
  - gpt-5.4 codex agent loop 성향 (final stop signal 못 냄)
  - Codex Responses API roundtrip에서 cycle 종결 신호 처리 자리
  - Doom + 우리 advice 환경 영향
- **다음 자리**: 답변 받기 전까지 `~/.claude/skills/` SKILL.md 폴더 → `gptel-make-tool` 자동 등록 본 구현 **보류**. 한 번 호출 = 한 번 답으로 끝나는 게 검증되어야 30+ skill 등록이 의미.

### 운영 노트

- PoC 등록한 `get_current_time` tool은 user emacs 메모리에만 등록 — emacs 재시작/`doom/reload` 하면 사라짐. 파일 수정 0건.
- `+user-info.el`에 "제안, 대안, 다음 단계 안내를 하지 마세요" 한 줄 추가 (commit `eea14bd`). gpt-5.4 후행 제안 톤 차단용.

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
