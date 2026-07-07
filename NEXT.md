# NEXT.md — doomemacs-config

> 시작할 때 무엇을 할지 몰라서 발생하는 진행 정체를 막는다.
> 일정은 의미 없다. 적은 만큼 할 수 있는 만큼만 — 진행은 진행된다.

운영 baseline은 [AGENTS.md](AGENTS.md). 후속 작업 / 미완 검증은 여기에.

---

## 🟢 완료 — ghostel 한글 IME 회귀: PR #510 **머지됨** + fork 은퇴 (Beat 1) 완료 (2026-07-07)

**현재 상태 (2026-07-07)**: PR **#510** https://github.com/dakra/ghostel/pull/510 **MERGED** — `1c70fc9`가 `dakra/ghostel main`에 들어감(SHA 보존 rebase 머지, upstream HEAD `eb806d1`). fork 은퇴 완료.
- v0.41.0+3(`f77efee`) 위로 rebase → CI 전부 SUCCESS (byte-compile 28.2/29.4/snapshot, test, test-evil, test-zig, build-native, melpazoid).
- dakra(owner)가 인라인 리뷰 1건 — "코드 코멘트 1~2줄로 줄여라 (상세는 test/git blame)". → 9줄 코멘트를 2줄로 trim, `--amend` + force-push (`adf2f02`→`1c70fc9`), 코멘트에 답글 완료.
- fork 브랜치 `junghan0611/ghostel:fix/lisp-ime-readonly-compose` @ `1c70fc9`. 로컬/origin/PR 동기.
- 로컬 full 검증: native 빌드(zig 0.15.2 = `/nix/store/n64wxy7nch118ggn9lbmda5a29jr775r-zig-0.15.2/bin`; 시스템 zig 0.16은 ghostty가 0.15.2 요구해서 실패) + `make test`/`test-native`/byte-compile/checkdoc 전부 green.
- **완료**: `packages.el` ghostel/evil-ghostel recipe를 fork → `dakra/ghostel :branch main`으로 전환 + straight repos remote 재정렬(프롬프트 회피) + `doom sync` (build @ `eb806d1`, `ghostel-ime-mode` 로드 확인). Beat 1 완료. 다음은 여유될 때 🟢 Beat 2 (공식 모듈 채택).

**근본 원인 (확정)**: 어제(2026-06-24) upstream `9e8460a Make ghostel buffers read-only`가
ghostel 버퍼를 `buffer-read-only = t`로 만들면서 한글이 깨졌다. **hangul 입력기가
`buffer-read-only` 변수를 직접 검사**한다:
```elisp
;; leim/quail/hangul.el — hangul2/hangul3/hangul390 전부 동일
(defun hangul2-input-method (key)
  (if (or buffer-read-only ...) (list key)  ; read-only면 조합 없이 키 패스스루
    ...compose...))
```
upstream이 read-only 적응하며 wrapper에 `inhibit-read-only t`만 감쌌는데, 그건 modification
배리어·text-prop만 가리고 hangul의 변수 직접 검사는 못 뚫는다 → hangul이 즉시 `(list key)`로
빠져 조합 안 함 → raw 키가 PTY로 forward. (live probe 실측: `pt-delta 0` + raw `d/k/j…` 전송.)

**수정 (발송됨)**: wrapper의 `(funcall original key)` 바인딩에 **`buffer-read-only nil` 추가**.
- fork 브랜치 `junghan0611/ghostel:fix/lisp-ime-readonly-compose` @ `b15cb5d` (force-push 완료)
- `lisp/ghostel-ime.el` 한 줄 + characterization test (`ghostel-test-ime-wrap-composes-in-read-only-buffer`,
  hangul read-only 게이트 모델링 — 수정 빼면 실패 확인)
- 검증: `make byte-compile` ✓ / `checkdoc`+`docquotes` ✓ / ime ert ✓ (10/10). live 실측 "김정한" forward 확인.
- `packages.el`: ghostel recipe를 이 fork 브랜치로 전환 (evil-ghostel은 dakra/main 유지).

**업스트림 재머지 (2026-06-27)**: 직전 `66a8778`은 orphan root commit이라 main과 공통 조상 없음.
업스트림이 `92bfcc5 Release version 0.39.0`까지 진행 → fork 브랜치를 **새 main 위 단일 커밋으로 재구성**.
- 방식: `git diff main fix -- ghostel-ime.el test/ghostel-ime-test.el`로 순수 추가분만 패치로 떠서
  main(`92bfcc5`) 위에 재적용 → 커밋 `b15cb5d` → `--force-with-lease`로 origin 갱신 (`66a8778...b15cb5d`).
- main 재확인: 0.39.0도 여전히 wrapper에 `inhibit-read-only`만 묶음 → **우리 수정 여전히 필요** (중복 아님).
- 결과: 브랜치가 깨끗한 PR-ready 모양 (최신 업스트림 + 우리 커밋 1개, +44/-1).

**다음 한 걸음**:
- [x] ~~업스트림 재머지 + PR 발송~~ → #510 발송 완료 (2026-07-06).
- [x] ~~owner 리뷰 대응~~ → dakra 코멘트 trim 요청 반영, `1c70fc9`.
- [x] ~~머지 대기 → Beat 1~~ → PR #510 머지됨 (2026-07-07). `packages.el` recipe fork → `dakra/main` 전환 + straight remote 재정렬 + `doom sync` (build @ `eb806d1`). fork 은퇴 완료.
- [ ] **(별개·미해결) reshow `number-or-marker-p nil`**: `my/ghostel-toggle` 재토글 때
      `my/ghostel--enter-insert` → `evil-ghostel--insert-state-entry` 커서 동기화가 재등장 타이밍에
      `ghostel--cursor-pos`/viewport-row nil 참조. 원인 커밋 후보 `3431d79`/`adac637`. 한글과 별개 트랙.
      급하면 `term-config.el`의 `my/ghostel--enter-insert` 호출 2곳 주석.
- **참조**: 아래 `## ghostel 한글 IME PR #343` (설계 SSOT, 재현 명령, GPT 백업 `~/.local/state/ghostel-ime-wip/`).

## 🟢 NEW — :term ghostel 공식 모듈 마이그레이션 (base 위임, 적당한 시점) (2026-07-01)

**배경**: Doom v3 재구조화로 모듈이 core → `sources/doom+`(doomemacs/modules 서브모듈)로 분리.
거기에 **공식 `:term ghostel` 모듈** 신설 (`.doommodule` since `26.07`, maintainer @hlissner,
`static/init.example.el`에 vterm을 "almost the best"로 강등하고 ghostel 등재). 우리는 이미
`lisp/term-config.el`(325줄)에 `use-package! ghostel`을 daily driver로 통합 — 지금은 우리 게 더 앞섬.
적당한 시점에 base를 공식 모듈로 위임하고 override만 남겨 유지보수 부담을 줄인다.

**하드 의존 (선행 게이트)**: 공식 모듈 `packages.el`은 `dakra/ghostel @ 0f0a9bd` pin.
우리 `packages.el:97`은 IME fix 위해 `junghan0611/ghostel` fork pin (위 🟡 IME 항목).
→ **IME PR이 dakra에 머지되기 전엔 공식 모듈 recipe 그대로 못 씀.** 두 경로:
- (a) IME PR 머지 후 → 공식 모듈 recipe 그대로 사용
- (b) 그 전엔 → 공식 모듈 켜되 우리 `packages.el`에서 `(package! ghostel :recipe ...fork)` override로 pin만 덮기

**공식 모듈이 덤으로 주는 것** (`+everywhere` 플래그, 우리엔 없음 → 획득):
comint 통합, `compilation-start` advice, `ghostel-eshell` visual-command, solaire face 연동,
persp `ghostel-buffer-name-function nil` 충돌 방지.

**유지 필수 (공식에 없음 → override 레이어로 남김)**:
- `ghostel-ime-mode`(한글), `evil-ghostel-escape 'terminal`(ESC 라우팅)
- popup rule + `my/ghostel-toggle`/`-here` — evil-insert 자동진입 + IME 활성화 포함.
  공식 `+ghostel/toggle`은 prefix-arg 경로에서 `buffer-name`을 변수로 잘못 참조(void)하고 insert/IME 없음.
- `my/pi-ghostel-start`, `term-sessions`(zmx), `my/zmx-launch`, `ghostel-eval-cmds`, OSC 9;4 spinner

**⚠️ 버전-lag 이슈 (핵심 — 2026-07-06 갱신)**: 공식 모듈 `packages.el`은 `dakra/ghostel @ 0f0a9bd`
(2026-07-01, "nushell integration") pin — **현 upstream main보다 22커밋 뒤 + 우리 read-only fix 없음**.
ghostel은 매우 빠르게 개발되어 Doom 모듈 pin이 항상 뒤처진다. → **공식 모듈을 켜더라도
`packages.el`에서 ghostel recipe를 unpin(override)해서 우리가 원하는 최신 버전을 써야 한다.**
이건 임시 우회가 아니라 상시 상태 — @hlissner이 pin을 우리 머지 이후로 bump하기 전까진 override 유지.
따라서 마이그레이션은 @hlissner 스케줄에 안 묶인다 (recipe override로 우리가 통제).

**두 박자로 분리 (묶지 말 것)**:

*Beat 1 — fork 은퇴 ✅ 완료 (2026-07-07)*:
- ~~`packages.el` ghostel/evil-ghostel recipe: fork → `dakra/ghostel :branch main`.~~ 완료.
  straight repos remote를 dakra로 재정렬(interactive 프롬프트 회피) 후 `doom sync` (build @ `eb806d1`, `ghostel-ime-mode`·`ghostel-ime-lisp-composing-p` 로드 확인).
- fork 유지보수 부담 소멸. "우리쪽 수정 최소화"의 절반 달성.

*Beat 2 — 공식 모듈 채택 (여유될 때, 리팩토링)*:
1. `init.el` `:term`에 `(:unless my/termux-p (ghostel +everywhere))` 추가 → `doom sync`
2. `packages.el`에서 ghostel recipe override로 `dakra/main`(또는 원하는 SHA) pin — 공식 모듈의 stale `0f0a9bd` unpin. **위 버전-lag 이슈 = 이 override 상시 필요.**
3. `term-config.el`(325줄) → override-only 레이어로 축소
4. 함수 충돌: `my/ghostel-toggle` + `SPC o t/T` 유지, 공식 `+ghostel/*`는 미바인딩(공존 무해)
5. 실익은 "코드 절감"보다 **유지보수 위임 + `+everywhere` 기능 획득**(comint/compilation/eshell/solaire/persp)

**참조**: 위 ghostel 한글 IME 항목 — PR #510 머지 완료 → **Beat 1 완료**. 남은 것은 Beat 2 (공식 모듈 채택, 여유될 때).

## 🟢 NEW — export-side frontmatter enrichment: 패키지 메타데이터 → md frontmatter 다리 (2026-06-29)

**한 줄**: 이맥스 패키지(denote/citar-denote/vc 등)가 **이미 구조적으로 들고 있는
메타데이터**를 ox-hugo export 산출물 md frontmatter로 흘려보내는 **다리(bridge)**를
export 파이프라인에 만든다. 단발 기능이 아니라 **반복 패턴** — 오늘 reference가 첫 사례,
git commit이 둘째. 앞으로 같은 틀로 늘어난다.

**설계 원칙 (GLG 명시, 이게 SSOT)**:
1. **날것 코드 금지, 구루 패키지 활용.** denote는 org 전용이 아니라 md(yaml/toml)
   frontmatter 포맷까지 추상화해뒀다(`citar-denote-file-types`). 이런 추상화를 직접
   재발명하지 말고 **그 위에 다리만 놓는다.** 패키지를 쓸수록 장기적으로 유연해진다.
2. **유연함이 코드에 드러나야 한다.** GLG가 원하는 방향 자체가 유연함이라, 코드가
   날것으로 가면 그 유연함이 사라진다. 패키지 경유 = 유연함이 구조로 보임.
3. **GLG가 도메인 정보를 준다.** 우리가 전부 파악하는 게 아니라, GLG가 어느 패키지·어느
   필드를 쓰는지 알려준다(오늘 대화처럼). 모르면 묻는다, 새로 만들지 않는다.
4. export 주입 지점은 `bin/denote-export.el` — 헤드리스 데몬이 export-config를 로드하니
   citar/citar-denote가 메모리에 떠 있다. post-export 블록(`date:` 박는 517-536) 옆이
   1차 후보. (pre-export `#+hugo_custom_front_matter:` 합성은 중첩 배열에 취약 → 비권장.)

**사례 1 — `#+reference:` → frontmatter → notes JSON-LD `citation` (footprints AEO)**:
- 배경: notes(v4) 담당이 넘긴 건. `#+reference:`(citar-denote 필드, citekey 보관)가 org
  1086 노트에 있으나 export md frontmatter엔 0건. ox-hugo는 이 키를 모르니 drop(의도적
  아님, 매핑 부재). notes Head.tsx는 frontmatter 데이터로 JSON-LD `citation` 배열을 짜려는데
  소스가 없음.
- 확정 사실: `#+reference:`는 **citar-denote 필드**(`citar-denote.el:157` `:reference-format`),
  ox-hugo 키워드 아님. 본문 BIBLIOGRAPHY(`csl-bib-body`)는 `[cite:@]`+`#+print_bibliography:`가
  만드는 **별개 본문 HTML 층** — frontmatter JSON-LD와 무관. footprints JSON-LD는 ox-hugo
  표준 경로로 못 얻음 → **frontmatter 주입이 유일 경로.**
- 재료(이미 있음): `citar-denote--retrieve-references(file)`(추출), `--generate-title`/
  `--format-author-editor`(메타 포맷), citar 메모리에서 citekey→{title,url} 해소(샘플 전부
  bib에 title+url 보유: `web-emacsageai`→"Emacs in the age of AI"+url 등).
- **스키마 확정 (notes + GPT + dexport 3자 합의)**: `refs: [{key*, title*, url, author, year, doi}]`
  (`*`필수). schema.org 매핑 url→url / author→author / year→datePublished / doi→identifier.
  bare citekey는 노이즈라 제외. **orphan(bib 미해소)·title 없음 = skip** — bare key가 public
  frontmatter에 절대 안 닿음. notes-side 수신부는 ~20줄(`{title,url}`→`citation`).
- **orphan 측정 (2026-06-29, config-bibfiles 8 .bib 대상)**: 1086 노트 / 고유 citekey 2271 /
  **해소 2247 (98.9%) / orphan 24 (1.1%)**. orphan 성격: 한글 청구기호, bib 미등록 도서, 끝에
  `:` 붙은 파싱 아티팩트. → **skip 정책 확정, 커버리지 98.9%면 충분.** (측정 명령은 커밋 로그
  `202c187` 이후 세션 기록 참조 — grep `^#+reference:` ~/org vs `@type{key,` bib 키 comm.)
- **vanilla core 완료**: `lisp/denote-export-refs.el` 순수 레이어 — `split`(order-preserving
  dedupe 포함) / `in-string` / `resolve` + `tests/fixtures/refs/`(가든 비의존 org 3 +
  sample.bib) + ERT 10개 green. **GPT 검수 통과**(byte-compile clean, nit 2개 — dedupe +
  `(require 'subr-x)` 명시 — 반영). 데이터 파이프라인은 재현 가능하게 고정. 남은 건 glue.

**사례 2 — git commit SHA → frontmatter (deprecated 감안 메타)**:
- 목적: "이 문서는 어느 리포의 어느 내용을 언제 수정한 것"인지 보이게. 문서는 다 deprecated된
  코드의 표현이므로 **그걸 감안하고 보라**는 메타 신호. 독자(사람+LLM)가 시점 기준을 알게.
- **①/② 의미 분리 확정 (GPT 제안·채택)** — 둘은 다른 신호라 스키마/lane을 가른다:
  - **① `doc_revision`**: 노트 파일(`~/sync/org`) 자체 마지막 커밋 = "문서 언제 손질됨"
    (freshness/provenance, schema.org `dateModified`). `vc-git` 파일 단위, 구현 쉬움.
  - **② `based_on`**: 노트가 가리키는 코드 리포(`~/repos/gh/...`) 커밋 = "어떤 코드 상태를
    설명" (deprecated 경고 본뜻, schema.org `isBasedOn`). 링크 파싱 필요, 무거움. 별도 lane.
  - notes JSON-LD에서 **②만 `isBasedOn`**, ①은 dateModified/freshness.
- 패키지: `vc.el`/`vc-git`이 1순위 (날것 shell git 금지).
- **미결: ① phase 0로 먼저냐 ② 본격이냐 — GLG 우선순위 입력 대기.**

**다음 한 걸음 (사례1 glue — vanilla core는 끝)**:
- [x] frontmatter 스키마 확정 — `refs:[{key*,title*,url,author,year,doi}]`
- [x] orphan 측정 — 98.9% 해소 / 1.1% orphan → skip 확정
- [x] vanilla core + 가든 비의존 테스트 (커밋 `202c187`)
- [ ] **glue 결선** (`bin/denote-export.el` / `denote-export-config.el`):
      ① citar-denote private API(`--retrieve-references`)를 **한 wrapper에 감싸** 호출
         (패키지 변경 시 한 군데만 고침 — GPT 지적).
      ② citar로 bib-index 공급 → `my/denote-export-refs-resolve`.
      ③ `yaml-encode`로 nested YAML 직렬화 (손으로 만들지 말 것 — GPT 지적).
      ④ `denote-export--enrich-frontmatter`로 post-export 주입(`date:` 블록 옆). **idempotent
         필수** — 재export 시 기존 `refs:` 블록 replace, 중복 삽입 금지 (GPT 지적, `date:`의
         "if not present" 패턴 동일).
      ⑤ batch summary 로그: `refs: N resolved, M unresolved`.
- [ ] PoC 1개 파일 export로 frontmatter 실증 → notes에 스키마 확정 통지
- [ ] notes-side 수신부(Head.tsx JSON-LD `citation`) — 통지 후 동결 해제
- [ ] 사례 2: git commit — GLG ①/② 우선순위 입력 후 설계 (별도 lane 분리)
- [ ] 다리 추상화: 사례 둘로 패턴 보이면 공통 enrich 훅 추출. 조급한 API 금지(AGENTS.md).

**협업**: notes(v4) Claude `@~/repos/gh/notes` (`20260629T111227-928e32`) + GPT
(`20260629T121901-98740b` @ this repo). 이 lane이 설계 SSOT. notes-side는 스키마 확정 통지
전까지 코드 동결(합의됨). 스키마/orphan/idempotent 합의 완료 — 남은 건 glue 구현 + PoC.

## TOP — lisp/ 리팩터 후속 큐: vanilla-first + export 정리 (2026-06-09)

**현재 원칙**: 처리 완료된 작업은 `CHANGELOG.md`로 이동한다. NEXT는 다음에 할 일만
남긴다. 상세 완료 로그는 `CHANGELOG.md`의 다음 릴리스 섹션을 본다.

**가이딩 센터**:
1. **로직은 vanilla**: `emacs -Q`에서 도는 함수 = Doom 의존 없는 로직 =
   패키지 추출 가능 = agent-server 재사용 가능. Doom 매크로(`map!`/`after!`/
   `use-package!`)는 glue·keybinding·package setup에 둔다.
2. **테스트가 게이트**: Tier A 리팩터 전 characterization test로 현재 동작을
   고정한다. `tests/run-tests.sh`는 `emacs -Q` ERT runner다.
3. **export는 리팩터 전 분류부터**: 가든 export는 live 인프라다. 호출 그래프와
   4분류를 먼저 문서화하고, 동작 보존 테스트 없이는 핵심 로직을 건드리지 않는다.

**lisp/ vs modules/ 판단**:
- 지금 목표는 `lisp/`를 Doom module처럼 정돈하는 것이다. 전부 `modules/`로 옮기지
  않는다. `lisp/`는 개인 `$DOOMDIR` concern 분리 자리이고, `modules/`는 켜고 끌 수
  있는 Doom module로 승격할 때만 쓴다.
- module 후보 조건: `doom!`에서 opt-in/out 가능, `packages.el`/`config.el`/
  `autoload/`/`doctor.el` 경계가 의미 있음, 다른 Doom 사용자도 쓸 수 있음,
  package dependency를 module 단위로 관리할 가치가 있음.
- 특히 `workflow-shared.el`, `denote-export-config.el`처럼 headless daemon
  (`bin/agent-server.el`, `bin/denote-export.el`)도 직접 로드하는 파일은 바로
  `modules/`로 옮기면 위험하다. 먼저 vanilla core와 Doom glue를 분리한다.
- `modules/.gitkeep`는 빈 사용자 module 디렉토리 보존용이다. Doom 2026-06-09
  구조 변경 후 `doom doctor`가 `$DOOMDIR/modules` 존재를 기대하므로 유지한다.

**다음 한 걸음** (우선순위):

- [ ] **stale reference 정정 — 함수라 보류**: `my/update-dblock-export-garden-all-parallel`
      (`denote-export-config.el:970`)이 존재하지 않는 `bin/denote-export-parallel.sh`
      를 찾음 → sequential fallback. interactive M-x surface. 제거 vs `run.sh export`
      위임 vs `.py` 경로 교정 중 GLG 결정 후 손댐. docstring의 옛 경로/`clean-run.sh`
      참조도 같이 정리.
- [ ] **denote-export test dead-path Tier 결정**: `test-denote-export.el`의
      `../+denote-export.el`은 없음. 실제 함수 `my/denote-link-ol-export`는
      `denote-export-config.el` 안에 있고 top-level `(require 'ox-hugo)` 때문에
      `emacs -Q` 불가. 억지 full-load 금지.
      - 선택 A: broken-link fallback 순수 분기 추출 → Tier A
      - 선택 B: ox-hugo/denote/straight 로드 별도 runner → Tier C
- [ ] **가이드 노트 Architecture 섹션 갱신**: `20251221T120044`의 옛 v2.0 서술이
      stale (`denote-export.sh` 진입, `lisp/denote-export.el` 오기, `docs/` 위치,
      mermaid run.sh 미언급). 새 헤딩1 호출 그래프와 일치하게 정정.
- [ ] **`denote-export-config.el` 함수별 live/dead 전수 분석**: 위 분류가 끝난 뒤.
      1045줄 8섹션 중 어디부터 볼지 GLG 지정. read-only 리스트 먼저, 제거는 별도.
- [ ] **다음 Tier A 표본 확산**: `denote-functions.el`(45/0) 순수함수 triage →
      characterization test. `tests/test-andenken.el` 문체를 기준으로 한다.
- [ ] **옵션**: andenken 소규모 리팩터 (`--search-sessions-window`의 `cl-loop` →
      `seq-map-indexed`)는 green 하에만. 백엔드 2e(`--view session`)가 더 큰 가치.

## telega `messageRichMessage` 렌더러 — telega/tdlib 버전업 시 주기 점검 (2026-06-18)

**상태**: ✅ 적용·live 검증 완료. **이건 깨지기 쉽다 — telega가 따라잡으면 점검·제거 대상.**

**갱신 (2026-06-22, tdlib 1.8.65)**: 순수 직렬화기를 `lisp/telega-rich-md.el`로
추출(`emacs -Q` 독립), advice/insert glue만 `ai-bot-config.el`에 남김. 회귀선 =
`tests/fixtures/rich/` **Txx 매트릭스**(실측 1.8.65 payload + golden, SSOT=MATRIX.org)
+ `tests/test-telega-rich.el`(`run-tests.sh` 자동 편입, 15/15). 갭이던 T07(sup/sub
→ 유니코드 `²`/`₂`)·T08(mark → `==..==`+face) 메움. **남은 비-✅: T13 각주·T14 수식
둘뿐, 둘 다 OpenClaw 봇쪽**(T14는 `$E`→`richTextCashtag` 봇 오파싱). draftMessage
신규 스키마 shim도 추가(root 목록 PP crash 우회). fixture 재캡처는 **user 소켓**
telega에서. (commits fc6d0ec, fe0745b)

**왜 만들었나**: TDLib 1.8.64가 리치 포맷 텍스트 메시지를 신규 content 타입
`messageRichMessage`(`message:richMessage`, `blocks:vector<PageBlock>`)로 보낸다.
telega.el은 아직 이 타입 렌더러가 없어 `telega-ins--content` pcase fallback이
`<TODO: messageRichMessage>`만 찍고, 봇(OpenClaw/Gemini) 메시지가 전부 깨졌다.
(telegram-desktop은 아예 표시조차 안 함 — 미지원.) 우리가 직접 markdown 소스
텍스트로 직렬화해 렌더.

**구현 요약** (모두 `lisp/ai-bot-config.el`):
- `my/telega-ins--content-rich-a` — `telega-ins--content`에 `:around` advice, content가
  `messageRichMessage`일 때만 가로채고 나머지는 원본 위임. (advice-add는 멱등)
- `my/telega--rich-rt->md` / `--rich-pb->md` / `--rich-blocks->md` — RichText/PageBlock을
  **markdown 소스 문자열로 직렬화**. 헤딩 `#`×size(1-6), 문단 사이 빈 줄, `**bold**`
  `*italic*` `` `code` `` `~~strike~~`(face 합성), `[text](url)`, `> ` 인용, `- `/`1.` 리스트,
  코드블록 펜스, `| | |` 테이블. WYSIWYG 아님 — 복사·에이전트 프롬프트용.
- 직렬화기는 전부 `cl-case (t)`로 **total** → 미지 타입도 inner text로 degrade, **절대
  에러 안 던짐**. (telega의 `telega-webpage--ins-pb`/`--ins-rt`는 `cl-ecase`라 신규
  타입에서 throw하고 root PP `telega-root--chat-known-pp`까지 깨뜨림 → 그래서 재사용 안 함.)

**버전업 시 점검 우선순위** (telega `git pull` 또는 tdlib 업그레이드 후):
1. **telega가 `messageRichMessage`를 지원하기 시작했나** — `grep -rn messageRichMessage`
   on telega.el 소스. 지원되면 **우리 블록(advice + 직렬화기 5개) 통째 제거** 후 telega
   기본 렌더와 비교. 이게 1순위 제거 트리거.
2. **PageBlock/RichText 필드 rename 추가** — 우리가 잡은 것: `pageBlockBlockQuote`
   `text→blocks`, `pageBlockListItem`/`pageBlockDetails` `page_blocks→blocks`. tdlib
   `td_api.tl`에서 컨테이너 블록 필드가 또 바뀌면 `--rich-pb->md`의 `(or :blocks
   :page_blocks)` 자리 갱신.
3. **신규 블록/리치텍스트 타입** — total fallback이 텍스트로는 잡아주지만 전용 렌더가
   필요한 게 생기면(`pageBlockTable` 류) `--rich-pb->md`/`--rich-rt->md`에 케이스 추가.
   확인: `td_api.tl`의 `= PageBlock;` / `= RichText;` 목록 vs 우리 cl-case.
4. **링크 클릭 복원(옵션)** — 현재 링크는 `[text](url)` **소스 텍스트**(클릭 불가).
   원하면 markdown-mode 류 font-lock으로 색만 입혀 클릭 살리는 follow-up.

**재현/검증**: live `user` Emacs에 파일 defun들을 재평가 → 가짜 `messageRichMessage`
plist를 `telega-ins--content`로 통과시켜 출력 확인. 실측 트리거: OpenClaw/Gemini 봇이
헤딩·리스트·테이블·코드블록 섞인 메시지를 보낼 때.

## ghostel 한글 IME PR #343 재작성·발송 완료, **메인테이너 리뷰 대기** (2026-05-29)

**상태**: ✅ **PR 발송 완료.** 재작성 → 단일 커밋 → force-push → 답글 → 닷파일 복원/검증 전부 끝. 이제 dakra/emil-e 리뷰만 기다림. 공이 메인테이너에게.

- **PR #343**: https://github.com/dakra/ghostel/pull/343 (OPEN, draft)
- **발송 커밋**: `2a9fd7d Add ghostel-ime-mode for Emacs Lisp input methods` — `fix/korean-ime-commit` 에 force-push (main=d3e3072 위 1 commit). 작업 브랜치 `feat/emacs-lisp-ime` 도 같은 커밋.
- **답글**: https://github.com/dakra/ghostel/pull/343#issuecomment-4571986049 (rewrite 요약 — 분리/minor-mode/comment trim/native drop/테스트 + pi·Claude Code daily-driver 확인)
- **검증**: `make byte-compile lint test` 전부 RC=0 (test-native 의 `ghostel-test-shell-integration` 만 환경실패 = NixOS `/bin/zsh` 부재, main 동일·무관). ime ert 8개 통과. **라이브 실측 통과** (pi + Claude Code streaming 중 한글 입력 정상).
- **닷파일**: `packages.el` github recipe(`:branch fix/korean-ime-commit`)로 복원 + `doom sync` (repos/ghostel @ 2a9fd7d, ghostel-ime.elc 빌드, terminfo 경고 해소). `term-config.el` `:hook (ghostel-mode . ghostel-ime-mode)` 영구 유지. (commit `f703f43`)

### 다음 한 걸음 (리뷰 응답)

- 메인테이너가 **변경 요청** 하면 → `feat/emacs-lisp-ime` 에서 수정 → 단일 커밋 유지 → `fix/korean-ime-commit` 에 force-push (같은 흐름).
- **머지되면** → `packages.el` 을 upstream `dakra/ghostel` recipe 로 전환할지 결정 (우리 fork patch 불요해짐). fork 운용 종료 판단.
- **거절(don't merge)** 이면 → fork `fix/korean-ime-commit` 유지, upstream merge 시 rebase 점검 (아래 "upstream 회귀 시 점검 우선순위").

### 설계 SSOT (회귀/리뷰 대응 시 참조)

- core 변경은 `ghostel.el` 단 하나 (+25/-1): `defcustom ghostel-inhibit-redraw-functions` (abnormal hook) + helper `ghostel--maybe-defer-redraw` + `ghostel--delayed-redraw` 진입에 한 줄 게이트. core 는 IME 를 전혀 모름.
- `ghostel-ime.el` (신규 170줄): opt-in `ghostel-ime-mode` minor-mode. commit-forward wrap + `ghostel-ime-lisp-composing-p` (quail-overlay) + install/uninstall + `--reassert` (post-command-hook depth 90, 외부 IM/state manager 자가복구) + `list` sentinel 가드.
- `test/ghostel-ime-test.el` (신규 217줄): ert 8개 (defer / mode wiring / list guard / reactivation / external-reset reassert / CJK 한·あ·中 / quail / native mode-switch 생존).
- 메인테이너 피드백 매핑: dakra "1 redraw line + 분리 + 루트 add-hook 금지 + minor-mode" / emil-e "terminal-live-p 류로 흡수 + comment trim" → 전부 충족. native module fallback 은 main 이 이미 `ghostel--load-module` user-error abort 로 동등 → drop.
- 견고성 버그 2개 (dogfooding): `list` sentinel 캡처 회피 / 외부 IM 매니저(evil 등) 우회 시 post-command reassert. 둘 다 ert 커버. evil 비종속 self-healing.
- GPT 자산 백업: `~/.local/state/ghostel-ime-wip/` (ghostel-ime.el 원본 + diff).
- 재현 명령: `cd ~/repos/gh/ghostel && make byte-compile lint test` + ime ert `emacs --batch -Q -L lisp -L test -l ert -l test/ghostel-test-helpers.el -l test/ghostel-ime-test.el --eval "(ert-run-tests-batch-and-exit t)"`.

---

### Upstream PR (2026-05-28)

- **PR URL**: https://github.com/dakra/ghostel/pull/343
- **Branch**: `junghan0611/ghostel @ fix/korean-ime-commit` → `dakra/ghostel main`
- **State**: OPEN, draft, mergeable (충돌 없음 — 0.30.0 + 118 commit churn 통과)
- **Title**: `[Draft] Forward IME-committed text and guard redraws during composition (CJK lisp IME)`

#### PR body — English original submitted

```markdown
Hi @dakra,

Before anything else — **thank you for ghostel**. This project is genuinely precious to me, and I mean that.

## A bit about who I am

I'm a Doom Emacs user and a CJK (Korean — Hangul) speaker, and I work daily in **both GUI Emacs and `-nw` (terminal) Emacs**. The `-nw` path matters to me as much as the GUI path. For years, getting Korean IME to behave reliably inside terminal-style buffers inside Emacs has been a constant low-grade pain. ghostel solved enough of that — and was clean enough underneath — that I rebuilt my terminal workflow around it.

I'm also a ghostty user, and honestly: I had been quietly thinking I might have to attempt this kind of project myself at some point. I'm very glad someone with your skill level got there first and built it properly. Truly grateful.

## How I use ghostel

- Doom Emacs config: https://github.com/junghan0611/doomemacs-config
- The exact ghostel setup I daily-drive (single SSOT for GUI + `-nw`): https://github.com/junghan0611/doomemacs-config/blob/main/lisp/term-config.el
- Main workload inside ghostel buffers: [`pi`](https://github.com/junghan0611/pi-shell-acp) (an ACP-based agent harness I develop) and Claude Code. Both are heavy streaming TUIs that emit small PTY chunks at high frequency — which turns out to be the exact race-condition trigger this PR addresses.
- Korean input method: `hangul2-input-method` via Emacs' built-in `quail`.

## What this PR proposes

Two related changes, ~170 lines total, **all in `lisp/ghostel.el`**, **zero native zig changes**.

### 1. Forward IME-committed text to PTY for Emacs input methods

Some Emacs input methods commit text by directly inserting into the buffer (`hangul-insert-character` → `(self-insert-command 1)` as a *function call* with `last-command-event` bound) instead of returning events from `input-method-function`. A function call bypasses `[remap self-insert-command]`, so the character lands in the ghostel buffer but is never sent to the PTY — the next redraw erases it.

Fix: locally wrap `input-method-function` in ghostel buffers. Around the original IME call, observe whether point advanced; if so the IME committed text by buffer insertion. Capture that text, delete it from the buffer, and forward it to the PTY as UTF-8. The shell echoes it back through the normal redraw path, so the buffer ends up consistent without racing the redraw. IMEs that already return events (most quail packages) pass straight through. Restricted to PTY-forwarding modes (semi-char, char); line mode untouched.

### 2. Guard `ghostel--redraw` against running mid-composition

When a TUI streams output via ~256B PTY chunks while the user composes Korean via `hangul2-input-method`, syllables vanish — or 30+ byte chaos sequences (literally like `자갈ㅓㅏㅓㅏㅏㅏㅓㅏㅓㅏㅓ`) get forwarded to the PTY.

Race: `hangul2-input-method` runs a `read-key-sequence` loop inside the IME wrapper, using the ghostel buffer as a scratch area; each key mutates the buffer via `hangul-insert-character` → `self-insert-command`. In parallel, agent output triggers `ghostel--filter`'s immediate-redraw path, which calls `ghostel--delayed-redraw` synchronously; that body invokes the native `ghostel--redraw` to erase + reinsert the buffer from libghostty's VT grid. `quail-overlay` markers get invalidated mid-loop while `inhibit-modification-hooks t` silences quail's notifications, and the wrapper's `buffer-substring` then captures stale text.

Existing limit: `ghostel--active-preedit-overlay` only tracks `x-preedit-overlay` / `pgtk-preedit-overlay` — GUI native IME. Emacs-lisp IMEs (`quail-overlay` family: hangul, anthy, …) were never in scope; `--capture-preedit-state` / `--restore-preedit-state` don't see them.

Fix: a small predicate `ghostel--ime-lisp-composing-p` (a dynamic flag set by the IME wrapper + a live `quail-overlay` check) guards every code path that can end in `ghostel--redraw` rewriting the buffer:

- The sync immediate-redraw branch in `ghostel--filter` — falls through to the bulk path during composition.
- The body of `ghostel--delayed-redraw` — split into `ghostel--delayed-redraw-body`, with the guard living in the renamed wrapper. Catches every caller (theme sync, PTY filter immediate path, PTY filter bulk timer, `M-x ghostel-force-redraw`, window resize) in one place.

**GUI native IME streaming behavior is intentionally unchanged** — the predicate excludes `x-preedit-overlay` / `pgtk-preedit-overlay`. Line mode is unaffected. While the symptom that surfaced this is Korean hangul, the fix uses `quail-overlay` (Emacs core), so it generalizes to **any lisp IME** — Japanese anthy, Chinese, …

Detailed race-window analysis, invariant, and refactor guidance for future-you are documented inline as commentary blocks (the `;; Lisp IME composition guard` section).

## Why this is a draft

I don't send PRs the moment I write something. I keep the patch on a fork branch, pull upstream into it on a regular cadence, and use the result as my daily driver — that's how I shake bugs out. ghostel is your main project; it deserves to be treated that way.

By now this series has survived **two upstream merges including the v0.30.0 release plus 118 commits**, including refactors that touched the exact same area (`9ff243f Centralize ghostel buffer terminal initialization`, `22e535a Break out invalidation logic out of redraw`) — with no merge conflicts and no behavioral regressions in daily use. That gives me reasonable confidence the placement is structural rather than coincidental.

That said, it stays as a **draft** until you weigh in. Things I'd love your judgement on:

- **Filesystem layout** — keep inline in `lisp/ghostel.el`, or move to a new `lisp/ghostel-ime.el`. I left it inline because every call site lives in `ghostel.el`, but I have no strong preference.
- **Squash policy** — currently 6 commits, preserving the diagnostic narrative (each commit body reads like a small race-condition write-up). Happy to fold into ~3 if you prefer fewer, larger commits.
- **Commentary tone** — I included a lot of "if upstream restructures X, the guard's intent is …" notes inside the file. Useful, or noise?
- **Tests** — the race is concurrent (PTY stream vs. IME compose loop), so I documented it as a commentary block rather than as ert. I can take a swing at a hypothesis-style test if you'd want one before merge.
- **Don't merge?** — entirely fine. Please just say so and I'll keep it on my fork. The main reason I opened the PR at all is so other CJK users running streaming TUI agents inside ghostel can find this.

## Branch / commits

- Fork & branch: [`junghan0611/ghostel` `fix/korean-ime-commit`](https://github.com/junghan0611/ghostel/tree/fix/korean-ime-commit)
- Foundation: `8d3320d Forward IME-committed text to PTY for Emacs input methods` (2026-05-07)
- Mode restriction follow-up: `d9c5b11 Restrict IME commit forwarding to PTY-forwarding input modes`
- Guard series: `3b518a0` (helpers, no behavior change) → `83f110c` (immediate-redraw) → `88cdb7a` (delayed-redraw safety net) → `db2484d` (commentary fix)

---

다시 한 번, 진심으로 감사합니다.

— @junghan0611
```

#### Maintainer feedback — @emil-e (co-maintainer with @dakra, 2026-05-28)

🔗 https://github.com/dakra/ghostel/pull/343#issuecomment-4561598593

> Hi @junghan0611, I'm a co-maintainer with @dakra. Thanks for the PR. I will need some time to review this more carefully since I'm pretty keen on avoiding increased complexity in this particular area, but here are some initial thoughts:
>
> - The blocking of redraw makes sense but there's a lot of code inhibiting redraws now, or at least it feels like that. I wonder if you can instead hook this into `ghostel--terminal-live-p` or something like that.
> - Many of the comments are quite wordy and could use quite a bit of trimming down.
>
> I do think proper support for Emacs IMEs is something we should have so would love to work with you to get this in. I don't have time right now for a more thorough review :)

**진짜 의미** (한 줄): "방향은 이쪽으로 잡아주세요. 그렇게 정리된 버전이 올라오면 그때 본격 review 합니다."

- `I'm pretty keen on avoiding increased complexity in this particular area` — **강한 가드레일** (현재 모양이 그쪽 기준 과다)
- `I wonder if you can instead hook this into terminal-live-p or something like that` — **architectural direction** (예시 함수까지 지목, "기존 헬퍼에 흡수")
- `I do think proper support for Emacs IMEs is something we should have` — **방향성 승인**
- `would love to work with you to get this in` — **협업 의지**

→ 일반적인 OSS PR review 흐름 1단계 (`skim → direction → contributor revises → maintainer thorough review`)의 정확한 자리. 공이 contributor에게 넘어왔음.

#### My reply (2026-05-28)

🔗 https://github.com/dakra/ghostel/pull/343#issuecomment-4561763793

> Hi @emil-e — thanks for the direction and the warm reception. Both points received.
>
> I'll read `ghostel--terminal-live-p` and the surrounding gating helpers carefully before I touch anything — the goal is to see whether the lisp-IME predicate can be absorbed into existing terminal-state machinery cleanly, the way you suggested. If that consolidation works the new call site in `ghostel--filter` and the `--delayed-redraw` split both go away, and the diff shrinks a lot.
>
> On the wordy comments — agreed, I'll trim to a single small block and move the race-window narrative to the commit body where it stops aging.
>
> I'll work at my own pace and force-push to this branch when the rewrite is ready. No need to ping me — just take a look whenever you have bandwidth.
>
> Thanks for the path.

#### Maintainer feedback — @dakra (owner, 2026-05-28 08:59 UTC)

🔗 https://github.com/dakra/ghostel/pull/343#issuecomment-4562357898

> In addition to @emil-e's comment, I had a very quick peek.
> It looks like there is a unrelated feature in there when native module loading fails. You can create a new PR with only that please (1 commit on top of main).
>
> The ime changes look very small but also besides the 1 redraw inhibit line completely independent. If we can somehow make it like @emil-e suggested that we don't directly need to call one of your new ime function, this could be a completely separate `ghostel-ime.el`.
>
> I'm also never a fan of adding hooks or changing someones Emacs just by loading a file. So imho the 2 `add-hook` calls on the root scope should not be there. How are other packages handling a situation like this normally? Could we make a minor-mode and tell users that want it to just do something like `:hook (ghostel-mode . ghostel-ime-mode)` or is it that every user always want this and we should just always activate / add-hook in the `ghostel` main function?
>
> Anyway, you can force push on this branch. Make a new PR, 1 commit, for the native module load feature, and this PR, 1 commit, on top of main.

**액션 4개**:
1. **별개 PR 분리** — native module load fallback (`5c89e8c fix(module): abort when native install fails`) 는 IME와 무관 → main 위 1 commit 새 PR.
2. **파일 분리** — redraw inhibit 1줄만 남기면 IME 코드는 독립 → `lisp/ghostel-ime.el` 로 완전 분리. 단 조건: core 가 IME 함수를 **직접 호출하지 않아야** 함.
3. **루트 스코프 add-hook 2개 거부** — "파일 로드만으로 사용자 Emacs 바꾸는 패턴 싫다." → `ghostel-ime-mode` minor-mode + `:hook (ghostel-mode . ghostel-ime-mode)`. "다른 패키지는 어떻게?" 직접 질문.
4. **이 PR force-push, main 위 1 commit.**

#### 확정 작업 분해 (코드 실사 완료 2026-05-29)

`lisp/ghostel.el` 실제 구조 확인 후 설계 확정. 두 메인테이너가 **같은 축**을 가리킴 — emil-e "redraw inhibit 흩어진 느낌, terminal-live-p 류로 흡수" = dakra "1 redraw inhibit line 외엔 독립적".

**핵심 발견 — 가드 한 자리는 이미 중복**:
- 현재 가드 2자리: `ghostel--filter` (L5801 `(not (ghostel--ime-lisp-composing-p))`) + `ghostel--delayed-redraw` wrapper (L6779 cond).
- filter 의 immediate-redraw path 는 **결국 `ghostel--delayed-redraw` 를 sync 로 호출** (L5807). 그 wrapper(L6779)가 composing 시 reschedule 하므로 **filter 가드(L5801)는 중복**.
- → L5801 가드 제거 + filter 는 항상 `ghostel--delayed-redraw` 호출. redraw inhibit 이 **단 한 줄**(L6779 wrapper)로 수렴. 3b518a0~88cdb7a 의 split 도 단순화.

**dakra "직접 IME 함수 호출 안 하게" 푸는 열쇠 — generic predicate hook**:
- core 에 `ghostel-inhibit-redraw-functions` (predicate list / abnormal hook) 변수 1개 도입.
- `ghostel--delayed-redraw` wrapper 는 `(run-hook-with-args-until-success 'ghostel-inhibit-redraw-functions)` 로 게이팅 — **core 는 IME 를 전혀 모름**.
- `ghostel-ime.el` 의 minor-mode 가 켜질 때 `ghostel--ime-lisp-composing-p` 를 그 hook 에 add. 완전 분리 + dakra 의 "directly call your ime function 회피" 동시 달성.
- 이게 emil-e 의 "hook into terminal-live-p or something" 의 정확한 구현형 (terminal-live-p 자체는 `(not copy-mode)` 라 IME 와 직교 — 흡수 대상은 그 함수가 아니라 그 **류의 게이팅 머시너리**).

**`ghostel-ime.el` 로 옮길 블록** (`ghostel.el` L2328~2448, defvar + 5 defun + add-hook 2개 전부):
- `ghostel--ime-original-input-method-function`, `--ime-lisp-composition-buffer`, `--ime-redraw-defer-pending` defvar
- `ghostel--ime-lisp-composing-p`, `--ime-wrap-input-method`, `--ime-install`, `--ime-uninstall`
- 루트 add-hook 2개 → **삭제**, minor-mode 안으로.

**`ghostel-ime-mode` minor-mode 설계** (buffer-local):
- on: `input-method-activate/deactivate-hook` 에 install/uninstall 을 buffer-local 등록 + `ghostel-inhibit-redraw-functions` 에 predicate 등록.
- off: 모두 제거 + `input-method-function` 원복.
- 사용자: `:hook (ghostel-mode . ghostel-ime-mode)` (opt-in).

**minor-mode 질문 답변 방향** (dakra "다른 패키지는?"):
- minor-mode 쪽이 정답. GUI native IME(x/pgtk preedit)는 upstream 이 이미 처리 → 이 기능은 **CJK lisp IME 사용자만 필요**, 전원 필요 아님. opt-in 이 맞다.
- Emacs 관용: 대부분 IME-aware / 환경 변경 기능은 minor-mode opt-in (evil-mode, corfu-mode, electric-pair-mode 등). "load 만으로 환경 바꾸지 마라"는 Emacs 커뮤니티 표준 원칙 — dakra 직관과 일치.
- 답변 톤: 두 옵션 다 가져가지 말고 minor-mode 로 합의 (봇 분석 동의).

#### A 폐기 — upstream 수렴 (2026-05-29)

dakra 가 "별도 PR 로 빼라" 한 native module load feature(`5c89e8c fix(module): abort when native install fails`)를 main 에 cherry-pick 시도 → 충돌. 확인 결과 **main 이 이미 같은 의도를 더 견고하게 구현**:
- `ghostel--compile-module` (main L756): 이미 성공 `t` / 실패·file-missing·error `nil` 반환 — 5c89e8c 핵심과 동일.
- `ghostel--load-module` (main L913): docstring "load failures signal `user-error' so the calling flow aborts". `prompt-user` 시 module-load 실패·module 없음 둘 다 `user-error` abort. ensure 반환값 대신 file-exists-p 로 판단해 "compile 성공했는데 file 없음" 케이스까지 잡음 (5c89e8c 보다 견고).

→ **PR 불필요. upstream 이 기다리는 사이 독립 수렴.** [[denote:20260220T113741][pi-mono lockSync]] 패턴 재현. dakra 답글에 "native module 부분은 upstream main 이 이미 동등 구현, 별도 PR 불필요" 한 줄. #343 을 main rebase 하면 `5c89e8c` 자동 제거되어 IME 만 남음.

봇로그 [[denote:20260529T084444]] "기여의 두 양식" 시리즈에 **세 번째 사례**로 추가 가치 — 같은 fork 에서 "보낸 PR(IME)" 과 "안 보내도 되는 것으로 판명난 부분(native module)" 이 한 PR 안에 공존했고, upstream sync 가 후자를 자동 해소.

#### 브랜치 전략 (repo 실사 2026-05-29)

`main`(5bce751, 최신 upstream — v0.31.0 이후)이 `fix/korean-ime-commit` 의 **정확한 base** (`git merge-base main HEAD == main`, `HEAD..main` 비어있음). dakra 가 원하는 "1 commit on top of main" 을 그대로 만들 수 있는 깨끗한 상태. main..HEAD 의 우리 고유 변경은 IME 6커밋 + native module `5c89e8c` 둘 뿐(나머지는 머지로 따라온 upstream).

- **A (native module PR)**: `git switch -c <name> main` → `5c89e8c` cherry-pick → main 위 1 commit → 새 PR. 리스크 작음, 먼저 떼면 #343 이 IME 만 남아 정리됨.
- **B (IME PR 재작성)** — GPT 2차 자문 확정 (worktree + #343 유지):
  - **작업 위치**: main 기반 **작업용 새 브랜치** `feat/emacs-lisp-ime` 를 **worktree** 에서. user Emacs 의 `fix/korean-ime-commit`(IME 작동) 안 흔들림. in-place reset 은 daily-driver 위험만 큼.
  - **PR 브랜치**: 완성 후 그 1 commit 을 `fix/korean-ime-commit` 에 force-push → **PR #343 유지** (reviewer context 보존, dakra force-push 명시 허가). 새 PR 로 갈아타지 않음.
  - **범위**: commit-forward(`8d3320d`) + redraw-guard(`88cdb7a`) **한 commit 유지** — 코드상 분리 가능하나 둘 다 있어야 "한글 IME + streaming TUI" race 완전 해결 (commit-forward 만: mid-redraw race 잔존 / redraw-guard 만: 직접 insert 텍스트 PTY 미전송 잔존). 최소 범위 = core hook 1 + `ghostel-ime.el` + opt-in mode + squash 1.
  - **#343 코멘트**: "native module split 시도했으나 main 이 이미 동등/더 강하게 구현해 rewrite 브랜치에서 drop" 투명 설명.

#### 기능 보존 보장 = 테스트 (GLG 핵심 우려)

> "메인테이너 요구사항(minor-mode/분리) 때문에 IME 지원을 제대로 못하게 되면 다시 고민" — 형태 변경이 race fix 를 깨뜨리지 않음을 **테스트로 증명**한다. race 전체(PTY stream ↔ IME compose loop concurrent)는 ert 재현이 어려우므로, **메커니즘의 부품을 단위 테스트**로 고정:

| 검증 | 태그 | 내용 |
|------|------|------|
| minor-mode on/off | elisp | on → `input-method-function` wrap + `ghostel-inhibit-redraw-functions` 에 predicate 등록 / off → 둘 다 원복·해제 |
| **opt-in 보장** | elisp | mode 안 켜진 ghostel buffer → wrap/predicate **둘 다 비활성** (dakra "load 만으로 환경 바꾸지 마라" 직접 검증) |
| predicate 로직 | elisp | `ghostel--ime-lisp-composing-p`: dynamic var bound 시 t / 살아있는 `quail-overlay` 시 t / 둘 다 없으면 nil |
| commit forward | elisp | buffer 직접 insert(self-insert) 시뮬 → wrap 이 delete + `ghostel--send-string` 호출 (send mock). 8d3320d 핵심 |
| redraw defer | **native** | composing 상태 → `ghostel--delayed-redraw` 가 body(native redraw) 호출 안 함 + reschedule / non-composing → body 호출. 88cdb7a 핵심 |

#### CJK 일반화 + 테스트 (GLG 핵심 우려 — "한글 대표로 처리하면 일·중도 되나")

`ghostel--ime-lisp-composing-p` 는 `quail-overlay` + wrap dynamic var 기반이라 **input method 종류와 직교**. 두 메커니즘이 서로 다른 IME 유형을 커버:
- **commit forward (8d3320d)**: buffer 에 직접 insert 하는 IM(hangul `hangul-insert-character` → `(self-insert-command 1)` 직접 호출). events 반환 안 하는 류.
- **redraw guard (88cdb7a)**: `quail-overlay` 쓰는 모든 lisp IM — pinyin/japanese quail 은 물론 hangul.el 도 overlay 사용.

**batch `-Q` 로드 검증 완료** (2026-05-29): 세 IM 다 내장(leim) 으로 batch 에서 activate 가능 →
- `korean-hangul` (hangul2, 독자 IM, `hangul-input-method-activate`) — dynamic var 경로
- `chinese-py` (pinyin, `quail/PY`) — quail-overlay 경로
- `japanese` (`quail/japanese`) — quail-overlay 경로

→ 테스트에서 셋 각각 activate 후 composing 진입 시 predicate 가 t 인지 증명. **이게 "CJK 대표 처리" 의 증거이자 PR 의 일반화 주장 뒷받침** ("symptom 은 한글이지만 fix 는 quail-overlay/Emacs core 라 anthy·중국어로 일반화" 를 코드 주석 아닌 테스트로).

#### 테스트 인프라 (실사 완료)

- 자리: `test/ghostel-ime-test.el` 새 파일. `TEST_BASES := $(notdir $(basename $(TEST_FILES)))` → **자동 포함** (`make test` / `test-native`).
- 분류: ERT `native` 태그. 순수 elisp(빠름, module 불요) 는 무태그, native redraw 검증만 `(tag native)`.
- 헬퍼: `ghostel-test--with-compile-buffer` (ghostel-mode buffer, native 불요), `ghostel-test--with-terminal-buffer` (terminal 붙음, native 필요).
- 실행: `make test` (elisp), `make test-native` (module), `make test-all`.

#### Emacs 표준 인터페이스 — 배포판 비종속 (GLG 동의)

minor-mode + buffer-local `input-method-activate/deactivate-hook` 등록 + core 의 generic predicate hook. Doom/spacemacs 등 어느 배포판도 모르는 순수 Emacs core 인터페이스. 사용자는 `:hook (ghostel-mode . ghostel-ime-mode)` 한 줄. 우리 `lisp/term-config.el` 도 그 형태로 따라감 (닷파일도 표준 인터페이스 사용).

#### 작업 순서

| # | 작업 | 대상 |
|---|------|------|
| ~~A~~ | **폐기 (2026-05-29)** — main(5bce751)이 이미 동등 구현. PR 불필요 (아래 "A 폐기" 참조) | — |
| B0 | **worktree** 에 main 기반 작업 브랜치 `feat/emacs-lisp-ime` 생성 (user Emacs 의 `fix/korean-ime-commit` 안 건드림). 완성분만 마지막에 force-push | worktree |
| B1 | L5801 filter 가드 제거 (중복) | #343 |
| B2 | core 에 `ghostel-inhibit-redraw-functions` (defcustom abnormal hook, predicate 는 `(&optional buffer)`) 도입. **`--delayed-redraw-body` split 폐기 → 기존 `ghostel--delayed-redraw` 에 guard 한 줄만 인라인** (GPT 리뷰: maintainer 복잡도 우려 → private body 함수가 노이즈). `(run-hook-with-args-until-success 'ghostel-inhibit-redraw-functions buffer)` | #343 |
| B3 | IME 블록(L2328~2448) → `lisp/ghostel-ime.el` 이동 | #343 |
| B4 | `ghostel-ime-mode` minor-mode 정의, 루트 add-hook 제거. **enable: buffer-local IM hook + inhibit hook 등록 + 이미 IME 활성 buffer 면 즉시 `ghostel--ime-install`** / **disable: hook 제거 + 현재 IMF 가 wrapper 일 때만 원복 + stale `--ime-original-input-method-function` clear** (deactivate-input-method 가 hook 전에 IMF 를 nil 로 만들 수 있음 주의) | #343 |
| B5 | **`test/ghostel-ime-test.el` 작성** — 위 부품 테스트 + CJK 3종 일반화 | #343 |
| B6 | 인라인 코멘트 1블록(≤10줄) 트리밍, race-window narrative → commit body | #343 |
| B7 | `make test`→`make test-native`(native redraw defer)→CJK predicate 확인. **구조 완성 직후 중간 수동 실측 1회** (시행착오 축소) | worktree |
| B8 | 최종 polish + **force-push 직전 수동 실측 1회**(emacs -nw + WezTerm + pi streaming + 한글) → `feat/emacs-lisp-ime` 의 1 commit 을 `fix/korean-ime-commit` 에 force-push | #343 |
| B9 | dakra/emil-e 답글 — minor-mode 합의(corfu/company/flyspell/yas/electric-pair/evil 예시) + CJK 테스트 + **native module drop 투명 설명** + 분리 완료 보고 | 댓글 |

#### GPT-5.5 리뷰 반영 (2026-05-29, task 66c9dee1 resume)

이전 race 분석을 함께한 분신(gpt-5.5)이 오늘 계획을 read-only 검토. 방향 승인 + 수정 2점 + 사실 1건.

**실측 확인**:
- **hangul2-input-method 가 실제로 `quail-overlay` 를 쓴다** (`quail-setup-overlays`/`quail-overlay`/`read-key-sequence`/`self-insert-command`). → predicate 의 quail-overlay 의존 타당. 5(a) 리스크 해소.

**수정 2점** (위 B2/B4 에 반영):
1. `--delayed-redraw-body` split 폐기 → 기존 함수에 guard 한 줄 인라인 (복잡도 최소).
2. minor-mode enable 시 "이미 활성 IME 면 즉시 install" 누락 금지. disable 시 stale original 정리.

**답변 방향 확정**:
- generic hook: `terminal-live-p` 에 직접 섞지 **말 것** (copy-mode freeze 의미와 일시 redraw defer 가 섞여 의미 흐려짐). 별도 `defcustom :type 'hook` abnormal hook 이 맞다. emil-e 의 "terminal-live-p 류로 흡수" 의도는 "흩어진 inhibit 을 한 게이팅으로 수렴" 이지 그 함수 자체가 아님 — 우리 해석과 일치.
- CJK 테스트 표현: "race 완전 재현" 이 아니라 **"race 를 구성하는 두 메커니즘(commit-forward[직접 insert IME] / redraw-guard[quail-overlay lisp IME])을 단위로 고정"**. native 태그 redraw-defer 테스트가 후자의 핵심 증거.
- dakra minor-mode 질문 답글 예시: `corfu-mode` / `company-mode` / `flyspell-mode` / `yas-minor-mode` (major-mode hook opt-in), `electric-pair-mode` / `evil-mode` (명시적 enable). "모두 항상 필요" 보다 "필요한 사용자가 `ghostel-mode` hook 에서 켠다".
- commit body race narrative: 5줄로 압축 (scratch 사용 → streaming redraw → state 깨짐 → inhibit hook defer → opt-in mode). 코드 주석은 더 짧게.
- (d) GUI native IME 충돌 없음: predicate 가 preedit overlay 를 안 보고, hook 기본 nil 이면 core 동작 불변.

**GPT 최종 권고 6단계** (우리 작업표와 일치): native 별도 PR → IME main 위 1 commit → ghostel.el 엔 public hook + guard 한 줄만 → 구현은 ghostel-ime.el + opt-in mode → filter 가드 제거 → delayed-redraw split 없이 guard 삽입.

**재고려 트리거**: B5 테스트에서 (1) minor-mode 형태로 wrap/guard 가 안 걸리거나, (2) CJK 3종 중 일부가 predicate 에 안 잡히면 → 형태를 다시 고민. 테스트가 이 자리를 잡아주는 안전망. 통과 못 하면 force-push 안 함.

**주의 — force-push 전 사용자 확인**: ghostel 은 별도 repo + 외부 공개 PR. 코드 재작성 + force-push + 답글 publish 는 GLG 결정 자리. B8/B9 전 diff·답글 초안 GLG 확인.

### Background — 우리 fork 운용 (적용 + 검증 완료, 2026-05-26)

ghostel은 우리 담당. fork branch `fix/korean-ime-commit` 운용 중.
**ghostel 업데이트가 빨라서 upstream merge 시 우리 patch와 맞춰야 함.**
회귀 대비 — 이 NEXT.md + commit msg + 코드 주석 세 자리에 진단/근거 박힘.

- 우리 자리: `~/repos/gh/ghostel` @ `fix/korean-ime-commit`
- straight clone: `~/.emacs.d/.local/straight/repos/ghostel` (동일 fork origin)
- build: `~/.emacs.d/.local/straight/build-30.2/ghostel/ghostel.elc`
- 브랜치 commits (이전 2 + 신규 4):
  ```
  db2484d  Fix function-name and pending-buffer wording in IME guard commentary
  88cdb7a  Defer delayed-redraw during lisp IME composition (safety net)
  83f110c  Suppress immediate-redraw during lisp IME composition
  3b518a0  Add lisp-IME composition guard helpers (no behavior change)
  d9c5b11  Restrict IME commit forwarding to PTY-forwarding input modes  (이전)
  8d3320d  Forward IME-committed text to PTY for Emacs input methods       (이전)
  ```

### 증상 (해결됨)

wezterm → emacs (-nw 또는 GUI) → ghostel buffer → pi-coding-agent 환경에서
에이전트 응답 중 / 직후 한글 입력 시 SPC 누르면 음절 사라짐, 또는
39바이트 카오스 음절 PTY 송신 (실측 사례: `"자갈ㅓㅏㅓㅏㅏㅏㅓㅏㅓㅏㅓ"`).

### race window 진단 (분신 검수 통과 + 실측 확정)

1. `hangul2-input-method`가 우리 IME wrapper 안에서 자체
   `read-key-sequence` 루프 + `hangul-insert-character` →
   `self-insert-command`로 ghostel buffer를 임시 작업장처럼 사용.
2. 동시에 pi 토큰 stream 도착 → `ghostel--filter`의 immediate-redraw
   path (`ghostel.el` 의 `(if (and (> ghostel-immediate-redraw-threshold 0) ...))`
   분기, output ≤ 256B + 직전 send ≤ 50ms전 조건) 가
   `ghostel--delayed-redraw`를 **sync로** 호출.
3. `--delayed-redraw` 본체가 native module `ghostel--redraw` 호출 →
   buffer erase+reinsert. `quail-overlay` marker invalidated.
   `(inhibit-modification-hooks t)` 이라 quail은 자신의 overlay 깨진
   사실도 모름.
4. wrapper의 `(buffer-substring before-point after-point)` 캡쳐가 stale
   state 잡아서 garbage PTY 송신.

### 기존 한계 (왜 ghostel이 못 잡았나)

`ghostel--active-preedit-overlay` 는 `'(x-preedit-overlay pgtk-preedit-overlay)`
두 GUI native IME만 추적. `quail-overlay` (emacs lisp IME — hangul,
anthy 등) 는 ghostel 설계 시야 밖.
`--capture-preedit-state` / `--restore-preedit-state` 동일.

### Invariant (이 patch가 박는 contract)

> **Lisp IME composition 중에는 terminal buffer rewrite 금지.
> PTY output은 `ghostel--pending-output` 에 누적, composition 끝나면
> deferred redraw 가 native parser로 flush.**

### 의도된 시각적 동작 (버그 아님)

한글 composition 중에는 ghostel buffer rewrite가 defer되므로:

- 에이전트 토큰 stream 도착해도 화면 갱신 안 됨
- **pi 스피너가 일시 정지된 것처럼 보임**
- composition 끝나면 (음절 commit / SPC / RET / 비한글 키) 누적된 frame이
  한 번에 flush → 스피너가 최신 위치로 점프

invariant의 직접 시각화. correctness > liveness tradeoff.

### Fix 구성 (적용 완료, 4-commit)

| SHA | 제목 | 변경 |
|-----|------|------|
| `3b518a0` | Add lisp-IME composition guard helpers (no behavior change) | helper 도입, dynamic flag, commentary block (`;; Lisp IME composition guard`) |
| `83f110c` | Suppress immediate-redraw during lisp IME composition | `ghostel--filter` 의 immediate-redraw 분기 조건에 `(not (ghostel--ime-lisp-composing-p))` 추가 |
| `88cdb7a` | Defer delayed-redraw during lisp IME composition (safety net) | `ghostel--delayed-redraw` 를 wrapper로, 본체를 `--delayed-redraw-body` 로 분리. wrapper 에서 composing 시 reschedule |
| `db2484d` | Fix function-name and pending-buffer wording in IME guard commentary | 분신 3차 review 정정 (`filter-output` → `filter`, "pending in libghostty" 표현 정정) |

각 commit msg에 분신 권고 6항목 박힘:
**증상 / race / 기존 한계 / invariant / 위치 근거 / 경계 / upstream refactor guidance**.

### 테스트 결과 (2026-05-26 실측)

| 라운드 | 환경 | 결과 |
|--------|------|------|
| 1 | semi-char + emacs -nw + WezTerm + pi streaming + 정적 스피너 | ✅ 한글 입력 정상, race 없음 |
| 2 | 위 + **default 12fps 스피너** 복원 | ✅ 여전히 정상. composition 중 스피너 일시 정지 → composition 끝나면 jump (의도된 동작) |

특히 emacs -nw에서도 작동 — race가 ghostel 본인 코드 내부에 있어서
frame mode (GUI / -nw) 와 무관하다는 강한 증거.

### whimsical.ts 원복

`~/repos/gh/agent-config/pi-extensions/whimsical.ts` 의 STATIC_INDICATOR
호출 제거 (commit `47bafb7`). root cause가 ghostel 쪽에서 잡혔으므로
워크어라운드 불필요. 코드 안에 history 코멘트 박음.

### Fix A (auto-reattach via post-command-hook) — 보류

분신 의견: root 잡힌 후에도 `wrapped=nil` snapshot이 재현되면 그때 별도
safety commit. 같은 묶음에는 비추천. 현재 테스트 통과로 **불필요 확정**.

### **upstream 회귀 시 점검 우선순위 (중요)**

업스트림이 다음 자리를 refactor 하면 우리 patch가 깨질 수 있음.
`git fetch upstream && git merge upstream/main` 후 점검 순서:

1. **`ghostel--filter` 함수** (현 `lisp/ghostel.el:5640`): immediate-redraw
   분기 condition 변경 시 → commit `83f110c` 의
   `(not (ghostel--ime-lisp-composing-p))` 가드를 새 condition 자리에
   옮김. 의도: **PTY filter → delayed-redraw 의 모든 sync path 차단
   (composition 중)**.

2. **`ghostel--delayed-redraw` 와 `--delayed-redraw-body` 분리** (현
   `lisp/ghostel.el:6641` 직후): 본체가 refactor되거나 native module
   호출 자리가 바뀌면 commit `88cdb7a` 의 wrapper/body 분리 구조를 새
   형태에 맞춰 옮김. 의도: **모든 caller에서 native `ghostel--redraw`
   진입 전 composition 가드**.

3. **`ghostel--active-preedit-overlay`** (현 `lisp/ghostel.el:6366`):
   upstream이 여기에 `quail-overlay` 또는 lisp IME overlay를 공식
   추가하면 → 우리 patch 폐기 가능. predicate 통합 검토.

4. **`ghostel--ime-wrap-input-method`** (현 `lisp/ghostel.el:2304` 부근):
   우리가 이미 patch 한 자리 (이전 2 commit + 신규 1 commit).
   upstream에 IME wrap 자체에 다른 mechanism 도입되면 그쪽으로 통합.

5. **`ghostel--capture-preedit-state` / `--restore-preedit-state`**: lisp
   IME overlay 보존이 upstream에 추가되면 commit `3b518a0` 의 helper
   에서 quail-overlay branch 빼기 가능.

### 진행 상태 (2026-05-26 18:XX 시점)

- ✅ debug instrumentation v1+v2 발사 (advice + variable-watcher +
  auto-reattach prototype). pi emacs 에 살아있음. 회귀 진단용.
- ✅ snapshot 캡쳐 + 분석 (broken state: `wrapped=nil`,
  `orig=hangul2-input-method`, `hangul-queue=[9 0 35 0 0 0]`).
- ✅ 분신 검수 3 pass (`task 66c9dee1`, gpt-5.5, 총 \$2.42): 가설 검증 →
  design v2 정정점 4개 → 코드 final review 정정점 2개.
- ✅ 4-commit 작성 + push (origin/fix/korean-ime-commit).
- ✅ straight clone sync 완료.
- ✅ 실측 검증 통과 (2 라운드 — static / default spinner).
- ✅ whimsical.ts 원복 commit + push (agent-config `47bafb7`).
- ⏸ debug instrumentation 정리 (`/tmp/ghostel-ime-debug*.el`) — 다음
  upstream merge 회귀 진단용으로 archive할지, 제거할지. 일단 살림.

### 관련 자리

- 분석 세션: 2026-05-26 GLG-Claude
- 분신 task id: `66c9dee1` (gpt-5.5, sync, resumable, 3-pass review)
- ghostel repo: `~/repos/gh/ghostel` @ `fix/korean-ime-commit`
- agent-config whimsical 원복: commit `47bafb7`
- debug instrumentation: `/tmp/ghostel-ime-debug.el`, `/tmp/ghostel-ime-debug-v2.el`

---

## andenken sessions client — 2e 머지 대기 (2026-05-28 신규)

`lisp/andenken-config.el` (346줄) — andenken sessions 트랙 한정 얇은
이맥스 클라이언트. 시간축 두 명령만:

- `M-x andenken-search-sessions-today` — 오늘 KST 일일창
- `M-x andenken-search-sessions-this-week` — 이번 주 (Mon~Sun) KST
- `M-x andenken-status` — 인덱스 통계 확인

두 명령 모두 prefix `C-u` = calendar 픽 (어제는 여기서). query 비우면
`--mode recent` (시간 내림차순, embedding skip). 채우면 `--mode hybrid`
(윈도우 안 semantic + BM25).

### 결정 (2026-05-28)

- **인터페이스 더 안 늘림.** "복잡하면 사용하지 않겠다는 것" — GLG 명시.
- **md 트랙 안 봄.** 글 형식이라 retrieval 기대치 다름. sessions 한정.
- **다축 합성은 `/recall` skill 자리.** andenken 은 windowed
  single-axis retrieval. boundary 유지.

### 알고 있는 한계 (백엔드 자리)

지금 한 프로젝트 메시지가 화면 잡아먹어 multi-repo 조망 안 됨. limit 30
어제 윈도우 봐도 같은 sessionFile chunk 가 반복됨. wrapper 측 아니라
andenken 백엔드 자리 — `andenken/NEXT.md` 2e "session-as-unit
windowed view".

### 다음 한 걸음

1. **andenken NEXT.md 2e 머지 후** — `search-sessions` 호출에
   `--view session` 추가. 5분 작업. 인터페이스 그대로. 자리:
   `lisp/andenken-config.el` `andenken--search-sessions-window`.
2. **키바인딩** — 사용 패턴 누적 후 `SPC n s` (search) 또는
   `+default/find-in-notes` 옆 결정. 일단 안 잡음.
3. **gptel 연계 (B 단계)** — 분산된 사실 종합이 필요해질 때.
   `andenken-ask` 한 명령: top-K chunks → gptel-request → 답 +
   출처. 단답형은 preview 본문에서 잡힘.
4. **`/recall` skill 활용 검증** — "어제 멀티리포 뭐했지" 같은 다축
   질문을 실제로 recall 로 던져서 boundary 가 작동하는지 확인. 분리
   유지의 정당성.

### upstream 면 (andenken)

`andenken/NEXT.md` Sub-plan 진행 순서 (GLG 정리, 담당자 SSOT):

| 단계 | 내용 | 상태 |
|---|---|---|
| 2e | session-as-unit windowed view (retriever 후처리 + cli `--view session`) | 진입점 |
| 2b | corpus noise threshold (시뮬 병행) | 시뮬만 |
| 2a | parsePiLine compaction schema fix + targeted reindex | 2e 다음 |
| 2c | golden 측정 (entwurf 결과 / multi-repo 의미연결 / entwurf 흐름) | pending |
| 2d | derived signals 인덱싱 (entwurf_task_id / commit SHA 등) | deferred |

2e 머지되면 알림 → 우리 측 한 줄 작업.

### 참조 자리

- 노트 (history + 설계 결정): `~/sync/org/notes/20250214T145633--§andenken-이맥스-임베딩-검색...org`
- 백엔드 SSOT: `~/repos/gh/andenken/NEXT.md`
- CLI wrapper: `~/.claude/skills/semantic-memory/SKILL.md`
- 다축 합성 면: `~/.claude/skills/recall/`

---

## gptel / gptel-agent 모니터링 (2026-05-26 정렬)

OAuth Codex backend 활용 자리. 우회 advice 4종 + 우리 `prompts/gptel-agent.md`
(subagent-free 변형) 가 `lisp/ai-gptel.el` (`use-package! gptel-agent :config`)
에 영구 박힘.

상세 작업기록 + issue 답변 draft:
[[file:~/org/llmlog/20260526T140435--gptel-oauth-codex-활용-검증-작업기록__codex_gptel_gptelagent_llmlog_oauth.org][20260526T140435]]

### subagent 회피 결정 (2026-05-26)

`+gptel--codex-stream-advice` 안에서 tool-only turn nil propagation 을 분기로
잡으려던 1·2차 시도는 모두 stream→non-stream 변환 layer 안의 우회. 본질은
default `agents/gptel-agent.md` 435줄에 박힌 DELEGATE/Agent 가이드 때문에 LLM
이 매 요청마다 subagent 호출을 시도하는 자리.

**결정**: `+gptel--codex-stream-advice` 의 cond 분기 수정은 모두 revert. 대신
`prompts/gptel-agent.md` (337 줄, subagent-free 변형) 를 우리 repo 에서 관리.
`gptel-agent-dirs` 에 우리 dir 을 append → `gptel-agent--update-agents` 의
`setf (alist-get name ... equal)` 가 같은 이름 entry 를 자연스럽게 덮어씀.
frontmatter `tools:` 리스트에서 `Agent` 도 빼서 preset 활성 시 tool slot 도
비활성 — system prompt + tool slot 두 자리에서 차단.

skills 인프라 + 다른 14 default tool 은 그대로 유지.

### upstream 변경 시 advice 재검토 자리

| advice | 불필요해질 조건 | tracking |
|--------|-----------------|----------|
| `+gptel--codex-stream-advice` | `gptel` 의 `gptel-request` wrapping point 또는 stream 자리 설계 결정 | [gptel #1432](https://github.com/karthink/gptel/issues/1432), [gptel-agent #107](https://github.com/karthink/gptel-agent/issues/107) |
| `+gptel--codex-clear-max-tokens-advice` | `gptel-agent.el:690` 의 `gptel-max-tokens 8192` 무조건 박는 자리에 OAuth backend 가드 추가될 때 | [gptel-agent #108](https://github.com/karthink/gptel-agent/issues/108) |
| `+gptel--disable-tool-confirmations` | `gptel-agent` 의 default tool 의 `:confirm t` 자리 변경 시 (영향 가능) | — |
| `+gptel-agent--inject-user-prompt` | `gptel-agent` 의 `agents/*.md` frontmatter format 변경 시 (영향 가능) | — |
| `gptel-agent-dirs` append + `prompts/gptel-agent.md` | upstream 이 subagent 가이드를 별도 toggle 로 분리 (예: `gptel-agent-subagents-enabled`) 시 | — |

### 답변 push 완료 (2026-05-26)

- [gptel #1432 follow-up](https://github.com/karthink/gptel/issues/1432#issuecomment-4540436258) — #2 retract + #1 확정 데이터 + #107 cross-link + gptel-agent side note
- [gptel-agent #107 코멘트](https://github.com/karthink/gptel-agent/issues/107#issuecomment-4540436517) — ekattsim 자리 인사 + 우리 advice 가 subagent flow 도 자동 우회 (추정)
- [gptel-agent #108 신규](https://github.com/karthink/gptel-agent/issues/108) — `gptel-agent.el:690` `gptel-max-tokens` 8192 default 가 OAuth Codex 와 충돌

karthink 답변 대기.

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
