# Neomacs 바닐라 최소세트

[Neomacs](https://github.com/eval-exec/neomacs)는 Emacs 코어 ~300K줄 C를 Rust로
다시 쓰는 프로젝트다. 이 디렉토리는 그 위에서 도는 **빌트인 전용 프로파일**과,
한국어(K) 동작을 재현 가능한 형태로 찍어내는 **프로브**다.

리포 이슈: [#8](https://github.com/junghan0611/doomemacs-config/issues/8) ·
리서치 노트: Denote `20260209T123532`

Doom과 완전히 분리된다 — 별도 `--init-directory`, 별도 server name(`neomacs`),
공유 상태 없음. 프로파일은 **stock GNU Emacs에서도 그대로 돈다**. 그게 설계
목적이다: 갈라짐이 나오면 Neomacs 탓인지 이 설정 탓인지 즉시 갈라야 한다.

## 구성

```
neomacs/
├── early-init.el          # package.el off, GC, frame chrome
├── init.el                # 빌트인 전용 최소세트
└── probe/
    ├── probe-lib.el       # 배치 리포트 헬퍼 (크래시를 FAIL로 흡수)
    ├── probe-env.el       # 런타임·빌트인 커버리지
    ├── probe-org-korean.el # 한글 폭·조합·표 정렬  ← K검수 본체
    ├── probe-org-export.el # ox-html / ox-md
    ├── probe-real-org.el   # 실제 ~/org 코퍼스 (합성 픽스처 아님, 읽기 전용)
    └── probe-tls.el       # upstream #121
bin/neomacs.sh             # 런처
```

## 사용

```bash
./bin/neomacs.sh --fetch          # 릴리즈 AppImage 받기 (gh 필요)
./bin/neomacs.sh                  # GUI
./bin/neomacs.sh --nw             # 터미널
./bin/neomacs.sh --probe          # 프로브 전체 (배치)
./bin/neomacs.sh --probe tls      # 하나만
./bin/neomacs.sh --gnu            # 같은 프로파일을 GNU Emacs로 (대화형)

NEOMACS_BIN=emacs ./bin/neomacs.sh --probe   # GNU 배치 베이스라인
```

`--gnu`는 즉시 exec 하므로 `--probe`와 조합되지 않는다 (`--gnu --probe`는 Emacs가
`--probe`를 인자로 받아 무시한다). **하지만 배치 베이스라인은 한 명령으로 뽑힌다** —
`resolve_runner`가 `NEOMACS_BIN`을 가장 먼저 보므로 위의 `NEOMACS_BIN=emacs` 한 줄이
그 문이다 (2026-09-26 실측: GNU 31.1에서 5파일 전체 통과, 아래 대조표의 GNU 칸이
그렇게 뽑힌 값이다).

**빌드 불필요.** NixOS에서는 릴리즈 AppImage가 `libfontconfig`를 못 찾으므로
런처가 `appimage-run`으로 감싼다. 네이티브 빌드가 있으면 `NEOMACS_BIN`으로
덮어쓴다.

프로브는 **파일마다 별도 프로세스**로 돈다. 런타임을 죽이는 버그가 나머지
프로브의 보고를 막지 않게 하기 위해서다 — 크래시 자체가 산출물이므로.

## 측정 결과 (2026-09-26, Neomacs 0.0.19 vs GNU Emacs 31.1)

같은 프로파일을 두 런타임에서 돌린 대조. **64 OK / 1 FAIL.**
(0.0.13 → 0.0.19 = upstream 4,259 커밋, `git rev-list --count v0.0.13..v0.0.19`)

| 프로브 | Neomacs 0.0.19 | GNU Emacs 31.1 | 0.0.13 대비 |
|--------|----------------|----------------|-------------|
| env | 19 OK / 0 FAIL / 3 SKIP | 20 OK / 0 FAIL / 2 SKIP | 동일 |
| org-korean | 15 OK / **1 FAIL** | 16 OK / 0 FAIL | 동일 — 갈라짐 2 살아있음 |
| org-export | 12 OK / 0 FAIL | 12 OK / 0 FAIL | 동일 |
| real-org (실제 노트) | 9 OK / 0 FAIL | 9 OK / 0 FAIL | 동일 |
| **tls** | **9 OK / 0 FAIL** | 9 OK / 0 FAIL | **6 OK/3 FAIL → 전부 OK** |

재현: `./bin/neomacs.sh --probe` ↔ `NEOMACS_BIN=emacs ./bin/neomacs.sh --probe`.
런타임 확인: `Neomacs 0.0.19 / git b715d50bd / rustc 1.96.1` (`--version`),
`GNU Emacs 31.1` (`/nix/store/…-emacs-gtk3-31.1`, 프로브 로그의 라이브러리 경로).

### 갈라짐 1 (`:nowait` TLS → ELPA) — **닫혔다**

0.0.13에서 3 FAIL이던 자리가 전부 OK다. 프로브가 그대로 찍은 값:

```
[OK] TLS connect, :nowait t                 status=open tls=yes
[OK] url-retrieve-synchronously https       189256 bytes retrieved
[OK] package-refresh-contents against GNU ELPA 508 packages -- #121 RESOLVED
```

**실제 설치도 된다.** 임시 `package-user-dir`로 확인 (2026-09-26):
`package-install 'rainbow-mode` → `rainbow-mode-1.0.7/rainbow-mode.elc` +
`rainbow-mode-1.0.7.signed` — GPG 서명 검증과 바이트 컴파일까지 통과했다.
즉 **"ELPA에서 패키지 설치"는 더 이상 차단 요인이 아니다.**

### 갈라짐 2 (org 표 안의 링크가 열 폭을 부풀림) — **살아있다**

```
Neomacs 0.0.19  [FAIL] 76 chars -- link sized by raw bracket form, table inflated
GNU 31.1        [OK]   51 chars -- link width collapsed (GNU behavior)
```

0.0.13과 **같은 76자**다. upstream 미보고. 실사용 주의점은 그대로 — 링크 있는 표를
건드리면 파일이 부풀어 다시 써진다.

### 남은 차이 2건 (설계/부수)

- **native-comp 부재** — Neomacs 설계상 예정된 부재 (SKIP).
- **`gnutls-available-p` 기능 목록이 짧다** — Neomacs는 `(ciphers macs digests
  gnutls3 gnutls)`, GNU 31.1은 ALPN·SNI·Session Ticket 등을 포함한 긴 목록. TLS
  연결·ELPA는 위처럼 통과하므로 지금은 차단 요인이 아니지만, ALPN/SNI에 의존하는
  코드는 갈라질 수 있는 자리로 적어둔다.

### GUI는 여기서 판정되지 않는다

이번에도 배치는 통과했다. **2026-07-19 회차가 남긴 교훈 그대로 — 배치 통과는 실사용을
보증하지 못한다.** upstream v0.0.18..v0.0.19에는 우리 갈라짐 3(메뉴·커서)과 같은
자리를 만지는 커밋이 있다 (`3671f66b2 fix(display): rebuild the GL window surface on
resize`, `879ac28bf fix(menu-bar): share resolved heading geometry for painting and
input`, `fd171c0db fix(menu): preserve measured Unicode text geometry across
redraws`, `5aa0462e2 test(gui): cover native submenu compositor survival`).
**로그 대조는 판정이 아니다.** GUI 판정은 GLG가 화면을 두드리는 자리로 남는다.

## 이전 회차 (2026-07-19, Neomacs 0.0.13 vs GNU Emacs 31.0.50)

**61 OK / 4 FAIL.** 아래 내용은 그 회차의 기록이며, 위 0.0.19 회차가 현재값이다.
(GNU 라벨 `31.0.50`은 그 시점 PATH의 Emacs 버전 문자열이므로 그대로 둔다 — 31.1은
2026-08 이후 값이고, 과거 측정 라벨을 지금 값으로 덮으면 측정 기록이 아니게 된다.)

| 프로브 | Neomacs 0.0.13 | GNU Emacs 31.0.50 |
|--------|----------------|-------------------|
| env | 19 OK / 0 FAIL / 3 SKIP | 20 OK / 0 FAIL / 2 SKIP |
| org-korean | 15 OK / **1 FAIL** | 16 OK / 0 FAIL |
| org-export | 12 OK / 0 FAIL | 12 OK / 0 FAIL |
| tls | 6 OK / **3 FAIL** | 9 OK / 0 FAIL |
| **real-org** (실제 837 노트) | **9 OK / 0 FAIL** | 9 OK / 0 FAIL |

**실제 코퍼스에서는 갈라지는 게 없다.** 837개 실노트 중 가장 큰 것들로:
48,878줄 방문, org-element 34,686 요소 파싱, 4,769 헤딩 순회, FILETAGS 20/20 파싱,
430K자 노트에 한글 삽입, 실제 표 정렬, 노트 3개 HTML 내보내기(349K/737K/486K bytes),
Markdown 162K자 한글 온전 — 전부 GNU와 동일한 숫자다.

env의 1건 차이는 native-comp 부재(설계상 예정). 진짜 갈라짐은 두 뿌리뿐이다.

| 영역 | 결과 |
|------|------|
| 빌트인 라이브러리 (org·ox·treesit·quail·ucs-normalize…) | 전부 로드 |
| 한글 폭 (`string-width "한"` = 2, 혼합 문자열) | 정확 |
| NFD 자모 → NFC 음절 정규화 | 정확 |
| 빌트인 `korean-hangul` 입력기 등록·활성화 | 동작 |
| **org 표 정렬 — 한글 / 한자+한글+ASCII 혼합** | **정렬 정확, 크래시 없음, 멱등** |
| 한글 fill-paragraph / heading 순회 / change hooks | 동작 |
| ox-html · ox-md 내보내기 (한글·표·코드·링크) | 전부 보존 |
| 파일 왕복 내보내기 (coding-system 경로) | 한글 온전 |
| treesit 컴파일 | 있음 (문법 미설치) |
| `make-thread` / `thread-join` | 동작 |
| **HTTPS 경유 ELPA** | **막힘 — 아래 참조** |

즉 **이슈 #8이 정의한 최소세트 범위는 지금 전부 돈다.** 6/15에 세운 K검수
관문(#129 org 표 CJK 크래시, #128 IME)은 실제로 닫혔음을 확인했다.

### 갈라짐 2 — org 표 안의 링크가 열 폭을 부풀린다

이번 실측에서 **새로 찾은 것**. upstream 미보고.

```elisp
(org-mode)
(insert "| a | [[https://example.com][설명]] |\n| bb | x |\n")
(org-table-align)
```

| | 결과 |
|---|---|
| GNU | `\| bb \| x    \|` — 51 chars |
| Neomacs | `\| bb \| x                             \|` — 76 chars |

GNU는 링크의 **표시 폭**(`org-link-display-format` → `"설명"`, 4칸)으로 열을 잡고,
Neomacs는 **원시 대괄호 형태 29칸**으로 잡는다. elisp 레벨 입력은 양쪽이 완전히
동일하다 — `org-link-display-format`, `org-string-width`, `org-descriptive-links`
전부 같은 값을 낸다. 갈라지는 곳은 `org-table-align` 자체의 폭 계산이다.

**실사용 주의점**: 노트 표에 링크는 흔하다. 그런 표를 건드리면 파일이 부풀어 디스크에
다시 써진다. 데이터 손실은 아니지만 diff가 오염된다. 실제 노트에서 재현됨
(1,199자 표 → 1,865자).

재현: `./bin/neomacs.sh --probe org-korean`

### 갈라짐 1 — upstream #121의 실체는 핸드셰이크가 아니라 `:nowait`

#121은 "GnuTLS error: 1"로 리포트됐지만, 6월 rustls facade 도입 후 **핸드셰이크
자체는 고쳐졌다.** 남은 것은 비동기 연결 경로다.

```elisp
(open-network-stream "x" buf "elpa.gnu.org" 443 :type 'tls)             ; 양쪽 200 OK
(open-network-stream "x" buf "elpa.gnu.org" 443 :type 'tls :nowait t)   ; GNU ok / Neomacs TLS 미협상
```

`url-http`는 모든 연결을 `:nowait t`로 연다. TLS 협상이 빠진 소켓으로 요청이
나가고, 서버가 `400 Bad Request — You're speaking plain HTTP to an SSL-enabled
server port` (561 bytes)를 돌려주고, `package-refresh-contents`는 빈
`archive-contents`를 보고한다.

elisp 레벨 호출 시퀀스는 GNU와 **완전히 동일**하다 (`open-network-stream`
`:type=tls`, `url-http-find-free-connection` gw=tls 모두 일치). 갈라지는 곳은
Rust 쪽 비동기 TLS connect 하나다.

`probe-tls.el`이 이 케이스를 그대로 들고 있으므로, 재현은 `--probe tls` 한 번이다.

## 실사용 판단 (2026-09-26, 0.0.19) — **배치는 열렸다, GUI는 미판정**

배치 축에서 차단 요인은 **한 건도 남지 않았다** (64 OK / 1 FAIL, 유일한 FAIL이
갈라짐 2). 그러나 **실사용 판정은 여전히 GUI에서만 난다** — 지난 회차의 교훈이
그것이었고, 그 자리는 이번에도 측정되지 않았다.

| 하는 일 | 0.0.13 (07-19) | 0.0.19 (09-26) |
|---------|----------------|----------------|
| org 노트 읽기 / 순회 / 검색 (배치) | 됨 | 됨 |
| 한글 폭·정규화·조합 (배치) | 됨 | 됨 |
| ox-html / ox-md 내보내기 (배치) | 됨 | 됨 |
| **ELPA에서 패키지 설치** | ✗ 막힘 | **✓ 됨 — 서명 검증·바이트컴파일까지** |
| **링크 있는 표를 정렬** | ⚠ 부풀어 오름 | ⚠ **그대로** (76자) |
| **GUI 메뉴 / 커서 렌더링** | ✗ 흔들림 (GLG 관측) | **? 미판정 — GLG가 두드려야 함** |
| 로컬 소스 패키지 (`load-path`) | 됨 | 됨 |

ELPA가 열렸다는 것은 **"로컬 소스로 우회"가 더 이상 유일한 길이 아니라는 뜻**이다.
다만 프로파일은 여전히 빌트인 전용이다 (`early-init.el`에서 package.el off) — 그
규약을 풀지 말지는 GUI 판정 다음 문제다.

## 이전 판단 (2026-07-19, 0.0.13) — **아직 아니다**

**GLG 판정: 실사용 수준 아님. 메뉴가 흔들린다.** 일단 해보는 것 정도.
재검토는 2주 뒤(2026-08-02). PR/이슈 제출은 하지 않는다 — 기록만 남긴다.

**이 판정 자체가 오늘의 결과다.** 배치 프로브는 61 OK로 통과했고 실제 837 노트
코퍼스에서 0 FAIL이었다. 그런데 화면에서는 아니었다. 즉 **배치 통과는 실사용을
보증하지 못한다.** GUI 렌더링(메뉴·커서 글리프)은 `--probe`가 닿지 않는 자리이고,
사람이 두드려야만 드러난다. 프로브 결과를 실사용 판단으로 승격하면 안 된다.

| 하는 일 | 상태 |
|---------|------|
| org 노트 읽기 / 순회 / 검색 (배치) | 됨 |
| 한글 폭·정규화·조합 (배치) | 됨 |
| ox-html / ox-md 내보내기 (배치) | 됨 (denote 링크 포함) |
| **GUI 메뉴 / 커서 렌더링** | ✗ **흔들림 — 실사용 차단 요인** |
| **링크 있는 표를 정렬** | ⚠ 파일이 부풀어 오름 — 갈라짐 2 |
| **ELPA에서 패키지 설치** | ✗ 막힘 — 갈라짐 1 |
| 로컬 소스 패키지 (`load-path`) | 됨 — denote가 이 경로로 붙어 있다 |

GUI 쪽 관측: 첫 기동에서 wgpu가 `cursor_glyph_mismatch`를 대량 로깅했다
(1,237줄 로그 중 대부분, 커서 Y가 셀보다 17px 아래 —
`cursor=(8.0,703.0)` vs `cell=(8.0,686.0)`, face=25 font=19.0).
메뉴 흔들림과 같은 렌더러 계열로 보이나 확정하지 않았다.

### 패키지는 ELPA 대신 로컬 소스로 (0.0.13 시절의 우회)

**0.0.19에서 ELPA는 열렸다** (위 갈라짐 1 참조). 아래는 막혀 있던 시절의 우회이며,
`my/neomacs-local-package-dirs`는 지금도 그대로 쓰인다 — 빌트인 전용 규약을 깨지
않고 denote를 붙이는 길이라서다.

#121로 ELPA가 막혀 있어도, 디스크에 이미 있는 순수 elisp 체크아웃을 `load-path`에
얹는 건 막히지 않는다. `init.el`의 `my/neomacs-local-package-dirs`가 그 자리다 —
현재 `denote` / `denote-org`를 Doom의 straight 체크아웃에서 끌어온다. 덕분에
`denote:` 링크가 해석되고 실노트 내보내기가 통과한다.

C 모듈이나 native-comp가 필요한 패키지는 넣지 않는다.

## 한계 / 알아둘 것

- **treesit 문법 미설치.** 0.0.13에서는 TLS가 막혀 스스로 받지 못하는 게 이유였다.
  **0.0.19에서 그 이유는 사라졌다** (ELPA·HTTPS 통과) — 아직 안 받았을 뿐이다.
  문법을 넣으면 하이라이트가 붙는다. 다음 회차 후보.
- **`hangul.el` require 실패는 Neomacs 결함이 아니다** — GNU Emacs도 동일하게
  SKIP이다 (해당 기능은 `korea-util`/`quail` 경유로 제공된다). 빌트인
  `korean-hangul` 입력기는 양쪽 다 정상 등록·활성화된다.
- **native-comp 없음** — Neomacs 설계상 예정된 부재.
- htmlize 부재로 ox-html src 블록이 plain text로 떨어진다. 빌트인 전용
  프로파일에서는 정상 동작.
- GUI 실동작(폰트 셰이핑, 실제 조합 입력, 커서)은 배치가 못 잡는다. 사람이
  두드려야 하는 자리로 남는다.
