# NEXT.md — doomemacs-config

> 시작할 때 무엇을 할지 몰라서 발생하는 진행 정체를 막는다.
> 일정은 의미 없다. 적은 만큼 할 수 있는 만큼만 — 진행은 진행된다.

운영 baseline은 [AGENTS.md](AGENTS.md). 후속 작업 / 미완 검증은 여기에.
작업 끝나면 항목 지우고, 새로 발견한 후속은 추가. 영속할 사실은
AGENTS.md / `lisp/*-config.el` 헤더 코멘트 / commit history 로 옮긴다.

---

## 1. 가든 export 정리 — force re-export 후 검증 (2026-05-10)

오늘 발견한 두 회귀(`download:`, `attachment:`)의 root cause는 모두
**헤드리스 데몬이 Doom org 모듈/사용자 org-config.el을 로드하지 않아 buffer-local
변수가 누락**되는 패턴이었다. workflow-shared.el SSOT로 묶음.

- [x] `download:` 링크 → `/images/...` 정상 출력 (b348898)
- [x] `attachment:` 링크 root cause fix + verify-figures.py + run.sh 통합 (d8b977a)
- [ ] **데몬 재기동 + force re-export 전체 (메타/노트/봇로그/bib)**
- [ ] `./run.sh verify` 결과 — REWRITE/DEAD/UNKNOWN 카운트가 이전(70건)보다
      줄어드는지 확인 (root cause fix 효과)
- [ ] `./run.sh fix` 로 REWRITE 자동 정정 (소스 파일 → static/images/ 복사 +
      md src 치환)
- [ ] notes 리포에서 diff 확인 후 단일 커밋

## 2. DEAD figure 55건 수동 처리

verify-figures `DEAD` 카테고리 — 소스 파일이 `~/screenshot/`, `~/org/.attach/`
어디에도 없는 경우. 대부분 구식 file: 링크 (`[[file:~/screenshot/foo.png]]`).

- [ ] verify 출력의 파일:라인 보고 원본 org에서 결정:
      - 의미 있는 figure → 새 스크린샷 첨부 후 `download:`/`attachment:` 로 재링크
      - 의미 잃은 figure → org 파일에서 제거
- 우선 후보: bib/, notes/2023~2024 시기 옛 노트들 (diff 가벼움)

## 3. UNKNOWN figure 6건 인스펙션

`assets/...`, `images/...` (leading slash 없음), `About%20David%20Kadavy/...`
같은 비표준 패턴.

- [ ] 의도된 패턴이면 verify-figures.py `is_alive` 에 추가
- 의도 안 된 패턴이면 원본 org에서 정리

## 4. ~/screenshot/ literal file: 링크 export hook (선택)

```org
[[file:~/screenshot/foo.png]]    ;; ox-hugo가 ~ 안 풀어서 figure src에 그대로 박힘
```

소스 org 파일에 누적된 literal `~` 파일 링크 → 새로 추가하는 download:/attachment:
보다 가볍지만 ox-hugo는 tilde 미전개. 후보:

- `denote-export-config.el`에 `my/org-rewrite-tilde-file-links` hook 추가
  (`[[file:~/...]]` → `[[file:/home/junghan/...]]` 사전 치환)
- 또는 소스 org 파일에서 `attachment:`로 마이그레이션

장기적으론 후자가 맞지만 양이 많으면 hook이 단기 buffer.

## 5. DONT span 래핑 결정 (대기)

`f998631` 부수효과로 export 데몬도 DONT를 TODO 키워드로 인식 → 옛 노트 188개에서
`## DONT foo` → `## <span class="org-todo done DONT">DONT</span> foo` 로 변경됨.

- 결정 옵션:
  - **A. 받아들임** — 가든 한 번 churn 후 일관됨 (188 파일 변동)
  - **B. export 데몬에서만 default org-todo-keywords로 override** — 가든 0 churn,
    `bin/denote-export.el`에 한 줄 + 주석
- 결정 기준: 가든 빌드 띄워봤을 때 DONT span의 CSS 처리가 깔끔한지
- [ ] **`./run.sh fix` 끝나고 가든 빌드 시각 확인 → A/B 결정**

## 6. workflow-shared 영향 격리 검토

`workflow-shared.el`의 `setq org-todo-keywords` 가 의도(agent-server agenda)와
다른 데몬(export)에 동일하게 영향. SSOT 원칙 + 데몬별 의도 차이 사이의 균형이
필요. 5번 결정과 연결 — B로 가면 SSOT와 데몬-override를 명시적 패턴으로 정착.

---

## 작업 외 발견 — 기록만

- ghostel 한글 IME race condition 간헐 재발 (2026-05-07~) — 1주차 사용 후
  upstream PR. 별 repo 이슈 [junghan0611/ghostel#1](https://github.com/junghan0611/ghostel/issues/1)
- agent-server의 DONT skip 로직 (`agent-org-agenda--skip-dont`) — `f998631`로
  들어감. inherited tags 빈 slot도 `04a23d1` (closes #7)로 정리됨.
