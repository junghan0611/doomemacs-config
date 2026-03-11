# Denote Export 데몬 이슈 (2026-03-12)

## 수정 완료

### 1. `after!` → `with-eval-after-load` (fix 완료)
- **파일**: `lisp/denote-export-config.el` 117행
- **원인**: `after!`는 Doom 전용 매크로. `--quick` 독립 데몬에 없음 → `(void-function after!)` 에러로 데몬 시작 실패
- **수정**: `(after! oc ...)` → `(with-eval-after-load 'oc ...)`

### 2. dblock 모드 GC 카운터 누락 (fix 완료)
- **파일**: `bin/denote-export.el` `denote-dblock-update-file`
- **원인**: `denote-export-file`에만 GC 카운터/수동 GC가 있고, `denote-dblock-update-file`에는 없었음
- **수정**: dblock 함수에도 동일한 GC 카운터 로직 추가

## 미해결: D2 데몬 67개 처리 후 crash

### 증상
- `./run.sh dblock meta 2` 실행 시 D2가 **정확히 67개** 처리 후 crash (socket 소멸)
- D1은 266개 전부 성공, D2는 67 성공 + 198 에러
- 결과: **333 success, 198 errors / 531 total**

### 조사 결과
- crash 지점 파일: `20240730T075847--† 해수욕장 해변 바다__beach_meta.org`
  - 이 파일에 `#+BEGIN: denote-links :regexp "해변\\|바다\\|해수욕"` dblock 있음
  - `#+begin_ai` 블록도 포함
- **단독 테스트 시 문제없음** — 데몬 새로 띄워서 이 파일만 처리하면 정상
- **67개 누적 후에만 crash** — 매번 같은 지점에서 재현
- RSS: 148MB(초기) → 372MB(65개 후). GC 수동 실행해도 RSS 안 줄어듦
- 65개 시점 상태: `7 buffers, gc=5, counter=66` — 버퍼 누수는 아님

### 추정 원인
1. **메모리 누적 + 무거운 dblock**: 67개 처리로 372MB 상태에서 `denote-links :regexp` dblock이 3000+ 파일 검색 → OOM crash
2. **org-element 파서 내부 상태 누적**: `kill-buffer` 해도 org-element 캐시 등 내부 상태가 쌓일 수 있음
3. **NBSP 파일명 관련**: 이 파일은 NBSP(U+00A0) 포함 파일명이지만, 단독 테스트에선 문제없으므로 가능성 낮음

### 다음 조사 방향
- [ ] 65개 처리 후 해수욕장 파일만 처리해서 crash 재현 확인
- [ ] `denote-links :regexp` dblock을 제거하고 테스트 (dblock이 원인인지)
- [ ] `gc-cons-threshold`를 128MB로 낮춰서 더 자주 GC 유도
- [ ] D2 데몬 프로세스에 `ulimit` 또는 NixOS cgroup 제한이 걸려있는지 확인
- [ ] `denote-dblock-update-file`에서 `org-update-all-dblocks` 대신 개별 dblock만 업데이트하도록 변경 테스트
