# Denote Export 데몬 이슈 (2026-03-12) — 해결 완료

## 수정 완료

### 1. `after!` → `with-eval-after-load` ✅
- **파일**: `lisp/denote-export-config.el` 117행
- **원인**: `after!`는 Doom 전용 매크로. `--quick` 독립 데몬에 없음
- **수정**: `(after! oc ...)` → `(with-eval-after-load 'oc ...)`

### 2. dblock 모드 GC 카운터 누락 ✅
- **파일**: `bin/denote-export.el` `denote-dblock-update-file`
- **수정**: dblock 함수에도 GC 카운터 로직 추가

### 3. 데몬 crash 근본 원인: `debug-on-error t` ✅
- **파일**: `bin/denote-export.el` 62행
- **원인**: `debug-on-error t` 상태에서 dblock 에러 발생 → 디버거 호출 → headless 데몬에 프레임 없음 → 프로세스 crash
- **수정**: `(setq debug-on-error nil)` + dblock 검색 패턴 `"^#\\+BEGIN: denote"` 한정 + 에러 시 revert-buffer

### 4. meta NBSP 파일명 정리 ✅
- **원인**: 344개 파일에 NBSP(U+00A0) 포함 → 에이전트/bash 혼란
- **수정**: title의 NBSP→일반공백 치환 후 `denote-rename-file-using-front-matter` 일괄 호출
- **핵심**: `(let ((denote-rename-confirmations nil) (denote-save-buffers nil)) ...)` 래핑 필수

## 최종 결과

```
531 success, 0 errors, 4 daemons, 35초, 15.19 files/sec
D1: 133 | D2: 133 | D3: 133 | D4: 132
```
