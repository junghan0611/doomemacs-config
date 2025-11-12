# Denote dblock Update System

meta 디렉토리의 Denote 노트에 있는 동적 블록(dblock)을 배치 모드로 업데이트하는 시스템입니다.

## 개요

### 발견사항

org/meta 디렉토리 (530개 파일)의 dblock 사용 현황:
- **denote-links**: 559개 (정규식으로 관련 링크 검색)
- **denote-backlinks**: 8개 (역링크 추적)
- **columnview**: 2개 (org-mode 컬럼뷰)
- **총 dblock 포함 파일**: 439개 (82.8%)

### 성능

```
처리 파일: 530개
dblock 포함: 416개 실제 처리
처리 시간: 10.57초
속도: 50.144 files/sec
```

## 파일 구조

```
doomemacs-config/
├── bin/
│   ├── denote-dblock-batch.el      # Emacs batch script
│   ├── denote-dblock-update.sh     # Shell wrapper
│   └── README-DBLOCK-UPDATE.md     # 이 문서
```

## 사용법

### 기본 사용 (meta 디렉토리)

```bash
cd ~/sync/emacs/doomemacs-config
./bin/denote-dblock-update.sh
```

### 특정 디렉토리 지정

```bash
./bin/denote-dblock-update.sh ~/org/bib
./bin/denote-dblock-update.sh ~/org/notes
```

### Emacs에서 직접 실행

```elisp
;; Lisp 평가 방식
M-x load-file RET ~/sync/emacs/doomemacs-config/+denote-export.el RET
M-x my/update-dblock-garden RET ~/org/meta RET

;; 또는 배치 스크립트 직접 실행
(shell-command "~/sync/emacs/doomemacs-config/bin/denote-dblock-update.sh ~/org/meta")
```

## 기술적 세부사항

### dblock 종류

1. **denote-links** (가장 많음)
   - 정규식으로 관련 노트 검색
   - 키워드 기반 자동 링크 생성
   - 예: `#+BEGIN: denote-links :regexp "변환\\|convert"`

2. **denote-backlinks** (소수)
   - 현재 노트를 참조하는 역링크
   - 예: `#+BEGIN: denote-backlinks :excluded-dirs-regexp "meta"`

3. **columnview** (매우 적음)
   - org-mode 속성 컬럼뷰
   - 예: `#+BEGIN: columnview :id local`

### 처리 로직

```elisp
;; 1. 파일 스캔 (pre-check)
- 첫 5KB에서 "^#\\+BEGIN:" 패턴 검색
- 있으면 실제 처리 대상으로 선정

;; 2. dblock 업데이트
- org-update-all-dblocks 호출
- 모든 dblock 타입 자동 처리
- 에러 발생 시 graceful degradation

;; 3. 버퍼 관리
- find-file-noselect로 열기
- save-buffer로 저장
- kill-buffer로 정리
```

### 에러 핸들링

"Error during update of dynamic block" 에러는 정상입니다:
- denote-links가 전체 디렉토리 스캔 시 일부 파일 누락 가능
- 파일 자체는 정상적으로 저장됨
- 다음 실행 시 자동 복구

## 병렬 처리 분석

### ❌ 병렬 처리 비추천 이유

1. **Emacs 싱글 스레드 제약**
   - 여러 emacsclient → 한 daemon → 순차 처리 (blocking)

2. **파일 간 의존성**
   - denote-links는 다른 파일들 스캔 필요
   - 동시 스캔 시 읽기 충돌 가능

3. **I/O 병목**
   - 디렉토리 스캔이 가장 느림
   - CPU 병렬화 효과 제한적

4. **소수 파일만 dblock 사용**
   - 530개 중 439개만 처리
   - 병렬 오버헤드 > 실제 이득

### ✅ 현재 순차 처리 성능

```
530 files in 10.57s = 50 files/sec
충분히 빠름 (약 11초)
```

## 통합 워크플로우

### 권장: 순차 dblock + 병렬 export

```elisp
(defun my/update-dblock-export-garden-all-optimal ()
  "Optimal workflow: Sequential dblock + Parallel export"
  (interactive)
  
  ;; 1단계: dblock 순차 업데이트 (11초, meta만)
  (shell-command "~/sync/emacs/doomemacs-config/bin/denote-dblock-update.sh ~/org/meta")
  
  ;; 2단계: export 병렬 처리 (2-3분, 전체)
  (let ((script (expand-file-name "bin/denote-export-parallel.sh" doom-user-dir)))
    (shell-command (format "%s meta 4" script))
    (shell-command (format "%s bib 4" script))
    (shell-command (format "%s notes 4" script))))
```

### 예상 시간

```
1. dblock 업데이트: 11초 (meta)
2. 병렬 export: 2-3분 (전체)
------------------------
총 소요 시간: 3분 이내
```

## 트러블슈팅

### 패키지 설치 실패

```bash
# denote-explore 수동 설치
emacs --batch \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
  --eval "(package-refresh-contents)" \
  --eval "(package-install 'denote-explore)"
```

### denote-directory 설정

```elisp
;; .dir-locals.el에서 설정 확인
((nil . ((denote-directory . "~/org"))))
```

### 파일명 공백/특수문자 문제

- null delimiter 사용 (`find -print0 | xargs -0`)
- 한글, †, ¤ 등 특수문자 모두 지원

## 참고

- Export 시스템: `docs/20251027T092900--denote-export-system__denote_export_hugo_guide.org`
- 병렬 Export: `bin/EXPORT-PARALLEL.org`
- 핵심 로직: `+denote-export.el`

---

**버전**: 1.0.0
**날짜**: 2025-11-10
**저자**: Junghan Kim
