# Doomemacs-config AGENT 가이드

## 프로젝트 개요

- **리포지토리**: junghan0611/doomemacs-config
- **목적**: 멀티 에이전트 중심 Doom Emacs 메인 설정
- **스타일**: hlissner (Doom Emacs 메인테이너) 스타일 기반
- **철학**: AI를 협력자로 대하는 "존재 대 존재 협업"

## 핵심 원칙

### 1. config.el은 로더로 유지
- **내용**: 로딩 로직 + 최소 필수 설정만
- **나머지**: 모두 lisp/ 또는 autoload/로 분리
- `(load! "lisp/module-name")` 패턴 사용

### 2. Outline 구조 (outli.el)
```elisp
;;; Level 1 섹션
;;;; Level 2 서브섹션
```

모든 `.el` 파일은 outline 구조로 관리

### 3. 파일 조직
- **autoload/**: `;;;###autoload` 마커가 있는 함수들 (lazy loading)
- **lisp/**: 독립적인 설정 라이브러리 (`(provide 'module-name)` 필수)
- **bin/**: Doom 없이 독립 실행 가능한 스크립트
- **한 기능 = 한 파일**: 설정이 여러 파일에 분산되지 않도록

### 4. 파일 헤더 표준
```elisp
;;; $DOOMDIR/lisp/module-name.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 모듈 설명

;;; Code:

;;;; 섹션1

;;;; 섹션2

(provide 'module-name)
;;; module-name.el ends here
```

## 작업 가이드

### 새 설정 추가 시

1. **어느 파일에 넣을까?**
   - AI 도구 → `lisp/ai-*.el`
   - Org 관련 → `lisp/org-config.el` (함수는 `org-functions.el`)
   - Denote 관련 → `lisp/denote-*.el`
   - Evil 관련 → `lisp/evil-config.el`
   - 편집 관련 → `lisp/editing-config.el`
   - UI/테마 → `lisp/ui-config.el`
   - 한글 입력 → `lisp/korean-input-config.el`
   - 유니코드 → `lisp/unicode-config.el`
   - 완성 시스템 → `lisp/completion-config.el`
   - 검색 → `lisp/search-config.el`
   - 키바인딩 → `lisp/keybindings-config.el`
   - tmux → `lisp/tmux-config.el`
   - Zellij → `lisp/zellij-config.el`
   - RSS → `lisp/elfeed-config.el`
   - 프로그래밍 → `lisp/prog-mode-config.el`
   - Export → `lisp/denote-export-config.el`
   - Termux/Android → `lisp/termux-config.el`

2. **함수인가?**
   - `;;;###autoload` 필요 → `autoload/junghan.el`
   - 내부 함수 → 해당 `lisp/*.el` 내부

### 파일 추가 시

1. lisp/ 또는 autoload/에 파일 생성
2. 헤더 작성 (위 표준 따르기)
3. `(provide 'module-name)` 추가
4. `config.el`에 `(load! "lisp/module-name")` 추가
5. Outline 구조 (`;;;`, `;;;;`) 유지

### bin/ 스크립트 수정 시

- `bin/denote-export.el`과 `lisp/denote-export-config.el`은 공유 로직
- advice/설정 변경은 `denote-export-config.el`에 넣어야 양쪽 반영
- `bin/denote-export.el`의 `get-org-hugo-section-from-path`에 새 폴더 추가 필수

## Git 커밋 가이드

### 커밋 메시지 스타일
```
feat: add EAF pyqterminal configuration

- Korean input via x-gtk-use-native-input
- Evil integration with SPC key handling
```

**중요**: "Generated with Claude" 또는 "Co-Authored-By" 제외!


