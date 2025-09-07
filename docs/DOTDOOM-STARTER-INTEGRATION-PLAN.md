# DOTDOOM-STARTER 통합 계획

**작성일**: 2025-09-03  
**목적**: Android 설정을 starter로 통합하고 최소 핵심 버전으로 재구성

## 설계 철학

### 핵심 원칙
1. **최소주의**: 필수 기능만 유지
2. **범용성**: 모든 환경에서 동작
3. **확장성**: user-profiles로 개인화
4. **자동화**: 환경 감지 및 최적화

## 구조 설계

### 기본 구조
```
dotdoom-starter/
├── config.el          # 핵심 설정 (최소화)
├── init.el            # 모듈 선택 (필수만)
├── packages.el        # 최소 패키지
├── custom.el          # 생성될 커스텀 설정
└── profiles/          # 환경별 추가 설정
    ├── android.el     # Android 전용 (Termux PATH 등)
    ├── server.el      # 서버 전용
    ├── terminal.el    # 터미널 전용
    └── user.el        # 사용자 개인 설정
```

## 통합 작업

### 1단계: 코어 설정 정리
```elisp
;; config.el - 최소 핵심만
(setq doom-font (font-spec :family "monospace" :size 12)
      doom-theme 'doom-one)

;; 환경 감지
(defconst IS-ANDROID (string-match "Android" (shell-command-to-string "uname -a")))
(defconst IS-TERMUX (getenv "TERMUX_VERSION"))
(defconst IS-SSH (getenv "SSH_CLIENT"))

;; 조건부 로드
(when IS-ANDROID (load! "profiles/android"))
(when IS-SSH (load! "profiles/server"))
(when (not (display-graphic-p)) (load! "profiles/terminal"))

;; 사용자 설정 (있으면)
(when (file-exists-p (expand-file-name "profiles/user.el" doom-private-dir))
  (load! "profiles/user"))
```

### 2단계: Android 기능 통합
```elisp
;; profiles/android.el
;;; Android Native GUI + Termux 통합

;; Termux PATH 연동 (기존 android 설정에서 가져옴)
(when IS-TERMUX
  (setenv "PATH" (concat "/data/data/com.termux/files/usr/bin:" (getenv "PATH")))
  (setq shell-file-name "/data/data/com.termux/files/usr/bin/bash"))

;; Android 파일시스템
(when IS-ANDROID
  (setq default-directory "/storage/emulated/0/")
  ;; 터치 최적화
  (global-set-key [touch-drag] 'mouse-set-region))
```

### 3단계: init.el 최소화
```elisp
;; init.el - 필수 모듈만
(doom! :input
       ;;chinese
       ;;japanese

       :completion
       company           ; 자동완성
       vertico          ; 검색 인터페이스

       :ui
       doom             ; 테마
       doom-dashboard   ; 시작화면
       hl-todo         ; TODO 하이라이트
       modeline        ; 상태바
       ophints         ; 힌트
       (popup +defaults)
       vi-tilde-fringe ; vi 스타일
       workspaces      ; 작업공간

       :editor
       (evil +everywhere) ; vim 키바인딩
       file-templates    ; 템플릿
       fold             ; 코드 접기
       multiple-cursors ; 다중 커서
       snippets         ; 스니펫

       :emacs
       dired            ; 파일 관리자
       electric         ; 자동 괄호
       undo            ; 실행취소
       vc              ; 버전 관리

       :term
       ;;eshell
       ;;shell
       term            ; 터미널
       vterm           ; 더 나은 터미널

       :checkers
       syntax          ; 문법 검사

       :tools
       editorconfig    ; 에디터 설정
       (eval +overlay) ; 평가
       lookup         ; 정의 찾기
       magit          ; git

       :lang
       emacs-lisp     ; elisp
       markdown       ; 마크다운
       org            ; org-mode
       sh             ; 쉘 스크립트

       :config
       (default +bindings +smartparens))
```

## 마이그레이션 단계

### Phase 1: 준비 (현재)
- [x] 전략 수립
- [ ] 기존 설정 분석
- [ ] 핵심/개인 설정 분리

### Phase 2: 통합
- [ ] dotdoom-starter 백업
- [ ] 핵심 설정만 남기기
- [ ] android 설정 profiles/android.el로 이동
- [ ] 개인 설정 profiles/user.el.example 생성

### Phase 3: 테스트
- [ ] Linux 터미널 테스트
- [ ] SSH 서버 테스트
- [ ] Android Native 테스트 (에뮬레이터)
- [ ] Termux 테스트 (가능시)

### Phase 4: 문서화
- [ ] README 업데이트
- [ ] user-profiles 가이드 작성
- [ ] 환경별 설정 예시

## 사용자 커스터마이징 가이드

### profiles/user.el 예시
```elisp
;;; profiles/user.el -*- lexical-binding: t; -*-
;;; 사용자 개인 설정

;; 폰트 (환경에 맞게)
(when (display-graphic-p)
  (setq doom-font (font-spec :family "JetBrains Mono" :size 14)))

;; 테마
(setq doom-theme 'doom-dracula)

;; 개인 키바인딩
(map! :leader
      "f p" #'my-custom-function)

;; 추가 패키지는 ../packages.el에
```

## 장점

1. **단일 코드베이스**: 하나의 starter로 모든 환경 지원
2. **유지보수 용이**: 중복 코드 제거
3. **확장 가능**: user-profiles로 개인화
4. **자동 최적화**: 환경 감지하여 적절한 설정 로드

## 주의사항

- 개인 설정은 절대 커밋하지 않음
- profiles/user.el은 .gitignore에 추가
- profiles/user.el.example만 제공
- 환경 감지 로직은 철저히 테스트

## 참고

- 기존 dotdoom-android- 설정 참조
- 기존 dotdoom-starter 구조 활용
- Doom Emacs 공식 모듈 문서