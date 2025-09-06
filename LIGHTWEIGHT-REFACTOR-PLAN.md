# DOTDOOM-STARTER 경량화 리팩토링 계획

**작성일**: 2025-09-06
**목표**: Doom Emacs 기본에 충실한 초경량 터미널 중심 설정

## 핵심 철학

> "개발은 VSCode/IntelliJ, 편집은 Emacs"

- Tree-sitter 빌트인 활용 (Emacs 29+)
- LSP 제거, Syntax highlighting만
- 터미널 모드 최적화
- 문서 작성과 설정 파일 편집에 집중

## 1. 제거할 모듈 (init.el)

### :tools 섹션
```elisp
;; 제거
- debugger          ; 디버거 불필요
- editorconfig      ; 프로젝트별 설정 불필요
- (eval +overlay)   ; 코드 평가 불필요
- lookup            ; 문서 조회 불필요
- lsp               ; Language Server Protocol 제거
- pass              ; 패스워드 관리 불필요
```

### :lang 섹션
```elisp
;; 제거 (모든 프로그래밍 언어)
- cc
- clojure  
- data (CSV는 유지 고려)
- emacs-lisp
- go
- java
- javascript
- kotlin
- lua
- python
- rust
- scala
- web

;; 유지
+ (org +roam2 +present)
+ markdown
+ (json +tree-sitter)
+ (yaml +tree-sitter)
+ sh
+ dockerfile
```

## 2. Tree-sitter 빌트인 설정

### 언어 소스 설정
```elisp
;; config.org에 추가
(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")
    (toml "https://github.com/ikatyang/tree-sitter-toml")))

;; 자동 모드 매핑
(setq major-mode-remap-alist
  '((bash-mode . bash-ts-mode)
    (json-mode . json-ts-mode)
    (yaml-mode . yaml-ts-mode)
    (toml-mode . toml-ts-mode)
    (dockerfile-mode . dockerfile-ts-mode)))
```

### Tree-sitter 문법 설치
```bash
# 필요한 문법 자동 설치
M-x treesit-install-language-grammar
```

## 3. 최종 모듈 구성

```elisp
(doom! :input
       :completion
       (corfu +orderless)  ; 가벼운 자동완성
       vertico            ; 세로 미니버퍼

       :ui
       doom              ; Doom 테마
       doom-dashboard    ; 시작 화면
       hl-todo          ; TODO 강조
       modeline         ; 모드라인
       (popup +defaults) ; 팝업 관리
       vc-gutter        ; Git 변경사항

       :editor
       (evil +everywhere) ; Vim 키바인딩
       file-templates    ; 파일 템플릿
       fold             ; 코드 접기
       multiple-cursors ; 다중 커서
       snippets         ; 스니펫

       :emacs
       dired            ; 파일 관리자
       electric         ; 자동 괄호
       undo            ; 실행취소
       vc              ; 버전 관리

       :term
       eshell          ; Emacs 쉘
       vterm           ; 터미널 에뮬레이터

       :checkers
       syntax          ; Flycheck 기본

       :tools
       direnv          ; 환경변수 관리
       docker          ; Docker 통합
       (magit +forge)  ; Git 인터페이스
       make           ; Makefile 지원
       terraform      ; IaC 도구

       :lang
       (org +roam2 +present)  ; 조직 모드
       markdown              ; 마크다운
       json                 ; JSON
       yaml                ; YAML
       sh                 ; 쉘 스크립트
       data              ; CSV/TSV

       :config
       (default +bindings +smartparens))
```

## 4. 패키지 정리 (packages.el)

### 제거할 패키지
```elisp
;; 개발 도구
- conjure, cider, sly     ; REPL 도구
- copilot, tabnine       ; AI 자동완성
- lsp-mode, lsp-ui       ; LSP 관련
- dap-mode              ; 디버거
- company              ; 자동완성 (corfu로 대체)

;; 언어별 패키지
- clojure-mode
- python-mode
- rust-mode
- go-mode
- js2-mode
```

### 유지/추가 패키지
```elisp
;; 문서 도구
+ org-roam           ; 지식 관리
+ org-modern        ; 모던 Org UI
+ markdown-mode     ; 마크다운
+ grip-mode        ; GitHub 마크다운 프리뷰

;; 설정 파일
+ dockerfile-mode   ; Dockerfile
+ json-mode        ; JSON
+ yaml-mode       ; YAML
+ toml-mode      ; TOML

;; 유틸리티
+ vterm          ; 터미널
+ magit         ; Git
+ forge        ; GitHub/GitLab
```

## 5. 성능 최적화

### 시작 최적화
```elisp
;; early-init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

;; Tree-sitter 우선
(setq treesit-font-lock-level 4)
```

### 터미널 모드 최적화
```elisp
;; 불필요한 UI 제거
(when (not (display-graphic-p))
  (setq-default 
    doom-themes-enable-bold nil
    doom-themes-enable-italic nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
```

## 6. Termux/Android 통합

```elisp
;; 기존 유지
(defconst IS-ANDROID (string-equal system-type "android"))
(defconst IS-TERMUX (and IS-ANDROID (getenv "TERMUX_VERSION")))

(when IS-TERMUX
  ;; Termux PATH 설정
  (let ((termux-bin "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat termux-bin ":" (getenv "PATH")))
    (push termux-bin exec-path))
  
  ;; 경량 설정
  (setq treesit-extra-load-path '("/data/data/com.termux/files/usr/lib"))
  (setq native-comp-speed -1))  ; 네이티브 컴파일 비활성화
```

## 7. 예상 결과

### 성능 개선
- **패키지 용량**: 220MB → 50-70MB (70% 감소)
- **시작 시간**: < 0.5초
- **메모리 사용**: 50-80MB (최소)
- **배터리 효율**: Termux에서 크게 개선

### 주요 기능
- ✅ 빠른 파일 편집
- ✅ Org-mode 문서 작성
- ✅ 설정 파일 관리 (JSON/YAML/TOML)
- ✅ Git 워크플로우
- ✅ Docker/Kubernetes 설정
- ✅ 쉘 스크립트 작성
- ❌ 코드 개발 (IDE 사용 권장)

## 8. 구현 순서

1. **백업 생성**
   ```bash
   cp -r ~/repos/gh/dotdoom-starter ~/repos/gh/dotdoom-starter.bak
   ```

2. **init.el 수정**
   - 불필요한 모듈 제거
   - Tree-sitter 모듈 활성화

3. **packages.el 정리**
   - 개발 도구 패키지 제거
   - 문서/설정 도구만 유지

4. **config.org 수정**
   - Tree-sitter 설정 추가
   - LSP 관련 설정 제거
   - 터미널 최적화 설정

5. **테스트**
   ```bash
   doom sync --profile starter
   ./start-emacs.sh
   ```

6. **성능 측정**
   - 시작 시간: `M-x emacs-init-time`
   - 메모리 사용: `M-x memory-report`

## 9. 참고 사항

- Doom Emacs v3.0 tree-sitter 지원 활용
- Emacs 29+ 빌트인 기능 우선
- 모듈별 독립성 유지
- 프로파일 시스템으로 실험/롤백 가능

---

**다음 단계**: 이 계획에 따라 직접 구현 진행