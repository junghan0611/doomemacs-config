# Doomemacs-config AGENT 가이드

## 프로젝트 개요

- **리포지토리**: junghan0611/doomemacs-config
- **목적**: 깔끔하고 유지보수 가능한 Doom Emacs 설정
- **스타일**: hlissner (Doom Emacs 메인테이너) 스타일
- **참고**: `/home/goqual/sync/man/dotsamples/doom/hlissner-dot-doom/`

## 디렉터리 구조

```
doomemacs-config/
├── init.el              # Doom 모듈 선택
├── packages.el          # 패키지 목록
├── config.el            # 메인 설정 (간결하게 유지! 목표 < 600줄)
├── custom.el            # Emacs customize 변수
│
├── autoload/            # 자동로드 함수 (;;;###autoload)
│   └── junghan.el       # 커스텀 함수들
│
├── lisp/                # 독립 라이브러리
│   ├── korean-input.el      # 한글 입력 전체
│   ├── evil-config.el       # Evil 모드
│   ├── completion-config.el # Corfu, Vertico, Consult
│   ├── ui-config.el         # Dashboard, Modeline, Themes
│   ├── org-config.el        # Org-mode
│   ├── denote-config.el     # Denote
│   ├── denote-silo.el       # Denote silo 동적 관리
│   ├── denote-export.el     # Denote export 시스템
│   ├── ai-gptel.el          # GPTel
│   └── ai-agent-shell.el    # Agent Shell, claude-code-ide
│
├── +keybindings.el      # 전역 키바인딩 (TODO: config.el로 통합 예정)
└── +user-info.el        # 사용자 정보
```

## 핵심 원칙

### 1. config.el은 간결하게
- **목표**: 600줄 이하 유지
- **내용**: 로딩 로직 + 최소 필수 설정만
- **나머지**: 모두 lisp/ 또는 autoload/로 분리

### 2. Outline 구조 (outli.el)
```elisp
;;; Level 1 섹션
;;;; Level 2 서브섹션
```

모든 `.el` 파일은 outline 구조로 관리

### 3. 파일 조직
- **autoload/**: `;;;###autoload` 마커가 있는 함수들 (lazy loading)
- **lisp/**: 독립적인 설정 라이브러리 (`(provide 'module-name)` 필수)
- **한 기능 = 한 파일**: 설정이 여러 파일에 분산되지 않도록

### 4. 파일 헤더 표준
```elisp
;;; $DOOMDIR/lisp/module-name.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; 모듈 설명

;;; Code:

;;;; 섹션1

;;;; 섹션2

(provide 'module-name)
;;; module-name.el ends here
```

## lisp/ 파일 가이드

### korean-input.el (18K, 461줄)
**내용**:
- Input method 설정 (korean-hangul)
- UTF-8 코딩 시스템
- 한영 전환 키바인딩
- 폰트 및 이모지 설정
- NFD → NFC 정규화 (터미널용)
- Evil 모드 자동 한영 전환
- Termux/Kitty term-keys 지원

**섹션**:
- Input System : Hangul
- Font and Emoji Settings
- NFD to NFC Normalization
- evil + hangul

### evil-config.el (6K, 161줄)
**내용**:
- Evil core 설정 (커서, 이동, undo)
- Evil-escape (,. 조합)
- Smartparens 연동 (org-mode, markdown pairs)

### completion-config.el (2.7K, 79줄)
**내용**:
- Corfu (in-buffer completion)
- Vertico (minibuffer completion)
- Consult (preview 설정)

### ui-config.el (6.8K, 192줄)
**내용**:
- Dashboard (fortune 위젯)
- Doom Modeline
- Themes (doom-one, 터미널 색상 처리)
- Outli (코드 outline)
- Pulse-line, Which-key

### org-config.el (14K, 371줄)
**내용**:
- Org TODO keywords
- Org Agenda 설정
- Org Capture templates
- Org-journal
- Citar (bibliography)

### denote-config.el (4.7K, 146줄)
**내용**:
- Denote 기본 설정
- Citar-denote 연동
- Denote-explore, denote-search

### denote-silo.el (7.7K, 210줄)
**기존 파일 유지**:
- 동적 silo 관리
- ~/org/, ~/claude-memory/ 등 디렉터리 관리

### denote-export.el (22K, 558줄)
**기존 파일 유지**:
- Denote to Markdown/HTML export
- Bibliography 처리

### ai-gptel.el (4.5K, 141줄)
**기존 파일 유지**:
- GPTel 설정 (Claude, OpenAI, Gemini 등)

### ai-agent-shell.el (3.6K, 110줄)
**내용**:
- Agent Shell (ACP) 설정
- agent-shell-manager, agent-shell-sidebar
- claude-code-ide
- MCP (주석 처리)

## 작업 가이드

### 새 설정 추가 시

1. **패키지 설정인가?**
   - YES → 적절한 `lisp/*.el` 파일에 추가
   - NO → `config.el`에 직접 추가

2. **어느 파일에 넣을까?**
   - Org 관련 → `lisp/org-config.el`
   - Denote 관련 → `lisp/denote-config.el`
   - Evil 관련 → `lisp/evil-config.el`
   - UI 관련 → `lisp/ui-config.el`
   - AI 도구 → `lisp/ai-*.el`
   - 한글 입력 → `lisp/korean-input.el`
   - 완성 시스템 → `lisp/completion-config.el`

3. **함수인가?**
   - `;;;###autoload` 필요 → `autoload/junghan.el`
   - 내부 함수 → 해당 `lisp/*.el` 내부

### config.el 수정 시

**❌ 하지 말 것**:
- 큰 섹션을 config.el에 직접 작성
- 패키지 설정을 config.el에 남기기
- outline 구조 깨뜨리기

**✅ 해야 할 것**:
- 새 lisp 파일 만들고 `(load! "lisp/filename")` 추가
- Outline 구조 유지 (`;;;`, `;;;;`)
- 간결함 유지

### 파일 추가 시

1. lisp/ 또는 autoload/에 파일 생성
2. 헤더 작성 (위 표준 따르기)
3. `(provide 'module-name)` 추가
4. `config.el`에 `(load! "lisp/module-name")` 추가
5. Outline 구조 (`;;;`, `;;;;`) 유지

## Git 커밋 가이드

### 커밋 메시지 스타일 (이 프로젝트)
```
refactor: extract org configuration to lisp/org-config.el

- 설명1
- 설명2

Part of hlissner-style refactoring
```

**중요**: "Generated with Claude" 또는 "Co-Authored-By" 제외! (깔끔한 커밋 로그 유지)

## 다음 리팩터링 단계

### Phase 1: 남은 섹션 정리 (진행 예정)
- defaults.el
- doom-overrides.el
- dev-tools.el
- platform-termux.el
- integrations.el

### Phase 2: 키바인딩 통합
- +keybindings.el → config.el 통합

### Phase 3: 테스트 및 병합
- refactor/doom-native → main 병합

### Phase 4: NixOS 통합
- config.local.el 생성 방식 연구
- hlissner-dotfiles-nix 패턴 적용

## 참고 파일

- 현재 작업: `NOW.md`
- 프로젝트 README: `README-KO.md`
- hlissner doom.d: `/home/goqual/sync/man/dotsamples/doom/hlissner-dot-doom/`
- hlissner nixos: `/home/goqual/sync/man/dotsamples/nixos/hlissner-dotfiles-nix/`
- fulllab config: `/home/goqual/sync/emacs/emacs-fulllab-config/`
