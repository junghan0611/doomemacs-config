# Doomemacs-config AGENT 가이드

## 프로젝트 개요

- **리포지토리**: junghan0611/doomemacs-config
- **목적**: 멀티 에이전트 중심 Doom Emacs 메인 설정
- **스타일**: hlissner (Doom Emacs 메인테이너) 스타일 기반
- **철학**: AI를 협력자로 대하는 "존재 대 존재 협업"

## 디렉터리 구조

```
doomemacs-config/
├── init.el              # Doom 모듈 선택
├── packages.el          # 패키지 목록
├── config.el            # 메인 설정 (로더, 간결하게 유지)
├── custom.el            # Emacs customize 변수
│
├── autoload/            # 자동로드 함수 (;;;###autoload)
│   └── junghan.el       # 커스텀 함수들
│
├── lisp/                # 독립 라이브러리 (39 파일)
│   ├── korean-input-config.el   # 한글 입력, 폰트, NFD→NFC
│   ├── unicode-config.el        # 유니코드/NBSP 처리
│   ├── evil-config.el           # Evil 모드
│   ├── editing-config.el        # 편집 관련 설정
│   ├── completion-config.el     # Corfu, Vertico, Consult
│   ├── search-config.el         # 검색 (consult-ripgrep 등)
│   ├── ui-config.el             # Dashboard, Themes
│   ├── modeline-config.el       # Modeline 설정
│   ├── tab-bar-config.el        # Tab-bar 설정
│   ├── org-config.el            # Org-mode
│   ├── org-functions.el         # Org 유틸리티 함수
│   ├── denote-config.el         # Denote 기본
│   ├── denote-silo-config.el    # Denote silo 동적 관리
│   ├── denote-export-config.el  # Denote export 시스템
│   ├── denote-functions.el      # Denote 유틸리티
│   ├── ai-gptel.el              # GPTel (Claude, OpenAI, Gemini)
│   ├── ai-agent-shell.el        # Agent Shell, ACP, Claude Code
│   ├── ai-gptel-acp.el          # GPTel + ACP 통합
│   ├── ai-gptel-acp-v2.el       # GPTel + ACP v2
│   ├── ai-orchestration.el      # AI 오케스트레이션
│   ├── ai-stt-eca-whisper.el    # 음성-텍스트 (Whisper)
│   ├── ai-tts-edge.el           # 텍스트-음성 (Edge TTS)
│   ├── +claude-code-ide-mcp-tools.el # Claude Code MCP 도구
│   ├── eaf-config.el            # EAF 애플리케이션
│   ├── elfeed-config.el         # RSS 리더 설정
│   ├── keybindings-config.el    # 전역 키바인딩
│   ├── keybindings-denote-config.el # Denote 키바인딩
│   ├── functions.el             # 일반 함수 (yank-code-with-context 등)
│   ├── project-config.el        # 프로젝트 설정
│   ├── prog-mode-config.el      # 프로그래밍 모드 설정
│   ├── module-emacs-config.el   # Emacs 내장 모듈 설정
│   ├── tmux-config.el           # tmux 통합
│   ├── zellij-config.el         # Zellij 통합
│   ├── sks-hub-nav.el           # SKS Hub 상태 머신 네비게이션
│   ├── zotero-config.el         # Zotero 서지 관리
│   ├── time-config.el           # 시간/캘린더
│   ├── android-config.el        # Android 전용 설정
│   ├── termux-config.el         # Termux 전용 설정
│   └── utils-config.el          # 유틸리티
│
├── bin/                 # 독립 실행 스크립트
│   ├── denote-export.el     # 통합 export/dblock 서버 (핵심!)
│   └── denote-export.sh     # Shell 래퍼
│
└── docs/                # 문서
```

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

## lisp/ 파일 가이드

### AI/에이전트 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `ai-gptel.el` | 36K | GPTel 전체 설정 (Claude, OpenAI, Gemini, 프롬프트) |
| `ai-agent-shell.el` | 8K | Agent Shell, ACP, Claude Code |
| `ai-gptel-acp.el` | 20K | GPTel + ACP 통합 |
| `ai-gptel-acp-v2.el` | 17K | GPTel + ACP v2 |
| `ai-orchestration.el` | 6K | AI 멀티 에이전트 오케스트레이션 |
| `ai-stt-eca-whisper.el` | 4K | ECA Whisper 음성-텍스트 |
| `ai-tts-edge.el` | 22K | Edge TTS 텍스트-음성 |
| `+claude-code-ide-mcp-tools.el` | 14K | Claude Code MCP 도구 정의 |

### 터미널 멀티플렉서 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `tmux-config.el` | 26K | tmux 세션/에이전트 통합 |
| `zellij-config.el` | 20K | Zellij 터미널 멀티플렉서 통합 |

### EAF 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `eaf-config.el` | 3K | EAF 앱 설정 (browser, pdf, pyqterminal) |

### 한글/입력/유니코드 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `korean-input-config.el` | 19K | 한글 입력 전체 (폰트, NFD→NFC, Evil 전환) |
| `unicode-config.el` | 5K | 유니코드 처리, NBSP 관리 |
| `keybindings-config.el` | 19K | 전역 키바인딩 |
| `keybindings-denote-config.el` | 12K | Denote 전용 키바인딩 |

### Denote 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `denote-config.el` | 7K | Denote 기본 설정 |
| `denote-silo-config.el` | 8K | 동적 silo 관리 |
| `denote-export-config.el` | 23K | Hugo/Markdown export |
| `denote-functions.el` | 19K | Denote 유틸리티 함수 |

### Org 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `org-config.el` | 16K | Org-mode 전체 |
| `org-functions.el` | 4K | Org 유틸리티 함수 |

### 편집/완성/검색

| 파일 | 크기 | 설명 |
|------|------|------|
| `evil-config.el` | 6K | Evil 모드 (커서, escape, smartparens) |
| `editing-config.el` | 8K | 편집 관련 (들여쓰기, 공백 등) |
| `completion-config.el` | 13K | Corfu, Vertico, Consult |
| `search-config.el` | 2K | 검색 (consult-ripgrep 등) |

### UI/테마

| 파일 | 크기 | 설명 |
|------|------|------|
| `ui-config.el` | 8K | Dashboard, Themes |
| `modeline-config.el` | 3K | Modeline 설정 |
| `tab-bar-config.el` | 3K | Tab-bar 설정 |

### 도구/유틸리티

| 파일 | 크기 | 설명 |
|------|------|------|
| `functions.el` | 14K | 일반 함수 (yank-code-with-context 등) |
| `project-config.el` | 3K | 프로젝트 설정 |
| `prog-mode-config.el` | 5K | 프로그래밍 모드 설정 |
| `module-emacs-config.el` | 3K | Emacs 내장 모듈 설정 |
| `sks-hub-nav.el` | 17K | SKS Hub 상태 머신 네비게이션 |
| `elfeed-config.el` | 4K | RSS 리더 (Elfeed) |
| `zotero-config.el` | 3K | Zotero 서지 관리 |
| `time-config.el` | 2K | 시간/캘린더 |
| `utils-config.el` | 1K | 기타 유틸리티 |

### 플랫폼별

| 파일 | 크기 | 설명 |
|------|------|------|
| `android-config.el` | 2K | Android 전용 설정 |
| `termux-config.el` | 4K | Termux 전용 설정 |

## bin/ 스크립트

### denote-export.el (핵심!)

**통합 Denote 작업 서버** - Doom 없이 독립 실행 가능

특징:
- Doom straight.el 패키지 자동 로딩
- 데몬/배치 모드 지원
- Hugo export + dblock update

사용법:
```bash
# 데몬 모드 (빠른 반복 작업)
emacs --daemon=denote-export-server --load bin/denote-export.el
emacsclient -s denote-export-server --eval '(denote-export-file "file.org")'

# 배치 모드 (CI/스크립트)
emacs --batch --load bin/denote-export.el -- dblock ~/org/meta
```

## 작업 가이드

### 새 설정 추가 시

1. **어느 파일에 넣을까?**
   - AI 도구 → `lisp/ai-*.el`
   - EAF 관련 → `lisp/eaf-config.el`
   - Org 관련 → `lisp/org-config.el` (함수는 `org-functions.el`)
   - Denote 관련 → `lisp/denote-*.el`
   - Evil 관련 → `lisp/evil-config.el`
   - 편집 관련 → `lisp/editing-config.el`
   - UI/테마 → `lisp/ui-config.el`
   - Modeline → `lisp/modeline-config.el`
   - 한글 입력 → `lisp/korean-input-config.el`
   - 유니코드 → `lisp/unicode-config.el`
   - 완성 시스템 → `lisp/completion-config.el`
   - 검색 → `lisp/search-config.el`
   - 키바인딩 → `lisp/keybindings-config.el`
   - tmux → `lisp/tmux-config.el`
   - Zellij → `lisp/zellij-config.el`
   - RSS → `lisp/elfeed-config.el`
   - 프로그래밍 → `lisp/prog-mode-config.el`
   - Termux/Android → `lisp/termux-config.el`, `lisp/android-config.el`

2. **함수인가?**
   - `;;;###autoload` 필요 → `autoload/junghan.el`
   - 내부 함수 → 해당 `lisp/*.el` 내부

### 파일 추가 시

1. lisp/ 또는 autoload/에 파일 생성
2. 헤더 작성 (위 표준 따르기)
3. `(provide 'module-name)` 추가
4. `config.el`에 `(load! "lisp/module-name")` 추가
5. Outline 구조 (`;;;`, `;;;;`) 유지

## Git 커밋 가이드

### 커밋 메시지 스타일
```
feat: add EAF pyqterminal configuration

- Korean input via x-gtk-use-native-input
- Evil integration with SPC key handling
```

**중요**: "Generated with Claude" 또는 "Co-Authored-By" 제외! (깔끔한 커밋 로그 유지)

## Issue Tracking

This project uses **br** (beads_rust) for issue tracking. Run `br init` to get started.

## Quick Reference

```bash
br ready              # Find available work
br show <id>          # View issue details
br update <id> --status in_progress  # Claim work
br close <id>         # Complete work
br sync --flush-only  # Export JSONL (git commit separately)
```

