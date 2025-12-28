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
├── lisp/                # 독립 라이브러리 (5000+ 줄)
│   ├── korean-input.el      # 한글 입력, 폰트, NFD→NFC
│   ├── evil-config.el       # Evil 모드
│   ├── completion-config.el # Corfu, Vertico, Consult
│   ├── ui-config.el         # Dashboard, Modeline, Themes
│   ├── org-config.el        # Org-mode
│   ├── denote-config.el     # Denote 기본
│   ├── denote-silo.el       # Denote silo 동적 관리
│   ├── denote-export.el     # Denote export 시스템
│   ├── denote-functions.el  # Denote 유틸리티
│   ├── ai-gptel.el          # GPTel (Claude, OpenAI, Gemini)
│   ├── ai-agent-shell.el    # Agent Shell, ACP, Claude Code
│   ├── ai-stt-eca-whisper.el # 음성-텍스트 (Whisper)
│   ├── ai-tts-edge.el       # 텍스트-음성 (Edge TTS)
│   ├── ai-gptel-acp.el      # GPTel + ACP 통합
│   ├── eaf-config.el        # EAF 애플리케이션
│   ├── keybindings-config.el # 키 바인딩
│   ├── keybindings-remap.el # 키 리맵
│   ├── project-config.el    # 프로젝트 설정
│   ├── time-config.el       # 시간/캘린더
│   ├── utils-config.el      # 유틸리티
│   └── functions.el         # 일반 함수
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
| `ai-gptel.el` | 5K | GPTel 기본 설정 (Claude, OpenAI, Gemini) |
| `ai-agent-shell.el` | 10K | Agent Shell, ACP, Claude Code IDE |
| `ai-stt-eca-whisper.el` | 4K | ECA Whisper 음성-텍스트 |
| `ai-tts-edge.el` | 11K | Edge TTS 텍스트-음성 |
| `ai-gptel-acp.el` | 20K | GPTel + ACP 통합 (실험적) |

### EAF 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `eaf-config.el` | 3K | EAF 앱 설정 (browser, pdf, pyqterminal) |

주요 설정:
- 조건부 로딩 (`file-directory-p`)
- 한글 입력 (`x-gtk-use-native-input`)
- Evil 통합 (SPC 키 앱별 분기)
- Doom popup rule (mode-line 표시)

### 한글/입력 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `korean-input.el` | 18K | 한글 입력 전체 (폰트, NFD→NFC, Evil 전환) |
| `keybindings-config.el` | 12K | 전역 키바인딩 |
| `keybindings-remap.el` | 0.5K | 키 리맵 |

### Denote 관련

| 파일 | 크기 | 설명 |
|------|------|------|
| `denote-config.el` | 7K | Denote 기본 설정 |
| `denote-silo.el` | 8K | 동적 silo 관리 |
| `denote-export.el` | 22K | Hugo/Markdown export |
| `denote-functions.el` | 2K | 유틸리티 함수 |

### 기타

| 파일 | 크기 | 설명 |
|------|------|------|
| `evil-config.el` | 6K | Evil 모드 (커서, escape, smartparens) |
| `completion-config.el` | 6K | Corfu, Vertico, Consult |
| `ui-config.el` | 7K | Dashboard, Modeline, Themes |
| `org-config.el` | 14K | Org-mode 전체 |
| `project-config.el` | 2K | 프로젝트 설정 |
| `time-config.el` | 2K | 시간/캘린더 |
| `functions.el` | 7K | 일반 함수 |

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
   - Org 관련 → `lisp/org-config.el`
   - Denote 관련 → `lisp/denote-*.el`
   - Evil 관련 → `lisp/evil-config.el`
   - UI 관련 → `lisp/ui-config.el`
   - 한글 입력 → `lisp/korean-input.el`
   - 완성 시스템 → `lisp/completion-config.el`
   - 키바인딩 → `lisp/keybindings-config.el`

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

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
