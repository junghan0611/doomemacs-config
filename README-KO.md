# doomemacs-config

AI 협업 워크플로우를 위한 멀티 에이전트 중심 Doom Emacs 설정

[English Documentation](./README.md)

## 개요

`doomemacs-config`는 경량 터미널 스타터에서 발전한 제 메인 Doom Emacs 설정입니다. AI 도구(GPTel, Agent Shell, ECA Whisper, Edge TTS)와 EAF를 통합하여 터미널에서도 한글 입력을 지원하는 강력한 GUI 경험을 제공합니다.

### 철학

> "존재 대 존재 협업(Being to Being Collaboration)" - AI를 도구가 아닌 협력자로 대합니다.

이 설정은 AI가 일상 작업을 처리하고 인간은 창조의 씨앗에 집중하는 워크플로우를 지원합니다. NixOS, Emacs, 디지털 가든으로 재현 가능한 환경을 구축합니다.

### 주요 특징

- **멀티 에이전트 통합**: GPTel, Agent Shell (ACP), Claude Code IDE
- **음성 인터페이스**: ECA Whisper (STT), Edge TTS (음성 합성)
- **EAF 애플리케이션**: 브라우저, PDF 뷰어, pyqterminal (한글 입력 지원!)
- **Denote 생태계**: Hugo 내보내기, dblock 자동화, silo 관리
- **크로스 플랫폼**: Ubuntu 24.04, NixOS 25.11, Termux

### 테스트 환경

- **플랫폼**: Ubuntu 24.04, NixOS 25.11, Termux
- **Emacs 버전**: 30.x
- **터미널**: Ghostty (권장), Kitty, Termux

## 설치

### 1. Emacs 설치

**Ubuntu 24.04**
```bash
snap install emacs --classic
```

**NixOS 25.11**
```nix
environment.systemPackages = [ pkgs.emacs ];
```

**Termux**
```bash
pkg install emacs-nox
```

### 2. Doom Emacs 및 doomemacs-config 설치

```bash
# Doom Emacs 클론
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs-starter

# doomemacs-config 클론
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/doomemacs-config.git ~/repos/gh/doomemacs-config

# 초기 동기화
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom sync
```

### 3. Shell 설정

```bash
# .bashrc 또는 .zshrc에 추가
alias esync='DOOMDIR="$HOME/repos/gh/doomemacs-config" $HOME/doomemacs-starter/bin/doom sync'
alias esyncf='DOOMDIR="$HOME/repos/gh/doomemacs-config" $HOME/doomemacs-starter/bin/doom sync -u -j 4'
alias e='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run -nw'
alias egui='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run'
```

## 구조

```
doomemacs-config/
├── init.el              # Doom 모듈 선언
├── config.el            # 메인 설정 (로더)
├── packages.el          # 패키지 선언
├── custom.el            # Emacs customize (git 제외)
│
├── lisp/                # 모듈화된 설정 (5000+ 줄)
│   ├── korean-input.el      # 한글 입력, 폰트, NFD→NFC
│   ├── evil-config.el       # Evil 모드 설정
│   ├── completion-config.el # Corfu, Vertico, Consult
│   ├── ui-config.el         # Dashboard, Modeline, Themes
│   ├── org-config.el        # Org-mode 설정
│   ├── denote-config.el     # Denote 설정
│   ├── denote-silo.el       # 동적 silo 관리
│   ├── denote-export.el     # Hugo/Markdown 내보내기
│   ├── ai-gptel.el          # GPTel (Claude, OpenAI, Gemini)
│   ├── ai-agent-shell.el    # Agent Shell, ACP, Claude Code
│   ├── ai-stt-eca-whisper.el # 음성-텍스트 변환
│   ├── ai-tts-edge.el       # 텍스트-음성 변환 (Edge)
│   ├── eaf-config.el        # EAF 애플리케이션
│   ├── keybindings-config.el # 키 바인딩
│   └── ...
│
├── bin/                 # 독립 스크립트
│   ├── denote-export.el     # 통합 export/dblock 서버
│   └── denote-export.sh     # Shell 래퍼
│
├── autoload/            # 자동로드 함수
└── docs/                # 문서
```

## 핵심 기능

### AI/에이전트 통합

| 도구 | 설명 | 파일 |
|------|------|------|
| **GPTel** | LLM 통합 (Claude, OpenAI, Gemini, 로컬) | `ai-gptel.el` |
| **Agent Shell** | ACP 프로토콜, agent-shell-manager | `ai-agent-shell.el` |
| **ECA Whisper** | whisper.cpp 기반 음성-텍스트 | `ai-stt-eca-whisper.el` |
| **Edge TTS** | Microsoft Edge 텍스트-음성 | `ai-tts-edge.el` |
| **Claude Code IDE** | Claude Code 연동 | `ai-agent-shell.el` |

### EAF (Emacs Application Framework)

`lisp/eaf-config.el`에서 설정:
- **eaf-browser**: Chromium 기반 웹 브라우저
- **eaf-pdf-viewer**: 빠른 PDF 뷰어
- **eaf-pyqterminal**: 한글 입력 지원 터미널 (Qt native input)

주요 설정:
```elisp
;; 한글 입력 활성화
(setq-local x-gtk-use-native-input t)

;; Evil 통합 + SPC 키 처리
;; M-\ 로 모든 모드에서 other-window
```

### Denote Export 시스템

**bin/denote-export.el** - 통합 Denote 작업 서버

특징:
- 빠른 반복 내보내기를 위한 데몬 모드
- CI/스크립트용 배치 모드
- relref 링크로 Hugo markdown 변환
- dblock 업데이트 자동화
- Doom straight.el 패키지 로딩 (독립 실행)

사용법:
```bash
# 데몬 모드
emacs --daemon=denote-export-server --load bin/denote-export.el
emacsclient -s denote-export-server --eval '(denote-export-file "note.org")'

# 배치 모드
emacs --batch --load bin/denote-export.el -- dblock ~/org/meta

# Shell 래퍼
./bin/denote-export.sh ~/org/notes
```

### 한글 입력

`lisp/korean-input.el`에서 종합적인 한글 지원:
- 입력 방식 설정 (korean-hangul)
- Sarasa/D2Coding Nerd 폰트
- NFD → NFC 정규화 (Termux 수정)
- Evil 모드 자동 전환
- EAF Qt native input 통합

### 키바인딩

`lisp/keybindings-config.el`로 전역 일관성:
- **M-\\**: `other-window` (vterm, eaf, dired, org에서 작동)
- **M-u/M-v**: 위/아래 스크롤
- **SPC**: Doom 리더 (EAF에서 컨텍스트 인식)

## 커스터마이징

### 머신별 설정

`per-machine.el` 생성 (git 제외):
```elisp
;;; per-machine.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "GLG Nerd Font Mono" :size 15.1))
(setq doom-theme 'doom-one)
```

### 패키지 추가

1. `packages.el`에 추가
2. 적절한 `lisp/*.el`에 설정
3. `esync` 실행

## 문제 해결

```bash
# 완전 재빌드
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom sync -u -j 4

# 진단
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom doctor

# 정리
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom clean
```

## 프로젝트 진화

경량 터미널 우선 설정에서 시작하여 종합적인 멀티 에이전트 환경으로 발전했습니다. 주요 이정표:
- 한글 입력 수정과 터미널 최적화
- AI 도구 통합 (GPTel → Agent Shell → 음성 인터페이스)
- GUI 기능을 위한 EAF 도입
- 디지털 가든 퍼블리싱을 위한 Denote export 시스템

기기 간 재현성을 유지하면서 AI 협업 워크플로우에 집중합니다.

## 라이선스

MIT License

## 관련 링크

- [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [EAF](https://github.com/emacs-eaf/emacs-application-framework)
- [GLG-Mono Font](https://github.com/junghan0611/GLG-Mono)
- [힣's 디지털가든](https://notes.junghanacs.com)
