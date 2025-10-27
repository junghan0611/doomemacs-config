# dotdoom-starter

간결하고 실용적인 터미널 최적화 Doom Emacs 설정

[English Documentation](./README.md)

## 개요

`dotdoom-starter`는 터미널(`-nw`) 사용에 최적화된 경량 Doom Emacs 설정입니다. Ubuntu 24.04, NixOS 25.05, Termux(Android) 환경에서 동일하게 작동합니다.

### 주요 특징

- **터미널 우선 설계**: `-nw` 모드에 최적화
- **크로스 플랫폼**: 노트북, 서버, 안드로이드에서 동일한 설정
- **경량 구성**: 약 2000줄의 집중된 설정
- **간단한 설치**: Doom profiles 대신 DOOMDIR 환경변수 활용

### 테스트 환경

- **플랫폼**: Ubuntu 24.04, NixOS 25.05, Termux
- **Emacs 버전**: 30.2
- **터미널**: Ghostty 권장 (다른 터미널에서도 작동)

## 설치

### 1. Emacs 설치

**Ubuntu 24.04**
```bash
snap install emacs --classic
```

**NixOS 25.05**
```nix
environment.systemPackages = [ pkgs.emacs ];
```

**Termux**
```bash
pkg install emacs-nox
bash install-termux-pkgs-for-emacs.sh  # 추가 패키지
```

### 2. Doom Emacs 및 dotdoom-starter 설치

```bash
# Doom Emacs 클론
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs-starter

# dotdoom-starter 클론
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/dotdoom-starter.git ~/repos/gh/dotdoom-starter

# 초기 동기화
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom sync
```

### 3. Shell 설정 (bashrc/zshrc)

```bash
# 별칭 설정
alias esync='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom sync'
alias esyncenv='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom env'
alias esyncf='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom sync -u -j 4'
alias e='env GTK_IM_MODULE=emacs XMODIFIERS=@im=emacs EMACS=emacs DOOMDIR=$HOME/repos/gh/dotdoom-starter $HOME/doomemacs-starter/bin/doom run -nw'
```

### 4. .desktop 파일 (선택사항, GUI용)

```bash
# 파일 복사 후 경로 수정
cp dotdoom-starter.desktop ~/.local/share/applications/
# 파일을 열어 경로를 자신의 환경에 맞게 수정
```

## 사용법

### 터미널 실행

```bash
# 기본 실행
e

# 파일 열기
e ~/document.org

# GUI 모드 (드물게 사용)
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom run
```

### 설정 동기화

```bash
# 일반 동기화 (init.el, packages.el 변경 시)
esync

# 환경 동기화
esyncenv

# 강제 동기화 (패키지 업데이트 포함)
esyncf
```

## 구조

```
dotdoom-starter/
├── init.el              # Doom 모듈 선언
├── config.el            # 주요 설정 파일
├── packages.el          # 패키지 선언
├── +user-info.el        # 사용자 정보
├── +gptel.el            # AI/LLM 통합 설정
├── +functions.el        # 커스텀 함수
├── per-machine.el       # 머신별 설정 (git 제외)
├── user-keys.el         # 커스텀 키바인딩 (git 제외)
├── custom.el            # Emacs 커스터마이즈 (git 제외)
├── snippets/            # 커스텀 스니펫
├── var/                 # 실행시 생성 데이터
└── docs/                # 프로젝트 문서
```

### 주요 파일

- **init.el**: Doom 모듈 활성화 (약 160줄)
- **config.el**: 핵심 설정 및 패키지 구성 (약 1850줄)
- **packages.el**: 패키지 추가/비활성화 (약 140줄)
- **per-machine.el**: 머신별 커스터마이징 (자동 로드, git 제외)

## 핵심 기능

### 에디터

- **Evil 모드**: Vim 키바인딩 (`+everywhere`)
- **완성**: Corfu + Orderless + Vertico
- **스니펫**: YASnippet + Tempel
- **파일 템플릿**: 빈 파일 자동 템플릿

### 도구

- **Git**: Magit
- **Tree-sitter**: 향상된 구문 강조
- **Direnv**: 프로젝트별 환경 관리
- **Docker**: 컨테이너 관리
- **LLM**: GPTel + Claude Code 통합

### 언어 지원

Python, Nix, JavaScript/TypeScript, Web (HTML/CSS), YAML, Zig, Janet, Emacs Lisp

### Org-mode

- **Denote**: 노트 관리 시스템 (silo, sequence 포함)
- **Org-journal**: 일기 기능
- **Org-contacts**: 연락처 관리
- **내보내기**: Hugo, Pandoc

### 퍼블리싱: Denote Export System

**통합 export 시스템** - Denote 노트를 Hugo markdown으로 병렬 변환

주요 기능:
- **Denote 링크 → Hugo relref** 자동 변환
- **보안 필터링**: ROT13, sensitive strings
- **병렬 처리**: 8 코어로 1,400+ 파일을 2-3분에 처리
- **순차/병렬 모드** 지원

사용법:
```elisp
;; 순차 방식
(my/update-dblock-export-garden-all)

;; 병렬 방식 (권장)
(my/update-dblock-export-garden-all-parallel)
```

상세 문서: `docs/20251027T092900--denote-export-system__denote_export_hugo_guide.org`

### 비활성화된 패키지

다음 패키지들은 의도적으로 비활성화되었습니다:

- LSP-mode (필요시 eglot 선호)
- Flycheck (flymake 사용)
- ...

전체 목록은 `packages.el` 참조.

## 커스터마이징

### 머신별 설정

`per-machine.el` 파일을 생성하여 머신별 설정 추가:

```elisp
;;; per-machine.el -*- lexical-binding: t; -*-

;; 폰트 설정
(setq doom-font (font-spec :family "JetBrains Mono" :size 14))

;; 테마
(setq doom-theme 'doom-one)
```

### 커스텀 키바인딩

`user-keys.el` 파일을 생성하여 개인 키바인딩 추가.

### 패키지 추가

1. `packages.el`에 패키지 선언 추가
2. `config.el`에 설정 추가
3. `esync` 실행

## 문제 해결

### 동기화 이슈

```bash
# 완전 재빌드
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom sync -u -j 4

# 바이트 컴파일 정리
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom clean
```

### 진단

```bash
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom doctor
```

### 서버 재시작

터미널 모드에서는 `starter` 이름의 Emacs 서버가 자동 시작됩니다:

```bash
# 서버 종료
emacsclient -s starter -e '(kill-emacs)'

# 재시작
e
```

## 프로젝트 배경

이 프로젝트는 개인 Emacs 설정이 너무 복잡해져서 스타터 키트로 적합하지 않다는 인식에서 시작되었습니다. Doom Emacs의 profiles 기능을 검토했으나 문제가 많아서, DOOMDIR 환경변수를 활용한 간단한 접근법을 선택했습니다.

목표는 터미널 환경에서 Claude Code와 같은 CLI 도구의 대안으로 활용할 수 있는, 다양한 기능을 커버하는 실용적인 Emacs 환경을 만드는 것입니다. 패키지와 설정이 계속 추가되고 있지만, 지속적으로 정리하면서 핵심 기능만 유지할 계획입니다.

## 라이선스

MIT License

## 기여

이슈 및 PR 환영합니다.

## 관련 링크

- [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [Ghostty Terminal](https://ghostty.org)
- [힣' 디지털가든: 닷파일 이맥스 스타터키트](https://notes.junghanacs.com/notes/20240915T235008)
