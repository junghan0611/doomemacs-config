# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- **yank-code-with-context**: AI 에이전트용 코드 복사 함수 (markdown 코드블록 + 경로:라인 메타데이터)
- **AI/Agent**: AI 오케스트레이션 (`ai-orchestration.el`), GPTel ACP v2, Claude Code MCP 도구
- **터미널 멀티플렉서**: tmux 통합 (`tmux-config.el`, 26K), Zellij 통합 (`zellij-config.el`, 20K)
- **모듈 분리**: editing-config, modeline-config, tab-bar-config, search-config, prog-mode-config, module-emacs-config, elfeed-config, unicode-config 등 19개 신규 파일
- **플랫폼별 설정**: android-config.el, termux-config.el 분리
- **SKS Hub 네비게이션**: Zig 상태 머신 네비게이션 도구 (`sks-hub-nav.el`)
- **Zotero 통합**: Zotero Translation Server 연동 (`zotero-config.el`)
- **Org 확장**: org-glossary, org-download, org-rich-yank, org-appear 추가
- **패키지**: casual (transient UI), adoc-mode (AsciiDoc), tmr (타이머)
- **Denote 키바인딩**: 전용 파일 분리 (`keybindings-denote-config.el`)
- **Embark + GPTel**: region/file 액션에 GPTel 통합
- **GPTel 프롬프트**: .poet 템플릿 지원, 프롬프트 선택 기능

### Changed
- **파일명 표준화**: `korean-input.el` → `korean-input-config.el`, `denote-export.el` → `denote-export-config.el`, `denote-silo.el` → `denote-silo-config.el`
- **GPTel 대폭 확장**: 5K → 36K (프롬프트, 백엔드, 도구 통합)
- **completion-config**: 6K → 13K 확장
- **denote-functions**: 2K → 19K 확장
- **LSP**: lsp-mode 재도입 (eglot과 병행)
- **Syntax checking**: flycheck 비활성화, flymake으로 전환
- **터미널 true-color**: 로직 정리 및 개선

### Removed
- **AI-code-interface**: ClaudeCode 관련 패키지 정리 (deprecation)
- **beads (bd)**: bd 제거, br (beads_rust)로 마이그레이션
- **keybindings-remap.el**: 제거
- **minuet**: 주석 처리
- **Tree-sitter**: Zig, Go 모듈에서 tree-sitter 제거

## [0.1.0] - 2025-10-03

### Project Inception
First public release of doomemacs-config - a terminal-optimized Doom Emacs configuration.

### Core Features

#### Editor & Completion
- Evil mode with vim keybindings (`+everywhere`)
- Corfu + Orderless + Vertico for completion
- YASnippet + Tempel for snippets
- File templates for empty files
- Multiple cursors support
- Aggressive indent mode
- Whitespace auto-trim

#### UI/UX
- Doom dashboard
- Modeline with configuration
- Neotree file browser
- Workspaces (tab emulation)
- Window selection with numbers
- Zen mode for distraction-free editing
- Smooth scrolling
- VC gutter for git diff display

#### Tools & Integration
- **Git**: Magit for version control
- **Tree-sitter**: Enhanced syntax highlighting for supported languages
- **Direnv**: Project-specific environment management
- **Docker**: Container management with tree-sitter support
- **LSP**: Eglot integration (LSP-mode disabled)
- **Syntax checking**: Flymake (Flycheck disabled)
- **LLM Integration**:
  - GPTel with multiple backends (OpenAI, Anthropic, xAI, Perplexity)
  - Claude Code integration (two implementations)
  - ACP (Agent Client Protocol) support
  - Monet (AI assistant, desktop only)
  - Agent Shell
- **Password Manager**: pass integration with menu
- **Terminal**:
  - Vterm support
  - Eshell
  - Clipetty for OSC52 clipboard support
  - Term-keys for Kitty terminal

#### Language Support
- **Emacs Lisp**: Full support
- **Python**: Tree-sitter enabled
- **Nix**: Tree-sitter enabled
- **JavaScript/TypeScript**: Tree-sitter enabled
- **Web (HTML/CSS)**: Tree-sitter enabled
- **YAML**: Tree-sitter enabled
- **JSON**: Tree-sitter enabled
- **Zig**: Tree-sitter enabled
- **Janet**: Tree-sitter enabled
- **C/C++**: Tree-sitter enabled
- **Shell**: sh/bash support
- **Data formats**: Generic support
- **Markdown**: Tree-sitter enabled
- **LaTeX**: Full support with preview, fragtog, laas

#### Org-mode Ecosystem
- **Core**: Org with extensive module support
- **Export**: Hugo, Pandoc, Gnuplot integration
- **Present**: Presentation mode
- **Contacts**: Contact management
- **Journal**: Journaling functionality
- **Denote**: Complete note-taking system
  - denote-org integration
  - denote-silo for separate note collections
  - denote-sequence for sequential notes
  - denote-markdown support
  - denote-explore for discovery
  - denote-search for finding notes
  - citar-denote for bibliography integration
- **Bibliography**: Biblio support
- **Email**: Notmuch integration with org support
- **Calendar**: Calendar application
- **RSS**: RSS reader with org integration

#### Custom Packages & Themes
- **Themes**: Custom doom-themes fork (junghan0611/doom-themes:ko branch)
- **Standard themes**: Additional theme options
- **Outli**: Code outlining
- **Imenu-list**: Custom fork for code navigation
- **Dired-preview**: File preview in dired
- **Jinx**: Spell checker
- **Casual**: Transient UI helpers

#### Platform Support
- **Ubuntu 24.04**: Full support via snap
- **NixOS 25.05**: Full support via nixpkgs
- **Termux (Android)**: Tested and working with platform-specific adaptations
  - Auto-detection with IS-TERMUX flag
  - Path adjustments for Android environment
  - emacs-nox optimized
- **macOS**: Basic support with macos module

#### Configuration Architecture
- **Modular structure**: Separated concerns across multiple files
  - `init.el`: Module declarations (~160 lines)
  - `config.el`: Main configuration (~1850 lines)
  - `packages.el`: Package management (~144 lines)
  - `+user-info.el`: User information
  - `+gptel.el`: LLM/AI configuration
  - `+functions.el`: Custom functions
  - `+office.el`: Office/productivity settings
  - `per-machine.el`: Machine-specific config (git-ignored)
  - `user-keys.el`: Custom keybindings (git-ignored)
  - `custom.el`: Emacs customize system (git-ignored)
- **Profile system**: Uses DOOMDIR environment variable instead of Doom's built-in profiles
- **Server mode**: Auto-starts Emacs server named "starter" in terminal mode
- **Helper scripts**:
  - `get-doomemacs.sh`: Install Doom Emacs
  - `start-emacs.sh`: Launch with proper environment
  - `sync-profile.sh`: Sync configuration
  - `install-termux-pkgs-for-emacs.sh`: Termux dependencies

### Intentionally Disabled
The following packages are explicitly disabled to maintain a focused, lightweight configuration:

- **LSP**: lsp-mode, lsp-python-ms (prefer eglot)
- **Syntax checking**: flycheck, flycheck-popup-tip, flycheck-plantuml, flyspell-lazy, flymake-popon
- **UI**: solaire-mode, treemacs-nerd-icons
- **Evil**: evil-snipe, evil-goggles, evil-terminal-cursor-changer
- **Dired**: diredfl, dirvish
- **Undo**: vundo, undo-fu-session
- **Code review**: code-review
- **RSS**: elfeed-goodies
- **Language**: demangle-mode, cuda-mode, opencl-mode, nose

### Known Issues
- Documentation was initially missing (resolved in this release)
- Emoji display in terminal can break line alignment (recommendation: avoid emojis in terminal mode)

[unreleased]: https://github.com/junghan0611/doomemacs-config/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/junghan0611/doomemacs-config/releases/tag/v0.1.0
