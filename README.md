# doomemacs-config

Multi-agent focused Doom Emacs configuration for AI-assisted workflows

[한국어 문서](./README-KO.md)

## Overview

`doomemacs-config` is my main Doom Emacs configuration, evolved from a lightweight terminal starter to a comprehensive multi-agent environment. It integrates AI tools (GPTel, Agent Shell, ECA Whisper, Edge TTS) with EAF for a powerful GUI experience including Korean input support in terminals.

### Philosophy

> "Being to Being Collaboration" - Treating AI as a collaborator, not just a tool.

This configuration supports the workflow where AI handles routine tasks while humans focus on creative seeds. Built for reproducibility with NixOS, Emacs, and Digital Garden.

### Key Features

- **Multi-agent integration**: GPTel, Agent Shell (ACP), Claude Code MCP tools, AI orchestration
- **Terminal multiplexer**: tmux/Zellij integration for multi-agent workflows
- **Voice interfaces**: ECA Whisper (STT), Edge TTS (text-to-speech)
- **AI collaboration**: `yank-code-with-context` for agent-friendly code sharing
- **EAF applications**: Browser, PDF viewer, pyqterminal with Korean input
- **Denote ecosystem**: Export to Hugo, dblock automation, silo management
- **Cross-platform**: Ubuntu 24.04, NixOS 25.05, Termux

### Tested Environments

- **Platforms**: Ubuntu 24.04, NixOS 25.05, Termux
- **Emacs version**: 30.x
- **Terminals**: Ghostty (recommended), Kitty, Termux

## Installation

### 1. Install Emacs

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
```

### 2. Install Doom Emacs and doomemacs-config

```bash
# Clone Doom Emacs
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs-starter

# Clone doomemacs-config
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/doomemacs-config.git ~/repos/gh/doomemacs-config

# Initial sync
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom sync
```

### 3. Shell Configuration

```bash
# Aliases for .bashrc or .zshrc
alias esync='DOOMDIR="$HOME/repos/gh/doomemacs-config" $HOME/doomemacs-starter/bin/doom sync'
alias esyncf='DOOMDIR="$HOME/repos/gh/doomemacs-config" $HOME/doomemacs-starter/bin/doom sync -u -j 4'
alias e='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run -nw'
alias egui='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run'
```

## Structure

```
doomemacs-config/
├── init.el              # Doom module declarations
├── config.el            # Main configuration (loader)
├── packages.el          # Package declarations
├── custom.el            # Emacs customize (git-ignored)
│
├── lisp/                # Modular configuration (39 files)
│   ├── ai-gptel.el          # GPTel (Claude, OpenAI, Gemini) - 36K
│   ├── ai-agent-shell.el    # Agent Shell, ACP, Claude Code
│   ├── ai-orchestration.el  # Multi-agent orchestration
│   ├── ai-gptel-acp.el      # GPTel + ACP integration
│   ├── ai-stt-eca-whisper.el # Speech-to-text (Whisper)
│   ├── ai-tts-edge.el       # Text-to-speech (Edge)
│   ├── tmux-config.el       # tmux integration - 26K
│   ├── zellij-config.el     # Zellij integration - 20K
│   ├── korean-input-config.el # Korean input, fonts, NFD→NFC
│   ├── denote-*.el          # Denote ecosystem (4 files)
│   ├── org-config.el        # Org-mode settings
│   ├── functions.el         # Utility functions (yank-code-with-context)
│   ├── keybindings-config.el # Key bindings
│   ├── eaf-config.el        # EAF applications
│   └── ...                  # 25+ more config modules
│
├── bin/                 # Standalone scripts
│   ├── denote-export.el     # Unified export/dblock server
│   └── denote-export.sh     # Shell wrapper
│
├── autoload/            # Autoloaded functions
└── docs/                # Documentation
```

## Core Features

### AI/Agent Integration

| Tool | Description | File |
|------|-------------|------|
| **GPTel** | LLM integration (Claude, OpenAI, Gemini, local) | `ai-gptel.el` |
| **Agent Shell** | ACP protocol, agent-shell-manager | `ai-agent-shell.el` |
| **AI Orchestration** | Multi-agent coordination | `ai-orchestration.el` |
| **Claude Code MCP** | MCP tool definitions for Claude Code | `+claude-code-ide-mcp-tools.el` |
| **ECA Whisper** | Speech-to-text via whisper.cpp | `ai-stt-eca-whisper.el` |
| **Edge TTS** | Microsoft Edge text-to-speech | `ai-tts-edge.el` |
| **tmux/Zellij** | Terminal multiplexer agent workflows | `tmux-config.el`, `zellij-config.el` |

### EAF (Emacs Application Framework)

Configured in `lisp/eaf-config.el`:
- **eaf-browser**: Chromium-based web browser
- **eaf-pdf-viewer**: Fast PDF viewing
- **eaf-pyqterminal**: Terminal with Korean input support (via Qt native input)

Key settings:
```elisp
;; Korean input enabled
(setq-local x-gtk-use-native-input t)

;; Evil integration with SPC key handling
;; M-\ for other-window across all modes
```

### Denote Export System

**bin/denote-export.el** - Unified Denote operations server

Features:
- Daemon mode for fast repeated exports
- Batch mode for CI/scripts
- Hugo markdown conversion with relref links
- Dblock update automation
- Doom straight.el package loading (standalone execution)

Usage:
```bash
# Daemon mode
emacs --daemon=denote-export-server --load bin/denote-export.el
emacsclient -s denote-export-server --eval '(denote-export-file "note.org")'

# Batch mode
emacs --batch --load bin/denote-export.el -- dblock ~/org/meta

# Shell wrapper
./bin/denote-export.sh ~/org/notes
```

### Korean Input

Comprehensive Korean support in `lisp/korean-input.el`:
- Input method configuration (korean-hangul)
- Sarasa/D2Coding Nerd fonts
- NFD → NFC normalization (Termux fix)
- Evil mode auto-switching
- EAF Qt native input integration

### Keybindings

Global consistency via `lisp/keybindings-config.el`:
- **M-\\**: `other-window` (works in vterm, eaf, dired, org)
- **M-u/M-v**: scroll up/down
- **SPC**: Doom leader (context-aware in EAF)

## Customization

### Per-machine Settings

Create `per-machine.el` (git-ignored):
```elisp
;;; per-machine.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "GLG Nerd Font Mono" :size 15.1))
(setq doom-theme 'doom-one)
```

### Adding Packages

1. Add to `packages.el`
2. Configure in appropriate `lisp/*.el`
3. Run `esync`

## Troubleshooting

```bash
# Full rebuild
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom sync -u -j 4

# Diagnostics
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom doctor

# Clean
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom clean
```

## Project Evolution

Started as a lightweight terminal-first configuration, evolved into a comprehensive multi-agent environment. Key milestones:
- Terminal optimization with Korean input fixes
- AI tool integration (GPTel → Agent Shell → Voice interfaces)
- EAF adoption for GUI capabilities
- Denote export system for Digital Garden publishing
- Multi-agent orchestration via tmux/Zellij integration
- Agent collaboration tools (yank-code-with-context, MCP tools)
- Modular architecture expansion (14 files → 39 files)

The focus remains on AI-assisted workflows while maintaining reproducibility across devices.

## License

MIT License

## Related Links

- [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [EAF](https://github.com/emacs-eaf/emacs-application-framework)
- [GLG-Mono Font](https://github.com/junghan0611/GLG-Mono)
- [힣's Digital Garden](https://notes.junghanacs.com)
