# dotdoom-starter

A lightweight, terminal-optimized Doom Emacs configuration for practical use

[한국어 문서](./README-KO.md)

## Overview

`dotdoom-starter` is a streamlined Doom Emacs configuration optimized for terminal (`-nw`) usage. It works consistently across Ubuntu 24.04, NixOS 25.05, and Termux (Android) environments.

### Key Features

- **Terminal-first design**: Optimized for `-nw` mode
- **Cross-platform**: Identical setup for laptop, server, and Android
- **Lightweight**: ~2000 lines of focused configuration
- **Simple setup**: Uses DOOMDIR environment variable instead of Doom's profile system

### Tested Environments

- **Platforms**: Ubuntu 24.04, NixOS 25.05, Termux
- **Emacs version**: 30.2
- **Terminal**: Ghostty recommended (works with others)

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
bash install-termux-pkgs-for-emacs.sh  # Additional packages
```

### 2. Install Doom Emacs and dotdoom-starter

```bash
# Clone Doom Emacs
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs-starter

# Clone dotdoom-starter
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/dotdoom-starter.git ~/repos/gh/dotdoom-starter

# Initial sync
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom sync
```

### 3. Shell Configuration (bashrc/zshrc)

```bash
# Aliases
alias esync='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom sync'
alias esyncenv='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom env'
alias esyncf='DOOMDIR="$HOME/repos/gh/dotdoom-starter" $HOME/doomemacs-starter/bin/doom sync -u -j 4'
alias e='env GTK_IM_MODULE=emacs XMODIFIERS=@im=emacs EMACS=emacs DOOMDIR=$HOME/repos/gh/dotdoom-starter $HOME/doomemacs-starter/bin/doom run -nw'
```

### 4. .desktop File (Optional, for GUI)

```bash
# Copy and modify paths in dotdoom-starter.desktop
cp dotdoom-starter.desktop ~/.local/share/applications/
# Edit the file to match your paths
```

## Usage

### Terminal Launch

```bash
# Basic launch
e

# Open file
e ~/document.org

# GUI mode (rarely used)
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom run
```

### Sync Configuration

```bash
# Normal sync (after init.el, packages.el changes)
esync

# Environment sync
esyncenv

# Force sync with package updates
esyncf
```

## Structure

```
dotdoom-starter/
├── init.el              # Doom module declarations
├── config.el            # Main configuration
├── packages.el          # Package declarations
├── +user-info.el        # User information
├── +gptel.el            # AI/LLM integration
├── +functions.el        # Custom functions
├── per-machine.el       # Machine-specific config (git-ignored)
├── user-keys.el         # Custom keybindings (git-ignored)
├── custom.el            # Emacs customize (git-ignored)
├── snippets/            # Custom snippets
├── var/                 # Runtime data
└── docs/                # Project documentation
```

### Key Files

- **init.el**: Doom module activation (~160 lines)
- **config.el**: Core settings and package configuration (~1850 lines)
- **packages.el**: Package additions/disables (~140 lines)
- **per-machine.el**: Machine-specific customization (auto-loaded, git-ignored)

## Core Features

### Editor

- **Evil mode**: Vim keybindings (`+everywhere`)
- **Completion**: Corfu + Orderless + Vertico
- **Snippets**: YASnippet + Tempel
- **File templates**: Auto-templates for empty files

### Tools

- **Git**: Magit
- **Tree-sitter**: Enhanced syntax highlighting
- **Direnv**: Project-specific environments
- **Docker**: Container management
- **LLM**: GPTel + Claude Code integration

### Language Support

Python, Nix, JavaScript/TypeScript, Web (HTML/CSS), YAML, Zig, Janet, Emacs Lisp

### Org-mode

- **Denote**: Note-taking system (with silo, sequence)
- **Org-roam**: Knowledge graph
- **Org-journal**: Journaling
- **Org-contacts**: Contact management
- **Export**: Hugo, Pandoc

### Publishing: Denote Export System

**Integrated export system** - Convert Denote notes to Hugo markdown with parallel processing

Key features:
- **Denote links → Hugo relref** automatic conversion
- **Security filtering**: ROT13, sensitive strings
- **Parallel processing**: Process 1,400+ files in 2-3 minutes with 8 cores
- **Sequential/parallel modes** supported

Usage:
```elisp
;; Sequential mode
(my/update-dblock-export-garden-all)

;; Parallel mode (recommended)
(my/update-dblock-export-garden-all-parallel)
```

Documentation: `docs/20251027T092900--denote-export-system__denote_export_hugo_guide.org`

### Disabled Packages

These packages are intentionally disabled:

- LSP-mode (prefer eglot when needed)
- Flycheck (using flymake)

See `packages.el` for complete list.

## Customization

### Per-machine Settings

Create `per-machine.el` for machine-specific configuration:

```elisp
;;; per-machine.el -*- lexical-binding: t; -*-

;; Font settings
(setq doom-font (font-spec :family "JetBrains Mono" :size 14))

;; Theme
(setq doom-theme 'doom-one)
```

### Custom Keybindings

Create `user-keys.el` for personal keybindings.

### Adding Packages

1. Add package declaration to `packages.el`
2. Add configuration to `config.el`
3. Run `esync`

## Troubleshooting

### Sync Issues

```bash
# Full rebuild
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom sync -u -j 4

# Clean byte-compiled files
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom clean
```

### Diagnostics

```bash
DOOMDIR="$HOME/repos/gh/dotdoom-starter" ~/doomemacs-starter/bin/doom doctor
```

### Server Restart

Terminal mode auto-starts an Emacs server named `starter`:

```bash
# Stop server
emacsclient -s starter -e '(kill-emacs)'

# Restart
e
```

## Project Background

This project was born from the realization that my personal Emacs configuration had become too complex to serve as a starter kit. After evaluating Doom's profiles feature and finding it problematic, I opted for a simpler approach using the DOOMDIR environment variable.

The goal is to create a practical Emacs environment that can serve as an alternative to CLI tools like Claude Code, covering diverse functionality in a terminal setting. While packages and configurations continue to grow, the focus remains on maintaining only essential features through continuous refinement.

## License

MIT License

## Contributing

Issues and PRs welcome.

## Related Links

- [Doom Emacs](https://github.com/doomemacs/doomemacs)
- [Ghostty Terminal](https://ghostty.org)
- [힣' Digital Garden: Dotfiles Emacs StarterKit](https://notes.junghanacs.com/notes/20240915T235008)
