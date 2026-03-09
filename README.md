# doomemacs-config

Multi-agent focused [Doom Emacs](https://github.com/doomemacs/doomemacs) configuration for AI-assisted workflows.

> "Being to Being Collaboration" — Treating AI as a collaborator, not just a tool.

## Overview

A comprehensive Doom Emacs environment integrating AI agents (GPTel, Pi Coding Agent, Claude Code) with Denote-based knowledge management and Digital Garden publishing. Built for reproducibility across NixOS, Ubuntu, and Termux.

### Highlights

- **Multi-agent orchestration** via tmux/Zellij — human and AI share a single org-agenda timeline
- **Denote export pipeline** — parallel multi-daemon org→Hugo conversion for 2,000+ notes
- **Korean-first** — full hangul input, NFD→NFC, Sarasa/D2Coding fonts across all modes
- **Cross-platform** — NixOS 25.11, Ubuntu 24.04, Termux (Galaxy Fold4)

## Installation

```bash
# 1. Doom Emacs
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs-starter

# 2. This config
mkdir -p ~/repos/gh/
git clone https://github.com/junghan0611/doomemacs-config.git ~/repos/gh/doomemacs-config

# 3. Sync
DOOMDIR="$HOME/repos/gh/doomemacs-config" ~/doomemacs-starter/bin/doom sync
```

### Shell Aliases

```bash
alias esync='DOOMDIR="$HOME/repos/gh/doomemacs-config" $HOME/doomemacs-starter/bin/doom sync'
alias e='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run -nw'
alias egui='DOOMDIR=$HOME/repos/gh/doomemacs-config $HOME/doomemacs-starter/bin/doom run'
```

## Structure

```
doomemacs-config/
├── init.el              # Doom module declarations
├── config.el            # Main configuration (loader, kept minimal)
├── packages.el          # Package declarations
├── custom.el            # Emacs customize (git-ignored)
├── per-machine.el       # Machine-specific settings (git-ignored)
│
├── lisp/                # Modular configuration (38 files)
│   ├── ai-*.el              # AI/Agent (6 files)
│   ├── denote-*.el          # Denote ecosystem (4 files)
│   ├── org-*.el             # Org-mode (2 files)
│   ├── keybindings-*.el     # Key bindings (2 files)
│   ├── korean-input-config.el   # Korean input, fonts, NFD→NFC
│   ├── denote-export-config.el  # Hugo export + dblock advice
│   ├── workflow-shared.el       # Human/Agent unified agenda
│   ├── tmux-config.el          # tmux agent integration
│   └── ...                     # 20+ more modules
│
├── bin/                 # Standalone scripts (no Doom dependency)
│   ├── denote-export.el         # Multi-daemon export server
│   ├── denote-export.sh         # Parallel export wrapper
│   ├── denote-export-parallel.py  # Python ProcessPoolExecutor
│   ├── agent-server.el          # Agent RPC server
│   └── edge-tts-convert.py     # TTS batch converter
│
└── autoload/            # Lazy-loaded functions (;;;###autoload)
```

## AI/Agent Integration

| Module | Description | File |
|--------|-------------|------|
| **GPTel** | Claude, OpenAI, Gemini, local models | `ai-gptel.el` |
| **Pi Coding Agent** | Lightweight AI agent via stdio RPC | `ai-pi-agent.el` |
| **Agent Shell** | ACP protocol, agent-shell-manager | `ai-agent-shell.el` |
| **Claude Code MCP** | MCP tool definitions | `+claude-code-ide-mcp-tools.el` |
| **Bot Config** | Agent behavior & prompt management | `ai-bot-config.el` |
| **ECA Whisper** | Speech-to-text | `ai-stt-eca-whisper.el` |
| **Edge TTS** | Text-to-speech (22K) | `ai-tts-edge.el` |
| **tmux/Zellij** | Terminal multiplexer agent workflows | `tmux-config.el`, `zellij-config.el` |

### Unified Agenda — Human & Agent Single Timeline

Human and AI agents share a single `org-agenda` timeline via `workflow-shared.el`. Both sides read/write `~/org/`, and the same agenda view is visible regardless of who calls it.

- **Dynamic `org-agenda-files`**: Built from `_aprj` tagged Denote files + `botlog/agenda/` + current journal
- **Category-based identity**: `#+category: Human` / `#+category: Agent`
- **Smart truncation**: Long agent entries auto-truncated to keep tags visible in split windows

#### `workflow-shared.el` — 인간/에이전트 공유 설정

Doom Emacs(사용자)와 `agent-server`(AI)가 **반드시 일치해야 하는** 설정을 한 곳에서 관리한다. 각 환경에서 org/denote 로드 이후에 평가된다.

| 환경 | 로딩 |
|------|------|
| Doom (사용자) | `config.el` → `(after! denote (require 'workflow-shared))` |
| agent-server (AI) | `bin/agent-server.el` → `(load "lisp/workflow-shared.el")` |

**공유 항목:**

| 설정 | 목적 |
|------|------|
| `org-tag-re` 및 관련 regex | 태그에 `-`, `_` 불허. Denote filetags와 일관성 유지. `[[:alnum:]@#%]+` |
| `org-agenda-files` 동적 구성 | `_aprj` 태그 + `botlog/agenda/` + 현재 주 journal |
| `my/org-journal-new-entry` | active timestamp 삽입으로 journal → agenda 연동 |

**원칙:** UI, 테마, 키바인딩은 각자 달라도 되지만, **데이터를 읽고 쓰는 규칙**(태그 파싱, agenda 범위, 타임스탬프 형식)은 반드시 동일해야 한다.

## Denote Export System

A multi-daemon parallel pipeline for exporting 2,000+ Denote org-mode notes to Hugo markdown for the [Digital Garden](https://notes.junghanacs.com).

### Architecture

```
~/org/{meta,bib,notes,botlog}/*.org
        │
        ▼
  denote-export.sh          # CLI entry point
        │
        ▼
  denote-export-parallel.py # Python ProcessPoolExecutor
        │
        ├─ daemon-1 ─┐
        ├─ daemon-2 ─┤  denote-export.el (Emacs daemons)
        ├─ daemon-3 ─┤  • Doom straight.el package loading
        └─ daemon-N ─┘  • ox-hugo export + link conversion
                         • Security filters (ROT13, sensitive strings)
        │
        ▼
  ~/sync/markdown/notes.junghanacs.com/content/{meta,bib,notes,botlog}/
```

### Usage

```bash
# Export all (meta → bib → notes → botlog)
bin/denote-export.sh all

# Single folder
bin/denote-export.sh botlog

# Force full re-export (skip mtime check)
bin/denote-export.sh all --force

# Dblock update only
bin/denote-export.sh dblock ~/org/meta
```

### Dblock Link Description — Citar-Style Dates

Custom advice on `denote-org--insert-links` provides folder prefix and citar-style date display:

```org
;; Format: folder/ title 'dateadded #datemodified
- [[denote:20250904T075937][notes/ @힣: AI 에이전트 편재성 '2025-09-04 #2026-02-14]]
- [[denote:20260307T131455][botlog/ SDF Zig 상태머신 리서치 '2026-03-07]]
```

- `'YYYY-MM-DD` — dateadded (from Denote ID, zero cost)
- `#YYYY-MM-DD` — datemodified (from `#+hugo_lastmod:`, 4KB read)
- `folder/` prefix — multi-folder sort boundary visibility

### Performance

| Folder | Files | Time (8 daemons) |
|--------|-------|-------------------|
| meta | 530 | ~9 min |
| bib | 649 | ~11 min |
| notes | 797 | ~13 min |
| botlog | 11 | <1 min |
| **Total** | **~2,000** | **~33 min** |

## Korean Input

Comprehensive Korean support in `korean-input-config.el`:

- **Input method**: korean-hangul with Evil auto-switching
- **Fonts**: Sarasa Gothic / D2Coding Nerd with proper CJK scaling
- **NFD→NFC**: macOS/Termux filename normalization
- **Unicode**: NBSP handling, zero-width space for ox-hugo (`unicode-config.el`)

## Key Bindings

| Key | Action | Scope |
|-----|--------|-------|
| `M-\` | `other-window` | Global (vterm, dired, org, eaf) |
| `M-u` / `M-v` | Scroll up/down | Global |
| `SPC` | Doom leader | Context-aware |

Full bindings: `keybindings-config.el`, `keybindings-denote-config.el`

## Customization

### Per-Machine Settings

Create `per-machine.el` (git-ignored):

```elisp
;;; per-machine.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Sarasa Fixed K Nerd Font" :size 15.0))
(setq doom-theme 'doom-one)
```

## Tested Environments

| Platform | Emacs | Terminal | Status |
|----------|-------|----------|--------|
| NixOS 25.11 | 30.x | Ghostty | Primary |
| Ubuntu 24.04 | 30.x | Kitty | Tested |
| Termux (Android) | 30.x (nox) | Termux | Tested |

## Changelog

### 2026-03 — Botlog & Dblock Enhancement

- **Botlog export**: Added `~/org/botlog` to Digital Garden pipeline
- **Dblock citar-style dates**: `'dateadded #datemodified` with folder prefix
- **denote-org advice**: Fixed upstream `include-date` file-type nil bug ([denote-org#21](https://github.com/protesilaos/denote-org/issues/21))
- **Export exclusion**: `agenda/` subdirectory filtered from export

### 2026-01 — Unified Agenda & Workflow

- **Unified agenda**: Human & Agent single timeline (`workflow-shared.el`)
- **Pi Coding Agent**: stdio RPC integration with Korean input
- **Bot config**: Agent behavior management (`ai-bot-config.el`)

### 2025-10 — Multi-Agent Architecture

- **Modular expansion**: 14 → 38 files in `lisp/`
- **tmux/Zellij integration**: Terminal multiplexer agent workflows
- **Denote export pipeline**: Multi-daemon parallel processing
- **Voice interfaces**: ECA Whisper (STT), Edge TTS (TTS)
- **GPTel expansion**: 5K → 36K with multi-backend support

### 2025-10-03 — v0.1.0 Initial Release

- First public release: terminal-optimized Doom Emacs configuration
- Evil + Corfu/Vertico completion, Denote ecosystem, Korean input
- Platform support: Ubuntu, NixOS, Termux

## License

MIT License

## Acknowledgments

This configuration stands on the shoulders of:

- [GNU Emacs](https://www.gnu.org/software/emacs/) — the extensible, self-documenting editor
- [Org Mode](https://orgmode.org/) — your life in plain text
- [Doom Emacs](https://github.com/doomemacs/doomemacs) — Henrik Lissner's opinionated Emacs framework
- [Protesilaos Stavrou](https://protesilaos.com/) — Denote, standard-themes, modus-themes, ef-themes, and a philosophy of computing that inspires
- [Pi Coding Agent](https://shittycodingagent.ai/) — Daniel Nouri's lightweight coding agent ([Emacs integration](https://danielnouri.org/notes/2025/12/30/an-emacs-mode-for-a-shitty-coding-agent/))
- All open-source contributors whose packages make this ecosystem possible

## Links

- [힣's Digital Garden](https://notes.junghanacs.com) — 2,000+ notes published from this setup
- [GLG-Mono Font](https://github.com/junghan0611/GLG-Mono) — custom programming font
- [NixOS Config](https://github.com/junghan0611/nixos-config) — reproducible system configuration
