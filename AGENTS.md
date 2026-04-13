# AGENTS.md — doomemacs-config Agent Guide

You are the **담당자** (agent-in-charge) for this repository.
This is not a toy dotfile. Read this before touching anything.

## What This Repo Is

A 16K-line Doom Emacs configuration that serves as the **frontend for a human-agent collaborative ecosystem**. Emacs here is not just a text editor — it is the harness where:

- **GLG (힣)** writes, thinks, and manages knowledge in org-mode
- **Agents** (Entwurf, secretaries, 힣봇s) read/write the same org files, stamp agenda entries, publish to the digital garden
- **Both sides share a single `org-agenda` timeline** via `workflow-shared.el`

The agent server (`bin/agent-server.el`) exposes elisp APIs over socket `"server"`. The human uses socket `"user"`. They operate on the same `~/org/` corpus.

## Architecture at a Glance

```
init.el                 # Doom modules + single-instance guard
config.el               # Loader only — requires lisp/*.el
├── lisp/ (39 files)    # One concern = one file
├── bin/                # Standalone scripts (no Doom dependency)
├── autoload/           # ;;;###autoload lazy functions
├── run.sh              # Unified CLI/TUI: sync, export, agent, IGC
└── flake.nix           # Emacs 31 IGC (MPS GC) via nix
```

## Code Organization Rules

### config.el is a loader

Only `(require ...)` and minimal glue. All logic lives in `lisp/`.

### One concern = one file

| Domain | File(s) |
|--------|---------|
| AI tools | `lisp/ai-*.el` (6 files) |
| Org-mode | `lisp/org-config.el`, `org-functions.el` |
| Denote | `lisp/denote-*.el` (4 files) |
| Export pipeline | `lisp/denote-export-config.el` + `bin/denote-export*.{el,py,sh}` |
| Korean input / fonts | `lisp/korean-input-config.el` |
| Unicode (NBSP, ZWS) | `lisp/unicode-config.el` |
| TTY (term-keys, kitty-graphics, clipboard) | `lisp/tty-config.el` |
| Evil | `lisp/evil-config.el` |
| Editing | `lisp/editing-config.el` |
| UI / theme | `lisp/ui-config.el` |
| Completion | `lisp/completion-config.el` |
| Search | `lisp/search-config.el` |
| Key bindings | `lisp/keybindings-config.el`, `keybindings-denote-config.el` |
| tmux / Zellij | `lisp/tmux-config.el`, `zellij-config.el` |
| RSS | `lisp/elfeed-config.el` |
| Programming | `lisp/prog-mode-config.el` |
| Termux/Android | `lisp/termux-config.el` |
| Tab bar | `lisp/tab-bar-config.el` |
| Presentation | `lisp/present-config.el` |
| Human-Agent shared | `lisp/workflow-shared.el` |

### File header standard

```elisp
;;; $DOOMDIR/lisp/module-name.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Description of what this module does.

;;; Code:

;;;; Section 1

;;;; Section 2

(provide 'module-name)
;;; module-name.el ends here
```

### Outline structure (outli.el)

Every `.el` file uses outline headings:

```elisp
;;; Level 1
;;;; Level 2
```

### Function placement

- Needs `;;;###autoload`? → `autoload/junghan.el`
- Internal to a module? → Inside that `lisp/*.el`

## Emacs Server Sockets

| Socket | Purpose | How it starts |
|--------|---------|---------------|
| `"user"` | GLG's GUI Emacs | `doom run` (Doom auto-calls `server-start` for GUI) |
| `"server"` | Agent daemon | `run.sh agent start` (separate `--init-directory`) |
| `"doom-igc"` | Emacs 31 IGC | `bin/emacs-igc.sh` |

The **single-instance guard** in `init.el` only blocks duplicate daemons. Non-daemon instances (`emacs -nw`, `doom run`) run independently.

## Key Workflows

### Adding a new config module

1. Create `lisp/my-feature-config.el` with header + `(provide 'my-feature-config)`
2. Add `(require 'my-feature-config)` to `config.el`
3. Maintain outline structure

### Editing bin/ scripts

- `bin/denote-export.el` and `lisp/denote-export-config.el` share logic
- Advice/settings go in `denote-export-config.el` so both sides pick them up
- New export folders → update `get-org-hugo-section-from-path` in `bin/denote-export.el`

### Agent server

```bash
./run.sh agent start    # Start (checks stale socket)
./run.sh agent status   # Health check
./run.sh agent restart  # Stop + start
./run.sh agent eval     # Interactive eval
```

### workflow-shared.el — the contract

This file defines rules that **both** user Emacs and agent-server **must** agree on:

| Setting | Why |
|---------|-----|
| `org-tag-re` | Tags allow only `[[:alnum:]@#%]+` — matches Denote filetags |
| `org-agenda-files` | Dynamic: `_aprj` tagged files + `botlog/agenda/` + current journal |
| Journal entry format | Active timestamps so entries appear in agenda |

**Rule**: UI/theme/keybindings can differ. **Data read/write rules must be identical.**

## Commit Messages

```
feat: add tty-config — term-keys, kitty-graphics, clipboard unified

- daemon guard now daemon-only (emacs -nw allowed)
- config.el terminal block → tty-config.el (45 lines removed)
```

**Never** include "Generated with Claude" or "Co-Authored-By".

## Things to Watch

- `doom sync` needed after `init.el` module changes, NOT for `config.el`/`lisp/` edits
- `per-machine.el` is git-ignored — font/theme overrides go there
- Emacs 31 IGC coexists with 30.2 via separate `EMACSDIR` and `server-name`
- Korean input edge cases: NFD→NFC, Evil state auto-switch, TTY clipboard
