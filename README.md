# doomemacs-config

> This is not just another Doom Emacs dotfile.
> This is the frontend of a human-agent ecosystem where both sides share one org-agenda timeline,
> publish to the same digital garden, and evolve the system together — in plain text.

## How to Read This

If you glance at `config.el` and think "just another messy dotfile" — look closer.

**What you're seeing is a harness.** A 16K-line Emacs configuration where a human (GLG/힣) and AI agents operate on the same `~/org/` knowledge base of 3,300+ notes. They share a unified agenda. They stamp timestamps on the same datetree. The agents publish comments on the digital garden. The human refines, connects, and creates.

The solutions here are not fancy. A 33-minute full export. Shell scripts calling Emacs daemons. Org files instead of databases. If you're looking for cutting-edge infrastructure, this will disappoint. But look at the layer above: **Emacs is a meta-editor. Org-mode is a meta-document format.** The point is not the speed of any single operation — it's that the entire system composes. Org files become Hugo pages, become semantic memory, become agent context, become new org files. The 33-minute export runs unattended while GLG sleeps; incremental runs finish in seconds.

**This is one piece of a larger ecosystem.** The agent harness is [agent-config](https://github.com/junghan0611/agent-config) — that's where skills, delegation, semantic memory, and multi-agent orchestration live. Agents don't need Emacs. They use agent-config's toolset. But when they collaborate with GLG, they share this Emacs interface: the same org files, the same agenda, the same elisp APIs over sockets. Emacs is not a requirement — it's the meeting point. You don't have to use Emacs to work with this ecosystem. But this is where human and agent workflows converge.

And this timeline is not hidden inside Emacs. [geworfen](https://github.com/junghan0611/geworfen) throws it into the world — a WebTUI at [agenda.junghanacs.com](https://agenda.junghanacs.com) that renders the same `org-agenda` data live. Human journal entries, agent commit stamps, diary schedules — all on one public time axis. The same elisp function agents call, the same data the browser fetches.

The "clunky terminal screens" you see? They are nodes in a living graph: Emacs → org-mode → Denote notes → Hugo [digital garden](https://github.com/junghanacs/notes.junghanacs.com) → semantic memory → agent skills → back to Emacs. Every ugly buffer is a connection point.

**Why Emacs in a terminal matters**: This full Doom Emacs runs in TTY with clipboard integration, Korean input, and remote access over SSH — identical to GUI. An agent can spawn Emacs anywhere via `emacsclient`, eval elisp, read org files, stamp agenda entries. The terminal is not a limitation. It is the universal interface — the one that works on a laptop, a NUC, an Oracle ARM server, and a Galaxy Fold.

**Why org-mode**: Org is the meta-document format. Through [memex-kb](https://github.com/junghan0611/memex-kb), org files convert to any format without Pandoc. Notes accumulate in Denote, export to the [garden](https://notes.junghanacs.com), get hierarchized by [andenken](https://github.com/junghan0611/andenken), and feed into semantic memory shared with agents. It is the protocol, not just a markup language.

## Overview

A [Doom Emacs](https://github.com/doomemacs/doomemacs) configuration for human-agent collaborative workflows, built on NixOS.

| | |
|---|---|
| **Emacs** | 30.2 (stable) + 31 IGC/MPS (experimental) |
| **Framework** | Doom Emacs (hlissner style) |
| **Notes** | 3,300+ Denote org-mode files in `~/org/` |
| **Garden** | [notes.junghanacs.com](https://notes.junghanacs.com) — 2,100+ published |
| **Platforms** | NixOS (laptop, NUC, Oracle ARM), Termux (Galaxy Fold4) |
| **Lines** | ~16K across config + lisp + bin + scripts |

## Structure

```
doomemacs-config/
├── init.el              # Doom modules + single-instance guard
├── config.el            # Loader — requires lisp/*.el, kept minimal
├── packages.el          # Package declarations
├── per-machine.el       # Machine-specific (git-ignored)
│
├── lisp/                # Modular config (39 files, one concern each)
│   ├── ai-*.el              # AI/Agent integration (6)
│   ├── denote-*.el          # Denote ecosystem (4)
│   ├── org-*.el             # Org-mode (2)
│   ├── korean-input-config.el   # Korean input, fonts, NFD→NFC
│   ├── tty-config.el           # TTY: term-keys, kitty-graphics, clipboard
│   ├── workflow-shared.el       # Human ↔ Agent shared contract
│   └── ...                      # 25+ more modules
│
├── bin/                 # Standalone scripts
│   ├── agent-server.el          # Agent RPC server (socket "server")
│   ├── denote-export.el         # Multi-daemon export engine
│   ├── denote-export-parallel.py  # Python parallel orchestrator
│   ├── emacs-igc.sh             # Emacs 31 IGC launcher
│   └── verify-relref.py         # Hugo relref link validator
│
├── run.sh               # Unified CLI/TUI management
└── flake.nix            # Emacs 31 IGC (MPS GC) Nix build
```

## Human-Agent Unified Agenda

The core design: **one timeline for both human and AI**.

`workflow-shared.el` is loaded by both user Emacs and `agent-server.el`. It defines:

- **Dynamic `org-agenda-files`** from `_aprj` tagged Denote files + botlog + journal
- **Tag regex contract** — `[[:alnum:]@#%]+` only, matching Denote filetags
- **Active timestamps** in journal entries for agenda visibility

Agents stamp entries like:

```org
**** pi-skills: feat: summarize skill :pi:commit:piskills:
<2026-03-01 Sat 11:53>
```

The same `org-agenda` view shows human tasks and agent activity side by side.

## Emacs Server Architecture

Three isolated server sockets coexist:

| Socket | Instance | Purpose |
|--------|----------|---------|
| `"user"` | Emacs 30.2 GUI | GLG's primary editor (doom run / emacsclient) |
| `"server"` | Emacs 30.2 headless | Agent daemon — agents eval elisp via emacsclient |
| `"doom-igc"` | Emacs 31 (MPS GC) | IGC experimental frontend |

Independent terminal instances (`emacs -nw`) run without connecting to any server.

## Denote Export Pipeline

Parallel multi-daemon pipeline exporting 2,000+ org notes to Hugo markdown:

```
~/org/{meta,bib,notes,botlog}/*.org
        │
  denote-export-parallel.py    # Python ProcessPoolExecutor
        │
   ┌────┼────┐
   d1   d2   dN               # N Emacs daemons (default 4-8)
   └────┼────┘                 # ox-hugo + link conversion + security filters
        │
  ~/sync/markdown/.../content/
```

```bash
./run.sh export all           # Incremental (mtime-based)
./run.sh export all --force   # Full re-export
./run.sh dblock all           # Update dynamic blocks only
```

Full export (~2,000 files, 8 daemons) takes ~33 min — runs unattended.
Incremental export (daily use) finishes in seconds, processing only changed files.

## AI/Agent Modules

| Module | File | Purpose |
|--------|------|---------|
| GPTel | `ai-gptel.el` | Multi-backend LLM (Claude, OpenAI, Gemini, local) |
| Pi Agent | `ai-pi-agent.el` | Pi coding agent stdio RPC |
| Agent Shell | `ai-agent-shell.el` | ACP protocol, shell manager |
| Bot Config | `ai-bot-config.el` | Agent behavior & prompts |
| ECA Whisper | `ai-stt-eca-whisper.el` | Speech-to-text |
| Edge TTS | `ai-tts-edge.el` | Text-to-speech |
| tmux | `tmux-config.el` | Terminal multiplexer orchestration |

## Korean Input & TTY

Full Korean support across GUI and terminal:

- **Input**: korean-hangul with Evil auto-switching, abbrev expansion
- **Fonts**: Sarasa Gothic / D2Coding Nerd with CJK scaling
- **NFD→NFC**: macOS/Termux filename normalization
- **TTY**: term-keys, kitty-graphics protocol, OSC 52 clipboard
- **Unicode**: NBSP handling, zero-width space for ox-hugo

The terminal setup ensures `emacs -nw` over SSH has clipboard, Korean input, and full functionality — identical to GUI except for images.

## Emacs 31 IGC (Experimental)

MPS garbage collector build coexisting with stable 30.2:

```bash
./run.sh igc run        # GUI
./run.sh igc tty        # Terminal (alias: eti)
./run.sh igc install    # First setup
./run.sh igc debug      # --debug-init
```

Built via `flake.nix` using nix-community/emacs-overlay. Separate `EMACSDIR` (`~/doomemacs-igc`) and server socket (`"doom-igc"`).

## run.sh — Unified Management

```bash
./run.sh                # Interactive TUI menu
./run.sh sync           # doom sync
./run.sh agent start    # Start agent server
./run.sh export all     # Export all folders
./run.sh igc tty        # IGC terminal mode
./run.sh verify fix     # Fix broken Hugo relrefs
```

## Installation

```bash
# Doom Emacs
git clone https://github.com/doomemacs/doomemacs.git ~/doomemacs

# This config
git clone https://github.com/junghan0611/doomemacs-config.git ~/repos/gh/doomemacs-config
ln -s ~/repos/gh/doomemacs-config ~/.doom.d

# Sync
~/doomemacs/bin/doom sync
```

### Shell Aliases

```bash
alias et='emacs -nw'          # Emacs 30 terminal (standalone)
alias eti='~/.doom.d/bin/emacs-igc.sh --nw'  # Emacs 31 IGC terminal
```

## Tested Environments

| Platform | Emacs | Terminal | Status |
|----------|-------|----------|--------|
| NixOS 25.11 | 30.2 + 31 IGC | Ghostty / WezTerm | Primary |
| Termux (Android) | 30.x (nox) | Termux | Tested |

## Links

- [Digital Garden](https://notes.junghanacs.com) — 2,100+ notes published from this setup
- [Garden repo](https://github.com/junghanacs/notes.junghanacs.com) — Hugo source for the digital garden
- [geworfen](https://github.com/junghan0611/geworfen) — org-agenda live at [agenda.junghanacs.com](https://agenda.junghanacs.com)
- [agent-config](https://github.com/junghan0611/agent-config) — Pi agent harness (skills, delegation, memory)
- [memex-kb](https://github.com/junghan0611/memex-kb) — Org-mode → any format conversion
- [andenken](https://github.com/junghan0611/andenken) — Note hierarchy → semantic memory
- [nixos-config](https://github.com/junghan0611/nixos-config) — Reproducible system
- [GLG-Mono](https://github.com/junghan0611/GLG-Mono) — Custom programming font

## Acknowledgments

- [GNU Emacs](https://www.gnu.org/software/emacs/) — the extensible, self-documenting editor
- [Org Mode](https://orgmode.org/) — your life in plain text
- [Doom Emacs](https://github.com/doomemacs/doomemacs) — Henrik Lissner's framework
- [Protesilaos Stavrou](https://protesilaos.com/) — Denote, modus-themes, and a philosophy of computing
- [Pi Coding Agent](https://shittycodingagent.ai/) — Daniel Nouri's coding agent harness

## FAQ

**Q: Emacs 인스턴스 여러 개 띄우면 메모리 문제 없나?**

Measured on 27GB RAM laptop (2026-04-13):

| Process | RSS |
|---------|-----|
| GUI Emacs (doom run, user server) | ~413 MB |
| TTY Emacs + pi RPC (each) | ~360 MB + ~170 MB |
| Agent server (headless) | ~124 MB |

5 TTY instances (each with pi) ≈ 3.5 GB total. No issue on 27GB. Native-comp eln cache is shared across instances, so memory growth is sub-linear.

**Q: Why `emacs -nw` instead of a terminal multiplexer + emacsclient?**

Each standalone `emacs -nw` is a full Doom Emacs with its own pi coding agent. The `init.el` guard only blocks duplicate *daemons* — non-daemon instances run freely. This gives each terminal its own isolated agent session while sharing the same `~/org/` data.

**Q: How does Korean input work in terminal Emacs over SSH?**

Five layers, each solved independently:

| Layer | Solution |
|-------|----------|
| Physical key → X11 | xkb `kr104`: Right Alt → `Alt_R` keysym |
| X11 → Terminal | fcitx5 Default group (English) → passthrough |
| Terminal → Escape seq | WezTerm: `RightAlt` → term-keys byte sequence |
| Escape seq → Emacs event | term-keys input-decode-map → `<Hangul>` |
| Emacs event → Korean text | `toggle-input-method` → `korean-hangul` (internal) |

No OS IME inside Emacs. No NFD issues. Works identically over SSH + tmux. See `tty-config.el`, `korean-input-config.el`.

**Q: How does clipboard work in terminal Emacs?**

Emacs 29+ built-in `xterm.el` OSC 52 via `send-string-to-terminal` (not clipetty).

| Scenario | Method |
|----------|--------|
| Local TTY Emacs → system clipboard | xterm.el OSC 52 (`gui-set-selection`) |
| System clipboard → Emacs | xclip (Doom `tty` module) |
| SSH+tmux → local clipboard (Emacs) | xterm.el OSC 52 + tmux `allow-passthrough on` |
| SSH+tmux copy-mode (non-Emacs) | WezTerm copy mode (OSC 52 not supported over SSH — [known issue](https://github.com/wezterm/wezterm/issues/764)) |

WezTerm copy mode works everywhere. tmux's OSC 52 does not reach WezTerm over SSH — use WezTerm's built-in copy mode instead. Ghostty handles SSH OSC 52 natively.

**Q: Why block cursor in normal mode, bar in insert?**

DECSCUSR escape sequences in `tty-config.el` — 3 lines, no extra package. `evil-terminal-cursor-changer` disabled (was conflicting with term-keys/KKP).

**Q: 33 minutes for 2000 files? Really?**

Full export runs 8 parallel Emacs daemons via Python ProcessPoolExecutor. Incremental (daily use) processes only changed files — typically seconds. The architecture trades single-run speed for composability: the same ox-hugo + Denote pipeline handles dblock updates, security filtering, and link conversion in one pass.

## License

MIT
