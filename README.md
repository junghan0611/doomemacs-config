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
│   ├── verify-relref.py         # Hugo relref link validator + fixer
│   ├── verify-figures.py        # Figure src validator + fixer (Hugo + markdown)
│   └── check-description.sh     # description / abstract 누락 검사
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

Four isolated server sockets coexist. Daily use is **one GUI + many TTY clients attached to the `pi` daemon**:

| Socket | Instance | Purpose |
|--------|----------|---------|
| `"user"` | Emacs 30.2 GUI | GLG's primary editor — `doom run`, attach via `emacsclient -s user -t` (`ecs` alias) |
| `"pi"` | Emacs 30.2 headless (full Doom) | TTY attach target — every WezTerm tab attaches via `./run.sh pi tty` (`ep` alias). One full Doom shared by N terminals |
| `"server"` | Emacs 30.2 headless | Agent RPC daemon — agents eval elisp via `emacsclient -s server` (loaded with `bin/agent-server.el`) |
| `"doom-igc"` | Emacs 31 (MPS GC) | IGC experimental frontend (`./run.sh igc run`) |

Standalone `emacs -nw` (no daemon) still works (`et` alias) but is no longer the primary path — the `pi` daemon shares Doom state across all TTY tabs and avoids per-tab init cost.

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
| Bot Config | `ai-bot-config.el` | Telegram bot chat (telega.el) — talk to AI bots from Emacs |
| Whisper STT | `ai-stt-whisper.el` | Speech-to-text via Groq whisper-large-v3 |
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
./run.sh agent start    # Start agent RPC daemon (socket "server")
./run.sh pi start       # Start pi Doom daemon (socket "pi")
./run.sh pi tty         # Attach a new TTY client to the pi daemon
./run.sh export all     # Export all folders (incremental)
./run.sh export all --force  # Force re-export
./run.sh igc tty        # IGC terminal mode
./run.sh verify         # Verify garden content (relref + description + figures)
./run.sh fix            # Fix issues (relref + anchors + figures, step-by-step y/N)
```

### Garden Verify / Fix

가든 export 후 콘텐츠 품질 검증 / 정정. TUI Verify 섹션의 V/F 두 키로 통합.

**`./run.sh verify`** — read-only 검증 3단계:

| 단계 | 도구 | 검사 항목 |
|------|------|----------|
| relref | `verify-relref.py --summary` | ALIVE / VIRTUAL / REWRITE / DEAD / AMBIGUOUS / MALFORMED |
| description | `check-description.sh` | botlog/notes/bib `#+description:` / abstract callout 누락 |
| figures | `verify-figures.py` | ALIVE / REWRITE / AMBIGUOUS / DEAD / UNKNOWN<br/>Hugo `{{< figure src=…>}}` + Markdown `![](…)` 둘 다 |

**`./run.sh fix`** — 단계별 dry-run 후 y/N 적용:

| 단계 | 동작 |
|------|------|
| relref | DEAD/MALFORMED → plain text 치환, REWRITE → 경로 정정 |
| anchors | heading anchor 정리 (`{#h-…}` → GFM 호환) |
| figures | REWRITE → 소스 파일을 `notes/static/images/` 로 복사 + markdown src 를 `/images/{basename}` 로 치환 |

`verify-figures.py` SEARCH_DIRS 우선순위: `notes/static/images/` → `~/screenshot/` → `~/org/.attach/`. 이전 export에서 이미 정착된 파일이 먼저 잡혀 복사 없이 src 치환만 일어난다 (sha256 동일 시 SKIP-IDENT).

**잔존 broken** (DEAD / UNKNOWN) 는 소스 파일 자체가 없는 경우 — 원본 org 파일에서 사용자가 결정 (figure 제거 / 새 스크린샷 첨부).

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
# Daily use — daemon attach
alias ecs='emacsclient -s user -t'             # Attach TTY client to GUI Emacs (user daemon)
alias ep='~/.doom.d/run.sh pi tty'             # Attach TTY client to pi daemon (full Doom)
alias es='~/.doom.d/run.sh agent restart'      # Restart agent RPC daemon (socket "server")
alias eip='~/.doom.d/run.sh igc run'           # Emacs 31 IGC GUI

# Standalone (fallback)
alias et='emacs -nw'                           # Emacs 30 terminal, no daemon
alias eti='~/.doom.d/bin/emacs-igc.sh --nw'    # Emacs 31 IGC terminal, no daemon
```

**Typical session**: launch `doom run` once for the GUI, run `./run.sh pi start`, then open WezTerm tabs and type `ep` in each. All tabs share the same pi daemon — config edits, packages, native-comp eln cache, even open buffers are shared. Agent RPC runs in a separate `server` daemon so it never blocks the editor.

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

**Q: Doesn't running multiple Emacs instances waste memory?**

After switching to the daemon-attach model (2026-05) the memory footprint dropped sharply. Opening N WezTerm tabs no longer spawns N Emacs processes — only `emacsclient -t` clients are added.

| Process | RSS |
|---------|-----|
| GUI Emacs (`doom run`, user daemon) | ~413 MB |
| Pi Emacs daemon (full Doom, all TTY tabs share) | ~360 MB |
| `emacsclient -t` (per terminal tab) | < 5 MB |
| Agent RPC daemon (`server` socket, headless) | ~124 MB |

The old standalone model spawned a fresh ~360 MB Doom process per TTY tab — five tabs ≈ 1.8 GB. Now five tabs cost `360 MB + 5 × 5 MB` ≈ 385 MB. The native-comp eln cache was already shared, but the bigger win is paying the Doom init cost only once.

**Q: Why `pi` daemon + `emacsclient -t` instead of standalone `emacs -nw` per tab?**

Originally each tab launched its own standalone `emacs -nw`. The upside was a dedicated pi coding agent session per instance — but Doom init ran every time, and the package cache, theme, and open buffers were isolated.

Now a single full Doom daemon started by `./run.sh pi start` accepts attach from every terminal. A buffer opened in one tab is visible in another, and gptel sessions, the pi agent shell, and org-agenda are shared. Agent RPC runs in a separate `server` daemon so editing and agent traffic never block each other.

Standalone `emacs -nw` is kept as a fallback (`et` alias) — useful when the daemon is down or an isolated environment is needed.

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
