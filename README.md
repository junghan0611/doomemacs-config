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
| **Emacs** | 30.2 (system stable) + unstable channel (auto-bumps to 31.1 on release) |
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
│   ├── tty-config.el           # TTY: term-keys, kitty-graphics, ghostel
│   ├── workflow-shared.el       # Human ↔ Agent shared contract
│   └── ...                      # 25+ more modules
│
├── bin/                 # Standalone scripts
│   ├── agent-server.el          # Agent RPC server (socket "server")
│   ├── denote-export.el         # Multi-daemon export engine
│   ├── denote-export-parallel.py  # Python parallel orchestrator
│   ├── emacs-unstable.sh        # Emacs unstable channel launcher (next stable)
│   ├── fix-org-links.el         # Stage 1 — ~/org link rewriter (file:~/repos/gh → GitHub URL)
│   ├── site-policy.el           # SSOT for host aliases, internal-path patterns, lychee opts
│   ├── verify-relref.py         # Hugo relref link validator + fixer
│   ├── verify-figures.py        # Figure src validator + fixer (Hugo + markdown)
│   ├── verify-content.py        # Garden content hygiene (host alias, internal path, GitHub 404)
│   ├── verify-org-links.py      # Stage 1.5 — ~/org GitHub URL lychee verify (read-only)
│   └── check-description.sh     # Check for missing description / abstract
│
├── prompts/             # gptel-agent system prompts (overrides upstream)
│   └── gptel-agent.md            # Subagent-free variant of upstream default
│
├── run.sh               # Unified CLI/TUI management
└── flake.nix            # Emacs unstable channel Nix build (overlay#emacs-unstable)
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
| `"doom-unstable"` | Emacs unstable channel | Next-stable preview frontend (`./run.sh unstable run`) |

Standalone `emacs -nw` (no daemon) still works (`et` alias) but is no longer the primary path — the `pi` daemon shares Doom state across all TTY tabs and avoids per-tab init cost.

## pi-shell-acp Integration

This config is the editor surface that [pi-shell-acp](https://github.com/junghan0611/pi-shell-acp) targets. pi-shell-acp is my ACP bridge between [pi](https://github.com/dnouri/pi-coding-agent) (the harness) and Claude/Codex/Gemini backends — it spawns coding agents that read org files, eval elisp, stamp agenda entries, and write to the shared Denote corpus, all through `emacsclient` against the `server` socket.

The bridge accepts `--emacs-agent-socket` and forwards it to ACP children as `PI_EMACS_AGENT_SOCKET`:

```elisp
;; pi-coding-agent buffer config
(setq pi-coding-agent-extra-args
      '("--entwurf-control" "--emacs-agent-socket" "server"))
```

```bash
# Skills inside the agent then call Emacs without hardcoding a socket
emacsclient -s "${PI_EMACS_AGENT_SOCKET:-server}" --eval '(agent-denote-add-history ...)'
```

The socket split is intentional:

- **`server`** — agent RPC daemon (`bin/agent-server.el`). Headless, loaded by `./run.sh agent start`. Agents do CRUD here so they never block the editor.
- **`pi`** — human TTY attach (`./run.sh pi tty`, `ep` alias). Full Doom. This is where GLG actually writes.
- **`user`** — GUI Emacs for visual work (Magit graphs, ePub, image-heavy org).

Because the primary editor surface is `emacs -nw` / `emacsclient -t`, every keybinding has to survive in TTY frames. Cases like `:lang org`'s `,` localleader being silently dropped under `emacs --daemon` (fixed in [org-config.el](lisp/org-config.el) by deferring `+org-init-keybinds-h` to `server-after-make-frame-hook`, the same pattern doom itself uses in [`doom-keybinds.el:80`](https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-keybinds.el)) are taken seriously here — not as Emacs trivia, but because the terminal is the surface agents and humans both touch.

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

The **main workflow** is `gptel-agent` (karthink) — a Markdown/Org agent definition
system that loads sub-agent specs from disk, registers skills from `~/.claude/skills/`,
and runs tools inside the Emacs buffer. Plain `gptel-send` chat is rarely used; almost
everything flows through agent presets.

| Module | File | Purpose |
|--------|------|---------|
| **gptel-agent** | `ai-gptel.el` (`use-package! gptel-agent`) | **Primary chat/agent surface** — agent presets, skills, in-buffer tool execution |
| GPTel core | `ai-gptel.el` | Multi-backend wiring (Claude, OpenAI-sub OAuth, Gemini, DeepSeek, local) + Codex stream advice |
| Pi Agent | `ai-pi-agent.el` | Pi coding agent stdio RPC |
| Agent Shell | `ai-agent-shell.el` | ACP protocol, shell manager |
| Bot Config | `ai-bot-config.el` | Telegram bot chat (telega.el) — talk to AI bots from Emacs |
| Whisper STT | `ai-stt-whisper.el` | Speech-to-text via Groq whisper-large-v3 |
| Edge TTS | `ai-tts-edge.el` | Text-to-speech |
| tmux | `tmux-config.el` | Terminal multiplexer orchestration |

### Custom gptel-agent prompts (`prompts/`)

`gptel-agent-dirs` is appended with our `prompts/` directory so files here
override upstream definitions of the same name. Currently shipping:

- `prompts/gptel-agent.md` — subagent-free variant of the upstream default.
  DELEGATE / `<tool name="Agent">` / researcher / executor / introspector
  guidance removed; skills + 14 default tools (TodoWrite / Glob / Grep / Read /
  Edit / Write / Bash / Eval / WebSearch / WebFetch / ...) preserved.

Why: the upstream default prompt instructs the LLM to delegate aggressively,
which (a) bloats every system message with ~100 lines of routing rules and
(b) triggers tool-only stream completions on the OAuth Codex backend, hitting
[gptel-agent #107](https://github.com/karthink/gptel-agent/issues/107). Removing
the delegation guidance keeps a single foreground agent loop — closer to how I
actually use Emacs day to day.

## Korean Input & TTY

Full Korean support across GUI and terminal:

- **Input**: korean-hangul with Evil auto-switching, abbrev expansion
- **Fonts**: Sarasa Gothic / D2Coding Nerd with CJK scaling
- **NFD→NFC**: macOS/Termux filename normalization
- **TTY**: term-keys, kitty-graphics protocol, OSC 52 clipboard
- **Unicode**: NBSP handling, zero-width space for ox-hugo

The terminal setup ensures `emacs -nw` over SSH has clipboard, Korean input, and full functionality — identical to GUI except for images.

## Emacs Unstable Channel

A second Emacs install that follows [emacs-overlay](https://github.com/nix-community/emacs-overlay)'s `emacs-unstable` attribute (= latest Savannah release tag). Today it's the same `30.2` as system stable; on the day Emacs 31.1 is officially tagged, this channel auto-bumps to 31.1 a few hours / days ahead of nixpkgs — that's the whole point: ride the next stable preview without leaving the stable line.

```bash
./run.sh unstable run        # GUI
./run.sh unstable tty        # Terminal (alias: etu)
./run.sh unstable install    # First setup
./run.sh unstable debug      # --debug-init
```

Built via `flake.nix` using nix-community/emacs-overlay. Separate `EMACSDIR` (`~/doomemacs-unstable`) and server socket (`"doom-unstable"`) so it coexists with the system stable Emacs.

## run.sh — Unified Management

```bash
./run.sh                       # Interactive TUI menu
./run.sh sync                  # doom sync
./run.sh agent start           # Start agent RPC daemon (socket "server")
./run.sh pi start              # Start pi Doom daemon (socket "pi")
./run.sh pi tty                # Attach a new TTY client to the pi daemon
./run.sh export all            # Export all folders (incremental)
./run.sh export all --force    # Force re-export
./run.sh unstable tty          # Emacs unstable channel — terminal mode
./run.sh verify                # Verify garden md ([1/4]~[4/4], read-only)
./run.sh fix                   # Fix garden md ([1/4]~[4/4], step-by-step y/N)
./run.sh fix-org               # Rewrite ~/org link patterns (dry-run + y/N + --apply)
./run.sh fix-org --check       # Verify ~/org GitHub URLs with lychee (read-only)
```

### Garden Verify / Fix

Post-export content quality verification and correction for the digital garden. Unified under the TUI Verify section's V/F keys. Four stages:

**`./run.sh verify`** — read-only verification [1/4]~[4/4]:

| Stage | Tool | Checks |
|-------|------|--------|
| [1/4] relref | `verify-relref.py --summary` | ALIVE / VIRTUAL / REWRITE / DEAD / AMBIGUOUS / MALFORMED |
| [2/4] description | `check-description.sh` | Missing `#+description:` / abstract callout in botlog / notes / bib |
| [3/4] figures | `verify-figures.py` | ALIVE / REWRITE / AMBIGUOUS / DEAD / UNKNOWN — covers both Hugo `{{< figure src=…>}}` and Markdown `![](…)` |
| [4/4] content | `verify-content.py --lychee` | HOST_ALIAS / INTERNAL_PATH / PRIVATE_ENDPOINT / URL_CRED / GITHUB_404 (+ ORPHAN, currently disabled) |

**`./run.sh fix`** — per-stage dry-run followed by y/N apply:

| Stage | Action |
|-------|--------|
| [1/4] relref | DEAD/MALFORMED → plain text, REWRITE → corrected path |
| [2/4] anchors | Strip leaked ox-hugo anchors: `{#title--relref-section-id-dot-md}` → `{#title}` (regression where transcoded markdown leaks into the slug of link-embedded headings) |
| [3/4] figures | REWRITE → copy source file into `notes/static/images/` and rewrite the markdown src to `/images/{basename}` |
| [4/4] content | HOST_ALIAS auto-alias rewrite; INTERNAL_PATH and GITHUB_404 → plain text. lychee verifies `github.com/USER/*` URLs return 200 OK. `site-policy.el` is the SSOT |

`verify-figures.py` SEARCH_DIRS priority: `notes/static/images/` → `~/screenshot/` → `~/org/.attach/`. Files already promoted by a prior export are matched first, so only the src is rewritten without a copy (sha256 match → SKIP-IDENT).

`verify-content.py` output annotates every detection with a `→ ~/org: <path>` mapping line so the source file can be opened directly in Emacs for editing.

**lychee operational notes**:
- `GITHUB_PERSONAL_ACCESS_TOKEN` (or `GITHUB_TOKEN`) recommended. Place it in `~/.env.local`; `run.sh` auto-sources it for verify/fix [4/4] and `fix-org --check`. Without a token results are advisory only — secondary abuse rate-limits trigger many false positives (measured: 84 broken without token → 1 with token).
- `.lycheecache` provides a 1-day cache with `cache-exclude-status: "404"` as a safety net — agenda-stamp SHA URLs flip from 404 → 200 over the push cycle, so caching them would persist a false positive.
- `max-concurrency: 16` (default 128 trips abuse detection).

**Remaining broken items** (DEAD / UNKNOWN / GITHUB_404, etc.) are decided by the user in the source org file: remove the figure, retake the screenshot, mark as private, demote to plain text, or rename.

### Org Hygiene — `./run.sh fix-org`

Rewrites links in source `~/org` files (Stage 1). The SSOT transformation that lives next to the garden export hook:

| Pattern | Rewritten to |
|---------|--------------|
| `[[file:~/repos/gh/REPO]]` | `[[https://github.com/junghan0611/REPO]]` |
| `[[file:~/repos/gh/REPO/path/file.el]]` | `[[https://github.com/junghan0611/REPO/blob/main/path/file.el]]` |
| `[[file:~/repos/gh/REPO/file.el::N]]` | `[[https://github.com/junghan0611/REPO/blob/main/file.el#LN]]` (line anchor preserved) |
| `[[https://OLD.junghanacs.com][...]]` | `site-policy.el` `host-aliases` substitution |

**Protected regions** (never touched): `[[denote:UUID]]`, `[[file:~/screenshot/...]]`, `[[file:~/org/.attach/...]]`, `[[file:~/org/...]]`, and the interior of code / verbatim / src-block (handled automatically via org-element).

**Modes**:
- `O` / `./run.sh fix-org` — dry-run → y/N → apply. Local-absent targets are marked with ⚠
- `./run.sh fix-org --apply` — apply directly without prompt
- `./run.sh fix-org --check` — no transformation. Verifies `[[https://github.com/USER/...]]` URLs in `~/org` via lychee and reports broken ones as file:line (user triages directly in `~/org`)

**SSOT**: a single line added to `bin/site-policy.el` is simultaneously visible to fix-org, verify-content, and verify-org-links.

### Garden Deploy Workflow

Standard operational flow after a full export:

```
Phase             Command                                  Purpose
─────             ───────                                  ───────
1. Org hygiene    O ) ./run.sh fix-org                     ~/repos/gh/ file: → GitHub URL
   (optional)       CLI: ./run.sh fix-org --check          Detect broken GitHub URLs (token required)

2. Export         7 ) ./run.sh export <dir>                Incremental
                  8 ) ./run.sh export <dir> --force        Force rebuild
                  9 ) submenu                              Per-folder incremental/full

3. Garden verify  V ) ./run.sh verify                      [1/4]~[4/4] checks

4. Garden fix     F ) ./run.sh fix                         Per-stage y/N. Only auto-fixable cases applied

5. User triage    (edit ~/org directly in Emacs)           Follow the → ~/org mapping from verify output

6. Deploy         cd ~/repos/gh/notes && git push          Publish the garden
                  cd ~/sync/org && git push                Push ~/org changes too
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
# Daily use — daemon attach
alias ecs='emacsclient -s user -t'             # Attach TTY client to GUI Emacs (user daemon)
alias ep='~/.doom.d/run.sh pi tty'             # Attach TTY client to pi daemon (full Doom)
alias es='~/.doom.d/run.sh agent restart'      # Restart agent RPC daemon (socket "server")
alias eup='~/.doom.d/run.sh unstable run'      # Emacs unstable channel GUI

# Standalone (fallback)
alias et='emacs -nw'                                # System stable Emacs terminal, no daemon
alias etu='~/.doom.d/bin/emacs-unstable.sh --nw'    # Emacs unstable channel terminal, no daemon
```

**Typical session**: launch `doom run` once for the GUI, run `./run.sh pi start`, then open WezTerm tabs and type `ep` in each. All tabs share the same pi daemon — config edits, packages, native-comp eln cache, even open buffers are shared. Agent RPC runs in a separate `server` daemon so it never blocks the editor.

## Tested Environments

| Platform | Emacs | Terminal | Status |
|----------|-------|----------|--------|
| NixOS 25.11 | 30.2 + unstable channel | Ghostty / WezTerm | Primary |
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
- [ghostel](https://github.com/dakra/ghostel) — dakra's terminal emulator inside Emacs (libghostty-based); the surface my pi / Claude Code TTY workflow runs on, and the project that finally made Korean IME inside terminal-Emacs feel solid

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
