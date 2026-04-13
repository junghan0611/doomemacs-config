# MEMORY.md — Session Context & Evolving Understanding

This file captures context from working sessions that shapes how the 담당자 agent
understands this repository. Not instructions — lived experience.

---

## 2026-04-12/13: Terminal Emacs Liberation

### What happened

GLG wanted `emacs -nw` to run as a standalone terminal instance alongside the GUI
Emacs server. The `init.el` single-instance guard was killing every non-daemon Emacs
because it detected the `"user"` server socket.

**Root cause**: The guard checked `(server-running-p)` unconditionally — both daemon
and non-daemon instances hit it. Since `doom run` (GUI) and `emacs -nw` both default
to `server-name "user"`, they'd find the socket and `kill-emacs`.

**Fix**: `(when (and (daemonp) (server-running-p)) ...)` — guard only applies to
daemon instances. Non-daemon Emacs (terminal, doom run) passes through freely.

**Discovery**: `doom run` from the desktop entry is NOT a daemon. But Doom's
`doom-editor.el` calls `(server-start)` when `(display-graphic-p)` — so GUI instances
become servers even without `--daemon`. This is important for understanding the socket
landscape.

### TTY config consolidation

Terminal-related settings were scattered across `config.el`. Consolidated into
`lisp/tty-config.el`:

- `term-keys` — always loaded (was `:unless (display-graphic-p)` which breaks daemon)
- `kitty-graphics` — uses `tty-setup-hook` instead of `doom-first-buffer-hook`
- OSC 52 clipboard — terminal clipboard integration

Key insight: `:unless (display-graphic-p)` in Doom's `use-package!` evaluates at
**daemon start time**, not per-frame. A GUI daemon that later gets TTY clients
never loads the package. Use `tty-setup-hook` for per-frame TTY setup.

### Emacs 31 IGC terminal mode

Added `--nw`/`--tty` option to `bin/emacs-igc.sh` and `t)` menu entry in `run.sh`.
IGC in terminal is interesting not as a GUI regression but as a universal interface —
the same powerful Emacs that runs locally, runs identically over SSH with clipboard
and Korean input.

### Agent server auto-start — attempted and reverted

Tried adding auto-start to `config.el` via `(when (display-graphic-p) ...)` hook.
Hit `end-of-file` parse error (likely parenthesis issue with nested lambdas in
`run-with-timer`). GLG decided: **agent server is not config.el's job**. It belongs
in the operational layer — `run.sh agent start`, or delegated to Entwurf.

**Lesson**: Don't put process management in Emacs config. The agent harness (pi,
skills, run.sh) handles lifecycle. Config handles editing environment.

## 2026-04-12: Denote Export Journal Fix

`my/denote-export-allowed-dirs` was missing `"journal"`. Journal files are not
auto-exported (intentionally), but when manually exported, their PREV/NEXT denote
links need to resolve as Hugo relrefs. Added `"journal"` to the allowed list.

**Key distinction GLG made**: "journal은 자동내보내기만 안하겠다는거지, 내보내기
대상이 아닌 게 아니다." Auto-export exclusion ≠ link resolution exclusion.

---

## Patterns Observed

### GLG's working style

- Rapid iteration: modifies Emacs config, restarts, tests, multiple times per session
- Prefers clean separation: one concern per file, config.el as loader only
- Korean-first thinker, English implementation — the language gap is a constant theme
- Values terminal parity with GUI — not as compromise but as the universal interface
- "떠드는 것" (rambling/talking through) is the creative process, not waste

### What agents should NOT do

- Don't add external process management to `config.el`
- Don't assume GUI-only — every feature should degrade gracefully to TTY
- Don't dismiss the terminal setup as trivial — for a Korean user, every keystroke
  of hangul/english switching is friction that compounds
- Don't "tidy up" by merging files — the one-concern-per-file rule exists for a reason

### Architecture awareness

- `workflow-shared.el` is a **contract** — changes affect both user Emacs and agent-server
- `bin/denote-export.el` shares advice with `lisp/denote-export-config.el` — always check both
- Server sockets (`user`, `server`, `doom-igc`) are isolated by design — don't merge them
- `run.sh` is the operational CLI — prefer it over raw emacsclient for agent server management
