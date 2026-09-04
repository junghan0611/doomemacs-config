> **What belongs here**: only things that do not change often — structure, conventions,
> contracts, and traps that keep recurring. Operational detail and tool specifics
> (verify categories, deploy stages, model specs, tuning numbers) do not. Before adding
> an entry, ask whether it will still be true next month.
>
> **Precedence when sources disagree** — code always wins:
>
> | Question | Authority |
> |---|---|
> | What the code actually does — flags, defaults, behavior | the code itself |
> | Why a value was chosen, and its measurement | the docstring / comment at that site |
> | How to operate the repo day to day | `README.md` |
> | Structure, conventions, contracts, recurring traps | this file |
>
> If this file contradicts the code, the code is right and this file is a bug.

# AGENTS.md — doomemacs-config Agent Guide

You are the **담당자** (agent-in-charge) for this repository.
This is not a toy dotfile. Read this before touching anything.

## Who I Am

**The public standing report is a Denote note, not this file.**

| | |
|---|---|
| Denote ID | `20260227T120800` |
| org (SSOT) | `~/org/botlog/20260227T120800--§doomemacs-config-*.org` |
| exported | `~/repos/gh/notes/content/botlog/20260227T120800.md` |
| public | <https://notes.junghanacs.com/botlog/20260227t120800> |

The split of duty: **this file is the standing baseline inside the repo** — structure,
conventions, contracts, and the traps that keep recurring. **The Denote note is where
the agent-in-charge reports outward** — what this house owns, what it refuses, where
the boundaries run, and the current judgment. When a report is written there, add a
`히스토리` line naming the date, the actor and what changed; never silently rewrite the
standing report.

Neighbours on the same axis:

| ID | What |
|----|------|
| `20260227T141200` | `⊨agent-server` — fence / playground / trust, the three layers of the agent RPC surface. A case doc thick enough to keep its own house; the steward note links it rather than absorbing it. |
| `20260408T120252` | 에이전트 기억층 — the cross-repo axis where each steward answers from their own seat. **This repo's seat is still empty** (2026-09-04). |
| `20260319T110800` | `§andenken` — the steward downstream of this repo's garden export; the tag pool is the contract between us. |

## What This Repo Is

A 20K-line Doom Emacs configuration that serves as the **frontend for a human-agent collaborative ecosystem**. Emacs here is not just a text editor — it is the harness where:

- **GLG (힣)** writes, thinks, and manages knowledge in org-mode
- **Agents** (Entwurf, secretaries, 힣봇s) read/write the same org files, stamp agenda entries, publish to the digital garden
- **Both sides share a single `org-agenda` timeline** via `workflow-shared.el`

The agent server (`bin/agent-server.el`) exposes elisp APIs over socket `"server"`. The human uses socket `"user"`. They operate on the same `~/org/` corpus.

## Architecture at a Glance

```
init.el                 # Doom modules + single-instance guard
config.el               # Loader only — requires lisp/*.el
├── lisp/               # One concern = one file
├── bin/                # Standalone scripts (no Doom dependency)
├── autoload/           # ;;;###autoload lazy functions
├── neomacs/            # Neomacs vanilla minimal profile + K-review probes (issue #8)
├── tests/              # ERT — vanilla `emacs -Q --batch`, no Doom
├── run.sh              # Unified CLI/TUI: sync, export, agent, verify, fix
└── flake.nix           # Emacs 31 channel (nixos-unstable emacs31-gtk3) via nix
```

## Code Organization Rules

### config.el is a loader

Only `(require ...)` and minimal glue. All logic lives in `lisp/`.

### One concern = one file

| Domain | File(s) |
|--------|---------|
| AI tools | `lisp/ai-*.el` |
| Org-mode | `lisp/org-config.el`, `org-functions.el` |
| Denote | `lisp/denote-*.el` |
| Export pipeline | `lisp/denote-export-config.el` + `bin/denote-export*.{el,py,sh}` |
| Korean input / writing hygiene | `lisp/korean-input-config.el` |
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

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Description of what this module does.

;;; Code:

;;;; Section 1

(provide 'module-name)
;;; module-name.el ends here
```

### Outline structure (outli.el)

Every `.el` file uses outline headings: `;;; Level 1`, `;;;; Level 2`.

### Function placement

- Needs `;;;###autoload`? → `autoload/junghan.el`
- Internal to a module? → Inside that `lisp/*.el`

### Elisp coding conventions

This repo is a long-lived personal Lisp system. New agent-written Elisp should
follow a small, repeatable style so later agents can extend or remove features
without inventing a new idiom each time.

- **Doom style baseline**: follow Doom's current `docs/contributing.org` unless
  this section narrows it: bbatsov Elisp style, no hanging parentheses, and use
  `DEPRECATED` only for code that will actually be removed. Prefer `mapc` over
  `seq-do` when iterating only for side effects.
- **Namespace**: custom public variables/functions use `my/...` (`my/termux-p`,
  `my/org-download-image-dir`). Private helpers still start with `my/` and include
  the concern name; use `--` for genuinely internal helpers inside a larger module,
  e.g. `my/termux--decode-arrow-key`.
- **Vanilla-first logic**: functions with clear input→output behavior should
  run under `emacs -Q` whenever possible. Keep Doom macros (`map!`, `after!`,
  `use-package!`) in glue, keybinding, or package-configuration layers, not in
  reusable logic.
- **Test gate before refactor**: for Tier A logic, write characterization tests
  before changing behavior or structure. `tests/run-tests.sh` is the vanilla
  `emacs -Q` ERT gate; package-dependent paths belong in a separate Tier C
  integration runner or should have their pure branch extracted first.
- **Avoid Doom v2 compat shims**: do not add new `IS-*`, `EMACS29+`, `setq!`,
  `featurep!`, `appendq!`, `pushnew!`, etc. Prefer `(featurep :system 'macos)`,
  `(>= emacs-major-version 31)`, `setopt`/`setq`, `cl-callf`, `add-to-list`, or
  a local `my/...` predicate when grep-ability matters.
- **Use Emacs libraries before hand-rolled loops**: `seq.el` for lists, `map.el`
  for alists, `subr-x` for string helpers, before writing long bespoke functions.
- **Data shape**: for small structured state, prefer alists with `:kebab-case`
  keyword keys, read via `map-elt` / `map-nested-elt`. Use plists/hash-tables only
  when there is a clear reason.
- **cl-lib surface**: `cl-defun` with `&key` is welcome for self-documenting
  call sites. Reach for other `cl-lib` forms only when they simplify the code.
- **Flatten control flow**: prefer `when`, `unless`, `when-let*`, and guard
  bindings over deeply nested `let`/`if` blocks. Use `let*` only when bindings
  depend on earlier bindings.
- **No premature API**: avoid new `defcustom` until the option has settled.
  Start with an internal `defvar`/`defconst` and promote later if needed.
- **Comments/docstrings**: English, explaining intent or invariants rather than
  restating the obvious. **When a value was chosen by measurement, record the number
  and the date at that spot**, so a later agent does not revert it on vibes.
- **Consistency first**: before adding a feature, search for the closest existing
  pattern in `lisp/` and mirror it unless there is a strong reason not to.

### map! prefix rule

**Stance**: keep whatever Doom binds; override only the keys that genuinely need to
change, **without taking Doom's group down with it**.

**The rule is one line**: never give a prefix a name in `map!`.

```elisp
;; No — both bind a keymap AT that key, destroying the prefix map already there
(map! :leader (:prefix     ("f" . "files")   "y" #'foo))
(map! :leader (:prefix-map ("j" . "pi-agent") "a" #'bar))

;; Yes — keys only. An existing map is walked into; a missing one is created
(map! :leader (:prefix "f" "y" #'foo))
```

Since Doom pulled general.el out of `map!` (upstream `de2a3364a`, 2026-07), a described
`:prefix` is no longer a label-only no-op — it binds a fresh empty keymap at that key.
The syntax is unchanged, so it breaks in silence. Group labels go in one place, the
`Leader prefix labels — SSOT` block in `lisp/keybindings-config.el`, via
`which-key-add-keymap-based-replacements`: non-destructive and load-order-independent.
The gate is `tests/test-keybinding-lint.el` (included in `tests/run-tests.sh`).
Background and the before/after table are in `README.md` § Keybindings.

**Note**: Doom binds the standard `help-map` directly at `SPC h`, so definitions under
`(:prefix "h" ...)` mutate the global `help-map` — `C-h t` being a theme map in this
repo is intentional.

### Upstream stance — we adapt, upstream does not

GLG's decision (2026-07-12): however upstream moves, **it is the dotfile's job to
follow**. When upstream changes, absorb it in our code — do not try to pull upstream
our way. If something breaks after `./run.sh G`, **look for our code to change first**.
An agent does not propose an issue or PR on its own. A PR is the last resort, taken
only after an observation period and only when **GLG explicitly calls for it**.

## Emacs Server Sockets

| Socket | Purpose | How it starts |
|--------|---------|---------------|
| `"user"` | GLG's GUI Emacs | `doom run` |
| `"pi"` | TTY attach target (full Doom, shared by N terminals) | `run.sh pi start` |
| `"server"` | Agent RPC daemon | `run.sh agent start` (separate `--init-directory`) |
| `"doom-unstable"` | Second Doom profile on the same Emacs 31 | `run.sh unstable` |
| `"neomacs"` | Neomacs vanilla profile | `bin/neomacs.sh --daemon` |

The **single-instance guard** in `init.el` only blocks duplicate daemons. Non-daemon
instances (`emacs -nw`, `doom run`) run independently.

## workflow-shared.el — the contract

This file defines the rules that all three contexts — user Emacs (GUI), agent-server,
and the **denote-export daemon** — must agree on. Daemons do not load Doom modules, so
anything that only gets set up automatically in the GUI must live here as SSOT and be
applied explicitly.

| Setting | Why |
|---------|-----|
| `org-tag-re` | Tags allow only `[[:alnum:]@#%]+` — matches Denote filetags |
| `org-agenda-files` | Dynamic: `_aprj` tagged files + `botlog/agenda/` + current journal |
| `org-todo-keywords` | TODO/NEXT/DONE/DONT(o) — daemons must know them for agent-server to skip DONT |
| `my/org-download-image-dir` | Resolves `[[download:foo.png]]` (`~/screenshot/`) |
| `my/org-attach-id-dir` | Resolves `[[attachment:foo.png]]` (`~/org/.attach/`) |
| Journal entry format | Active timestamps so entries appear in agenda |

**Rule**: UI/theme/keybindings may differ. **Data read/write rules must be identical.**

**SSOT applier pattern** — when a buffer-local org variable turns out to be missing in
a daemon, it goes here:

```elisp
;; lisp/workflow-shared.el
(defvar my/X "...")
(defun my/apply-X () (setq-default X my/X) (setq X my/X))

;; lisp/org-config.el (GUI)      → (require 'workflow-shared) + (my/apply-X)
;; bin/denote-export.el (daemon) → (my/apply-X) right after loading workflow-shared
;; bin/agent-server.el (daemon)  → (my/apply-X) right after loading workflow-shared
```

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
./run.sh agent start|status|restart|eval
```

`stop`/`restart` are hung-daemon safe — a wedged daemon does not need a manual `kill`
+ `rm socket`. The daemon loads no Doom modules, so anything the GUI sets up
automatically must be applied explicitly (see § workflow-shared.el).

### Garden operations — export / verify / fix / fix-org / fix-bold

```bash
./run.sh export <dir> [--force]    # incremental / full rebuild
./run.sh verify                    # verify garden md (read-only, 4 stages)
./run.sh fix                       # apply only auto-fixable cases, per-stage y/N
./run.sh fix-org [--apply|--check] # rewrite ~/org links / lychee verification
./run.sh fix-bold                  # **bold** → *bold* (never journal/)
```

The standard order is **fix-org → export → verify → fix → user triage → push**.
Per-stage categories, SEARCH_DIRS, and lychee tuning change often — `README.md`
§ Garden Verify / Fix plus `bin/verify-*.py` and `bin/site-policy.el` are the SSOT.
Do not copy them here.

- **A policy line goes only into `bin/site-policy.el`** — `fix-org`, `verify-content`,
  and `verify-org-links` all pick it up at once.
- **`journal/` is never scanned by any automatic fixer** (GLG's manual export surface).
- **Denote links and figure paths are protected regions**: `[[denote:UUID]]`,
  `[[file:~/screenshot/...]]`, `[[file:~/org/.attach/...]]`, `[[file:~/org/...]]`.
- **A tag reaches the garden only if a meta note defines it.** The pool is the union
  of `#+filetags:` in `meta/*.org` headers, and the filter drops everything else at
  export time — org sources are never rewritten. The gate is provenance, **not
  language**: nothing in the code prefers English, so a Korean filetag in a meta note
  would publish. To add a tag, write its meta note; to retire one, remove the filetag.
  Control lives in the notes; SSOT is § Section 1.7 of `lisp/denote-export-config.el`.
  Two things bypass the filter — `@`-prefixed categories (a separate namespace) and
  `#+hugo_tags:` (short-circuits the hook entirely).
- **This tag pool is a contract with `andenken`**, whose md search track consumes the
  exported garden. Widening or narrowing it changes retrieval quality in another repo,
  so treat a pool change as cross-repo, not local.

### Neomacs K-review — `bin/neomacs.sh`

[Neomacs](https://github.com/eval-exec/neomacs) (a Rust rewrite of the Emacs core)
runs a **vanilla profile** here. Issue #8. Fully separated from Doom: its own
`--init-directory`, its own server name, no shared state.

**Core rule: the profile must behave identically on Neomacs and on stock GNU Emacs.**
That is what makes a single `--gnu` run decide whether a divergence belongs to Neomacs
or to our config, so nothing under `neomacs/` may use Doom macros or `use-package`, and
nothing may be fetched. Builtins only, with one standing exception: a pure-Elisp Denote
checkout already present on disk may be put on `load-path` so `denote:` links resolve
against a real corpus. Probes run one process per file — a crash that kills the runtime
is itself a finding.

Measurements, pins, and upstream cross-checks live in `neomacs/README.md` (SSOT).

## Commit Messages

```
feat: add tty-config — term-keys, kitty-graphics, clipboard unified

- daemon guard now daemon-only (emacs -nw allowed)
- config.el terminal block → tty-config.el (45 lines removed)
```

**Never** include "Generated with Claude" or "Co-Authored-By".

## Things to Watch

Recurring traps only. Details live in the code comment at each site.

- `doom sync` is needed after `init.el` module changes **and after any `packages.el`
  declaration** — not for `config.el`/`lisp/` edits. Adding a dependency without
  syncing leaves it uninstalled.
- `per-machine.el` is git-ignored — font/theme overrides go there.
- **The system Emacs is 31.1** (default since 2026-09-02; `nixos-config` unstable
  overlay). `"user"`, `"pi"` and `"server"` all run it. The `doom-unstable` channel is
  no longer "the new version" — it is a second Doom profile on the same Emacs, kept
  apart by `EMACSDIR` (`~/doomemacs-unstable`) and `server-name`. Which package set
  `flake.nix` pins moves with the release cycle — read the flake, not this line.
- **straight builds are per Emacs version** (`build-30.2/`, `build-31.1/`) and the eln
  cache is too. A version bump means a full rebuild and a regenerated profile init, so
  measure against a freshly started daemon, never one that predates the bump.
- **If a package's own autoloads register something and it never takes effect, suspect
  Doom's autoload inlining.** Doom concatenates every package's `*-autoloads.el` into
  one giant defun inside a `no-byte-compile` profile init. Loading that source eagerly
  macro-expands the defun body, so an `eval-and-compile` block runs *at load time* —
  before the `defconst` sitting next to it in the same body has executed — and any
  `(when (boundp '...) ...)` guard around the registration is silently false. It is
  never retried, because the form is replaced by a constant. Loading the real
  autoloads file (`(load (locate-library "<pkg>-autoloads"))`) replays the forms in
  order. Measured on `tramp-rpc` 2026-09-02 on Emacs 30.2 and 31.1 alike, so this is
  not version-bound; see `lisp/project-config.el`.
- **A declaration removed from `packages.el` can still be installed as a transitive
  dependency.** straight reads `Package-Requires`, so dropping `(package! tramp)` did
  not stop `tramp-rpc` from pulling ELPA tramp back in. Use `(package! NAME :built-in t)`
  — Doom turns it into `(straight-override-recipe '(NAME . (:type built-in)))` and the
  dependency resolves to Emacs's own copy. Verify by grepping the generated
  `init.<version>.el` for the build path, not by reading `packages.el`.
- Korean input edge cases: NFD→NFC, Evil state auto-switch, TTY clipboard.
- WezTerm + terminal Emacs + built-in Korean input is a custom path. If minibuffer or
  search prompt spacing breaks, **inspect TTY width drift first** — especially a
  hardcoded `…` in Consult truncation — before blaming Hangul input.
- **Headless daemons do not load Doom modules** (`bin/denote-export.el`,
  `bin/agent-server.el`). Buffer-local org variables that only get set up in the GUI
  are how broken figures accumulate in the garden. The SSOT applier pattern in
  § workflow-shared.el is the fix — first place to suspect on regression.
  (2026-05-10, `b348898`/`d8b977a`)
- **If keybindings vanish wholesale, suspect a `map!` prefix.** When Doom defaults like
  `SPC f s` or `SPC h d` become `undefined`, a described `:prefix` almost certainly
  overwrote the existing map. Checking whether `(lookup-key doom-leader-map "f")` is
  `eq` to `doom-leader-file-map` settles it immediately. See § map! prefix rule.
  (2026-07-12, upstream `de2a3364a`)
- **If `agent-denote-add-link` files a link under the wrong section, suspect the heading
  regex.** The standard heading is written as one word with no internal space; an
  older regex that required whitespace missed it entirely and instead matched sibling
  headings that merely start with the same prefix. Anchor the whole heading. SSOT is
  `agent-server--related-notes-heading-re`, gated by
  `tests/test-agent-denote-link.el`. (2026-07-17)
- **Do not grow gptel backends or models.** The default is one backend — OpenAI-sub
  (ChatGPT subscription OAuth) — with `my/gptel-models` as the SSOT for the model
  list. **Add only on that one line**, and never hand-copy a model spec
  (`my/gptel--model-specs` pulls it from upstream). Adding or reviving a backend needs
  GLG's decision first. **An approved exception is dated and written to be deleted** —
  it names its expiry and the exact forms to remove at its own definition site, so a
  later agent can retire it without re-deriving the decision. The live one is the
  Copilot block in `lisp/ai-gptel.el` — Claude/Gemini axis only, subscribed 2026-08-22,
  **running to mid-September 2026 and not being renewed** (GLG, 2026-09-04). One
  exception never licenses the next. (2026-07-22, `950bd05`)
- **If gptel summarize/translate "used to work and now doesn't", re-measure the model
  tier.** Availability differs sharply by tier on the subscription rail, and a
  congested request comes back as **HTTP 200 with a payload-level
  `server_is_overloaded`** — so reporting `:status` alone makes remote congestion look
  like our regression. The same model staying healthy in pi (Codex CLI) at the same
  moment is just the CLI swallowing it with backoff retries. Diagnosis goes through
  `my/gptel-error-message`, absorption through `my/gptel-request-retry`.
  **`my/gptel-model-fast` is a measured slot, not a tier name** — the numbers and their
  date are in the `lisp/ai-gptel.el` comment. (2026-08-11)
- **If `evil-collection` takes a gptel key, suspect a dead option.** When upstream
  deletes an option and moves the behavior to a shared REPL abstraction, something you
  had turned off silently comes back — a dead `setq` gives no signal. It is now
  disabled for gptel alone via a per-map `:enabled` lambda in
  `evil-collection-binding-overrides`. (2026-07-22)
- Broken garden links never break the build. Clean up periodically with
  `./run.sh verify` → `./run.sh fix`.
- **Run `./run.sh fix` right after an export, for as long as the anchor regression
  lives.** ox-hugo currently leaks `{#title--relref-…}` noise into anchors of
  link-embedded headings, and the fix step is the safety net; skipping it leaves a
  brief exposure right after export — the usual cause of "looks like a new bug".
  Delete this entry once the leak stops reproducing and the `--fix-anchors` guard is
  no longer catching anything.
