# GPTEL-ACP-VISION

Why we're building this and where it leads.

## The Starting Point

We were setting up tmux orchestration for Claude Code sessions. Multiple terminals, multiple agents, manual coordination. It works, but something felt missing:

> "This gathering of information into Emacs - is that really orchestration? I don't think so."

The real desire: **An agent that lives in Emacs and orchestrates for me.**

## The Conversation That Changed Direction

### Steve Yegge's VC Discovery

Steve Yegge built VC (VibeCoder) - an AI Supervisor that orchestrates coding agents:

```
VC Shell (REPL) ← User talks here
    ↓
AI Supervisor (Sonnet 4.5) ← Orchestrates
    ↓
Worker Agents (Amp, Claude Code)
    ↓
Code Changes
```

Key insight from VC README:
> "Build a colony of coding agents, not the world's largest ant."

But Steve's approach: VC controls everything, Claude Code runs in print mode for short bursts.

Our approach differs:
> "Each Claude Code is excellent on its own. I want to keep coordinating while they work, letting each Claude Code fully utilize its sub-agents."

### The wrapper vs ACP Question

wrapper (claude-code-openai-wrapper) was analyzed:

```
┌─────────────────────────────────────────────────────────────┐
│                    3-Layer Data Flow                        │
├─────────────────────────────────────────────────────────────┤
│  gptel          → full messages every time (OpenAI standard)│
│  wrapper        → session_manager accumulates messages      │
│  claude-agent-sdk → query() stateless / Client() stateful   │
└─────────────────────────────────────────────────────────────┘
```

Problem: Triple context risk (gptel + wrapper + CLI resume = duplication)

Alternative discovered: **ACP (Agent Client Protocol)**

```
gptel → acp.el → claude-code-acp
         ↓
Session management, streaming, MCP servers - all native
```

ACP is more structured. Session management is built-in.

### The yqrashawn Connection

Found yqrashawn's work:
- `acp-integration.el` - gptel backend using ACP
- `gptel-tools/` directory - Emacs native tools:
  - edit-file.el (36KB)
  - read.el, shell.el, ripgrep.el, elisp.el...

This is exactly what Efrit does! But using Claude Code (subscription) instead of direct API.

### The Philosophy

> "I don't want to use tools and throw them away. Handling things in Emacs and storing them in my knowledge base - that becomes my framework of knowing."

gptel isn't just a tool. It's the interface to our knowledge.

## The Four-Layer Architecture

```
1. ai-gptel-acp.el (Foundation)
   gptel ↔ ACP ↔ Claude Code subscription

   WHY: Native protocol, session management, streaming
   WITHOUT THIS: The other layers have no foundation

2. gptel-tools (Emacs Native Tools)
   File edit, read, shell, ripgrep, elisp eval...

   WHY: Let Claude Code manipulate Emacs directly
   WITHOUT THIS: Just another chat interface

3. Orchestration Agent (gptel-agent style)
   System prompt: "You are an orchestrator managing sub-agents"
   Tools: tmux-send, tmux-capture, bd-create, bd-ready

   WHY: User talks to ONE agent, it coordinates the rest
   WITHOUT THIS: User manually coordinates everything

4. tmux Sessions (Worker Agents)
   claude-pm, claude-code, claude-test - fully isolated
   Each can use sub-agents, work on complex tasks

   WHY: Real coding work needs full Claude Code capability
   WITHOUT THIS: Limited to single-turn conversations
```

## Why wrapper Is Still Valid

For knowledge base queries (org files, documents):
- wrapper is simpler
- No need for complex orchestration
- Quick Q&A works fine

The ACP path is for **when we need Emacs-level integration**.

## The Key Insight

```
gptel-agent (karthink's):
  Sub-agents = API calls (separate billing)

Our approach:
  Sub-agents = tmux Claude Code sessions (subscription)

Same architecture, different execution layer.
```

## Current State

- [x] tmux-config.el: Separate sessions per agent (claude-pm, claude-code, etc.)
- [ ] ai-gptel-acp.el: Incomplete, based on yqrashawn's work
- [ ] gptel-tools integration: Not started
- [ ] Orchestration agent definition: Conceptual only

## Next Steps

1. Complete ai-gptel-acp.el - the foundation
2. Define tmux-related gptel-tools (tmux-send, tmux-capture)
3. Create orchestrator preset using gptel-agent pattern
4. Test the full loop: user → orchestrator → tmux agents → results

## References

- Steve Yegge's VC: `~/repos/3rd/vc/`
- yqrashawn's dotfiles: `~/sync/man/dotsamples/dotall/yqrashawn-dot-doom-clj/.doom.d/`
- karthink's gptel-agent: `~/repos/3rd/gptel-agent/`
- Efrit analysis: `20251126T120729--efrit-왜-emacs에서-멀티에이전트인가-steve-yegge의-비전`

## The Question That Drives This

> "Why do more when current performance is sufficient?"

Because the current approach requires **me** to be the orchestrator.
I switch terminals. I copy-paste. I track progress manually.

The vision: **I talk to one agent. It handles the rest.**

That's not over-engineering. That's the whole point.

---

*Captured from conversation on 2026-01-01*
*Context: tmux orchestration → Steve Yegge's VC → ACP discovery → Four-layer architecture*
