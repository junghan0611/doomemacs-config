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

## 2026-01-02 Update: Gas Town 발견

Steve Yegge가 **Gas Town**을 공개했습니다 (2025-12-29 작동 시작, 2026-01-01 공개).

### Gas Town이 해결한 것

우리 4-Layer 중 **Layer 3, 4를 이미 구현**:

| Layer | 우리 계획 | Gas Town |
|-------|----------|----------|
| 4: tmux Sessions | claude-pm, claude-code, etc. | ✅ Polecat, Crew, Witness, Refinery |
| 3: Orchestration | gptel-agent style | ✅ Mayor, Deacon, Convoy 시스템 |
| 2: gptel-tools | Emacs native tools | ❌ CLI 방식 (gt 명령어) |
| 1: ai-gptel-acp.el | gptel ↔ ACP | ❌ Emacs 프론트엔드 없음 |

### Gas Town 핵심 개념

```
GUPP (Gastown Universal Propulsion Principle)
"Hook에 작업이 있으면 무조건 실행"
→ Claude Code의 "세션 끝남" 문제 해결

MEOW Stack (Molecular Expression of Work)
Formula → Protomolecule → Molecule/Wisp → Digest
→ 워크플로우를 git-backed Beads로 표현

Convoy
→ 작업 단위 추적 (gt convoy create, gt convoy list)
```

### 전략 변경

**터미널에서 Gas Town 먼저 익숙해지기 → 그 다음 Emacs 프론트엔드**

```
Phase 1: Gas Town 터미널 숙달
  - gt install ~/gt
  - gt rig add, gt sling, gt convoy
  - Mayor와 대화, Polecat 관찰

Phase 2: Emacs 프론트엔드 (나중에)
  - gt.el: Emacs에서 gt 명령어 래핑
  - gptel과 Mayor 세션 연결
```

## Next Steps (Updated)

1. ~~Complete ai-gptel-acp.el~~ → Gas Town 터미널 숙달
2. ~~Define tmux-related gptel-tools~~ → gt 명령어 학습
3. ~~Create orchestrator preset~~ → Mayor/Deacon 역할 이해
4. ~~Test full loop~~ → Convoy 생성 → 완료 확인 실습

## References

- Steve Yegge's Gas Town: `~/repos/3rd/gastown/`
- Steve Yegge's Beads: `~/repos/3rd/beads/`
- Steve Yegge's VC: `~/repos/3rd/vc/`
- yqrashawn's dotfiles: `~/sync/man/dotsamples/dotall/yqrashawn-dot-doom-clj/.doom.d/`
- karthink's gptel-agent: `~/repos/3rd/gptel-agent/`

## The Question That Drives This

> "Why do more when current performance is sufficient?"

Because the current approach requires **me** to be the orchestrator.
I switch terminals. I copy-paste. I track progress manually.

The vision: **I talk to one agent. It handles the rest.**

Gas Town provides this. Emacs frontend comes after mastering the terminal.

---

*Captured from conversation on 2026-01-01*
*Updated on 2026-01-02: Gas Town integration*
*Context: tmux orchestration → Steve Yegge's VC → ACP discovery → Four-layer architecture → Gas Town*
