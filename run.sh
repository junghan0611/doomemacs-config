#!/usr/bin/env bash
# run.sh — Doom Emacs 닷파일 통합 관리 스크립트
#
# Copyright (C) 2025-2026 Junghan Kim
# https://github.com/junghan0611/doomemacs-config
#
# 인간과 에이전트 모두를 위한 단일 진입점.
# doom sync, denote dblock/export, agent-server를 하나로.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOOM_BIN="$HOME/.config/emacs/bin/doom"
DOOM_STABLE_DIR="$HOME/doomemacs"
DOOM_UNSTABLE_DIR="$HOME/doomemacs-unstable"
BIN_DIR="$SCRIPT_DIR/bin"
PYTHON_EXPORT="$BIN_DIR/denote-export-parallel.py"
PYTHON_VERIFY="$BIN_DIR/verify-relref.py"
PYTHON_FIGURES="$BIN_DIR/verify-figures.py"
PYTHON_CONTENT="$BIN_DIR/verify-content.py"
CHECK_DESC="$BIN_DIR/check-description.sh"
ORG_ROOT="$HOME/org"

# Agent server
AGENT_DAEMON="server"
AGENT_LOAD="$BIN_DIR/agent-server.el"
AGENT_SOCKET="/run/user/$(id -u)/emacs/$AGENT_DAEMON"
# Dedicated init dir (Doom bypass). Also the unique pgrep token for the daemon:
# no other process carries `--init-directory=/tmp/agent-emacs-init`.
AGENT_INIT_DIR="/tmp/agent-emacs-init"

# Pi full Doom daemon (TTY emacsclient attach target)
PI_DAEMON="pi"
PI_SOCKET="/run/user/$(id -u)/emacs/$PI_DAEMON"
PI_CLIENT="emacsclient -s $PI_DAEMON"

# Emacs 31 preview channel (flake output #emacs-unstable, pinned to Savannah emacs-31)
UNSTABLE_SCRIPT="$BIN_DIR/emacs-unstable.sh"

# Hugo content dir
HUGO_CONTENT_DIR="$HOME/repos/gh/notes/content"

# Defaults
DEFAULT_JOBS=4

# Colors (bash $'...' literal — NixOS safe)
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
BLUE=$'\033[0;34m'
BOLD=$'\033[1m'
NC=$'\033[0m'

info()    { echo "${BLUE}ℹ${NC} $*"; }
success() { echo "${GREEN}✓${NC} $*"; }
warn()    { echo "${YELLOW}⚠${NC} $*"; }
err()     { echo "${RED}✗${NC} $*"; }

# Load secrets (e.g. GITHUB_PERSONAL_ACCESS_TOKEN) on demand.
# Used only by lychee-invoking steps so we don't leak env in unrelated commands.
load_env_local() {
  # shellcheck disable=SC1090
  [[ -r "$HOME/.env.local" ]] && source "$HOME/.env.local" 2>/dev/null || true
}

# Cleanup daemons on interrupt
cleanup() {
  echo ""
  warn "Interrupted — cleaning up export daemons..."
  pkill -f "denote-export-daemon" 2>/dev/null || true
  exit 130
}
trap cleanup SIGINT SIGTERM

# ━━━ Menu ━━━

show_menu() {
  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "${GREEN}Doom Emacs 닷파일 통합 관리${NC}"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo ""
  echo "  ${YELLOW}Doom${NC}"
  echo "    1) doom sync"
  echo "    2) doom sync -u -j 2 (업데이트)"
  echo "    3) doom doctor"
  echo "    G) doom git pull (~/doomemacs + ~/doomemacs-unstable)"
  echo ""
  echo "  ${YELLOW}Denote Dblock${NC}"
  echo "    4) dblock meta (기본)"
  echo "    5) dblock all (meta+notes+bib+botlog)"
  echo "    6) dblock 커스텀 디렉토리"
  echo ""
  echo "  ${YELLOW}Denote Export (Hugo)${NC}"
  echo "    7) export all (증분)"
  echo "    8) export all --force (전체)"
  echo "    9) export 폴더 선택"
  echo ""
  echo "  ${YELLOW}Agent Server${NC} (Emacs 30.2)"
  echo "    a) agent start"
  echo "    s) agent status"
  echo "    x) agent stop"
  echo "    r) agent restart"
  echo "    e) agent eval"
  echo ""
  echo "  ${YELLOW}Pi Emacs${NC} (full Doom daemon)"
  echo "    P) pi start"
  echo "    L) pi status"
  echo "    X) pi stop"
  echo "    R) pi restart"
  echo "    T) pi tty client"
  echo ""
  echo "  ${YELLOW}Verify${NC} (가든 md — notes 리포)"
  echo "    V) verify  (relref + description + figures + content, 검증만)"
  echo "    F) fix     (relref + anchors + figures + content, 단계별 y/N)"
  echo ""
  echo "  ${YELLOW}Org Hygiene${NC} (~/org 원본 — site-policy.el SSOT 기반)"
  echo "    O) fix-org    (link 정정, dry-run + --apply)"
  echo "       CLI: ./run.sh fix-org --check  (~/org GitHub URL lychee 검증, read-only)"
  echo ""
  echo "  ${YELLOW}Emacs unstable${NC} (preview channel: emacs-31)"
  echo "    i) unstable run      (doom run, GUI)"
  echo "    t) unstable tty      (doom run -nw, 터미널)"
  echo "    d) unstable debug    (--debug-init)"
  echo "    I) unstable install  (sync + env + profile)"
  echo "    S) unstable sync     (doom sync)"
  echo "    U) unstable update   (doom sync -u)"
  echo "    E) unstable env      (doom env)"
  echo "    D) unstable doctor   (doom doctor)"
  echo "    K) unstable kill     (서버 종료)"
  echo "    v) unstable version"
  echo ""
  echo "    0) Exit"
  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

execute_cmd() {
  local cmd="$1"
  echo ""
  info "실행: $cmd"
  echo ""
  eval "$cmd"
  local status=$?
  echo ""
  if [[ $status -eq 0 ]]; then
    success "완료!"
  else
    err "실패 (exit code: $status)"
  fi
  return $status
}

# ━━━ Helper ━━━

resolve_org_dir() {
  local name="$1"
  case "$name" in
    meta|bib|notes|botlog|llmlog|test)
      echo "$ORG_ROOT/$name" ;;
    /*)
      echo "$name" ;;
    *)
      echo "$ORG_ROOT/$name" ;;
  esac
}

ask_jobs() {
  local default="${1:-$DEFAULT_JOBS}"
  read -p "병렬 작업 수 [${default}]: " jobs
  echo "${jobs:-$default}"
}

# ━━━ Doom ━━━

cmd_sync() {
  execute_cmd "$DOOM_BIN sync"
}

cmd_sync_update() {
  execute_cmd "$DOOM_BIN sync -u -j 2"
}

cmd_doctor() {
  execute_cmd "$DOOM_BIN doctor"
}

cmd_doom_pull_repo() {
  local repo_path="$1"
  local label="$2"

  [[ -d "$repo_path" ]] || { err "$label 디렉토리 없음: $repo_path"; return 1; }
  git -C "$repo_path" rev-parse --is-inside-work-tree &>/dev/null || {
    err "$label git 리포 아님: $repo_path"
    return 1
  }

  info "$label pull: $repo_path"
  git -C "$repo_path" pull --ff-only --recurse-submodules
  git -C "$repo_path" submodule sync --recursive
  git -C "$repo_path" submodule update --init --recursive
  git -C "$repo_path" submodule status --recursive || true
  success "$label 완료"
}

cmd_doom_pull() {
  local target="${1:-all}"

  case "$target" in
    stable)
      cmd_doom_pull_repo "$DOOM_STABLE_DIR" "doomemacs"
      ;;
    unstable)
      cmd_doom_pull_repo "$DOOM_UNSTABLE_DIR" "doomemacs-unstable"
      ;;
    all)
      cmd_doom_pull_repo "$DOOM_STABLE_DIR" "doomemacs"
      echo ""
      cmd_doom_pull_repo "$DOOM_UNSTABLE_DIR" "doomemacs-unstable"
      ;;
    *)
      err "doom-pull: stable|unstable|all"
      return 1
      ;;
  esac
}

# ━━━ Dblock ━━━

cmd_dblock() {
  local dir_path="$1"
  local jobs="$2"
  [[ -d "$dir_path" ]] || { err "디렉토리 없음: $dir_path"; return 1; }
  execute_cmd "python3 $PYTHON_EXPORT dblock $dir_path $jobs"
}

cmd_dblock_all() {
  local jobs="$1"
  local start_time=$SECONDS
  for dir in meta notes bib botlog; do
    local dir_path
    dir_path=$(resolve_org_dir "$dir")
    if [[ -d "$dir_path" ]]; then
      info "dblock: $dir ($jobs workers)"
      python3 "$PYTHON_EXPORT" dblock "$dir_path" "$jobs"
      success "$dir 완료"
    else
      warn "디렉토리 없음: $dir_path (건너뜀)"
    fi
  done
  local elapsed=$(( SECONDS - start_time ))
  echo ""
  success "전체 dblock 완료 ($((elapsed/60))m $((elapsed%60))s)"
}

# ━━━ Export ━━━

cmd_export_all() {
  local jobs="$1"
  local force="${2:-}"
  local start_time=$SECONDS
  local folders=(meta bib notes botlog)
  local total=${#folders[@]}
  local i=1

  info "전체 export 시작 ($jobs workers, ${force:-증분})"
  for dir in "${folders[@]}"; do
    local dir_path
    dir_path=$(resolve_org_dir "$dir")
    if [[ -d "$dir_path" ]]; then
      info "[$i/$total] $dir"
      if [[ "$i" -lt "$total" ]]; then
        python3 "$PYTHON_EXPORT" export "$dir_path" "$jobs" $force --no-cleanup
      else
        python3 "$PYTHON_EXPORT" export "$dir_path" "$jobs" $force
      fi
      success "$dir 완료"
    else
      warn "디렉토리 없음: $dir_path (건너뜀)"
    fi
    i=$((i + 1))
  done

  local elapsed=$(( SECONDS - start_time ))
  echo ""
  success "전체 export 완료 ($((elapsed/60))m $((elapsed%60))s)"
}

cmd_export_single() {
  local dir_path="$1"
  local jobs="$2"
  local force="${3:-}"
  [[ -d "$dir_path" ]] || { err "디렉토리 없음: $dir_path"; return 1; }
  info "export: $dir_path ($jobs workers, ${force:-증분})"
  execute_cmd "python3 $PYTHON_EXPORT export $dir_path $jobs $force"
}

show_export_submenu() {
  echo ""
  echo "  ${YELLOW}Export 폴더 선택${NC}"
  echo "    1) meta     (증분)     f1) meta     (전체)"
  echo "    2) bib      (증분)     f2) bib      (전체)"
  echo "    3) notes    (증분)     f3) notes    (전체)"
  echo "    4) botlog   (증분)     f4) botlog   (전체)"
  echo "    5) llmlog   (증분)     f5) llmlog   (전체)"
  echo "    6) 커스텀 디렉토리"
  echo ""
  read -p "선택 (1-6, f 접두사 = force): " sub

  local force=""
  if [[ "$sub" == f* ]]; then
    force="--force"
    sub="${sub:1}"
  fi

  local jobs
  jobs=$(ask_jobs)

  local folders=(meta bib notes botlog llmlog)
  case "$sub" in
    [1-5])
      local idx=$((sub - 1))
      cmd_export_single "$(resolve_org_dir "${folders[$idx]}")" "$jobs" "$force"
      ;;
    6)
      read -p "디렉토리 경로: " custom_dir
      cmd_export_single "$custom_dir" "$jobs" "$force"
      ;;
    *) warn "잘못된 선택" ;;
  esac
}

# ━━━ Verify ━━━
# V/F 두 키로 통합된 검증/수정 파이프라인.
# 각 단계는 독립적이라 한 단계만 적용/취소 가능.

# [4/4]의 lychee는 네트워크 호출이라 수 분 걸린다. 나머지 content 검사
# (host alias / internal path / endpoint / cred)는 로컬이라 몇 초면 끝나므로,
# 단계를 통째로 건너뛰지 않고 lychee만 끌 수 있게 먼저 묻는다.
# 출력: LYCHEE_FLAG 배열 (--lychee 또는 빈 배열)
_ask_lychee() {
  load_env_local
  LYCHEE_FLAG=(--lychee)
  read -p "GitHub 404 검사(lychee) 포함? 수 분 소요 (Y/n): " c
  if [[ "$c" =~ ^[Nn]$ ]]; then
    LYCHEE_FLAG=()
    info "lychee 생략 — 로컬 검사만 수행 (GITHUB_404 미검출)"
  fi
}

_verify_step_relref_summary() {
  info "[1/4] relref 검증"
  python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --summary || true
}

_verify_step_description() {
  info "[2/4] description/abstract 검사"
  if ! "$CHECK_DESC"; then
    warn "description/abstract 누락 항목 있음"
  fi
}

_verify_step_figures_summary() {
  info "[3/4] figure src 검증"
  python3 "$PYTHON_FIGURES" "$HUGO_CONTENT_DIR" || true
}

_verify_step_content() {
  info "[4/4] content 위생 검증 (host alias / internal path / endpoint / cred / 404)"
  _ask_lychee
  python3 "$PYTHON_CONTENT" "$HUGO_CONTENT_DIR" --summary "${LYCHEE_FLAG[@]}" || true
}

_fix_step_relref() {
  info "[1/4] relref 수정 (dry-run)"
  python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix || true
  echo ""
  read -p "relref 수정 적용? (y/N): " c
  if [[ "$c" =~ ^[Yy]$ ]]; then
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix --apply
  else
    info "relref 적용 취소"
  fi
}

_fix_step_anchors() {
  info "[2/4] heading anchor 정리 (dry-run)"
  python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors || true
  echo ""
  read -p "anchor 정리 적용? (y/N): " c
  if [[ "$c" =~ ^[Yy]$ ]]; then
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors --apply
  else
    info "anchor 적용 취소"
  fi
}

_fix_step_figures() {
  info "[3/4] figure src 정정 (dry-run)"
  python3 "$PYTHON_FIGURES" "$HUGO_CONTENT_DIR" --fix || true
  echo ""
  read -p "figure 경로 정정 + 정적 복사 적용? (y/N): " c
  if [[ "$c" =~ ^[Yy]$ ]]; then
    python3 "$PYTHON_FIGURES" "$HUGO_CONTENT_DIR" --fix --apply
  else
    info "figure 적용 취소"
  fi
}

_fix_step_content() {
  info "[4/4] content 위생 정정 (dry-run)"
  _ask_lychee
  python3 "$PYTHON_CONTENT" "$HUGO_CONTENT_DIR" --fix "${LYCHEE_FLAG[@]}" || true
  echo ""
  read -p "content 위생 정정 적용? (y/N): " c
  if [[ "$c" =~ ^[Yy]$ ]]; then
    python3 "$PYTHON_CONTENT" "$HUGO_CONTENT_DIR" --fix "${LYCHEE_FLAG[@]}" --apply
  else
    info "content 적용 취소"
  fi
}

cmd_fix_org() {
  local target="$HOME/org"
  # Pick first non-option arg as target (anything not starting with --).
  # Use ${1:-} so nounset (set -u) doesn't trip when called from TUI (no args).
  if [[ -n "${1:-}" && "$1" != --* ]]; then
    target="$1"
    shift
  fi

  # --check: ~/org GitHub URL lychee 검증 (read-only, 변환 없음)
  for arg in "$@"; do
    if [[ "$arg" == "--check" ]]; then
      info "fix-org --check (~/org GitHub URL lychee 검증): ${target}"
      load_env_local
      python3 "$BIN_DIR/verify-org-links.py" "$target"
      return
    fi
  done

  # If --apply passed explicitly, run once and done.
  for arg in "$@"; do
    if [[ "$arg" == "--apply" ]]; then
      info "fix-org --apply: ${target}"
      emacs --batch -Q --load "$BIN_DIR/fix-org-links.el" -- "$target" "$@"
      return
    fi
  done

  # Default flow: dry-run → prompt → apply
  info "fix-org dry-run: ${target}"
  emacs --batch -Q --load "$BIN_DIR/fix-org-links.el" -- "$target" "$@"
  echo ""
  read -p "위 변경 적용? (y/N): " c
  if [[ "$c" =~ ^[Yy]$ ]]; then
    info "fix-org --apply: ${target}"
    emacs --batch -Q --load "$BIN_DIR/fix-org-links.el" -- "$target" --apply "$@"
  else
    info "취소"
  fi
}

cmd_verify() {
  [[ -d "$HUGO_CONTENT_DIR" ]] || { err "content 디렉토리 없음: $HUGO_CONTENT_DIR"; return 1; }
  _verify_step_relref_summary
  echo ""
  _verify_step_description
  echo ""
  _verify_step_figures_summary
  echo ""
  _verify_step_content
}

cmd_fix() {
  [[ -d "$HUGO_CONTENT_DIR" ]] || { err "content 디렉토리 없음: $HUGO_CONTENT_DIR"; return 1; }
  _fix_step_relref
  echo ""
  _fix_step_anchors
  echo ""
  _fix_step_figures
  echo ""
  _fix_step_content
}

# ━━━ Agent ━━━

cmd_agent_start() {
  if [[ -S "$AGENT_SOCKET" ]]; then
    # Stale socket check — 소켓은 있지만 프로세스가 죽은 경우
    if ! emacsclient -s "$AGENT_DAEMON" --eval '(+ 1 1)' &>/dev/null; then
      warn "Stale 소켓 감지 — 정리 후 재시작"
      rm -f "$AGENT_SOCKET"
    else
      success "이미 실행 중 (socket: $AGENT_SOCKET)"
      return 0
    fi
  fi
  info "에이전트 서버 시작..."
  # --init-directory: Doom init 우회 (서버 충돌 방지)
  # agent-server.el이 straight build에서 직접 load-path 구성
  mkdir -p "$AGENT_INIT_DIR"
  emacs --init-directory="$AGENT_INIT_DIR" --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" 2>&1 | tail -5
  success "에이전트 서버 시작됨"
}

# Daemon PIDs by cmdline. Start-anchored on the emacs binary (`^[^ ]*emacs `)
# so a mere shell command *mentioning* the token — e.g. this script's own
# pgrep — can never match; only a real emacs daemon does. Empty (exit 0) when
# none.
_agent_daemon_pids() {
  pgrep -f -- "^[^ ]*emacs .*--init-directory=$AGENT_INIT_DIR" 2>/dev/null || true
}

_pi_daemon_pids() {
  pgrep -f -- "^[^ ]*emacs .*--daemon=$PI_DAEMON" 2>/dev/null || true
}

cmd_agent_stop() {
  if [[ ! -S "$AGENT_SOCKET" ]] && [[ -z "$(_agent_daemon_pids)" ]]; then
    info "실행 중이 아님"
    return 0
  fi
  # 1) Graceful, BOUNDED. A hung daemon (busy in a synchronous eval) never
  #    services this request, so without `timeout` the whole restart blocks
  #    until the caller's own timeout fires. 5s is plenty for a healthy daemon.
  timeout 5 emacsclient -s "$AGENT_DAEMON" --eval '(kill-emacs)' &>/dev/null || true
  sleep 0.5
  # 2) Force-stop fallback. If the daemon didn't exit (socket or PID still
  #    around) it was hung — kill by PID, escalate to -9 if it resists.
  if [[ -S "$AGENT_SOCKET" ]] || [[ -n "$(_agent_daemon_pids)" ]]; then
    local pids
    pids="$(_agent_daemon_pids)"
    if [[ -n "$pids" ]]; then
      warn "graceful 무응답 — daemon 강제 종료 (PID: $pids)"
      # word-split intended: one signal per PID
      # shellcheck disable=SC2086
      kill $pids 2>/dev/null || true
      for _ in 1 2 3 4 5 6; do
        [[ -z "$(_agent_daemon_pids)" ]] && break
        sleep 0.5
      done
      pids="$(_agent_daemon_pids)"
      if [[ -n "$pids" ]]; then
        warn "SIGTERM 무시 — SIGKILL"
        # shellcheck disable=SC2086
        kill -9 $pids 2>/dev/null || true
        sleep 0.5
      fi
    fi
  fi
  # 3) Stale socket cleanup — dead daemons leave the socket behind.
  [[ -S "$AGENT_SOCKET" ]] && rm -f "$AGENT_SOCKET" && warn "Stale 소켓 제거됨"
  success "에이전트 서버 중지됨"
}

cmd_agent_status() {
  if [[ -S "$AGENT_SOCKET" ]]; then
    local result
    result=$(emacsclient -s "$AGENT_DAEMON" --eval '(agent-server-status)' 2>/dev/null)
    success "실행 중"
    echo "$result"
  else
    warn "실행 중이 아님"
  fi
}

cmd_agent_eval() {
  if [[ ! -S "$AGENT_SOCKET" ]]; then
    err "에이전트 서버가 실행 중이 아닙니다"
    return 1
  fi
  read -p "eval 식: " expr
  emacsclient -s "$AGENT_DAEMON" --eval "$expr"
}

cmd_pi_start() {
  if [[ -S "$PI_SOCKET" ]]; then
    if timeout 5 $PI_CLIENT --eval '(+ 1 1)' &>/dev/null; then
      success "이미 실행 중 (socket: $PI_SOCKET)"
      return 0
    else
      warn "Stale 소켓 감지 — 정리 후 재시작"
      rm -f "$PI_SOCKET"
    fi
  fi

  info "Pi Emacs daemon 시작..."
  EMACS_SERVER_NAME="$PI_DAEMON" emacs --daemon="$PI_DAEMON" 2>&1 | tail -5

  local retries=20
  while (( retries > 0 )); do
    if timeout 5 $PI_CLIENT --eval '(format "pi-ready:%s" server-name)' &>/dev/null; then
      success "Pi Emacs daemon 시작됨"
      return 0
    fi
    sleep 0.5
    retries=$((retries - 1))
  done

  err "Pi Emacs daemon 시작 확인 실패"
  return 1
}

cmd_pi_stop() {
  if [[ ! -S "$PI_SOCKET" ]] && [[ -z "$(_pi_daemon_pids)" ]]; then
    info "Pi Emacs daemon이 실행 중이 아님"
    return 0
  fi
  # Bounded graceful stop — a hung daemon never services this eval. (see cmd_agent_stop)
  timeout 5 $PI_CLIENT --eval '(kill-emacs)' &>/dev/null || true
  sleep 0.5
  # Force-stop fallback: hung daemon → kill by PID, escalate to -9 if it resists.
  if [[ -S "$PI_SOCKET" ]] || [[ -n "$(_pi_daemon_pids)" ]]; then
    local pids
    pids="$(_pi_daemon_pids)"
    if [[ -n "$pids" ]]; then
      warn "graceful 무응답 — pi daemon 강제 종료 (PID: $pids)"
      # word-split intended: one signal per PID
      # shellcheck disable=SC2086
      kill $pids 2>/dev/null || true
      for _ in 1 2 3 4 5 6; do
        [[ -z "$(_pi_daemon_pids)" ]] && break
        sleep 0.5
      done
      pids="$(_pi_daemon_pids)"
      if [[ -n "$pids" ]]; then
        warn "SIGTERM 무시 — SIGKILL"
        # shellcheck disable=SC2086
        kill -9 $pids 2>/dev/null || true
        sleep 0.5
      fi
    fi
  fi
  [[ -S "$PI_SOCKET" ]] && rm -f "$PI_SOCKET" && warn "Stale 소켓 제거됨"
  success "Pi Emacs daemon 중지됨"
}

cmd_pi_status() {
  if [[ -S "$PI_SOCKET" ]]; then
    local result
    result=$(timeout 5 $PI_CLIENT --eval '(list :server server-name :daemon (daemonp) :default-directory default-directory)' 2>/dev/null)
    success "Pi Emacs daemon 실행 중"
    echo "$result"
  else
    warn "Pi Emacs daemon이 실행 중이 아님"
  fi
}

cmd_pi_tty() {
  if [[ ! -S "$PI_SOCKET" ]]; then
    cmd_pi_start
  fi
  info "Pi Emacs TTY client 접속 (-s $PI_DAEMON)"
  emacsclient -t -s "$PI_DAEMON"
}

# ━━━ CLI Mode (비대화) ━━━

cli_mode() {
  local cmd="$1"
  shift || true

  case "$cmd" in
    sync)          cmd_sync ;;
    sync-update)   cmd_sync_update ;;
    doctor)        cmd_doctor ;;
    doom-pull)     cmd_doom_pull "${1:-all}" ;;
    dblock)
      local target="${1:-meta}"; shift || true
      local jobs="${1:-$DEFAULT_JOBS}"
      if [[ "$target" == "all" ]]; then
        cmd_dblock_all "$jobs"
      else
        cmd_dblock "$(resolve_org_dir "$target")" "$jobs"
      fi
      ;;
    export)
      local target="${1:-all}"; shift || true
      local jobs="$DEFAULT_JOBS" force=""
      for arg in "$@"; do
        [[ "$arg" =~ ^[0-9]+$ ]] && jobs="$arg"
        [[ "$arg" == "--force" ]] && force="--force"
      done
      if [[ "$target" == "all" ]]; then
        cmd_export_all "$jobs" "$force"
      else
        cmd_export_single "$(resolve_org_dir "$target")" "$jobs" "$force"
      fi
      ;;
    agent)
      local action="${1:-status}"; shift || true
      case "$action" in
        start)   cmd_agent_start ;;
        stop)    cmd_agent_stop ;;
        restart) cmd_agent_stop; sleep 1; cmd_agent_start ;;
        status)  cmd_agent_status ;;
        eval)    emacsclient -s "$AGENT_DAEMON" --eval "$@" ;;
        *)       err "agent: start|stop|restart|status|eval" ;;
      esac
      ;;
    pi)
      local action="${1:-status}"; shift || true
      case "$action" in
        start)   cmd_pi_start ;;
        stop)    cmd_pi_stop ;;
        restart) cmd_pi_stop; sleep 1; cmd_pi_start ;;
        status)  cmd_pi_status ;;
        tty|client) cmd_pi_tty ;;
        eval)    emacsclient -s "$PI_DAEMON" --eval "$@" ;;
        *)       err "pi: start|stop|restart|status|tty|client|eval" ;;
      esac
      ;;
    unstable)
      local action="${1:-run}"; shift || true
      case "$action" in
        run)     "$UNSTABLE_SCRIPT" ;;
        tty|nw)  "$UNSTABLE_SCRIPT" --nw ;;
        debug)   "$UNSTABLE_SCRIPT" --debug ;;
        install) "$UNSTABLE_SCRIPT" --install ;;
        sync)    "$UNSTABLE_SCRIPT" --sync ;;
        update)  "$UNSTABLE_SCRIPT" --update ;;
        env)     "$UNSTABLE_SCRIPT" --env ;;
        doctor)  "$UNSTABLE_SCRIPT" --doctor ;;
        kill)    "$UNSTABLE_SCRIPT" --kill ;;
        version) "$UNSTABLE_SCRIPT" --version ;;
        *)       err "unstable: run|debug|install|sync|update|env|doctor|kill|version" ;;
      esac
      ;;
    verify)  cmd_verify ;;
    fix)     cmd_fix ;;
    fix-org) cmd_fix_org "$@" ;;
    help|--help|-h)
      echo "CLI: ./run.sh <sync|sync-update|doctor|doom-pull|dblock|export|agent|pi|unstable|verify|fix|fix-org> [args]"
      echo "     doom-pull [stable|unstable|all]   # default: all"
      echo "TUI: ./run.sh (인자 없이)"
      ;;
    *)           err "알 수 없는 명령: $cmd" ;;
  esac
}

# ━━━ Main ━━━

main() {
  cd "$SCRIPT_DIR"

  # CLI mode: 인자가 있으면 비대화 실행
  if [[ $# -gt 0 ]]; then
    cli_mode "$@"
    return
  fi

  # TUI mode: 인터랙티브 메뉴
  while true; do
    show_menu
    read -p "선택하세요: " choice

    case "$choice" in
      1) cmd_sync ;;
      2) cmd_sync_update ;;
      3) cmd_doctor ;;
      G) cmd_doom_pull all ;;
      4)
        local jobs; jobs=$(ask_jobs)
        cmd_dblock "$(resolve_org_dir meta)" "$jobs"
        ;;
      5)
        local jobs; jobs=$(ask_jobs)
        cmd_dblock_all "$jobs"
        ;;
      6)
        read -p "디렉토리 (meta/notes/bib/경로): " dir_name
        local jobs; jobs=$(ask_jobs)
        cmd_dblock "$(resolve_org_dir "$dir_name")" "$jobs"
        ;;
      7)
        local jobs; jobs=$(ask_jobs)
        cmd_export_all "$jobs"
        ;;
      8)
        local jobs; jobs=$(ask_jobs)
        cmd_export_all "$jobs" "--force"
        ;;
      9) show_export_submenu ;;
      a) cmd_agent_start ;;
      s) cmd_agent_status ;;
      x) cmd_agent_stop ;;
      r) cmd_agent_stop; sleep 1; cmd_agent_start ;;
      e) cmd_agent_eval ;;
      P) cmd_pi_start ;;
      L) cmd_pi_status ;;
      X) cmd_pi_stop ;;
      R) cmd_pi_stop; sleep 1; cmd_pi_start ;;
      T) cmd_pi_tty ;;
      i) "$UNSTABLE_SCRIPT"; read -p "계속하려면 Enter..."; continue ;;
      t) "$UNSTABLE_SCRIPT" --nw; read -p "계속하려면 Enter..."; continue ;;
      d) "$UNSTABLE_SCRIPT" --debug; read -p "계속하려면 Enter..."; continue ;;
      I) "$UNSTABLE_SCRIPT" --install; read -p "계속하려면 Enter..."; continue ;;
      S) "$UNSTABLE_SCRIPT" --sync; read -p "계속하려면 Enter..."; continue ;;
      U) "$UNSTABLE_SCRIPT" --update; read -p "계속하려면 Enter..."; continue ;;
      E) "$UNSTABLE_SCRIPT" --env; read -p "계속하려면 Enter..."; continue ;;
      D) "$UNSTABLE_SCRIPT" --doctor; read -p "계속하려면 Enter..."; continue ;;
      K) "$UNSTABLE_SCRIPT" --kill; read -p "계속하려면 Enter..."; continue ;;
      v) "$UNSTABLE_SCRIPT" --version ;;
      V) cmd_verify ;;
      F) cmd_fix ;;
      O) cmd_fix_org ;;
      0|q) echo ""; success "종료"; exit 0 ;;
      *) warn "잘못된 선택: $choice" ;;
    esac

    echo ""
    read -p "계속하려면 Enter..."
  done
}

main "$@"
