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
BIN_DIR="$SCRIPT_DIR/bin"
PYTHON_EXPORT="$BIN_DIR/denote-export-parallel.py"
PYTHON_VERIFY="$BIN_DIR/verify-relref.py"
CHECK_DESC="$BIN_DIR/check-description.sh"
ORG_ROOT="$HOME/org"

# Agent server
AGENT_DAEMON="server"
AGENT_LOAD="$BIN_DIR/agent-server.el"
AGENT_SOCKET="/run/user/$(id -u)/emacs/$AGENT_DAEMON"

# Pi full Doom daemon (TTY emacsclient attach target)
PI_DAEMON="pi"
PI_SOCKET="/run/user/$(id -u)/emacs/$PI_DAEMON"
PI_CLIENT="emacsclient -s $PI_DAEMON"

# Emacs 31 IGC
IGC_SCRIPT="$BIN_DIR/emacs-igc.sh"

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
  echo "  ${YELLOW}Verify${NC}"
  echo "    V) verify relref       (검증만)"
  echo "    C) verify description  (description/abstract 검사)"
  echo "    F) fix relref          (DEAD/REWRITE 수정)"
  echo "    A) fix anchors         (heading anchor 정리)"
  echo ""
  echo "  ${YELLOW}Emacs 31 IGC${NC} (MPS GC)"
  echo "    i) igc run      (doom run, GUI)"
  echo "    t) igc tty      (doom run -nw, 터미널)"
  echo "    d) igc debug    (--debug-init)"
  echo "    I) igc install  (sync + env + profile)"
  echo "    S) igc sync     (doom sync)"
  echo "    U) igc update   (doom sync -u)"
  echo "    E) igc env      (doom env)"
  echo "    D) igc doctor   (doom doctor)"
  echo "    K) igc kill     (서버 종료)"
  echo "    v) igc version"
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

cmd_verify_relref() {
  local fix="${1:-}"
  [[ -d "$HUGO_CONTENT_DIR" ]] || { err "content 디렉토리 없음: $HUGO_CONTENT_DIR"; return 1; }
  if [[ "$fix" == "--fix" ]]; then
    info "relref 검증 + 수정 (dry-run)"
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors
    echo ""
    read -p "적용하시겠습니까? (y/N): " confirm
    if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
      python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix --apply
      python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors --apply
    else
      info "취소됨"
    fi
  elif [[ "$fix" == "--fix-anchors" ]]; then
    info "heading anchor 정리 (dry-run)"
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors
    echo ""
    read -p "적용하시겠습니까? (y/N): " confirm
    if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
      python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --fix-anchors --apply
    else
      info "취소됨"
    fi
  else
    python3 "$PYTHON_VERIFY" "$HUGO_CONTENT_DIR" --summary
  fi
}

cmd_verify_description() {
  [[ -x "$CHECK_DESC" ]] || { err "스크립트 없음: $CHECK_DESC"; return 1; }
  "$CHECK_DESC" "$@"
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
  mkdir -p /tmp/agent-emacs-init
  emacs --init-directory=/tmp/agent-emacs-init --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" 2>&1 | tail -5
  success "에이전트 서버 시작됨"
}

cmd_agent_stop() {
  if [[ ! -S "$AGENT_SOCKET" ]]; then
    info "실행 중이 아님"
    return 0
  fi
  emacsclient -s "$AGENT_DAEMON" --eval '(kill-emacs)' 2>/dev/null || true
  # Cleanup: 소켓이 남아있으면 강제 제거
  sleep 0.5
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
  if [[ ! -S "$PI_SOCKET" ]]; then
    info "Pi Emacs daemon이 실행 중이 아님"
    return 0
  fi
  timeout 5 $PI_CLIENT --eval '(kill-emacs)' 2>/dev/null || true
  sleep 0.5
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
    igc)
      local action="${1:-run}"; shift || true
      case "$action" in
        run)     "$IGC_SCRIPT" ;;
        tty|nw)  "$IGC_SCRIPT" --nw ;;
        debug)   "$IGC_SCRIPT" --debug ;;
        install) "$IGC_SCRIPT" --install ;;
        sync)    "$IGC_SCRIPT" --sync ;;
        update)  "$IGC_SCRIPT" --update ;;
        env)     "$IGC_SCRIPT" --env ;;
        doctor)  "$IGC_SCRIPT" --doctor ;;
        kill)    "$IGC_SCRIPT" --kill ;;
        version) "$IGC_SCRIPT" --version ;;
        *)       err "igc: run|debug|install|sync|update|env|doctor|kill|version" ;;
      esac
      ;;
    verify)
      local action="${1:-summary}"; shift || true
      case "$action" in
        summary)     cmd_verify_relref ;;
        fix)         cmd_verify_relref --fix ;;
        anchors)     cmd_verify_relref --fix-anchors ;;
        description) cmd_verify_description "$@" ;;
        *)           err "verify: summary|fix|anchors|description" ;;
      esac
      ;;
    help|--help|-h)
      echo "CLI: ./run.sh <sync|sync-update|doctor|dblock|export|agent|pi|igc|verify> [args]"
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
      i) "$IGC_SCRIPT"; read -p "계속하려면 Enter..."; continue ;;
      t) "$IGC_SCRIPT" --nw; read -p "계속하려면 Enter..."; continue ;;
      d) "$IGC_SCRIPT" --debug; read -p "계속하려면 Enter..."; continue ;;
      I) "$IGC_SCRIPT" --install; read -p "계속하려면 Enter..."; continue ;;
      S) "$IGC_SCRIPT" --sync; read -p "계속하려면 Enter..."; continue ;;
      U) "$IGC_SCRIPT" --update; read -p "계속하려면 Enter..."; continue ;;
      E) "$IGC_SCRIPT" --env; read -p "계속하려면 Enter..."; continue ;;
      D) "$IGC_SCRIPT" --doctor; read -p "계속하려면 Enter..."; continue ;;
      K) "$IGC_SCRIPT" --kill; read -p "계속하려면 Enter..."; continue ;;
      v) "$IGC_SCRIPT" --version ;;
      V) cmd_verify_relref ;;
      C) if ! cmd_verify_description; then warn "description/abstract 누락 항목 있음"; fi ;;
      F) cmd_verify_relref --fix ;;
      A) cmd_verify_relref --fix-anchors ;;
      0|q) echo ""; success "종료"; exit 0 ;;
      *) warn "잘못된 선택: $choice" ;;
    esac

    echo ""
    read -p "계속하려면 Enter..."
  done
}

main "$@"
