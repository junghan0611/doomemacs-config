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
ORG_ROOT="$HOME/org"

# Agent server
AGENT_DAEMON="agent-server"
AGENT_LOAD="$BIN_DIR/agent-server.el"
AGENT_SOCKET="/run/user/$(id -u)/emacs/$AGENT_DAEMON"

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
  echo "    9) export 개별 선택"
  echo ""
  echo "  ${YELLOW}Agent Server${NC}"
  echo "    a) agent start"
  echo "    s) agent status"
  echo "    x) agent stop"
  echo "    r) agent restart"
  echo "    e) agent eval"
  echo ""
  echo "  ${YELLOW}Patch${NC}"
  echo "    p) citar-denote docstring 패치"
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
  local jobs
  jobs=$(ask_jobs)
  echo ""
  echo "  ${YELLOW}Export 대상 선택${NC}"
  echo "    1) meta    2) bib    3) notes    4) botlog"
  echo "    5) 커스텀 디렉토리"
  echo "    f) --force 옵션 추가"
  echo ""
  local force=""
  read -p "선택 (1-5, f 접두사 가능 예: f1): " sub
  if [[ "$sub" == f* ]]; then
    force="--force"
    sub="${sub:1}"
  fi
  case "$sub" in
    1) cmd_export_single "$(resolve_org_dir meta)" "$jobs" "$force" ;;
    2) cmd_export_single "$(resolve_org_dir bib)" "$jobs" "$force" ;;
    3) cmd_export_single "$(resolve_org_dir notes)" "$jobs" "$force" ;;
    4) cmd_export_single "$(resolve_org_dir botlog)" "$jobs" "$force" ;;
    5)
      read -p "디렉토리 경로: " custom_dir
      cmd_export_single "$custom_dir" "$jobs" "$force"
      ;;
    *) warn "잘못된 선택" ;;
  esac
}

# ━━━ Agent ━━━

cmd_agent_start() {
  if [[ -S "$AGENT_SOCKET" ]]; then
    success "이미 실행 중 (socket: $AGENT_SOCKET)"
    return 0
  fi
  info "에이전트 서버 시작..."
  emacs --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" 2>&1 | tail -5
  success "에이전트 서버 시작됨"
}

cmd_agent_stop() {
  if [[ ! -S "$AGENT_SOCKET" ]]; then
    info "실행 중이 아님"
    return 0
  fi
  emacsclient -s "$AGENT_DAEMON" --eval '(kill-emacs)' 2>/dev/null || true
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

# ━━━ Patch ━━━

cmd_patch_citar() {
  info "citar-denote docstring 패치..."
  local files=(
    "$HOME/doomemacs/.local/straight/repos/citar-denote/citar-denote.el"
    "$HOME/doomemacs/.local/straight/build-30.2/citar-denote/citar-denote.el"
  )
  local patched=0
  for file in "${files[@]}"; do
    if [[ -f "$file" ]] && grep -q "\`citar-denote-keyword'\"." "$file"; then
      sed -i "s/\`citar-denote-keyword'\"./\`citar-denote-keyword'.\"/" "$file"
      rm -f "${file}c" 2>/dev/null
      success "패치됨: $(basename "$(dirname "$file")")/$(basename "$file")"
      patched=$((patched + 1))
    fi
  done
  if [[ $patched -eq 0 ]]; then
    info "패치 불필요 (이미 적용됨 또는 파일 없음)"
  fi
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
    patch)       cmd_patch_citar ;;
    help|--help|-h)
      echo "CLI: ./run.sh <sync|sync-update|doctor|dblock|export|agent|patch> [args]"
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
      p) cmd_patch_citar ;;
      0|q) echo ""; success "종료"; exit 0 ;;
      *) warn "잘못된 선택: $choice" ;;
    esac

    echo ""
    read -p "계속하려면 Enter..."
  done
}

main "$@"
