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

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

info()    { printf '%b\n' "${BLUE}ℹ${NC} $*"; }
success() { printf '%b\n' "${GREEN}✓${NC} $*"; }
warn()    { printf '%b\n' "${YELLOW}⚠${NC} $*"; }
error()   { printf '%b\n' "${RED}✗${NC} $*"; exit 1; }

# Cleanup daemons on interrupt
cleanup() {
  echo ""
  warn "Interrupted — cleaning up export daemons..."
  pkill -f "denote-export-daemon" 2>/dev/null || true
  exit 130
}
trap cleanup SIGINT SIGTERM

show_help() {
  printf '%b\n' "${BOLD}Doom Emacs 닷파일 통합 관리${NC}"
  echo ""
  printf '%b\n' "${BLUE}사용법:${NC}  ./run.sh <명령> [옵션]"
  echo ""
  printf '%b\n' "${BOLD}━━━ Doom 관리 ━━━${NC}"
  echo "  sync                    doom sync"
  echo "  sync-update             doom sync -u -j 2"
  echo "  doctor                  doom doctor"
  echo ""
  printf '%b\n' "${BOLD}━━━ Denote Dblock 업데이트 ━━━${NC}"
  echo "  dblock [DIR] [N]        dblock 업데이트 (기본: ~/org/meta, N=${DEFAULT_JOBS})"
  echo "  dblock meta [N]         ~/org/meta dblock"
  echo "  dblock notes [N]        ~/org/notes dblock"
  echo "  dblock all [N]          meta + notes + bib + botlog 순차"
  echo ""
  printf '%b\n' "${BOLD}━━━ Denote Export (Hugo) ━━━${NC}"
  echo "  export all [N]          전체 export (meta→bib→notes→botlog)"
  echo "  export meta [N]         ~/org/meta export"
  echo "  export bib [N]          ~/org/bib export"
  echo "  export notes [N]        ~/org/notes export"
  echo "  export botlog [N]       ~/org/botlog export"
  echo "  export DIR [N]          커스텀 디렉토리 export"
  echo "  export --force ...      증분 무시, 전체 강제"
  echo ""
  printf '%b\n' "${BOLD}━━━ Agent Server ━━━${NC}"
  echo "  agent start             에이전트 서버 데몬 시작"
  echo "  agent stop              에이전트 서버 중지"
  echo "  agent restart           재시작"
  echo "  agent status            상태 확인"
  echo "  agent eval EXPR         emacsclient eval 실행"
  echo ""
  printf '%b\n' "${BOLD}옵션:${NC}"
  echo "  N = 병렬 작업 수 (기본: ${DEFAULT_JOBS})"
  echo ""
  printf '%b\n' "${BOLD}예제:${NC}"
  echo "  ./run.sh sync                     # doom sync"
  echo "  ./run.sh sync-update              # doom sync -u -j 2"
  echo "  ./run.sh dblock                   # meta dblock (4 workers)"
  echo "  ./run.sh dblock all 8             # 전체 dblock (8 workers)"
  echo "  ./run.sh export all               # 전체 export (증분)"
  echo "  ./run.sh export meta --force      # meta 전체 강제 export"
  echo "  ./run.sh agent start              # 에이전트 서버 시작"
  echo "  ./run.sh agent eval '(agent-server-status)'"
}

# ━━━ Doom ━━━

cmd_sync() {
  info "doom sync..."
  "$DOOM_BIN" sync
  success "doom sync 완료"
}

cmd_sync_update() {
  info "doom sync -u -j 2..."
  "$DOOM_BIN" sync -u -j 2
  success "doom sync -u 완료"
}

cmd_doctor() {
  info "doom doctor..."
  "$DOOM_BIN" doctor
}

# ━━━ Denote Dblock ━━━

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

cmd_dblock() {
  local target="${1:-meta}"
  shift || true

  # Parse jobs
  local jobs="$DEFAULT_JOBS"
  for arg in "$@"; do
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
      jobs="$arg"
    fi
  done

  if [[ "$target" == "all" ]]; then
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
    success "전체 dblock 완료 ($((elapsed/60))m $((elapsed%60))s)"
  else
    local dir_path
    dir_path=$(resolve_org_dir "$target")
    [[ -d "$dir_path" ]] || error "디렉토리 없음: $dir_path"
    info "dblock: $dir_path ($jobs workers)"
    python3 "$PYTHON_EXPORT" dblock "$dir_path" "$jobs"
    success "dblock 완료"
  fi
}

# ━━━ Denote Export ━━━

cmd_export() {
  local target="${1:-all}"
  shift || true

  # Parse flags and jobs
  local jobs="$DEFAULT_JOBS"
  local force=""
  for arg in "$@"; do
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
      jobs="$arg"
    elif [[ "$arg" == "--force" ]]; then
      force="--force"
    fi
  done

  if [[ "$target" == "all" ]]; then
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
    success "전체 export 완료 ($((elapsed/60))m $((elapsed%60))s)"
  else
    local dir_path
    dir_path=$(resolve_org_dir "$target")
    [[ -d "$dir_path" ]] || error "디렉토리 없음: $dir_path"
    info "export: $dir_path ($jobs workers, ${force:-증분})"
    python3 "$PYTHON_EXPORT" export "$dir_path" "$jobs" $force
    success "export 완료"
  fi
}

# ━━━ Agent Server ━━━

cmd_agent() {
  local action="${1:-status}"
  shift || true

  case "$action" in
    start)
      if [[ -S "$AGENT_SOCKET" ]]; then
        success "이미 실행 중 (socket: $AGENT_SOCKET)"
        return 0
      fi
      info "에이전트 서버 시작..."
      emacs --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" 2>&1 | tail -5
      success "에이전트 서버 시작됨"
      ;;
    stop)
      if [[ ! -S "$AGENT_SOCKET" ]]; then
        info "실행 중이 아님"
        return 0
      fi
      emacsclient -s "$AGENT_DAEMON" --eval '(kill-emacs)' 2>/dev/null || true
      success "에이전트 서버 중지됨"
      ;;
    restart)
      cmd_agent stop
      sleep 1
      cmd_agent start
      ;;
    status)
      if [[ -S "$AGENT_SOCKET" ]]; then
        local result
        result=$(emacsclient -s "$AGENT_DAEMON" --eval '(agent-server-status)' 2>/dev/null)
        success "실행 중"
        echo "$result"
      else
        warn "실행 중이 아님"
      fi
      ;;
    eval)
      if [[ ! -S "$AGENT_SOCKET" ]]; then
        error "에이전트 서버가 실행 중이 아닙니다. ./run.sh agent start"
      fi
      emacsclient -s "$AGENT_DAEMON" --eval "$@"
      ;;
    *)
      error "알 수 없는 agent 명령: $action (start|stop|restart|status|eval)"
      ;;
  esac
}

# ━━━ Main ━━━

COMMAND="${1:-help}"
shift || true

case "$COMMAND" in
  help|--help|-h)      show_help ;;
  sync)                cmd_sync ;;
  sync-update)         cmd_sync_update ;;
  doctor)              cmd_doctor ;;
  dblock)              cmd_dblock "$@" ;;
  export)              cmd_export "$@" ;;
  agent)               cmd_agent "$@" ;;
  *)                   error "알 수 없는 명령: $COMMAND (./run.sh help)" ;;
esac
