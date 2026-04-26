#!/usr/bin/env bash
# agent-server-healthcheck.sh — cron용 자동 복구
# crontab: */5 * * * * /home/junghan/repos/gh/doomemacs-config/bin/agent-server-healthcheck.sh
#
# 동작: 소켓 존재 + emacsclient ping 성공이면 OK, 아니면 재시작
# emacsclient 호출은 timeout으로 감싸서 daemon hang 케이스도 복구한다.
# 2026-04-27: hang 사고 이후 timeout + pkill 추가 (이전엔 hang에 무한 대기였음)

set -euo pipefail

AGENT_DAEMON="server"
AGENT_SOCKET="/run/user/$(id -u)/emacs/$AGENT_DAEMON"
AGENT_LOAD="$(dirname "$0")/agent-server.el"
LOG="/tmp/agent-server-healthcheck.log"
PING_TIMEOUT=5  # daemon hang 시 무한 대기 방지

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"; }

ping_ok() {
  timeout "$PING_TIMEOUT" emacsclient -s "$AGENT_DAEMON" --eval '(+ 1 1)' &>/dev/null
}

# 소켓이 있고 응답하면 OK
if [[ -S "$AGENT_SOCKET" ]]; then
  if ping_ok; then
    exit 0  # healthy
  else
    log "WARN: Stale or hung daemon detected — killing process and removing socket"
    pkill -9 -f "daemon=$AGENT_DAEMON" 2>/dev/null || true
    rm -f "$AGENT_SOCKET"
  fi
fi

# 재시작
log "INFO: Starting agent-server..."
mkdir -p /tmp/agent-emacs-init
emacs --init-directory=/tmp/agent-emacs-init --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" &>/dev/null

if ping_ok; then
  log "OK: agent-server restarted successfully"
else
  log "ERROR: agent-server failed to start"
fi
