#!/usr/bin/env bash
# agent-server-healthcheck.sh — cron용 자동 복구
# crontab: */5 * * * * /home/junghan/repos/gh/doomemacs-config/bin/agent-server-healthcheck.sh
#
# 동작: 소켓 존재 + emacsclient ping 성공이면 OK, 아니면 재시작

set -euo pipefail

AGENT_DAEMON="server"
AGENT_SOCKET="/run/user/$(id -u)/emacs/$AGENT_DAEMON"
AGENT_LOAD="$(dirname "$0")/agent-server.el"
LOG="/tmp/agent-server-healthcheck.log"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG"; }

# 소켓이 있고 응답하면 OK
if [[ -S "$AGENT_SOCKET" ]]; then
  if emacsclient -s "$AGENT_DAEMON" --eval '(+ 1 1)' &>/dev/null; then
    exit 0  # healthy
  else
    log "WARN: Stale socket detected — removing"
    rm -f "$AGENT_SOCKET"
  fi
fi

# 재시작
log "INFO: Starting agent-server..."
mkdir -p /tmp/agent-emacs-init
emacs --init-directory=/tmp/agent-emacs-init --daemon="$AGENT_DAEMON" --load "$AGENT_LOAD" &>/dev/null

if emacsclient -s "$AGENT_DAEMON" --eval '(+ 1 1)' &>/dev/null; then
  log "OK: agent-server restarted successfully"
else
  log "ERROR: agent-server failed to start"
fi
