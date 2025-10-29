#!/usr/bin/env bash
# denote-export-safe.sh - Safe sequential export by folder
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./bin/denote-export-safe.sh
#
# Features:
#   - Folder by folder sequential processing (meta → bib → notes)
#   - Single daemon per folder (no concurrency issues)
#   - Detailed logging
#   - Error tracking

set -euo pipefail

# Configuration
ORG_DIR="${HOME}/org"
DOOM_DIR="${HOME}/repos/gh/doomemacs-config"
SERVER_SCRIPT="${DOOM_DIR}/bin/denote-export-server.el"
DAEMON_NAME="denote-export-safe"
LOG_FILE="/tmp/denote-export-safe-$(date +%Y%m%d-%H%M%S).log"

# Folders to export (sequential order)
FOLDERS=("meta" "bib" "notes")

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log_info() {
  echo -e "${GREEN}[INFO]${NC} $*" | tee -a "$LOG_FILE"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*" | tee -a "$LOG_FILE"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*" | tee -a "$LOG_FILE"
}

# Start daemon
start_daemon() {
  log_info "Starting Emacs daemon: $DAEMON_NAME"

  # Stop if already running
  if emacsclient -s "$DAEMON_NAME" --eval 't' &>/dev/null 2>&1; then
    log_warn "Daemon already running, stopping..."
    emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null || true
    sleep 2
  fi

  # Start daemon
  emacs --quick --daemon="$DAEMON_NAME" --load "$SERVER_SCRIPT" &>/dev/null &

  # Wait for daemon to start
  log_info "Waiting for daemon to start..."
  ELAPSED=0
  TIMEOUT=30
  while ! emacsclient -s "$DAEMON_NAME" --eval 't' &>/dev/null 2>&1; do
    if [ $ELAPSED -ge $TIMEOUT ]; then
      log_error "Daemon failed to start within ${TIMEOUT}s"
      exit 1
    fi
    sleep 1
    ELAPSED=$((ELAPSED + 1))
  done

  # Wait for full initialization
  log_info "Waiting for daemon full initialization..."
  ELAPSED=0
  TIMEOUT=60
  while ! emacsclient -s "$DAEMON_NAME" --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null | grep -q 't'; do
    if [ $ELAPSED -ge $TIMEOUT ]; then
      log_error "Daemon initialization timeout (${TIMEOUT}s)"
      exit 1
    fi
    sleep 1
    ELAPSED=$((ELAPSED + 1))
  done

  log_info "✓ Daemon ready!"
}

# Stop daemon
stop_daemon() {
  log_info "Stopping daemon..."
  if emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null 2>&1; then
    log_info "✓ Daemon stopped"
  else
    log_warn "Daemon was not running"
  fi
}

# Export single file
export_file() {
  local FILE="$1"
  local RESULT

  RESULT=$(emacsclient -s "$DAEMON_NAME" --eval "(denote-export-file \"$FILE\")" 2>&1)

  if echo "$RESULT" | grep -q "^\"SUCCESS:"; then
    echo "✓ $(basename "$FILE")" | tee -a "$LOG_FILE"
    return 0
  else
    echo "✗ $(basename "$FILE")" | tee -a "$LOG_FILE"
    echo "  Error: $RESULT" >> "$LOG_FILE"
    return 1
  fi
}

# Export folder
export_folder() {
  local FOLDER="$1"
  local FOLDER_PATH="${ORG_DIR}/${FOLDER}"

  if [ ! -d "$FOLDER_PATH" ]; then
    log_warn "Folder not found: $FOLDER_PATH"
    return 0
  fi

  local FILE_COUNT=$(find "$FOLDER_PATH" -name "*.org" -type f 2>/dev/null | wc -l)

  log_info "======================================"
  log_info "Exporting: $FOLDER"
  log_info "Files: $FILE_COUNT"
  log_info "======================================"

  local START=$(date +%s)
  local SUCCESS=0
  local ERRORS=0
  local COUNTER=0

  # Export each file
  while IFS= read -r FILE; do
    COUNTER=$((COUNTER + 1))
    printf "[%3d/%d] " "$COUNTER" "$FILE_COUNT"

    if export_file "$FILE"; then
      SUCCESS=$((SUCCESS + 1))
    else
      ERRORS=$((ERRORS + 1))
    fi
  done < <(find "$FOLDER_PATH" -name "*.org" -type f 2>/dev/null | sort)

  local END=$(date +%s)
  local DURATION=$((END - START))
  local SPEED=$(echo "scale=2; $FILE_COUNT / $DURATION" | bc)

  log_info "--------------------------------------"
  log_info "✓ $FOLDER completed"
  log_info "  Success: $SUCCESS"
  log_info "  Errors: $ERRORS"
  log_info "  Duration: ${DURATION}s"
  log_info "  Speed: ${SPEED} files/sec"
  log_info "======================================"
  log_info ""
}

# Main execution
main() {
  echo ""
  log_info "=== Denote Safe Export ==="
  log_info "Daemon: $DAEMON_NAME"
  log_info "Log: $LOG_FILE"
  log_info "Folders: ${FOLDERS[*]}"
  log_info ""

  # Start daemon once
  start_daemon

  # Export each folder sequentially
  local TOTAL_START=$(date +%s)

  for FOLDER in "${FOLDERS[@]}"; do
    export_folder "$FOLDER"
  done

  local TOTAL_END=$(date +%s)
  local TOTAL_DURATION=$((TOTAL_END - TOTAL_START))
  local TOTAL_FILES=$(find "${ORG_DIR}"/{meta,bib,notes} -name "*.org" -type f 2>/dev/null | wc -l)
  local TOTAL_SPEED=$(echo "scale=2; $TOTAL_FILES / $TOTAL_DURATION" | bc)

  log_info "=========================================="
  log_info "=== FULL EXPORT COMPLETED ==="
  log_info "Total files: $TOTAL_FILES"
  log_info "Total time: ${TOTAL_DURATION}s ($(echo "scale=1; $TOTAL_DURATION / 60" | bc)min)"
  log_info "Average speed: ${TOTAL_SPEED} files/sec"
  log_info "Log file: $LOG_FILE"
  log_info "=========================================="

  # Stop daemon
  stop_daemon

  log_info ""
  log_info "Next steps:"
  log_info "  1. Check log: cat $LOG_FILE | grep -E '✗|ERROR'"
  log_info "  2. Verify files: ls ~/repos/gh/notes/content/{meta,bib,notes}/*.md | wc -l"
  log_info "  3. Git commit: cd ~/repos/gh/notes && git status"
}

# Cleanup on exit
cleanup() {
  log_warn "Interrupted! Cleaning up..."
  stop_daemon
}

trap cleanup EXIT INT TERM

main "$@"
