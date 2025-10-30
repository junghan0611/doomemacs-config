#!/usr/bin/env bash
# denote-export-folder.sh - Export single folder with dedicated daemons
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./bin/denote-export-folder.sh <folder_name>
#   ./bin/denote-export-folder.sh test
#   ./bin/denote-export-folder.sh meta
#
# Features:
#   - 2 dedicated daemons per folder
#   - Round-robin file distribution
#   - Automatic cleanup
#   - Detailed logging

set -euo pipefail

# Configuration
ORG_DIR="${HOME}/org"
DOOM_DIR="${HOME}/repos/gh/doomemacs-config"
SERVER_SCRIPT="${DOOM_DIR}/bin/denote-export-server.el"
NUM_DAEMONS=1

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log_info() {
  echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*"
}

# Check arguments
if [ $# -eq 0 ]; then
  echo "Usage: $0 <folder_name>"
  echo "Example: $0 test"
  echo "         $0 meta"
  exit 1
fi

FOLDER="$1"
FOLDER_PATH="${ORG_DIR}/${FOLDER}"

# Check folder exists
if [ ! -d "$FOLDER_PATH" ]; then
  log_error "Folder not found: $FOLDER_PATH"
  exit 1
fi

# Count files
FILE_COUNT=$(find "$FOLDER_PATH" -name "*.org" -type f 2>/dev/null | wc -l)

log_info "======================================"
log_info "Denote Export: $FOLDER"
log_info "Files: $FILE_COUNT"
log_info "Daemons: $NUM_DAEMONS"
log_info "======================================"
echo ""

# Start daemons
log_info "Starting $NUM_DAEMONS daemons for $FOLDER..."
for i in $(seq 1 $NUM_DAEMONS); do
  DAEMON_NAME="denote-export-${FOLDER}-${i}"

  # Stop if already running
  if emacsclient -s "$DAEMON_NAME" --eval 't' &>/dev/null 2>&1; then
    log_warn "Daemon $i already running, stopping..."
    emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null || true
    sleep 1
  fi

  # Start daemon
  log_info "Starting daemon $i: $DAEMON_NAME"
  emacs --quick --daemon="$DAEMON_NAME" --load "$SERVER_SCRIPT" &>/dev/null &
done

# Wait for daemons to start
log_info "Waiting for daemons to initialize..."
for i in $(seq 1 $NUM_DAEMONS); do
  DAEMON_NAME="denote-export-${FOLDER}-${i}"

  # Wait for daemon to respond
  ELAPSED=0
  TIMEOUT=30
  while ! emacsclient -s "$DAEMON_NAME" --eval 't' &>/dev/null 2>&1; do
    if [ $ELAPSED -ge $TIMEOUT ]; then
      log_error "Daemon $i failed to start within ${TIMEOUT}s"
      exit 1
    fi
    sleep 1
    ELAPSED=$((ELAPSED + 1))
  done

  # Wait for ready flag
  ELAPSED=0
  TIMEOUT=60
  while ! emacsclient -s "$DAEMON_NAME" --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null | grep -q 't'; do
    if [ $ELAPSED -ge $TIMEOUT ]; then
      log_error "Daemon $i initialization timeout (${TIMEOUT}s)"
      exit 1
    fi
    sleep 1
    ELAPSED=$((ELAPSED + 1))
  done

  log_info "✓ Daemon $i ready!"
done

echo ""
log_info "All $NUM_DAEMONS daemons ready!"
echo ""

# Export files
log_info "Starting export..."
START=$(date +%s)

SUCCESS=0
ERRORS=0
COUNTER=0

# Process files with round-robin distribution
while IFS= read -r FILE; do
  COUNTER=$((COUNTER + 1))
  DAEMON_ID=$((COUNTER % NUM_DAEMONS + 1))
  DAEMON_NAME="denote-export-${FOLDER}-${DAEMON_ID}"

  printf "[%3d/%d D%d] " "$COUNTER" "$FILE_COUNT" "$DAEMON_ID"

  RESULT=$(emacsclient -s "$DAEMON_NAME" --eval "(denote-export-file \"$FILE\")" 2>&1)

  if echo "$RESULT" | grep -q "SUCCESS:"; then
    echo "✓ $(basename "$FILE")"
    SUCCESS=$((SUCCESS + 1))
  else
    echo "✗ $(basename "$FILE")"
    ERRORS=$((ERRORS + 1))
  fi
done < <(find "$FOLDER_PATH" -name "*.org" -type f 2>/dev/null | sort)

END=$(date +%s)
DURATION=$((END - START))

# Calculate speed
if [ $DURATION -gt 0 ]; then
  SPEED=$(echo "scale=3; $FILE_COUNT / $DURATION" | bc)
else
  SPEED="N/A"
fi

echo ""
log_info "======================================"
log_info "Export completed: $FOLDER"
log_info "Success: $SUCCESS"
log_info "Errors: $ERRORS"
log_info "Duration: ${DURATION}s"
log_info "Speed: ${SPEED} files/sec"
log_info "======================================"

# Cleanup daemons
echo ""
log_info "Stopping daemons..."
for i in $(seq 1 $NUM_DAEMONS); do
  DAEMON_NAME="denote-export-${FOLDER}-${i}"
  if emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null 2>&1; then
    log_info "✓ Daemon $i stopped"
  fi
done

log_info "Done!"
