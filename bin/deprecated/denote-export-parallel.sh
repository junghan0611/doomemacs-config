#!/usr/bin/env bash
# denote-export-parallel.sh - True parallel export with independent servers
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./bin/denote-export-parallel.sh <folder_name> <num_servers>
#   ./bin/denote-export-parallel.sh test 4
#   ./bin/denote-export-parallel.sh meta 4
#
# Features:
#   - Each server processes its own file list independently
#   - True parallel execution (no emacsclient blocking)
#   - Progress logging per server

set -euo pipefail

# Configuration
ORG_DIR="${HOME}/org"
DOOM_DIR="${HOME}/repos/gh/doomemacs-config"
SERVER_SCRIPT="${DOOM_DIR}/bin/denote-export-server.el"
LOG_DIR="/tmp"

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
if [ $# -lt 2 ]; then
  echo "Usage: $0 <folder_name> <num_servers>"
  echo "Example: $0 test 4"
  echo "         $0 meta 4"
  exit 1
fi

FOLDER="$1"
NUM_SERVERS="$2"
FOLDER_PATH="${ORG_DIR}/${FOLDER}"

# Check folder exists
if [ ! -d "$FOLDER_PATH" ]; then
  log_error "Folder not found: $FOLDER_PATH"
  exit 1
fi

# Get file list
FILES=($(find "$FOLDER_PATH" -name "*.org" -type f 2>/dev/null | sort))
TOTAL_FILES=${#FILES[@]}

if [ $TOTAL_FILES -eq 0 ]; then
  log_error "No org files found in $FOLDER_PATH"
  exit 1
fi

log_info "======================================"
log_info "Denote Parallel Export: $FOLDER"
log_info "Total files: $TOTAL_FILES"
log_info "Servers: $NUM_SERVERS"
log_info "======================================"
echo ""

# Calculate files per server
FILES_PER_SERVER=$((TOTAL_FILES / NUM_SERVERS))
REMAINDER=$((TOTAL_FILES % NUM_SERVERS))

# Start servers and distribute files
log_info "Starting $NUM_SERVERS servers..."
SERVER_PIDS=()

for i in $(seq 1 $NUM_SERVERS); do
  DAEMON_NAME="denote-parallel-${FOLDER}-${i}"
  LOG_FILE="${LOG_DIR}/denote-export-${FOLDER}-${i}.log"

  # Stop if already running
  emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null 2>&1 || true

  # Start server
  log_info "Starting server $i: $DAEMON_NAME"
  emacs --quick --daemon="$DAEMON_NAME" --load "$SERVER_SCRIPT" &>/dev/null &

  # Wait for server to start
  sleep 3

  # Wait for ready flag
  ELAPSED=0
  TIMEOUT=60
  while ! emacsclient -s "$DAEMON_NAME" --eval '(boundp '"'"'denote-export-server-ready)' 2>/dev/null | grep -q 't'; do
    if [ $ELAPSED -ge $TIMEOUT ]; then
      log_error "Server $i initialization timeout"
      exit 1
    fi
    sleep 1
    ELAPSED=$((ELAPSED + 1))
  done

  log_info "✓ Server $i ready!"
done

echo ""
log_info "All $NUM_SERVERS servers ready!"
log_info "Distributing files to servers..."
echo ""

# Distribute files and start processing
for i in $(seq 1 $NUM_SERVERS); do
  DAEMON_NAME="denote-parallel-${FOLDER}-${i}"
  LOG_FILE="${LOG_DIR}/denote-export-${FOLDER}-${i}.log"

  # Calculate file range for this server
  START_IDX=$(( (i - 1) * FILES_PER_SERVER ))
  if [ $i -eq $NUM_SERVERS ]; then
    # Last server gets remainder
    END_IDX=$TOTAL_FILES
  else
    END_IDX=$(( i * FILES_PER_SERVER ))
  fi

  # Get file list for this server
  SERVER_FILES=("${FILES[@]:$START_IDX:$((END_IDX - START_IDX))}")
  FILE_COUNT=${#SERVER_FILES[@]}

  log_info "Server $i: processing files $((START_IDX + 1))-${END_IDX} ($FILE_COUNT files)"

  # Build elisp list
  ELISP_LIST="(list"
  for f in "${SERVER_FILES[@]}"; do
    ELISP_LIST+=" \"$f\""
  done
  ELISP_LIST+=")"

  # Send batch to server (background execution)
  log_info "Server $i: starting batch export..."
  emacsclient -s "$DAEMON_NAME" --eval "(denote-export-batch-files $ELISP_LIST \"$LOG_FILE\")" &>/dev/null &
  SERVER_PIDS+=($!)
done

echo ""
log_info "All servers are processing independently!"
log_info "Waiting for completion..."
echo ""

# Monitor progress by checking log files
OVERALL_START=$(date +%s)

while true; do
  COMPLETED=0

  for i in $(seq 1 $NUM_SERVERS); do
    LOG_FILE="${LOG_DIR}/denote-export-${FOLDER}-${i}.log"
    if [ -f "$LOG_FILE" ]; then
      if grep -q "Batch completed:" "$LOG_FILE" 2>/dev/null; then
        COMPLETED=$((COMPLETED + 1))
      fi
    fi
  done

  if [ $COMPLETED -eq $NUM_SERVERS ]; then
    break
  fi

  # Show progress every 10 seconds
  ELAPSED=$(($(date +%s) - OVERALL_START))
  echo -ne "\r[${ELAPSED}s] Servers completed: $COMPLETED/$NUM_SERVERS"

  sleep 5
done

OVERALL_END=$(date +%s)
OVERALL_DURATION=$((OVERALL_END - OVERALL_START))

echo ""
echo ""
log_info "======================================"
log_info "All servers completed!"
log_info "Total duration: ${OVERALL_DURATION}s"
log_info "======================================"
echo ""

# Show results from each server
for i in $(seq 1 $NUM_SERVERS); do
  LOG_FILE="${LOG_DIR}/denote-export-${FOLDER}-${i}.log"
  if [ -f "$LOG_FILE" ]; then
    SUMMARY=$(grep "Batch completed:" "$LOG_FILE" | tail -1)
    log_info "Server $i: $SUMMARY"
  fi
done

echo ""
log_info "Log files: ${LOG_DIR}/denote-export-${FOLDER}-*.log"

# Cleanup servers
echo ""
log_info "Stopping servers..."
for i in $(seq 1 $NUM_SERVERS); do
  DAEMON_NAME="denote-parallel-${FOLDER}-${i}"
  emacsclient -s "$DAEMON_NAME" --eval '(kill-emacs)' &>/dev/null 2>&1 || true
  log_info "✓ Server $i stopped"
done

log_info "Done!"
