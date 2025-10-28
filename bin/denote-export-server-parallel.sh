#!/usr/bin/env bash
# denote-export-server-parallel.sh - Fast parallel export using Emacs server
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./denote-export-server-parallel.sh [CORES]
#
# Environment:
#   CORES: Number of parallel jobs (default: 8)
#   ORG_DIR: Base org directory (default: ~/org)
#   DOOM_DIR: Doom config directory (default: auto-detect)

set -euo pipefail

# Configuration
CORES="${1:-8}"
ORG_DIR="${ORG_DIR:-$HOME/org}"
SERVER_NAME="denote-export-server"

# Expand tilde in DOOM_DIR
if [[ -n "${DOOM_DIR:-}" ]]; then
  DOOM_DIR="${DOOM_DIR/#\~/$HOME}"
else
  if [[ -d "$HOME/repos/gh/doomemacs-config" ]]; then
    DOOM_DIR="$HOME/repos/gh/doomemacs-config"
  elif [[ -d "$HOME/.config/doom" ]]; then
    DOOM_DIR="$HOME/.config/doom"
  else
    echo "ERROR: Cannot find Doom config directory"
    exit 1
  fi
fi

SERVER_SCRIPT="$DOOM_DIR/bin/denote-export-server.el"

# Directories to export
DIRS=(
  "$ORG_DIR/meta"
  "$ORG_DIR/bib"
  "$ORG_DIR/notes"
  # "$ORG_DIR/test"
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
  echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*"
}

# Check prerequisites
check_prerequisites() {
  log_info "Checking prerequisites..."

  # Check emacsclient
  if ! command -v emacsclient &>/dev/null; then
    log_error "emacsclient not found (install Emacs)"
    exit 1
  fi

  # Check parallel (prefer GNU Parallel)
  if command -v parallel &>/dev/null && parallel --version &>/dev/null 2>&1; then
    log_info "Using GNU Parallel for processing"
  else
    log_warn "GNU Parallel not found (install with: sudo apt install parallel)"
    log_info "Falling back to xargs -P"
  fi

  # Check server script
  if [[ ! -f "$SERVER_SCRIPT" ]]; then
    log_error "Server script not found: $SERVER_SCRIPT"
    exit 1
  fi

  log_info "✓ Prerequisites OK"
}

# Start multiple Emacs servers
start_servers() {
  log_info "Starting $CORES Emacs servers for parallel processing..."

  for i in $(seq 1 $CORES); do
    local server_name="${SERVER_NAME}-${i}"

    # Check if server already running
    if emacsclient -s "$server_name" --eval 't' &>/dev/null; then
      log_warn "Server $i already running, stopping..."
      emacsclient -s "$server_name" --eval '(kill-emacs)' &>/dev/null || true
      sleep 1
    fi

    # Start server in background
    emacs --quick --daemon="$server_name" --load "$SERVER_SCRIPT" &>/dev/null &

    # Wait for this server to be ready (max 10 seconds per server)
    local timeout=10
    local elapsed=0
    while ! emacsclient -s "$server_name" --eval 't' &>/dev/null; do
      if [[ $elapsed -ge $timeout ]]; then
        log_error "Server $i failed to start within ${timeout}s"
        exit 1
      fi
      sleep 0.5
      elapsed=$((elapsed + 1))
    done
  done

  log_info "✓ Started $CORES servers"
}

# Stop all Emacs servers
stop_servers() {
  log_info "Stopping all Emacs servers..."

  for i in $(seq 1 $CORES); do
    local server_name="${SERVER_NAME}-${i}"
    if emacsclient -s "$server_name" --eval '(kill-emacs)' &>/dev/null 2>&1; then
      : # Success, no message
    fi
  done

  sleep 1
  log_info "✓ All servers stopped"
}

# Export single file via emacsclient
export_file() {
  local file="$1"
  local result

  result=$(emacsclient -s "$SERVER_NAME" --eval "(denote-export-file \"$file\")" 2>&1)

  if echo "$result" | grep -q "✓ Exported"; then
    echo "$result" | grep "✓ Exported"
    return 0
  else
    echo "✗ Export failed: $file"
    return 1
  fi
}

# Export function for parallel
export_file_parallel() {
  export -f export_file
  export SERVER_NAME
  export_file "$1"
}

# Count org files
count_files() {
  local total=0
  for dir in "${DIRS[@]}"; do
    if [[ -d "$dir" ]]; then
      local count=$(find "$dir" -name "*.org" -type f 2>/dev/null | wc -l)
      total=$((total + count))
      echo "  $dir: $count files" >&2
    else
      echo "  Directory not found: $dir" >&2
    fi
  done
  echo "$total"
}

# Main export function
export_parallel() {
  log_info "Starting parallel export with $CORES cores..."

  local start_time=$(date +%s)
  local total_files
  total_files=$(count_files)

  if [[ $total_files -eq 0 ]]; then
    log_warn "No org files found to export."
    return 0
  fi

  log_info "Total files to export: $total_files"
  log_info ""

  # Global counters (use temp files for subshell communication)
  local counter_file=$(mktemp)
  local success_file=$(mktemp)
  local error_file=$(mktemp)
  echo "0" > "$counter_file"
  echo "0" > "$success_file"
  echo "0" > "$error_file"

  # Export each directory
  for dir in "${DIRS[@]}"; do
    if [[ ! -d "$dir" ]]; then
      log_warn "Skipping non-existent directory: $dir"
      continue
    fi

    local dir_files=$(find "$dir" -name "*.org" -type f 2>/dev/null | wc -l)
    local dir_name=$(basename "$dir")

    log_info "Exporting: $dir ($dir_files files)"

    # Export with GNU Parallel or xargs
    if command -v parallel &>/dev/null && parallel --version &>/dev/null 2>&1; then
      # Use {%} to get job slot number (0 to CORES-1) for server selection
      find "$dir" -name "*.org" -type f -print0 | \
        parallel --null --jobs "$CORES" --no-notice --line-buffer \
          'emacsclient -s '"$SERVER_NAME"'-{%} --eval "(denote-export-file \"{}\")"' | \
        while IFS= read -r line; do
          # Remove surrounding quotes
          line="${line#\"}"
          line="${line%\"}"

          # Update counter in file
          local current_count=$(cat "$counter_file")
          current_count=$((current_count + 1))
          echo "$current_count" > "$counter_file"

          if [[ "$line" =~ ^SUCCESS:(.+):(.+)$ ]]; then
            local current_success=$(cat "$success_file")
            current_success=$((current_success + 1))
            echo "$current_success" > "$success_file"

            local src="${BASH_REMATCH[1]}"
            local dst="${BASH_REMATCH[2]}"
            printf "[%3d/%d] ✓ Exported: %s → %s\n" "$current_count" "$total_files" "$src" "$dst"
          elif [[ "$line" =~ ^ERROR:(.+):(.+)$ ]]; then
            local current_errors=$(cat "$error_file")
            current_errors=$((current_errors + 1))
            echo "$current_errors" > "$error_file"

            local src="${BASH_REMATCH[1]}"
            local err="${BASH_REMATCH[2]}"
            printf "[%3d/%d] ✗ Export failed: %s (%s)\n" "$current_count" "$total_files" "$src" "$err"
          fi
        done || true
    else
      log_warn "Using xargs -P"
      find "$dir" -name "*.org" -type f -print0 | \
        xargs -0 -P "$CORES" -I {} \
          emacsclient -s "$SERVER_NAME" --eval "(denote-export-file \"{}\")" | \
        while IFS= read -r line; do
          # Remove surrounding quotes
          line="${line#\"}"
          line="${line%\"}"

          # Update counter in file
          local current_count=$(cat "$counter_file")
          current_count=$((current_count + 1))
          echo "$current_count" > "$counter_file"

          if [[ "$line" =~ ^SUCCESS:(.+):(.+)$ ]]; then
            local current_success=$(cat "$success_file")
            current_success=$((current_success + 1))
            echo "$current_success" > "$success_file"

            local src="${BASH_REMATCH[1]}"
            local dst="${BASH_REMATCH[2]}"
            printf "[%3d/%d] ✓ Exported: %s → %s\n" "$current_count" "$total_files" "$src" "$dst"
          elif [[ "$line" =~ ^ERROR:(.+):(.+)$ ]]; then
            local current_errors=$(cat "$error_file")
            current_errors=$((current_errors + 1))
            echo "$current_errors" > "$error_file"

            local src="${BASH_REMATCH[1]}"
            local err="${BASH_REMATCH[2]}"
            printf "[%3d/%d] ✗ Export failed: %s (%s)\n" "$current_count" "$total_files" "$src" "$err"
          fi
        done || true
    fi

    # Read final counts from files
    local final_success=$(cat "$success_file")
    local final_errors=$(cat "$error_file")

    log_info "✓ Completed: $dir ($final_success success, $final_errors errors)"
    log_info ""
  done

  local end_time=$(date +%s)
  local duration=$((end_time - start_time))

  # Read final totals
  local total_success=$(cat "$success_file")
  local total_errors=$(cat "$error_file")

  # Cleanup temp files
  rm -f "$counter_file" "$success_file" "$error_file"

  log_info "=================================="
  log_info "✓ Parallel export completed!"
  log_info "  Total files: $total_files"
  log_info "  Success: $total_success"
  log_info "  Errors: $total_errors"
  log_info "  Cores used: $CORES"
  log_info "  Duration: ${duration}s"
  log_info "  Speed: $(awk "BEGIN {printf \"%.1f\", $total_files/$duration}") files/sec"
  log_info "=================================="
  log_info ""
  log_info "Next steps:"
  log_info "  1. cd ~/repos/gh/notes/"
  log_info "  2. ./clean-run.sh"
}

# Cleanup function
cleanup() {
  log_info "Cleaning up..."
  stop_servers
}

# Trap EXIT to ensure server cleanup
trap cleanup EXIT INT TERM

# Main execution
main() {
  echo ""
  log_info "=== Denote Server-based Export ==="
  log_info "Server: $SERVER_NAME"
  log_info "Cores: $CORES"
  log_info "Org directory: $ORG_DIR"
  log_info ""

  check_prerequisites
  start_servers
  export_parallel
}

main "$@"
