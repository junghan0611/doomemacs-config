#!/usr/bin/env bash
# denote-export-parallel.sh - Parallel export of Denote notes to Hugo markdown
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./denote-export-parallel.sh [CORES]
#
# Prerequisites:
#   - GNU Parallel: apt install parallel
#   - Emacs with Doom configuration
#
# Environment:
#   CORES: Number of parallel jobs (default: 8)
#   ORG_DIR: Base org directory (default: ~/org)
#   DOOM_DIR: Doom config directory (default: ~/.config/doom)
#   DOOMDIR="$HOME/repos/gh/dotdoom-starter"

set -euo pipefail

# Configuration
CORES="${1:-8}"
ORG_DIR="${ORG_DIR:-$HOME/org}"

# Expand tilde in DOOM_DIR
if [[ -n "${DOOM_DIR:-}" ]]; then
  # User provided DOOM_DIR
  DOOM_DIR="${DOOM_DIR/#\~/$HOME}"
else
  # Default paths to check
  if [[ -d "$HOME/repos/gh/doomemacs-config" ]]; then
    DOOM_DIR="$HOME/repos/gh/doomemacs-config"
  elif [[ -d "$HOME/.config/doom" ]]; then
    DOOM_DIR="$HOME/.config/doom"
  else
    echo "ERROR: Cannot find Doom config directory"
    exit 1
  fi
fi

BATCH_SCRIPT="$DOOM_DIR/bin/denote-export-batch.el"

# Directories to export
DIRS=(
  # "$ORG_DIR/meta"
  # "$ORG_DIR/bib"
  # "$ORG_DIR/notes"
  "$ORG_DIR/test"
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

  # Check parallel (prefer GNU Parallel, fallback to xargs)
  if command -v parallel &>/dev/null && parallel --version &>/dev/null 2>&1; then
    log_info "Using GNU Parallel for processing"
  else
    log_warn "GNU Parallel not found (install with: sudo apt install parallel)"
    log_info "Falling back to xargs -P (works but no progress bar)"
  fi

  # Check batch script
  if [[ ! -f "$BATCH_SCRIPT" ]]; then
    log_error "Batch script not found: $BATCH_SCRIPT"
    exit 1
  fi

  # Make batch script executable
  chmod +x "$BATCH_SCRIPT"

  log_info "✓ Prerequisites OK"
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

  # Export each directory
  for dir in "${DIRS[@]}"; do
    if [[ ! -d "$dir" ]]; then
      log_warn "Skipping non-existent directory: $dir"
      continue
    fi

    log_info "Exporting: $dir"

    # Use parallel processing
    # Check if GNU Parallel is available (has --version option)
    if command -v parallel &>/dev/null && parallel --version &>/dev/null 2>&1; then
      # GNU Parallel available
      find "$dir" -name "*.org" -type f -print0 | \
        parallel --null --jobs "$CORES" --bar --eta \
          'emacs --batch --load '"$BATCH_SCRIPT"' {} 2>&1 | grep -v "^Loading"' || true
    else
      # Fallback to xargs
      log_warn "GNU Parallel not found, using xargs -P (no progress bar)"
      find "$dir" -name "*.org" -type f -print0 | \
        xargs -0 -P "$CORES" -I {} \
          emacs --batch --load "$BATCH_SCRIPT" {} 2>&1 | grep -v "^Loading" || true
    fi

    log_info "✓ Completed: $dir"
    log_info ""
  done

  local end_time=$(date +%s)
  local duration=$((end_time - start_time))

  log_info "=================================="
  log_info "✓ Parallel export completed!"
  log_info "  Total files: $total_files"
  log_info "  Cores used: $CORES"
  log_info "  Duration: ${duration}s"
  log_info "=================================="
  log_info ""
  log_info "Next steps:"
  log_info "  1. cd ~/repos/gh/notes/"
  log_info "  2. ./clean-run.sh"
}

# Main execution
main() {
  echo ""
  log_info "=== Denote Parallel Export ==="
  log_info "Cores: $CORES"
  log_info "Org directory: $ORG_DIR"
  log_info ""

  check_prerequisites
  export_parallel
}

main "$@"
