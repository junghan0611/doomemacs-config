#!/usr/bin/env bash
# denote-dblock-update.sh - Update dblocks in Denote meta notes
#
# Copyright (C) 2025 Junghan Kim
# Author: Junghan Kim <junghanacs@gmail.com>
#
# Usage:
#   ./bin/denote-dblock-update.sh [directory]
#   ./bin/denote-dblock-update.sh ~/org/meta
#
# Features:
#   - Smart pre-filtering (only process files with dblocks)
#   - Progress tracking
#   - Summary statistics

set -euo pipefail

# Configuration
DOOM_DIR="${HOME}/sync/emacs/doomemacs-config"
BATCH_SCRIPT="${DOOM_DIR}/bin/denote-dblock-batch.el"
DEFAULT_DIR="${HOME}/org/meta"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
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

log_step() {
  echo -e "${BLUE}[STEP]${NC} $*"
}

# Check arguments
TARGET_DIR="${1:-$DEFAULT_DIR}"

if [ ! -d "$TARGET_DIR" ]; then
  log_error "Directory not found: $TARGET_DIR"
  echo ""
  echo "Usage: $0 [directory]"
  echo "Example: $0 ~/org/meta"
  exit 1
fi

if [ ! -f "$BATCH_SCRIPT" ]; then
  log_error "Batch script not found: $BATCH_SCRIPT"
  exit 1
fi

log_info "======================================"
log_info "Denote dblock Update"
log_info "======================================"
log_info "Target: $TARGET_DIR"
log_info "Script: $BATCH_SCRIPT"
echo ""

# Count files
log_step "Scanning directory..."
TOTAL_FILES=$(find "$TARGET_DIR" -name "*.org" -type f | wc -l)
log_info "Found $TOTAL_FILES org files"

# Pre-scan for dblock files (use null delimiter for filenames with spaces)
log_step "Pre-scanning for dblocks..."
DBLOCK_FILES=$(find "$TARGET_DIR" -name "*.org" -type f -print0 | xargs -0 grep -l "BEGIN:" 2>/dev/null | wc -l)
log_info "Found $DBLOCK_FILES files with dblocks"

if [ "$DBLOCK_FILES" -eq 0 ]; then
  log_warn "No files with dblocks found. Nothing to do."
  exit 0
fi

echo ""
log_step "Starting batch update..."
echo ""

# Run batch script
START_TIME=$(date +%s)

emacs --batch --load "$BATCH_SCRIPT" "$TARGET_DIR" 2>&1

EXIT_CODE=$?
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

echo ""

if [ $EXIT_CODE -eq 0 ]; then
  log_info "======================================"
  log_info "✓ Batch update completed!"
  log_info "Duration: ${DURATION}s"
  log_info "Files processed: $TOTAL_FILES"
  log_info "Files with dblocks: $DBLOCK_FILES"
  log_info "======================================"
else
  log_error "======================================"
  log_error "✗ Batch update failed!"
  log_error "Exit code: $EXIT_CODE"
  log_error "======================================"
  exit $EXIT_CODE
fi
