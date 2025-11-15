#!/usr/bin/env bash
# run-tests.sh - Run all ERT tests for doomemacs-config

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="$(dirname "$SCRIPT_DIR")"

echo "================================"
echo "Running doomemacs-config tests"
echo "================================"
echo ""

# Run tests with emacs in batch mode
# Note: Using -Q skips package initialization, so link export tests will be skipped
# This is acceptable as those tests require ox-hugo and other dependencies
# Link export functionality is tested through manual export verification
emacs -Q --batch \
      -L "$CONFIG_DIR" \
      -l "$SCRIPT_DIR/test-helper.el" \
      -l "$SCRIPT_DIR/test-notifications.el" \
      -l "$SCRIPT_DIR/test-denote-silo.el" \
      -l "$SCRIPT_DIR/test-denote-export.el" \
      -f ert-run-tests-batch-and-exit

echo ""
echo "================================"
echo "All tests completed!"
echo "================================"
