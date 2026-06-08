#!/usr/bin/env bash
# run-tests.sh - Run all ERT tests for doomemacs-config
#
# Auto-discovers every tests/test-*.el file (test-helper.el loaded first).
# Drop a new test-<module>.el in this directory and it runs with no edits here.
#
# Harness contract (see TESTING-GUIDELINES.org § Tier A/B):
#   Tests run under `emacs -Q --batch` — NO Doom, NO package init.
#   So only Doom-free logic (Tier A) is exercised here.  Functions that need
#   ox-hugo / denote / other packages must `skip-unless` their dependency
#   (they show up as SKIPPED, not failures) or be verified manually.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="$(dirname "$SCRIPT_DIR")"

echo "================================"
echo "Running doomemacs-config tests"
echo "================================"
echo ""

# test-helper.el must load before any test file; the rest are discovered.
LOAD_ARGS=(-l "$SCRIPT_DIR/test-helper.el")
for f in "$SCRIPT_DIR"/test-*.el; do
    [ "$(basename "$f")" = "test-helper.el" ] && continue
    echo "  discovered: $(basename "$f")"
    LOAD_ARGS+=(-l "$f")
done
echo ""

emacs -Q --batch \
      -L "$CONFIG_DIR" \
      "${LOAD_ARGS[@]}" \
      -f ert-run-tests-batch-and-exit

echo ""
echo "================================"
echo "All tests completed!"
echo "================================"
