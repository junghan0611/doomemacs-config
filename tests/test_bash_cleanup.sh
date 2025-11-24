#!/usr/bin/env bash
# test_bash_cleanup.sh - Test bash trap cleanup for denote-export.sh
#
# 이 테스트는 bash 스크립트의 trap 기능을 검증합니다.
#
# 실행 방법:
#   cd tests
#   ./test_bash_cleanup.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXPORT_SCRIPT="${SCRIPT_DIR}/../bin/denote-export.sh"

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

pass_count=0
fail_count=0

log_pass() {
  echo -e "${GREEN}[PASS]${NC} $*"
  pass_count=$((pass_count + 1))
}

log_fail() {
  echo -e "${RED}[FAIL]${NC} $*"
  fail_count=$((fail_count + 1))
}

log_info() {
  echo -e "${YELLOW}[INFO]${NC} $*"
}

# Test 1: Script syntax check
test_syntax() {
  log_info "Test 1: Checking bash syntax..."
  if bash -n "$EXPORT_SCRIPT" 2>/dev/null; then
    log_pass "Bash syntax is valid"
  else
    log_fail "Bash syntax error"
  fi
}

# Test 2: Trap is defined
test_trap_defined() {
  log_info "Test 2: Checking trap definition..."
  if grep -q "trap cleanup_on_exit SIGINT SIGTERM" "$EXPORT_SCRIPT"; then
    log_pass "Trap for SIGINT/SIGTERM is defined"
  else
    log_fail "Trap not found in script"
  fi
}

# Test 3: cleanup_on_exit function exists
test_cleanup_function() {
  log_info "Test 3: Checking cleanup_on_exit function..."
  if grep -q "cleanup_on_exit()" "$EXPORT_SCRIPT"; then
    log_pass "cleanup_on_exit function exists"
  else
    log_fail "cleanup_on_exit function not found"
  fi
}

# Test 4: pkill is called in cleanup
test_pkill_in_cleanup() {
  log_info "Test 4: Checking pkill in cleanup..."
  if grep -q 'pkill -f "denote-export-daemon"' "$EXPORT_SCRIPT"; then
    log_pass "pkill command found in cleanup"
  else
    log_fail "pkill not found in cleanup"
  fi
}

# Test 5: CLEANUP_DAEMONS is set in each command
test_cleanup_daemons_var() {
  log_info "Test 5: Checking CLEANUP_DAEMONS variable..."

  local commands=("all" "meta" "bib" "notes" "test" "run")
  local missing=()

  for cmd in "${commands[@]}"; do
    # Check if CLEANUP_DAEMONS is set after NUM_DAEMONS in each case
    if ! grep -A3 "^  ${cmd})" "$EXPORT_SCRIPT" | grep -q 'CLEANUP_DAEMONS='; then
      missing+=("$cmd")
    fi
  done

  if [ ${#missing[@]} -eq 0 ]; then
    log_pass "CLEANUP_DAEMONS is set in all commands"
  else
    log_fail "CLEANUP_DAEMONS missing in: ${missing[*]}"
  fi
}

# Test 6: Python script has signal handlers
test_python_signal_handlers() {
  log_info "Test 6: Checking Python signal handlers..."
  local python_script="${SCRIPT_DIR}/../bin/denote-export-parallel.py"

  if grep -q "signal.signal(signal.SIGINT" "$python_script" && \
     grep -q "signal.signal(signal.SIGTERM" "$python_script"; then
    log_pass "Python signal handlers are registered"
  else
    log_fail "Python signal handlers not found"
  fi
}

# Test 7: Python script has atexit
test_python_atexit() {
  log_info "Test 7: Checking Python atexit..."
  local python_script="${SCRIPT_DIR}/../bin/denote-export-parallel.py"

  if grep -q "atexit.register(cleanup_daemons)" "$python_script"; then
    log_pass "Python atexit is registered"
  else
    log_fail "Python atexit not found"
  fi
}

# Test 8: Python cleanup_daemons function exists
test_python_cleanup_function() {
  log_info "Test 8: Checking Python cleanup_daemons..."
  local python_script="${SCRIPT_DIR}/../bin/denote-export-parallel.py"

  if grep -q "def cleanup_daemons():" "$python_script"; then
    log_pass "Python cleanup_daemons function exists"
  else
    log_fail "Python cleanup_daemons function not found"
  fi
}

# Main
echo "=============================================="
echo "Denote Export Cleanup Tests (Bash)"
echo "=============================================="
echo ""

test_syntax
test_trap_defined
test_cleanup_function
test_pkill_in_cleanup
test_cleanup_daemons_var
test_python_signal_handlers
test_python_atexit
test_python_cleanup_function

echo ""
echo "=============================================="
echo "Results: ${pass_count} passed, ${fail_count} failed"
echo "=============================================="

if [ $fail_count -gt 0 ]; then
  exit 1
fi
exit 0
