#!/usr/bin/env bash
# denote-export.sh - Denote → Hugo 병렬 변환 wrapper 스크립트
#
# Copyright (C) 2025 Junghan Kim
#
# 이 스크립트는 Python multi-daemon export를 쉽게 실행하기 위한 wrapper입니다.
# Python 스크립트가 실제 작업을 수행합니다.
#
# 성능: 1.8 files/sec (18x faster), 100% 성공률
# 기술: Python ProcessPoolExecutor + Multi Emacs Daemon
# Unicode: NBSP (U+00A0) 안전 처리

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_SCRIPT="${SCRIPT_DIR}/denote-export-parallel.py"
DEFAULT_ORG_DIR="${HOME}/org"
DEFAULT_DAEMONS=4

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

show_usage() {
  cat <<EOF
${GREEN}Denote Export - 병렬 처리 기반 Hugo 변환${NC}

${BLUE}사용법:${NC}
  $0 [명령] [옵션]

${BLUE}명령:${NC}
  run [dir] [daemons]    - 병렬 export 실행 (기본)
  bg  [dir] [daemons]    - 백그라운드 실행 (장시간 작업)
  test [daemons]         - 테스트 폴더만 실행
  help                   - 이 도움말 표시

${BLUE}옵션:${NC}
  dir      - Org 파일 디렉토리 (기본: ~/org)
  daemons  - 병렬 daemon 개수 (기본: 4, 권장: CPU 코어 수)

${BLUE}예제:${NC}
  ${GREEN}# 기본 실행 (~/org, 4 daemons)${NC}
  $0

  ${GREEN}# 특정 디렉토리, 8 daemons${NC}
  $0 run ~/org/notes 8

  ${GREEN}# 백그라운드 실행 (잠자기 전)${NC}
  $0 bg ~/org 8

  ${GREEN}# 테스트 폴더만 (빠른 검증)${NC}
  $0 test 2

${BLUE}성능:${NC}
  - 속도: 1.8 files/sec
  - 품질: 100% 성공률
  - 2008 파일 기준:
    * 4 daemons: ~18분
    * 8 daemons: ~9분

${BLUE}로그 확인:${NC}
  # 실시간 로그
  tail -f /tmp/denote-export-*.log

  # 에러만 확인
  grep "✗" /tmp/denote-export-*.log

${BLUE}문제 해결:${NC}
  # Daemon 강제 종료
  pkill -f "denote-export-daemon"

  # 테스트 실행
  cd ${SCRIPT_DIR}/../tests && ./run-tests.sh

${BLUE}상세 문서:${NC}
  ${SCRIPT_DIR}/README.md
  ${SCRIPT_DIR}/EXPORT-PARALLEL.org

EOF
}

log_info() {
  echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
  echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
  echo -e "${RED}[ERROR]${NC} $*"
}

# Check Python script exists
if [ ! -f "$PYTHON_SCRIPT" ]; then
  log_error "Python script not found: $PYTHON_SCRIPT"
  exit 1
fi

# Parse command
COMMAND="${1:-run}"

case "$COMMAND" in
  help|--help|-h)
    show_usage
    exit 0
    ;;

  run)
    ORG_DIR="${2:-$DEFAULT_ORG_DIR}"
    NUM_DAEMONS="${3:-$DEFAULT_DAEMONS}"

    if [ ! -d "$ORG_DIR" ]; then
      log_error "Directory not found: $ORG_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - 병렬 처리"
    log_info "Directory: $ORG_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    # Run Python script
    python3 "$PYTHON_SCRIPT" "$ORG_DIR" "$NUM_DAEMONS"
    ;;

  bg)
    ORG_DIR="${2:-$DEFAULT_ORG_DIR}"
    NUM_DAEMONS="${3:-$DEFAULT_DAEMONS}"

    if [ ! -d "$ORG_DIR" ]; then
      log_error "Directory not found: $ORG_DIR"
      exit 1
    fi

    TIMESTAMP=$(date +%Y%m%d-%H%M%S)
    LOG_FILE="/tmp/denote-export-${TIMESTAMP}.log"

    log_info "======================================"
    log_info "Denote Export - 백그라운드 실행"
    log_info "Directory: $ORG_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "Log file: $LOG_FILE"
    log_info "======================================"
    echo ""

    # Run in background
    nohup python3 "$PYTHON_SCRIPT" "$ORG_DIR" "$NUM_DAEMONS" \
      > "$LOG_FILE" 2>&1 &

    PID=$!
    echo "$PID" > /tmp/denote-export.pid

    log_info "프로세스 시작: PID=$PID"
    log_info "로그 확인: tail -f $LOG_FILE"
    log_info "프로세스 종료: kill $PID"
    ;;

  test)
    NUM_DAEMONS="${2:-2}"
    TEST_DIR="${HOME}/org/test"

    if [ ! -d "$TEST_DIR" ]; then
      log_warn "Test directory not found: $TEST_DIR"
      log_warn "Using default org directory instead"
      TEST_DIR="$DEFAULT_ORG_DIR"
    fi

    log_info "======================================"
    log_info "Denote Export - 테스트 실행"
    log_info "Directory: $TEST_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    # Run Python script
    python3 "$PYTHON_SCRIPT" "$TEST_DIR" "$NUM_DAEMONS"
    ;;

  *)
    log_error "Unknown command: $COMMAND"
    echo ""
    show_usage
    exit 1
    ;;
esac
