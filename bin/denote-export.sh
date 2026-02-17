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
ORG_ROOT="${HOME}/org"
DEFAULT_DAEMONS=8  # 16 threads / 2 = 8 daemons (200MB each, ~1.6GB total)

# Cleanup function for trap
cleanup_on_exit() {
  echo ""
  echo -e "${YELLOW}[WARN]${NC} Interrupted! Cleaning up daemons..."

  # Kill any remaining daemon processes
  pkill -f "denote-export-daemon" 2>/dev/null || true

  # Double-check with emacsclient (graceful shutdown)
  for i in $(seq 1 "${CLEANUP_DAEMONS:-4}"); do
    emacsclient -s "denote-export-daemon-$i" --eval '(kill-emacs)' 2>/dev/null || true
  done

  echo -e "${GREEN}[INFO]${NC} Cleanup completed."
  exit 130
}

# Register trap for SIGINT (Ctrl+C) and SIGTERM
trap cleanup_on_exit SIGINT SIGTERM

# Track daemon count for cleanup (matches DEFAULT_DAEMONS)
CLEANUP_DAEMONS=$DEFAULT_DAEMONS

# Target directories
META_DIR="${ORG_ROOT}/meta"
BIB_DIR="${ORG_ROOT}/bib"
NOTES_DIR="${ORG_ROOT}/notes"
TEST_DIR="${ORG_ROOT}/test"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

show_usage() {
  cat <<EOF
${GREEN}Denote Export - 병렬 처리 기반 Hugo 변환 & Dblock 업데이트${NC}

${BLUE}사용법:${NC}
  denote-export.sh [명령] [옵션]

${BLUE}Export 명령 (멀티 데몬):${NC}
  all [daemons]          - 전체 폴더 순차 처리 (meta, bib, notes) ${YELLOW}⭐ 권장${NC}
  meta [daemons]         - ~/org/meta 폴더만 (530 files)
  bib [daemons]          - ~/org/bib 폴더만 (649 files)
  notes [daemons]        - ~/org/notes 폴더만 (797 files)
  test [daemons]         - ~/org/test 폴더만 (빠른 검증)
  run [dir] [daemons]    - 커스텀 디렉토리 지정

${BLUE}증분 내보내기:${NC}
  기본 동작은 마지막 내보내기 이후 변경된 파일만 처리합니다.
  --force                - 타임스탬프 무시, 전체 내보내기 강제

${BLUE}Dblock 명령 (멀티 데몬):${NC}
  dblock [dir] [daemons] - 디렉토리 내 dblock 업데이트 (기본: ~/org/meta, 재귀)
  --help                 - 이 도움말 표시

${BLUE}옵션:${NC}
  daemons  - 병렬 daemon 개수 (기본: 2, 메모리 여유 시 4까지)

${BLUE}예제:${NC}
  ${GREEN}# 전체 export - 증분 (변경분만)${NC}
  denote-export.sh all

  ${GREEN}# 전체 export - 전체 강제${NC}
  denote-export.sh all --force

  ${GREEN}# meta 폴더만, 8 daemons${NC}
  denote-export.sh meta 8

  ${GREEN}# meta 폴더만, 전체 강제${NC}
  denote-export.sh meta --force

  ${GREEN}# 테스트 폴더만 (빠른 검증)${NC}
  denote-export.sh test 2

  ${GREEN}# 커스텀 디렉토리${NC}
  denote-export.sh run ~/custom/path 4

  ${GREEN}# dblock 업데이트 (meta 폴더, 2 daemons)${NC}
  denote-export.sh dblock

  ${GREEN}# dblock 업데이트 (커스텀 디렉토리, 4 daemons)${NC}
  denote-export.sh dblock ~/org/notes 4

${BLUE}성능 (2 daemons, 메모리 안정):${NC}
  - meta:  530 files → ~9분
  - bib:   649 files → ~11분
  - notes: 797 files → ~13분
  - 전체:  1976 files → ~33분

${BLUE}로그 확인:${NC}
  # 에러만 확인
  grep "✗" /tmp/denote-export-*.log

${BLUE}문제 해결:${NC}
  # Daemon 강제 종료
  pkill -f "denote-export-daemon"

  # 테스트 실행
  cd ${SCRIPT_DIR}/../tests && ./run-tests.sh

${BLUE}상세 문서:${NC}
  ${SCRIPT_DIR}/README.org

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

# Detect --force flag from any position
FORCE_FLAG=""
ARGS=()
for arg in "$@"; do
  if [ "$arg" = "--force" ]; then
    FORCE_FLAG="--force"
  else
    ARGS+=("$arg")
  fi
done

# Parse command from filtered args
COMMAND="${ARGS[0]:-all}"

case "$COMMAND" in
  help|--help|-h)
    show_usage
    exit 0
    ;;

  all)
    NUM_DAEMONS="${ARGS[1]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    MODE_DESC="증분"
    [ -n "$FORCE_FLAG" ] && MODE_DESC="전체 (force)"

    log_info "=========================================="
    log_info "Denote Export - 전체 폴더 순차 처리 ($MODE_DESC)"
    log_info "Daemons per folder: $NUM_DAEMONS"
    log_info "Folders: meta → bib → notes"
    log_info "=========================================="
    echo ""

    # Process each directory sequentially, reusing daemons between folders
    FOLDERS=(meta bib notes)
    TOTAL_FOLDERS=${#FOLDERS[@]}
    FOLDER_NUM=1
    ALL_START=$(date +%s)

    for DIR_NAME in "${FOLDERS[@]}"; do
      DIR_PATH="${ORG_ROOT}/${DIR_NAME}"

      if [ ! -d "$DIR_PATH" ]; then
        log_warn "Directory not found: $DIR_PATH (skipping)"
        FOLDER_NUM=$((FOLDER_NUM + 1))
        continue
      fi

      echo ""
      log_info "=================================================="
      log_info "[$FOLDER_NUM/$TOTAL_FOLDERS] Processing: $DIR_NAME"
      log_info "=================================================="
      echo ""

      # Keep daemons alive between folders, cleanup only on last folder
      if [ "$FOLDER_NUM" -lt "$TOTAL_FOLDERS" ]; then
        python3 "$PYTHON_SCRIPT" export "$DIR_PATH" "$NUM_DAEMONS" $FORCE_FLAG --no-cleanup
      else
        python3 "$PYTHON_SCRIPT" export "$DIR_PATH" "$NUM_DAEMONS" $FORCE_FLAG
      fi

      echo ""
      log_info "✓ Folder $DIR_NAME completed"
      echo ""

      FOLDER_NUM=$((FOLDER_NUM + 1))
    done

    ALL_END=$(date +%s)
    ALL_DURATION=$((ALL_END - ALL_START))
    ALL_MIN=$((ALL_DURATION / 60))
    ALL_SEC=$((ALL_DURATION % 60))

    echo ""
    log_info "=========================================="
    log_info "✓ 전체 export 완료! (${ALL_MIN}m ${ALL_SEC}s)"
    log_info "=========================================="
    ;;

  meta)
    NUM_DAEMONS="${ARGS[1]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ ! -d "$META_DIR" ]; then
      log_error "Directory not found: $META_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - meta 폴더"
    log_info "Directory: $META_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" export "$META_DIR" "$NUM_DAEMONS" $FORCE_FLAG
    ;;

  bib)
    NUM_DAEMONS="${ARGS[1]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ ! -d "$BIB_DIR" ]; then
      log_error "Directory not found: $BIB_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - bib 폴더"
    log_info "Directory: $BIB_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" export "$BIB_DIR" "$NUM_DAEMONS" $FORCE_FLAG
    ;;

  notes)
    NUM_DAEMONS="${ARGS[1]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ ! -d "$NOTES_DIR" ]; then
      log_error "Directory not found: $NOTES_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - notes 폴더"
    log_info "Directory: $NOTES_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" export "$NOTES_DIR" "$NUM_DAEMONS" $FORCE_FLAG
    ;;

  test)
    NUM_DAEMONS="${ARGS[1]:-2}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ ! -d "$TEST_DIR" ]; then
      log_error "Test directory not found: $TEST_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - 테스트 실행"
    log_info "Directory: $TEST_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" export "$TEST_DIR" "$NUM_DAEMONS" $FORCE_FLAG
    ;;

  run)
    ORG_DIR="${ARGS[1]}"
    NUM_DAEMONS="${ARGS[2]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ -z "$ORG_DIR" ]; then
      log_error "Directory argument required for 'run' command"
      echo ""
      show_usage
      exit 1
    fi

    if [ ! -d "$ORG_DIR" ]; then
      log_error "Directory not found: $ORG_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Export - 커스텀 디렉토리"
    log_info "Directory: $ORG_DIR"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" export "$ORG_DIR" "$NUM_DAEMONS" $FORCE_FLAG
    ;;

  dblock)
    DBLOCK_DIR="${ARGS[1]:-$META_DIR}"
    NUM_DAEMONS="${ARGS[2]:-$DEFAULT_DAEMONS}"
    CLEANUP_DAEMONS="$NUM_DAEMONS"

    if [ ! -d "$DBLOCK_DIR" ]; then
      log_error "Directory not found: $DBLOCK_DIR"
      exit 1
    fi

    log_info "======================================"
    log_info "Denote Dblock - 멀티 데몬 업데이트"
    log_info "Directory: $DBLOCK_DIR (재귀)"
    log_info "Daemons: $NUM_DAEMONS"
    log_info "======================================"
    echo ""

    python3 "$PYTHON_SCRIPT" dblock "$DBLOCK_DIR" "$NUM_DAEMONS"
    ;;

  *)
    log_error "Unknown command: $COMMAND"
    echo ""
    show_usage
    exit 1
    ;;
esac
