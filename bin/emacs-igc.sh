#!/usr/bin/env bash
# Emacs 31 IGC (MPS GC) launcher
# 기존 30.2와 공존 — server-name "doom-igc"로 분리
#
# Usage:
#   ./bin/emacs-igc.sh              # GUI 실행 (daemon + client)
#   ./bin/emacs-igc.sh --daemon     # daemon만 시작
#   ./bin/emacs-igc.sh --client     # 기존 daemon에 접속
#   ./bin/emacs-igc.sh --kill       # daemon 종료
#   ./bin/emacs-igc.sh --direct     # daemon 없이 직접 실행

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOOM_DIR="$SCRIPT_DIR/.."
DOOM_BIN="$HOME/.config/emacs/bin/doom"

# flake에서 빌드된 emacs-igc 경로
IGC_STORE=$(nix build "${DOOM_DIR}#emacs-igc" --no-link --print-out-paths 2>/dev/null)
IGC_EMACS="${IGC_STORE}/bin/emacs"
IGC_CLIENT="${IGC_STORE}/bin/emacsclient"

SERVER_NAME="doom-igc"

# 입력 메서드 환경 (i3wm + fcitx5)
export GTK_IM_MODULE=emacs
export XMODIFIERS=@im=emacs
export DOOMDIR="${DOOM_DIR}"

case "${1:-}" in
  --kill)
    $IGC_CLIENT -s "${SERVER_NAME}" --eval '(kill-emacs)' 2>/dev/null || true
    echo "Daemon ${SERVER_NAME} stopped."
    ;;
  --direct)
    # daemon 없이 직접 실행
    exec env EMACS="${IGC_EMACS}" "${DOOM_BIN}" run
    ;;
  --version)
    $IGC_EMACS --version | head -1
    $IGC_EMACS --batch --eval '(princ (format "MPS/IGC: %s\n" (fboundp (quote igc-info))))' 2>/dev/null
    ;;
  *)
    # 기본: doom run으로 실행 (EMACS 환경변수로 31 IGC 바이너리 지정)
    exec env EMACS="${IGC_EMACS}" "${DOOM_BIN}" run
    ;;
esac
