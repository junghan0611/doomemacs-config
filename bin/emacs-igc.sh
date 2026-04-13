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
DOOMDIR="$SCRIPT_DIR/.."
EMACSDIR="$HOME/doomemacs-igc"
DOOM_BIN="${EMACSDIR}/bin/doom"

# flake에서 빌드된 emacs-igc 경로
# GC root를 만들어 nix-gc에서 보호 (--no-link은 GC root 없음 → 삭제됨)
GC_ROOT="$HOME/.local/state/nix/gcroots/emacs-igc"
mkdir -p "$(dirname "$GC_ROOT")"
IGC_STORE=$(nix build "${DOOMDIR}#emacs-igc" --out-link "$GC_ROOT" --print-out-paths 2>/dev/null)
IGC_EMACS="${IGC_STORE}/bin/emacs"
IGC_CLIENT="${IGC_STORE}/bin/emacsclient"

SERVER_NAME="doom-igc"

# 입력 메서드 환경 (i3wm + fcitx5)
export GTK_IM_MODULE=emacs
export XMODIFIERS=@im=emacs
export DOOMDIR
export EMACSDIR
export EMACS="${IGC_EMACS}"
# 서버 이름 분리 — 기존 "server"(30.2)와 충돌 방지
export EMACS_SERVER_NAME="${SERVER_NAME}"

case "${1:-}" in
  --kill)
    $IGC_CLIENT -s "${SERVER_NAME}" --eval '(kill-emacs)' 2>/dev/null || true
    echo "Daemon ${SERVER_NAME} stopped."
    ;;
  --sync)
    unset EMACS_SERVER_NAME
    exec "${DOOM_BIN}" sync
    ;;
  --env)
    unset EMACS_SERVER_NAME
    exec "${DOOM_BIN}" env
    ;;
  --install)
    # sync + env + profile 한방
    unset EMACS_SERVER_NAME
    "${DOOM_BIN}" sync && "${DOOM_BIN}" env && "${DOOM_BIN}" profile sync --all
    ;;
  --nw|--tty)
    # 터미널 모드 — daemon 없이 독립 인스턴스
    exec "${DOOM_BIN}" run -nw
    ;;
  --direct)
    # --nw의 이전 이름 (호환)
    exec "${DOOM_BIN}" run -nw
    ;;
  --debug)
    # debug-init으로 직접 실행 — 에러 추적용
    exec "${IGC_EMACS}" --init-directory "${EMACSDIR}" --debug-init
    ;;
  --update)
    # sync -u (패키지 업데이트 포함)
    unset EMACS_SERVER_NAME
    "${DOOM_BIN}" sync -u -j 2
    ;;
  --doctor)
    unset EMACS_SERVER_NAME
    exec "${DOOM_BIN}" doctor
    ;;
  --version)
    $IGC_EMACS --version | head -1
    $IGC_EMACS --batch --eval '(princ (format "MPS/IGC: %s\n" (fboundp (quote igc-info))))' 2>/dev/null
    ;;
  *)
    # 기본: doom run으로 실행
    exec "${DOOM_BIN}" run
    ;;
esac
