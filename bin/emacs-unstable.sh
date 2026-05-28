#!/usr/bin/env bash
# Emacs unstable channel launcher
# 시스템 stable(30.2)과 공존 — server-name "doom-unstable"로 분리.
# `emacs-overlay`의 `emacs-unstable` attribute를 따라간다 (latest release tag).
# 31.1 release day에 자동으로 30.2 → 31.1 점프.
#
# Usage:
#   ./bin/emacs-unstable.sh              # GUI 실행 (daemon + client)
#   ./bin/emacs-unstable.sh --daemon     # daemon만 시작
#   ./bin/emacs-unstable.sh --client     # 기존 daemon에 접속
#   ./bin/emacs-unstable.sh --kill       # daemon 종료
#   ./bin/emacs-unstable.sh --direct     # daemon 없이 직접 실행

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOOMDIR="$SCRIPT_DIR/.."
EMACSDIR="$HOME/doomemacs-unstable"
DOOM_BIN="${EMACSDIR}/bin/doom"

# flake에서 빌드된 emacs-unstable 경로
# GC root를 만들어 nix-gc에서 보호 (--no-link은 GC root 없음 → 삭제됨)
GC_ROOT="$HOME/.local/state/nix/gcroots/emacs-unstable"
mkdir -p "$(dirname "$GC_ROOT")"
# stderr는 살려둔다 — 캐시 miss 시 빌드 진행이 안 보이면 hang 처럼 느껴짐
echo "[emacs-unstable] resolving emacs-unstable store path (nix build, may take a while on cache miss)..." >&2
EMACS_STORE=$(nix build "${DOOMDIR}#emacs-unstable" --out-link "$GC_ROOT" --print-out-paths)
EMACS_BIN="${EMACS_STORE}/bin/emacs"
EMACS_CLIENT="${EMACS_STORE}/bin/emacsclient"

SERVER_NAME="doom-unstable"

# 입력 메서드 환경 (i3wm + fcitx5)
export GTK_IM_MODULE=emacs
export XMODIFIERS=@im=emacs
export DOOMDIR
export EMACSDIR
export EMACS="${EMACS_BIN}"
# 서버 이름 분리 — 기존 "server"(stable)와 충돌 방지
export EMACS_SERVER_NAME="${SERVER_NAME}"

case "${1:-}" in
  --kill)
    $EMACS_CLIENT -s "${SERVER_NAME}" --eval '(kill-emacs)' 2>/dev/null || true
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
    exec "${EMACS_BIN}" --init-directory "${EMACSDIR}" --debug-init
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
    $EMACS_BIN --version | head -1
    ;;
  *)
    # 기본: doom run으로 실행
    exec "${DOOM_BIN}" run
    ;;
esac
