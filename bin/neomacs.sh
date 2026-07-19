#!/usr/bin/env bash
# Neomacs vanilla minimal profile launcher — doomemacs-config issue #8
#
# Neomacs is a from-scratch Rust reimplementation of the Emacs core
# (github.com/eval-exec/neomacs).  This runs it against the builtin-only
# profile in neomacs/, fully separate from Doom: different init directory,
# different server name, no shared state.
#
# The release AppImage is dynamically linked against libfontconfig et al,
# which NixOS does not provide at the usual paths, so it is launched
# through appimage-run.  NEOMACS_BIN overrides that for a native build.
#
# Usage:
#   ./bin/neomacs.sh                 # GUI
#   ./bin/neomacs.sh --nw            # terminal
#   ./bin/neomacs.sh --probe         # run every K-review probe (batch)
#   ./bin/neomacs.sh --probe env     # run one probe
#   ./bin/neomacs.sh --daemon        # start daemon
#   ./bin/neomacs.sh --kill          # stop daemon
#   ./bin/neomacs.sh --version
#   ./bin/neomacs.sh --fetch [TAG]   # download a release AppImage
#   ./bin/neomacs.sh --gnu ...       # same profile on stock GNU Emacs
#
# Comparing a probe run under --gnu against the default run is how a
# finding gets attributed to Neomacs rather than to this config.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
PROFILE_DIR="${REPO_DIR}/neomacs"
PROBE_DIR="${PROFILE_DIR}/probe"

NEOMACS_VERSION="${NEOMACS_VERSION:-0.0.13}"
APPIMAGE_DIR="${NEOMACS_APPIMAGE_DIR:-$HOME/.local/bin}"
APPIMAGE="${APPIMAGE_DIR}/neomacs-${NEOMACS_VERSION}-x86_64-unknown-linux-gnu.AppImage"

SERVER_NAME="neomacs"
export EMACS_SERVER_NAME="${SERVER_NAME}"

# fcitx5 is bypassed on purpose: the profile drives Neomacs' own builtin
# Hangul composition, so a K finding is never confounded by the toolkit IME.
export GTK_IM_MODULE=emacs
export XMODIFIERS=@im=emacs

die() { echo "[neomacs] $*" >&2; exit 1; }

# Resolve the runner. Order: explicit NEOMACS_BIN, native neomacs on PATH,
# AppImage via appimage-run.
resolve_runner() {
  if [[ -n "${NEOMACS_BIN:-}" ]]; then
    RUNNER=("${NEOMACS_BIN}")
  elif command -v neomacs >/dev/null 2>&1; then
    RUNNER=(neomacs)
  elif [[ -x "${APPIMAGE}" ]]; then
    if command -v appimage-run >/dev/null 2>&1; then
      RUNNER=(appimage-run "${APPIMAGE}")
    else
      RUNNER=(nix run nixpkgs#appimage-run -- "${APPIMAGE}")
    fi
  else
    die "no Neomacs found. Run: $0 --fetch"
  fi
}

fetch_appimage() {
  local tag="${1:-v${NEOMACS_VERSION}}"
  command -v gh >/dev/null 2>&1 || die "gh CLI required for --fetch"
  mkdir -p "${APPIMAGE_DIR}"
  echo "[neomacs] downloading ${tag} AppImage to ${APPIMAGE_DIR}"
  ( cd "${APPIMAGE_DIR}" && \
    gh release download "${tag}" --repo eval-exec/neomacs \
       --pattern "*x86_64-unknown-linux-gnu.AppImage" --clobber )
  chmod +x "${APPIMAGE_DIR}"/neomacs-*.AppImage
  ls -lh "${APPIMAGE_DIR}"/neomacs-*.AppImage
}

# Run every probe in its own process. A probe that takes the runtime down
# must not prevent the remaining probes from reporting — that isolation is
# the point, since a hard crash is itself a finding.
run_probes() {
  local only="${1:-}"
  local -a probes
  if [[ -n "${only}" ]]; then
    probes=("${PROBE_DIR}/probe-${only}.el")
    [[ -f "${probes[0]}" ]] || die "no such probe: ${only}"
  else
    probes=("${PROBE_DIR}"/probe-env.el \
            "${PROBE_DIR}"/probe-org-korean.el \
            "${PROBE_DIR}"/probe-org-export.el \
            "${PROBE_DIR}"/probe-tls.el)
  fi

  # --batch does not load init files, so the profile is loaded explicitly.
  # Without this the probes measure the bare runtime and never touch the
  # config they are supposed to be validating.
  local failed=0
  for probe in "${probes[@]}"; do
    echo
    echo "########## $(basename "${probe}") ##########"
    if ! "${RUNNER[@]}" --batch \
           --init-directory "${PROFILE_DIR}" \
           -l "${PROFILE_DIR}/init.el" \
           -l "${probe}"; then
      failed=$((failed + 1))
      echo "[neomacs] ${probe##*/} exited non-zero (rc=$?)"
    fi
  done

  echo
  if (( failed > 0 )); then
    echo "[neomacs] ${failed} probe file(s) reported failures"
    return 1
  fi
  echo "[neomacs] all probe files passed"
}

case "${1:-}" in
  --fetch)
    fetch_appimage "${2:-}"
    exit 0
    ;;
  --gnu)
    # Same profile, stock GNU Emacs — the divergence baseline.
    shift
    RUNNER=(emacs)
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}" "$@"
    ;;
esac

resolve_runner

case "${1:-}" in
  --version)
    exec "${RUNNER[@]}" --version
    ;;
  --probe)
    run_probes "${2:-}"
    ;;
  --nw|--tty)
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}" -nw
    ;;
  --daemon)
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}" \
         --daemon="${SERVER_NAME}"
    ;;
  --kill)
    "${RUNNER[@]}" --batch --eval \
      "(progn (require 'server) (server-eval-at \"${SERVER_NAME}\" '(kill-emacs)))" \
      2>/dev/null || true
    echo "[neomacs] daemon ${SERVER_NAME} stopped (if it was running)"
    ;;
  --debug)
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}" --debug-init
    ;;
  "")
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}"
    ;;
  *)
    exec "${RUNNER[@]}" --init-directory "${PROFILE_DIR}" "$@"
    ;;
esac
