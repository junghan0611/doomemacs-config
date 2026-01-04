#!/usr/bin/env bash
# build-steveyegge-tools.sh
# Steve Yegge's AI agent tools (Beads, VC, Gas Town) build script for NixOS

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

BEADS_DIR="${HOME}/repos/3rd/beads"
VC_DIR="${HOME}/repos/3rd/vc"
GASTOWN_DIR="${HOME}/repos/3rd/gastown"
INSTALL_DIR="${HOME}/.local/bin"

GO_VERSION="1.24"
GO_CMD="go"

echo -e "${YELLOW}=== Steve Yegge AI Tools Builder (NixOS) ===${NC}"
echo ""

mkdir -p "${INSTALL_DIR}"

ensure_go() {
    if command -v go &> /dev/null; then
        GO_CMD="go"
        local current_version
        current_version=$(go version 2>/dev/null | grep -oP 'go\K[0-9]+\.[0-9]+' || echo "0")
        echo -e "${GREEN}✓ Go found: ${current_version}${NC}"
    elif command -v nix-shell &> /dev/null; then
        echo -e "${YELLOW}Go not found, using nix-shell...${NC}"
        GO_CMD="nix-shell -p go --run"
    else
        echo -e "${RED}Error: Go not found and nix-shell not available${NC}"
        exit 1
    fi
}

run_go() {
    if [[ "$GO_CMD" == "go" ]]; then
        go "$@"
    else
        nix-shell -p go --run "go $*"
    fi
}

clone_or_update() {
    local name="$1"
    local src_dir="$2"

    if [ -d "${src_dir}/.git" ]; then
        echo -e "${YELLOW}Updating ${name} (git pull)...${NC}"
        git -C "${src_dir}" pull --rebase --autostash || true
        echo -e "${GREEN}✓ Updated ${name}${NC}"
    else
        echo -e "${YELLOW}Cloning ${name}...${NC}"
        git clone "git@github.com:steveyegge/${name}.git" "${src_dir}"
        echo -e "${GREEN}✓ Cloned ${name}${NC}"
    fi
}

fix_vc_gomod() {
    local gomod="${VC_DIR}/go.mod"
    if [ -f "${gomod}" ] && grep -q "/Users/stevey/src/beads" "${gomod}"; then
        echo -e "${YELLOW}Fixing vc/go.mod replace directive...${NC}"
        sed -i 's|=> /Users/stevey/src/beads|=> '"${BEADS_DIR}"'|g' "${gomod}"
        echo -e "${GREEN}✓ Updated vc go.mod${NC}"
    fi
}

fix_gastown_gomod() {
    local gomod="${GASTOWN_DIR}/go.mod"
    if [ -f "${gomod}" ] && grep -q "/Users/stevey/src/beads" "${gomod}"; then
        echo -e "${YELLOW}Fixing gastown/go.mod replace directive...${NC}"
        sed -i 's|=> /Users/stevey/src/beads|=> '"${BEADS_DIR}"'|g' "${gomod}"
        echo -e "${GREEN}✓ Updated gastown go.mod${NC}"
    fi
}

ask() {
    local prompt="$1"
    while true; do
        read -rp "${prompt} [y/N]: " ans
        case "$ans" in
            [Yy]*) return 0 ;;
            [Nn]*|"") return 1 ;;
        esac
    done
}

build_project() {
    local name="$1"
    local src_dir="$2"
    local binary="$3"

    echo ""
    if ! ask "Install/update ${name}?"; then
        echo -e "${YELLOW}Skipping ${name}${NC}"
        return 0
    fi

    echo -e "${YELLOW}Preparing ${name}...${NC}"
    clone_or_update "${name}" "${src_dir}"

    cd "${src_dir}"
    echo "  Running go mod tidy..."
    run_go mod tidy 2>&1 | grep -v "^go: downloading" || true

    echo "  Building..."
    run_go build -o "${binary}" "./cmd/${binary}"

    if [ -f "${binary}" ]; then
        cp -f "${binary}" "${INSTALL_DIR}/"
        rm "${binary}"
        echo -e "${GREEN}✓ ${name} installed to ${INSTALL_DIR}/${binary}${NC}"
    else
        echo -e "${RED}✗ ${name} build failed${NC}"
        return 1
    fi
}

ensure_go

# beads
build_project "beads" "${BEADS_DIR}" "bd"

# vc
clone_or_update "vc" "${VC_DIR}"
fix_vc_gomod
build_project "vc" "${VC_DIR}" "vc"

# gastown
clone_or_update "gastown" "${GASTOWN_DIR}"
fix_gastown_gomod
build_project "gastown" "${GASTOWN_DIR}" "gt"

echo ""
echo -e "${GREEN}=== Done ===${NC}"
