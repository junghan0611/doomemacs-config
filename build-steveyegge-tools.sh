#!/usr/bin/env bash
# build-steveyegge-tools.sh
# Steve Yegge's AI agent tools (Beads, VC, Gas Town) build script for NixOS
# Usage: ./build-steveyegge-tools.sh
#
# Note: vc depends on beads, so beads must be built first.
#       vc/go.mod uses local replace directive pointing to beads.
#       gastown also depends on beads (import path).
#       Repos will be auto-cloned from junghan0611 fork if not present.

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Paths
BEADS_DIR="${HOME}/repos/3rd/beads"
VC_DIR="${HOME}/repos/3rd/vc"
GASTOWN_DIR="${HOME}/repos/3rd/gastown"
INSTALL_DIR="${HOME}/.local/bin"

# Go version requirement
GO_VERSION="1.24"

# Go command - will be set by ensure_go()
GO_CMD="go"

echo -e "${YELLOW}=== Steve Yegge AI Tools Builder (NixOS) ===${NC}"
echo ""

# Ensure install directory exists
mkdir -p "${INSTALL_DIR}"

# Ensure Go is available (NixOS compatible)
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
        echo "Install Go or use NixOS/nix-shell"
        exit 1
    fi
}

# Run go command (handles both direct and nix-shell modes)
run_go() {
    if [[ "$GO_CMD" == "go" ]]; then
        go "$@"
    else
        nix-shell -p go --run "go $*"
    fi
}

# Fix vc go.mod replace directive for local environment
fix_vc_gomod() {
    local gomod="${VC_DIR}/go.mod"

    if [ -f "${gomod}" ]; then
        # Check if it points to Steve's Mac path
        if grep -q "/Users/stevey/src/beads" "${gomod}"; then
            echo -e "${YELLOW}Fixing vc/go.mod replace directive...${NC}"
            sed -i 's|=> /Users/stevey/src/beads|=> '"${BEADS_DIR}"'|g' "${gomod}"
            echo -e "${GREEN}✓ Updated replace directive to ${BEADS_DIR}${NC}"
        fi
    fi
}

# Fix gastown go.mod replace directive for local environment
fix_gastown_gomod() {
    local gomod="${GASTOWN_DIR}/go.mod"

    if [ -f "${gomod}" ]; then
        # Check if it points to Steve's Mac path
        if grep -q "/Users/stevey/src/beads" "${gomod}"; then
            echo -e "${YELLOW}Fixing gastown/go.mod replace directive...${NC}"
            sed -i 's|=> /Users/stevey/src/beads|=> '"${BEADS_DIR}"'|g' "${gomod}"
            echo -e "${GREEN}✓ Updated replace directive to ${BEADS_DIR}${NC}"
        fi
    fi
}

# Function to clone repo if not exists
clone_if_needed() {
    local name="$1"
    local src_dir="$2"
    
    if [ ! -d "${src_dir}" ]; then
        echo -e "${YELLOW}Cloning ${name}...${NC}"
        git clone "git@github.com:junghan0611/${name}.git" "${src_dir}"
        echo -e "${GREEN}✓ Cloned ${name} to ${src_dir}${NC}"
    fi
}

# Function to build a Go project
build_project() {
    local name="$1"
    local src_dir="$2"
    local binary="$3"
    
    echo -e "${YELLOW}Building ${name}...${NC}"
    
    # Clone if not exists
    clone_if_needed "${name}" "${src_dir}"
    
    cd "${src_dir}"
    
    # Run go mod tidy first to ensure dependencies are correct
    echo "  Running go mod tidy..."
    run_go mod tidy 2>&1 | grep -v "^go: downloading" || true
    
    # Build
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

# Ensure Go is available
ensure_go

echo ""

# Build Beads (bd) first - vc depends on it
build_project "beads" "${BEADS_DIR}" "bd"

echo ""

# Clone vc if needed, then fix go.mod, then build
clone_if_needed "vc" "${VC_DIR}"
fix_vc_gomod

# Build VC
build_project "vc" "${VC_DIR}" "vc"

echo ""

# Clone gastown if needed, then fix go.mod, then build
clone_if_needed "gastown" "${GASTOWN_DIR}"
fix_gastown_gomod

# Build Gas Town
build_project "gastown" "${GASTOWN_DIR}" "gt"

echo ""
echo -e "${YELLOW}=== Verifying installations ===${NC}"

# Verify bd
if command -v bd &> /dev/null; then
    echo -e "${GREEN}✓ bd:${NC} $(bd version 2>&1 | head -1)"
else
    echo -e "${RED}✗ bd not found in PATH${NC}"
    echo "  Add to PATH: export PATH=\"\${HOME}/.local/bin:\${PATH}\""
fi

# Verify vc
if command -v vc &> /dev/null; then
    # vc doesn't have version command, check with --help
    echo -e "${GREEN}✓ vc:${NC} VibeCoder v2 ($(vc --help 2>&1 | head -1))"
else
    echo -e "${RED}✗ vc not found in PATH${NC}"
    echo "  Add to PATH: export PATH=\"\${HOME}/.local/bin:\${PATH}\""
fi

# Verify gt
if command -v gt &> /dev/null; then
    echo -e "${GREEN}✓ gt:${NC} Gas Town ($(gt version 2>&1 | head -1 || echo 'installed'))"
else
    echo -e "${RED}✗ gt not found in PATH${NC}"
    echo "  Add to PATH: export PATH=\"\${HOME}/.local/bin:\${PATH}\""
fi

echo ""
echo -e "${GREEN}=== Done ===${NC}"
echo ""
echo "Quick start (Beads):"
echo "  cd your-project"
echo "  bd init --quiet"
echo "  bd create \"First task\" -t task -p 2"
echo "  bd ready"
echo ""
echo "Quick start (VibeCoder):"
echo "  cd your-project"
echo "  vc init"
echo "  vc doctor"
echo "  vc create \"First issue\""
echo "  vc list"
echo ""
echo "Quick start (Gas Town):"
echo "  gt install ~/gt          # Create town workspace"
echo "  gt rig add myproject --remote=https://github.com/you/repo.git"
echo "  gt convoy create \"Feature X\" issue-123"
echo "  gt sling issue-123 myproject"
echo "  gt convoy list           # Track progress"
