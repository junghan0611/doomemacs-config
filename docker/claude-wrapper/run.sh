#!/usr/bin/env bash
# Claude Code OpenAI Wrapper - Cross-platform runner (NixOS/Ubuntu)
# Usage: run.sh [--update] [--stop] [--status]

set -e

IMAGE_NAME="claude-wrapper:latest"
CONTAINER_NAME="claude-wrapper-container"
# Use forked version with Doom Emacs/gptel integration
SOURCE_DIR="$HOME/repos/gh/claude-code-openai-wrapper"
REPO_URL="https://github.com/junghan0611/claude-code-openai-wrapper.git"
SCRIPT_PATH="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
SYMLINK_PATH="$HOME/.local/bin/run-claude-wrapper"

# 색상 출력
info() { echo -e "\033[1;34m[INFO]\033[0m $1"; }
warn() { echo -e "\033[1;33m[WARN]\033[0m $1"; }
error() { echo -e "\033[1;31m[ERROR]\033[0m $1"; exit 1; }

# 사용법
usage() {
    echo "Usage: $(basename "$0") [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --update    git pull 후 Docker 이미지 재빌드"
    echo "  --stop      컨테이너 중지 및 제거"
    echo "  --status    컨테이너 상태 확인"
    echo "  --help      이 도움말 표시"
    echo ""
    echo "Environment Variables:"
    echo "  MAX_TIMEOUT              요청 타임아웃 ms (default: 300000 = 5분)"
    echo "  DEFAULT_MODEL            기본 모델 (default: claude-sonnet-4-5-20250929)"
    echo "  RATE_LIMIT_ENABLED       Rate limiting (default: false)"
    echo "  CLAUDE_INDEPENDENT_MODE  MCP/플러그인 비활성화로 빠른 시작 (default: true)"
    echo "  CLAUDE_MINIMAL_TOOLS     최소 도구셋으로 빠른 응답 (default: true)"
    echo ""
    echo "Examples:"
    echo "  MAX_TIMEOUT=600000 run-claude-wrapper  # 10분 타임아웃"
    echo "  DEFAULT_MODEL=claude-haiku-4-5-20251001 run-claude-wrapper  # 빠른 모델"
    exit 0
}

# 심볼릭 링크 설정
setup_symlink() {
    if [ ! -L "$SYMLINK_PATH" ] || [ "$(readlink -f "$SYMLINK_PATH")" != "$SCRIPT_PATH" ]; then
        mkdir -p "$HOME/.local/bin"
        ln -sf "$SCRIPT_PATH" "$SYMLINK_PATH"
        info "심볼릭 링크 생성: $SYMLINK_PATH -> $SCRIPT_PATH"

        # PATH 확인
        if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
            warn "~/.local/bin이 PATH에 없습니다. 셸 설정에 추가하세요:"
            echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
        fi
    fi
}

# 컨테이너 중지
stop_container() {
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        info "컨테이너 중지 및 제거 중..."
        docker rm -f "$CONTAINER_NAME" >/dev/null
        info "완료"
    else
        info "실행 중인 컨테이너가 없습니다"
    fi
    exit 0
}

# 상태 확인
check_status() {
    echo "=== Container Status ==="
    if docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker ps --filter "name=$CONTAINER_NAME" --format "table {{.Status}}\t{{.Ports}}"
        echo ""
        echo "Health check:"
        curl -s http://localhost:8000/health 2>/dev/null || echo "응답 없음"
    else
        echo "컨테이너가 실행 중이 아닙니다"
    fi
    exit 0
}

# 소스 업데이트 및 이미지 재빌드
update_and_rebuild() {
    info "소스 업데이트 중..."

    if [ ! -d "$SOURCE_DIR" ]; then
        info "소스 디렉토리가 없습니다. Git clone 중..."
        mkdir -p "$(dirname "$SOURCE_DIR")"
        git clone -b ko "$REPO_URL" "$SOURCE_DIR"
    else
        cd "$SOURCE_DIR"
        git fetch origin
        LOCAL=$(git rev-parse HEAD)
        # Use ko branch for forked version
        REMOTE=$(git rev-parse origin/ko 2>/dev/null || git rev-parse origin/main 2>/dev/null || git rev-parse origin/master)

        if [ "$LOCAL" != "$REMOTE" ]; then
            info "새 커밋 발견. pull 중..."
            git pull origin ko
        else
            info "이미 최신 버전입니다"
        fi
    fi

    info "Docker 이미지 재빌드 중... (시간이 걸릴 수 있습니다)"
    docker build --no-cache -t "$IMAGE_NAME" "$SOURCE_DIR"
    info "빌드 완료: $IMAGE_NAME"
}

# 메인 로직
main() {
    local do_update=false

    # 인자 파싱
    while [[ $# -gt 0 ]]; do
        case $1 in
            --update) do_update=true; shift ;;
            --stop) stop_container ;;
            --status) check_status ;;
            --help|-h) usage ;;
            *) error "알 수 없는 옵션: $1" ;;
        esac
    done

    # 1. Docker 확인
    command -v docker >/dev/null 2>&1 || error "Docker가 설치되어 있지 않습니다"

    # 2. Claude 인증 확인
    [ -d "$HOME/.claude" ] || error "~/.claude 디렉토리가 없습니다. Claude Code CLI 인증을 먼저 완료하세요"

    # 3. 심볼릭 링크 설정
    setup_symlink

    # 4. 업데이트 요청 시 rebuild
    if [ "$do_update" = true ]; then
        update_and_rebuild
    else
        # 소스 디렉토리 없으면 clone (ko branch)
        if [ ! -d "$SOURCE_DIR" ]; then
            info "소스 디렉토리가 없습니다. Git clone 중..."
            mkdir -p "$(dirname "$SOURCE_DIR")"
            git clone -b ko "$REPO_URL" "$SOURCE_DIR"
            info "Clone 완료: $SOURCE_DIR (ko branch)"
        fi

        # 이미지 없으면 빌드
        if ! docker image inspect "$IMAGE_NAME" >/dev/null 2>&1; then
            info "Docker 이미지가 없습니다. 빌드 중..."
            docker build -t "$IMAGE_NAME" "$SOURCE_DIR"
            info "빌드 완료: $IMAGE_NAME"
        fi
    fi

    # 5. 기존 컨테이너 정리
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        info "기존 컨테이너 제거 중..."
        docker rm -f "$CONTAINER_NAME" >/dev/null
    fi

    # 6. 컨테이너 실행
    info "컨테이너 시작 중..."
    docker run -d -p 8000:8000 \
      -v "$HOME/.claude:/root/.claude" \
      -v "$HOME/org:/workspace" \
      -e CLAUDE_CWD=/workspace \
      -e MAX_TIMEOUT="${MAX_TIMEOUT:-300000}" \
      -e DEFAULT_MODEL="${DEFAULT_MODEL:-claude-sonnet-4-5-20250929}" \
      -e RATE_LIMIT_ENABLED="${RATE_LIMIT_ENABLED:-false}" \
      -e CLAUDE_INDEPENDENT_MODE="${CLAUDE_INDEPENDENT_MODE:-true}" \
      -e CLAUDE_MINIMAL_TOOLS="${CLAUDE_MINIMAL_TOOLS:-true}" \
      --name "$CONTAINER_NAME" \
      "$IMAGE_NAME"

    info "실행 완료! http://localhost:8000/health 로 확인하세요"
    info "중지: run-claude-wrapper --stop"
}

main "$@"
