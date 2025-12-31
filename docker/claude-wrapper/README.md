# Claude Code OpenAI Wrapper

Claude Code 정액제를 gptel에서 사용하기 위한 Docker 설정.

## 사전 준비

1. Claude Code CLI 인증 완료 (`~/.claude/` 존재)
2. Docker 설치

## 사용법

```bash
# 최초 실행 (자동: git clone → docker build → symlink 생성)
bash ~/sync/emacs/doomemacs-config/docker/claude-wrapper/run.sh

# 이후 어디서든 사용 가능
run-claude-wrapper              # 서비스 시작
run-claude-wrapper --update     # 최신 코드로 재빌드 후 시작
run-claude-wrapper --stop       # 서비스 중지
run-claude-wrapper --status     # 상태 확인
run-claude-wrapper --help       # 도움말
```

## gptel 자동 백엔드 선택

`lisp/ai-gptel.el`에서 Emacs 시작 시 자동으로 백엔드 선택:

- **서버 실행 중** → `Claude-Code` 백엔드 사용
- **서버 없음** → `OpenRouter` 백엔드 사용

### 수동 전환

```elisp
M-x gptel-switch-to-claude-code   ; Claude-Code로 전환
M-x gptel-switch-to-openrouter    ; OpenRouter로 전환
```

## 모델

| 모델 | 설명 | 가격 (in/out) |
|------|------|---------------|
| claude-sonnet-4-5-20250929 | 추천 (코딩/추론) | $3/$15 |
| claude-opus-4-5-20251101 | **최신 Opus** - 최고 성능 | $5/$25 |
| claude-haiku-4-5-20251001 | 빠름/저렴 | $1/$5 |

## 지원 기능

| 기능 | 상태 | 설명 |
|------|------|------|
| 파일 접근 | ✅ | Read, Write, Edit, Glob, Grep |
| 명령 실행 | ✅ | Bash |
| 웹검색 | ✅ | WebSearch, WebFetch |
| 시스템 프롬프트 | ✅ | gptel 시스템 메시지 전달 |
| 도구 결과 | ✅ | 실행 결과가 응답에 포함 |

## 환경변수

| 변수 | 기본값 | 설명 |
|------|--------|------|
| MAX_TIMEOUT | 300000 | 요청 타임아웃 (ms) |
| DEFAULT_MODEL | claude-sonnet-4-5-20250929 | 기본 모델 |
| RATE_LIMIT_ENABLED | false | Rate limiting |

## 참고

- https://github.com/junghan0611/claude-code-openai-wrapper (포크)
- https://github.com/RichardAtCT/claude-code-openai-wrapper (원본)
- https://github.com/karthink/gptel/issues/1175
