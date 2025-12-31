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
| claude-opus-4-5-20251101 | 최고 성능 | $5/$25 |
| claude-haiku-4-5-20251001 | 빠름/저렴 | $1/$5 |

## 제한 사항

- **웹검색 미지원**: Claude Code wrapper는 코딩 도구만 사용 가능
- 웹검색이 필요하면 OpenRouter 백엔드 사용

## 참고

- https://github.com/RichardAtCT/claude-code-openai-wrapper
- https://github.com/karthink/gptel/issues/1175
