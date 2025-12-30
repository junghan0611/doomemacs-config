# Claude Code OpenAI Wrapper

Claude Code 정액제를 gptel에서 사용하기 위한 Docker 설정.

## 사전 준비

1. Claude Code CLI 인증 완료 (`~/.claude/` 존재)
2. Docker 설치

## 사용법

```bash
# 1. 이미지 빌드 (최초 1회)
cd ~/repos/3rd/claude-code-openai-wrapper
docker build -t claude-wrapper:latest .

# 2. 서비스 시작
cd ~/sync/emacs/doomemacs-config/docker/claude-wrapper
docker-compose up -d

# 3. 상태 확인
curl http://localhost:8000/health

# 4. 서비스 중지
docker-compose down
```

## gptel 설정

`lisp/ai-gptel.el`에 이미 설정됨:

```elisp
(gptel-make-openai "Claude-Code"
  :host "localhost:8000"
  :endpoint "/v1/chat/completions"
  :stream t
  :models '(claude-sonnet-4-5-20250929
            claude-opus-4-5-20250929
            claude-haiku-4-5-20251001))
```

## 모델

| 모델 | 설명 | 가격 |
|------|------|------|
| claude-opus-4-5-20251101 | 최고 성능 | $5/$25 |
| claude-sonnet-4-5-20250929 | 추천 (코딩/추론) | $3/$15 |
| claude-haiku-4-5-20251001 | 빠름/저렴 | $1/$5 |

## 참고

- https://github.com/RichardAtCT/claude-code-openai-wrapper
- https://github.com/karthink/gptel/issues/1175
