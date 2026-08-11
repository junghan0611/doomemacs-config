;;; $DOOMDIR/lisp/ai-pimacs.el --- Pimacs — one-buffer Pi client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; pimacs.el (https://github.com/ananthakumaran/pimacs.el) — Pi 프론트엔드.
;; ai-pi-agent.el 의 pi-coding-agent 와 같은 `pi --mode rpc' 백엔드를 쓰되,
;; UI 모델이 다르다. 둘은 공존한다 — 어느 손맛이 맞는지 재는 중이다.
;;
;;   pi-coding-agent   2 버퍼 (chat + 별도 입력 버퍼).  evil 전용 모듈 있음.
;;   pimacs            1 버퍼 (읽기전용 chat + 하단 프롬프트 위젯).
;;                     magit 식 섹션 접기.  evil 지원 없음.
;;
;; 평가 단계 (2026-08-11):
;;   1. entwurf 없이 기본 기능       ← 지금 여기
;;   2. 키바인딩 손보기 (필요하면)
;;   3. entwurf 붙이기 — `pimacs-flags' 참고
;;
;; 알아둘 것:
;; - `/login' 은 RPC 에 노출되지 않아 pimacs 에서 안 된다 (pi issue #885).
;;   인증은 터미널에서 `pi' 를 한 번 띄워 끝내 둔다.
;; - pimacs 는 pi 0.82.0+ 를 요구하고 기동 시 버전을 검사한다.
;; - pi-coding-agent 와 달리 `auto-mode-alist' / `treesit-major-mode-remap-alist'
;;   를 건드리지 않는다. md-ts-mode 가 markdown-mode 를 가로채던 사고
;;   (ai-pi-agent.el 의 WORKAROUND) 는 여기서 재발하지 않는다.

;;; Code:

;;;; 모델 SSOT

(defvar my/pimacs-models
  '("xai/grok-4.5"
    "openai-codex/gpt-5.6-terra"
    "openai-codex/gpt-5.6-sol"
    ;; "openai-codex/gpt-5.6-luna"
    ;; "kimi-coding/k3"
    )
  "Pimacs 세션을 시작할 때 고를 모델 목록. 모델 추가는 이 한 줄에서만 한다.
각 항목은 pi 의 `--model' 패턴이다 — \"provider/id\", 뒤에 `:<thinking>' 선택.
인증된 프로바이더(xai / openai-codex / kimi-coding) 기준으로 실측 확인했다
\(2026-08-11, RPC `get_state' 왕복).")

;;;; 기본 설정

(use-package! pimacs
  :commands (pimacs-chat pimacs-doctor pimacs-list-sessions pimacs-switch-session)
  :custom
  ;; tree-sitter 렌더러는 opt-in 이다. markdown/markdown-inline 그래머는
  ;; prog-mode-config.el 의 `treesit-auto-install-grammar' 로 이미 붙어 있다.
  ;; 렌더가 이상하면 이 줄만 지우면 markdown-mode 기반 기본 렌더러로 돌아간다.
  (pimacs-markdown-renderer #'pimacs--render-markdown)
  :config
  ;; 단계 3 에서 열 자리. entwurf 를 붙일 때 pi 로 넘길 플래그를 여기에 둔다.
  ;; ai-pi-agent.el 의 `pi-coding-agent-extra-args' 와 같은 역할.
  ;;   (setq pimacs-flags '("--entwurf-control" "--emacs-agent-socket" "pimacs"))
  ;; 주의: entwurf 의 mux 레일은 TMUX/TMUX_PANE 상속을 요구한다. Emacs 가
  ;; tmux 밖에서 떴으면 형제를 *새로 여는* 부름(fresh/launch)은 거부된다.
  ;; 확장 로드와 이미 살아 있는 형제와의 대화는 영향받지 않는다.

  (add-hook 'pimacs-chat-mode-hook #'doom-mark-buffer-as-real-h)
  ;; 스트리밍 렌더 비용이 큰 버퍼라 line numbers 를 끈다 (pi-coding-agent 와 동일).
  (add-hook 'pimacs-chat-mode-hook (lambda () (display-line-numbers-mode -1)))

  ;; `--models' 는 pi 의 순환 목록이다. 이걸 넘겨 둬야 세션 안에서
  ;; `pimacs-cycle-model' 이 쓸모를 갖는다.
  (setq pimacs-flags
        (append pimacs-flags
                (list "--models" (string-join my/pimacs-models ",")))))

;;;; 모델 선택
;;
;; pimacs 는 세션을 *시작할 때* 모델을 못 고른다. `pimacs-chat' 은 name/root 만
;; 받고(C-u 트랜지언트도 그 둘뿐), 모델은 pi 의 기본값(~/.pi/agent/settings.json
;; 의 defaultProvider/defaultModel)으로 뜬다. 세션 안에서는 `pimacs-select-model'
;; (전체 카탈로그 조회) / `pimacs-cycle-model' (--models 순환)로 바꿀 수 있다.
;;
;; 시작 시점에 고르려면 pi 에 `--model' 을 넘기는 수밖에 없는데, `pimacs-flags'
;; 는 전역 defcustom 이다. 다행히 `pimacs--start-agent' 가 `pimacs-chat--create'
;; 안에서 동기로 불리므로 let-binding 이 그대로 먹는다.

(defun my/pimacs-chat-with-model (model &optional name)
  "MODEL 을 지정해 Pimacs 세션을 시작한다.
접두 인자를 주면 세션 이름도 묻는다.

주의 두 가지:
- 모델은 pi 프로세스가 *새로 뜰 때만* 적용된다. 같은 root+name 의 에이전트가
  이미 살아 있으면 `pimacs-chat--create' 가 기동을 건너뛰므로 MODEL 은 무시된다.
  그럴 때는 세션 안에서 `pimacs-select-model' 을 쓴다.
- 세션 키는 root 와 name 으로 갈린다. 같은 프로젝트에서 모델이 다른 두 세션을
  띄우려면 서로 다른 NAME 을 줘야 한다."
  (interactive
   (list (completing-read "Pimacs model: " my/pimacs-models nil nil)
         (when current-prefix-arg
           (read-string "Session name: "))))
  ;; `:commands' lazy-load 경로라 `pimacs-chat' 호출 전에는 패키지가 안 뜬다.
  ;; let 이 `pimacs-flags' 를 먼저 읽으므로 require 를 앞에 둔다 — 안 그러면
  ;; void-variable. require 가 :config 도 끌어 `--models' 가 이미 붙은 상태다.
  (require 'pimacs)
  (let ((pimacs-flags (append pimacs-flags (list "--model" model)))
        ;; `pimacs-chat' 은 `current-prefix-arg' 를 보고 트랜지언트를 띄운다.
        ;; 여기서는 접두 인자를 이름 묻기에 이미 썼으므로 가린다.
        (current-prefix-arg nil))
    (pimacs-chat name)))

;;;; 키바인딩
;;
;; SPC j 는 이미 붐빈다. 점유 현황 (2026-08-11):
;;   j f l q  ai-pi-agent.el      pi-coding-agent
;;   p        present-config.el   *prefix* — 발표 (j p p/q/s/a)
;;   t T M-t  ai-bot-config.el    telega
;;   z a      term-config.el      zmx
;; `p' 는 명령이 아니라 prefix 라서 그 자리에 명령을 박으면 present-config 의
;; `j p p' 바인딩이 "non-prefix key" 로 죽는다. m/M/d 로 간다.
;; prefix 에 이름을 달지 않는다 — AGENTS.md § map! prefix 규약.

(map! :leader
      (:prefix "j"
       :desc "Pimacs chat w/ model" "m" #'my/pimacs-chat-with-model
       :desc "Pimacs sessions"      "M" #'pimacs-list-sessions
       ;; :desc "Pimacs chat"          "n" #'pimacs-chat
       ;; :desc "Pimacs doctor"        "d" #'pimacs-doctor
       ))

;;;; 조작 메모
;;
;; chat 버퍼:  TAB 섹션 토글 / S-TAB 전역 순환 / 1 2 3 · M-1 M-2 M-3 레벨
;;             n p 섹션 이동 / N P 유저 메시지 / l 최신 / i 프롬프트 / q 종료
;; 프롬프트:   RET 전송 / C-j 개행 / M-p M-n 히스토리 / C-r 히스토리 검색
;;             M-RET 대체 스트리밍 / TAB 파일·슬래시 완성
;;
;; evil 주의: 프롬프트가 `widget.el' 의 editable-field 라 위젯 키맵이
;; 텍스트 프로퍼티로 깔린다. 그 우선순위가 evil 상태 맵보다 위다. chat 버퍼의
;; 단일키(i n p q 1 2 3)도 evil normal 과 정면으로 겹친다. 단계 2 에서 볼 것.
;;
;; 긴 한국어 프롬프트에 RET=전송이 마찰이면 위젯 키맵 한 줄로 바꾼다:
;;   (keymap-set pimacs-chat-widget-field-keymap "RET" #'newline)
;;   (keymap-set pimacs-chat-widget-field-keymap "C-c C-c" #'widget-field-activate)

;;; Provide

(provide 'ai-pimacs)

;;; ai-pimacs.el ends here
