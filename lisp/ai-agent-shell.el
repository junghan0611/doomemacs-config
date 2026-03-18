;;; $DOOMDIR/lisp/ai-agent-shell.el --- Agent Shell Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Agent Shell (ACP - Agent Client Protocol) 설정
;; - agent-shell 0.48.1: Claude Code, Pi, Qwen 등 AI 에이전트 인터페이스
;; - agent-shell-manager: 버퍼 관리
;; - agent-shell-sidebar: 사이드바 UI
;;
;; 2026-03-18 업그레이드: viewport 모드, session resume, Pi ACP, usage tracking

;;; Code:

;;;; ACP (Agent Client Protocol)

;; https://agentclientprotocol.com/
;; https://github.com/xenodium/agent-shell/issues/27

(progn
  (require 'shell-maker)
  (require 'acp)
  (require 'agent-shell)

  ;; Ensure claude-agent-acp / pi-acp in exec-path for Termux
  ;; NOTE 2026-03-18: claude-code-acp → claude-agent-acp 리네임 완료
  (when IS-TERMUX
    (add-to-list 'exec-path "/data/data/com.termux/files/usr/bin"))

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  (unless (display-graphic-p)
    (setq agent-shell-header-style nil))

  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

;;;; Session Strategy — 세션 재개 지원

  ;; 'prompt: 시작 시 이전 세션 재개 or 새 세션 선택
  ;; 'new: 항상 새 세션 (bootstrapped, 모델/모드 변경 가능)
  ;; 'new-deferred: 기존 동작 (첫 프롬프트까지 초기화 지연)
  ;; 'latest: 항상 최근 세션 재개
  (setq agent-shell-session-strategy 'prompt)

  ;; session/resume 선호 (lightweight, 메시지 재생 없음)
  ;; session/load는 아직 edge case 있으므로 resume 기본
  (setq agent-shell-prefer-session-resume t)

;;;; Viewport Interaction — 제작자 추천 모드

  ;; viewport는 읽기(view-mode) + 작성(edit-mode)이 분리됨
  ;; view-mode: y=yes, m=more, a=again, 1-9=선택 — 빠른 응답
  ;; edit-mode: C-c C-c=전송, M-p/M-n=히스토리, @=파일완성, /=명령완성
  (setq agent-shell-prefer-viewport-interaction nil)

;;;; Usage Tracking — 토큰/비용 모니터링

  ;; 헤더에 컨텍스트 사용량 표시 (green→yellow→red)
  (setq agent-shell-show-context-usage-indicator 'detailed) ; "➤ 29k/200k (29%)"

  ;; 턴 종료 시 토큰/비용 요약 표시
  (setq agent-shell-show-usage-at-turn-end t)

;;;; Agent-Shell Manager

  (require 'agent-shell-manager)
  (setq agent-shell-manager-side 'bottom)  ; Options: 'left, 'right, 'top, 'bottom
  (map! :in "s-;" #'agent-shell-manager-toggle)
  (map! :map agent-shell-mode-map
        :i "RET" #'+default/newline-below
        :inv "M-RET" #'comint-send-input
        :inv "M-<return>" #'comint-send-input
        :in "C-RET" #'shell-maker-submit
        :in "C-<return>" #'shell-maker-submit

        :inv "M-\\" #'other-window
        :nv "DEL" 'evil-switch-to-windows-last-buffer ; Backspace
        :i "DEL" #'evil-delete-backward-char-and-join
        )

  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
	    (lambda ()
	      (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
		(evil-emacs-state))))

;;;; Evil + Viewport 공존

  ;; viewport-view-mode는 read-only — Evil emacs-state로 전환하여
  ;; y=yes, m=more, a=again, 1-9=선택이 Evil normal 키와 충돌 안 하게
  (add-hook 'agent-shell-viewport-view-mode-hook #'evil-emacs-state)

  ;; viewport-edit-mode는 텍스트 작성 — Evil insert-state로
  (add-hook 'agent-shell-viewport-edit-mode-hook #'evil-insert-state)

;;;; Sidebar

  (require 'agent-shell-sidebar)
  (setq agent-shell-sidebar-width "25%"
        agent-shell-sidebar-minimum-width 86
        agent-shell-sidebar-maximum-width "50%"
        agent-shell-sidebar-position 'right
        agent-shell-sidebar-locked t
        agent-shell-sidebar-default-config (agent-shell-anthropic-make-claude-code-config)
        )

;;;; Pi ACP — pi 스킬 + 시맨틱 메모리 연동

  ;; pi-acp: pi --mode rpc를 ACP JSON-RPC로 브릿지
  ;; pi 스킬 24개(denotecli, bibcli, gitcli 등) → ACP 세션에서 그대로 사용
  ;; pi 익스텐션 tool(session_search, knowledge_search) → pi가 로드하므로 동작 예상
  ;; pi 익스텐션 slash command → 미지원 (pi-acp 제한)
  (require 'agent-shell-pi)

;;;; Misc

  ;; agent-shell 버퍼를 실제 버퍼로 표시 (버퍼 목록에서 보이게)
  (add-hook 'agent-shell-mode-hook #'doom-mark-buffer-as-real-h)

  ;; 에이전트가 파일 쓸 때 aggressive-indent 등 비활성화
  (setq agent-shell-write-inhibit-minor-modes '(aggressive-indent-mode))
  )

;;;; meta-agent-shell

(use-package! meta-agent-shell
  :after agent-shell
  :commands (meta-agent-shell-start
             meta-agent-shell-start-dispatcher
             meta-agent-shell-jump-to-dispatcher
             meta-agent-shell-heartbeat-start
             meta-agent-shell-heartbeat-stop
             meta-agent-shell-heartbeat-send-now
             meta-agent-shell-big-red-button)
  :init
  (setq meta-agent-shell-heartbeat-file "~/org/meta-agent-heartbeat.org"
        meta-agent-shell-heartbeat-interval 900
        meta-agent-shell-heartbeat-cooldown 300
        meta-agent-shell-start-function #'agent-shell)
  :config
  (map! :leader
        (:prefix ("o m" . "meta-agent")
         :desc "Meta-agent session" "m" #'meta-agent-shell-start
         :desc "Project dispatcher" "d" #'meta-agent-shell-start-dispatcher
         :desc "Jump to dispatcher" "D" #'meta-agent-shell-jump-to-dispatcher
         :desc "Start heartbeat" "h" #'meta-agent-shell-heartbeat-start
         :desc "Stop heartbeat" "H" #'meta-agent-shell-heartbeat-stop
         :desc "Send heartbeat now" "s" #'meta-agent-shell-heartbeat-send-now
         :desc "STOP ALL AGENTS" "!" #'meta-agent-shell-big-red-button)))

;;; Provide

(provide 'ai-agent-shell)

;;; ai-agent-shell.el ends here
