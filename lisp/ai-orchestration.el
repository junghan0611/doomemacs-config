;;; $DOOMDIR/lisp/ai-orchestration.el --- AI Orchestration Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; AI 오케스트레이션 도구 설정 (Steve Yegge 생태계)
;; - Efrit: Emacs Control Plane (site-lisp 조건부 로딩)
;; - Beads: 이슈 트래킹 (straight 패키지)
;;
;; 아키텍처:
;;   Control Plane (Emacs/Efrit) ←→ Execution Plane (Claude Code 등)
;;
;; 버전: steveyegge/efrit main 브랜치 (2025-12-22 기준)

;;; Code:

;;;; Efrit (Steve Yegge's AI Assistant)

;; AI-powered autonomous Emacs assistant
;; - efrit-chat: Multi-turn conversational interface
;; - efrit-do: Natural language command execution
;; - efrit-agent: Structured agent session buffer
;; - efrit-remote-queue: AI agent communication channel
;;
;; Note: packages.el에서 조건부로 package! 선언됨
;;       → 디렉터리 없으면 이 블록 전체 스킵됨

; (progn
; (defvar efrit-directory "~/.emacs.d/site-lisp/efrit/")
;
; (when (file-directory-p efrit-directory)
;
; ;;;;; Core Loading (v0.4.1 구조)
;   (let ((efrit-lisp (expand-file-name "lisp" efrit-directory)))
;     (add-to-list 'load-path efrit-lisp)
;     (add-to-list 'load-path (expand-file-name "core" efrit-lisp))
;     (add-to-list 'load-path (expand-file-name "interfaces" efrit-lisp))
;     (add-to-list 'load-path (expand-file-name "support" efrit-lisp))
;     (add-to-list 'load-path (expand-file-name "tools" efrit-lisp)))
;
;   ;; 데이터 디렉토리 (로드 전에 설정)
;   (setq efrit-data-directory (expand-file-name ".efrit" doom-user-dir))
;
;   (require 'efrit)
;   (require 'efrit-common)
;
; ;;;;; Model Configuration (Anthropic)
;
;   ;; 사용 가능한 모델:
;   ;; - claude-sonnet-4-5-20250929 (기본값, 빠름)
;   ;; - claude-opus-4-5-20251101 (더 강력함)
;   (setq efrit-default-model "claude-sonnet-4-5-20250929")
;   (setq efrit-default-max-tokens 8192)
;
; ;;;;; Session Safety Limits
;
;   (setq efrit-max-retries 3)
;   (setq efrit-max-tool-calls-per-session 100)
;   (setq efrit-max-continuations-per-session 50)
;   (setq efrit-session-timeout 300)  ; 5분
;
; ;;;;; API Key
;
;   ;; 우선순위:
;   ;; 1. efrit-api-key 변수
;   ;; 2. ANTHROPIC_API_KEY 환경변수
;   ;; 3. authinfo: machine api.anthropic.com login personal password sk-ant-xxxxx
;
;   ;; 환경변수가 이미 설정되어 있으면 efrit-common-get-api-key가 자동으로 사용
;   ;; authinfo에서 로드하려면:
;   (unless (getenv "ANTHROPIC_API_KEY")
;     (require 'auth-source)
;     (when-let* ((found (car (auth-source-search :host "api.anthropic.com"
;                                                  :user "personal"
;                                                  :max 1)))
;                 (secret (plist-get found :secret))
;                 (key (if (functionp secret) (funcall secret) secret)))
;       (setenv "ANTHROPIC_API_KEY" key)))
;
; ;;;;; Keybindings
;
;   (map! :leader
;         (:prefix ("\\" . "efrit/beads")
;          :desc "Efrit REPL" "e" #'efrit
;          :desc "Chat" "c" #'efrit-chat
;          :desc "Do (async)" "d" #'efrit-do
;          :desc "Do (sync)" "D" #'efrit-do-sync
;          :desc "Agent buffer" "a" #'efrit-agent
;          :desc "Remote queue" "q" #'efrit-remote-queue-start
;          :desc "Queue status" "Q" #'efrit-remote-queue-status
;          :desc "Doctor" "?" #'efrit-doctor))
;   )
; )

;;;; Beads (Issue Tracker)

;; Git-synced issue tracking for AI agents
;; - bd CLI와 연동
;; - Efrit 내부 beads tools 사용

(use-package! beads)

(provide 'ai-orchestration)
;;; ai-orchestration.el ends here
