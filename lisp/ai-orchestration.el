;;; $DOOMDIR/lisp/ai-orchestration.el --- AI Orchestration Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; AI 오케스트레이션 도구 설정 (Steve Yegge 생태계)
;; - Efrit: Emacs Control Plane (site-lisp 조건부 로딩)
;; - Beads: 이슈 트래킹 (straight 패키지)
;; - VC: VibeCoder CLI (별도 설치)
;;
;; 아키텍처:
;;   Control Plane (Emacs/Efrit) ←→ Execution Plane (Claude Code 등)
;;
;; 참고: docs/20251222T124500--efrit-architecture-analysis.org

;;; Code:

;;;; Efrit (Steve Yegge's AI Assistant)

;; AI-powered autonomous Emacs assistant
;; - efrit-chat: Multi-turn conversational interface
;; - efrit-do: Natural language command execution
;; - efrit-agent: Structured agent session buffer
;; - efrit-remote-queue: AI agent communication channel

(defvar efrit-directory "~/.emacs.d/site-lisp/efrit/")

(when (file-directory-p efrit-directory)

;;;;; Core Loading

  (add-to-list 'load-path (expand-file-name "lisp" efrit-directory))

  (require 'efrit)

;;;;; API Configuration (Anthropic)

  ;; API Key: ANTHROPIC_API_KEY 환경변수 또는 authinfo 사용
  (setq efrit-api-key
        (lambda ()
          (auth-info-password
           (car (auth-source-search :host "api.anthropic.com" :user "apikey")))))

  ;; Model selection
  ;; claude-sonnet-4-5-20250929
  ;; claude-opus-4-5-20251101
  (setq efrit-model "claude-sonnet-4-5-20250929")
  (setq efrit-max-tokens 8192)

  ;; Data directory
  (setq efrit-data-directory (expand-file-name ".efrit" doom-user-dir))

;;;;; Performance Settings

  (setq efrit-performance-cache-ttl 300) ; Cache for 5 minutes
  (setq efrit-performance-max-sessions 10)
  (setq efrit-async-max-work-log-entries 50)

;;;;; Keybindings

  (map! :leader
        (:prefix ("3" . "efrit/beads")
         :desc "Efrit REPL" "e" #'efrit
         :desc "Chat" "c" #'efrit-chat
         :desc "Do (async)" "d" #'efrit-do
         :desc "Do (sync)" "D" #'efrit-do-sync
         :desc "Agent buffer" "a" #'efrit-agent
         :desc "Remote queue" "q" #'efrit-remote-queue-start
         :desc "Queue status" "Q" #'efrit-remote-queue-status
         :desc "Doctor" "?" #'efrit-doctor))

  ) ; end when efrit-directory

;;;; Beads (Issue Tracker)

;; Git-synced issue tracking for AI agents
;; - bd CLI와 연동
;; - Efrit 내부 beads tools 사용

(use-package! beads
  :defer 2
  :commands (beads beads-list beads-ready beads-show beads-create)
  :hook (doom-after-init . beads-eldoc-mode)
  :config
  (map! :leader
        (:prefix ("3" . "efrit/beads")
         :desc "Beads" "3" #'beads
         :desc "List issues" "l" #'beads-list
         :desc "Ready issues" "r" #'beads-ready
         :desc "Create issue" "n" #'beads-create)))

(provide 'ai-orchestration)
;;; ai-orchestration.el ends here
