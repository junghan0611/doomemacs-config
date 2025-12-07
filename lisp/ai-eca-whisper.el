;;; ai-eca-whisper.el --- ECA + Whisper 음성 입력 설정 -*- lexical-binding: t; -*-
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Created: 2025-12-07
;;
;;; Commentary:
;;
;; Emacs + Claude Code (ECA) + 음성제어 워크플로우
;; - ECA: Claude Max 구독 OAuth 인증
;; - whisper.el: Groq API (무료 whisper-large-v3)
;; - 한글 입력 지원
;;
;;; Code:

;;; semext

;; (use-package! semext
;;   :init
;;   ;; Replace provider with whatever you want, see https://github.com/ahyatt/llm
;;   (setopt semext-provider (make-llm-ollama :chat-model "gemma3:1b")))

;;; AI CODE Interface (AIDER.el)

(use-package! ai-code-interface
  :config
  (ai-code-set-backend  'claude-code-ide) ;; use claude-code-ide as backend
  ;; Enable global keybinding for the main menu
  ;; (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use vterm if you prefer, by default it is eat
  ;; (setq claude-code-terminal-backend 'vterm) ;; for openai codex, github copilot cli, opencode; for claude-code-ide.el and gemini-cli.el, you can check their config
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

;;; ECA (Editor Code Assistant) 설정

(use-package! eca
  :defer t
  :commands (eca eca-restart eca-stop eca-show-stderr)
  :config

  ;; 채팅 창 설정
  (setq eca-chat-window-side 'right
        eca-chat-window-width 0.45
        eca-chat-use-side-window t
        eca-chat-focus-on-open t)

  ;; 커서 컨텍스트 자동 추적
  (setq eca-chat-auto-add-cursor t
        eca-chat-auto-add-repomap nil)

  ;; ECA 설정 파일: ~/claude-config/eca/config.json
  ;; (심볼릭 링크: ~/.config/eca/config.json)
  )

;;; ============================================================================
;;; Whisper.el 설정 (Groq API - 무료)
;;; ============================================================================

(use-package! whisper
  :defer t
  :commands (whisper-run whisper-file whisper-select-language)
  :config
  ;; Groq API 모드 (로컬 whisper.cpp 설치 불필요!)
  (setq whisper-server-mode 'openai
        ;; whisper-openai-api-key (auth-info-password (car (auth-source-search :host "api.openai.com" :user "apikey")))
        whisper-openai-api-baseurl "https://api.groq.com/openai/"
        whisper-openai-api-key (auth-info-password (car (auth-source-search :host "api.groq.com" :user "apikey")))
        whisper-openai-model "whisper-large-v3"
        whisper-language "ko"
        whisper-insert-text-at-point t
        whisper-return-cursor-to-start nil
        whisper-show-progress-in-mode-line t)

  ;; 녹음 타임아웃 (초)
  (setq whisper-recording-timeout 120))

;;; ECA + Whisper 통합 워크플로우

(defun jh/eca-whisper-input ()
  "ECA 채팅에 음성으로 입력하기.
1. ECA 채팅 버퍼로 이동
2. 음성 녹음 시작
3. 녹음 완료 후 자동 입력"
  (interactive)
  (require 'eca nil t)
  (if (and (fboundp 'eca-session)
           (eca-session)
           (fboundp 'eca--session-id))
      (let* ((session-id (eca--session-id (eca-session)))
             (eca-buf (get-buffer (format "<eca-chat:%s>" session-id))))
        (if eca-buf
            (progn
              (pop-to-buffer eca-buf)
              (goto-char (point-max))
              (whisper-run))
          (message "ECA 채팅 버퍼를 찾을 수 없습니다.")))
    (message "ECA 세션이 없습니다. M-x eca 로 시작하세요.")))

;;; 키바인딩

;; Whisper 음성 입력
(map! :leader
      (:prefix ("-" . "voice")
       :desc "녹음/중지" "w" #'whisper-run
       :desc "파일 변환" "f" #'whisper-file
       :desc "언어 선택" "l" #'whisper-select-language))

;; ECA
(map! :leader
      (:prefix ("=" . "AI")
       :desc "ECA 음성 입력" "v" #'jh/eca-whisper-input
       :desc "ECA 시작/채팅" "e" #'eca
       :desc "ECA 재시작" "r" #'eca-restart
       :desc "ECA 중지" "q" #'eca-stop
       :desc "ECA 로그" "l" #'eca-show-stderr))

(global-set-key (kbd "M-a") #'whisper-run)

;;; 사용법
;;
;; 1. ECA 시작
;;    SPC a e  →  Claude Max OAuth 인증 (/login anthropic)
;;
;; 2. 음성 녹음
;;    SPC v w  →  녹음 시작, 다시 누르면 중지 & 변환
;;
;; 3. ECA + 음성 통합
;;    SPC a v  →  ECA 채팅으로 이동 후 음성 녹음
;;
;; 필수 패키지:
;;    - ffmpeg (녹음용): sudo apt install ffmpeg
;;    - curl (API 호출): 기본 설치됨
;;
;; 설정 파일:
;;    - ECA: ~/claude-config/eca/config.json
;;    - Groq API 키: pass api/groq/junghanacs
;;
;; 성능:
;;    - Groq whisper-large-v3: 무료, 한국어 CER ~11%
;;    - 로컬 설치 불필요, API 호출만으로 동작
;;
;;; ai-eca-whisper.el ends here

(provide 'ai-eca-whisper)
