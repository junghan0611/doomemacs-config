;;; $DOOMDIR/lisp/ai-agent-shell.el --- Agent Shell Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Agent Shell (ACP - Agent Client Protocol) 설정
;; - agent-shell: Claude Code, Qwen 등 AI 에이전트 인터페이스
;; - agent-shell-manager: 버퍼 관리
;; - agent-shell-sidebar: 사이드바 UI

;;; Code:

;;;; claude-code (stevemolitor/claude-code.el)

(use-package! claude-code
  :config
  (setq claude-code-terminal-backend 'vterm)

  (defun my-claude-notify-with-sound (title message)
    "Display a Linux notification with sound."
    (when (executable-find "notify-send")
      (call-process "notify-send" nil nil nil title message))
    ;; Play sound if paplay is available
    (when (executable-find "paplay")
      (call-process "paplay" nil nil nil "/usr/share/sounds/freedesktop/stereo/complete.oga")))

  (setq claude-code-notification-function #'my-claude-notify-with-sound)

  ;; optional IDE integration with Monet
  (when (locate-library "monet")
    (require 'monet)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (monet-mode 1))

  (set-popup-rule! "^\\*claude" :vslot -15 :width 90 :side 'right :ttl t :select t :quit nil :modeline t)

  (claude-code-mode)

  (add-hook 'claude-code-start-hook
            (lambda ()
              ;; Only increase scrollback for vterm backend
              (when (eq claude-code-terminal-backend 'vterm)
                (visual-line-mode -1)
                (toggle-truncate-lines 1)
                (setq-local x-gtk-use-native-input t)
                (define-key claude-code-command-map (kbd "M-RET") 'claude-code--vterm-send-alt-return)
                (define-key vterm-mode-map (kbd "M-RET") 'claude-code--vterm-send-alt-return)
                (setq-local vterm-max-scrollback 100000)))))

;;;; claude-code-ide (manzaltu/claude-code-ide.el)

(use-package! claude-code-ide
  :init
  ;; Open Claude at the bottom with custom height
  (setq claude-code-ide-window-side 'right
        claude-code-ide-window-width 84
        claude-code-ide-window-height 50)
  :config
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-use-ide-diff nil)
  (claude-code-ide-emacs-tools-setup)

  (load! "+claude-code-ide-mcp-tools")

  (after! vterm
    (define-key vterm-mode-map (kbd "M-RET") 'claude-code-ide-insert-newline)
    (define-key vterm-mode-map (kbd "C-g") 'claude-code-ide-send-escape))
  ) ; optionally enable Emacs MCP tools

(after! vterm
  ;; sync/code/default/claude-code.el/claude-code.el
  (defun my/vterm-send-alt-return ()
    "Send <alt>-<return> to vterm."
    (interactive)
    (vterm-send-key "" nil t))

  (setq vterm-always-compile-module t) ;; Compile Vterm without asking.
  (undefine-key! vterm-mode-map "M-," "M-e" "M-." "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0") ;; 2025-07-13 Simpler
  (map! :map vterm-mode-map
        :i "M-RET" #'my/vterm-send-alt-return
        :inv "M-y" #'vterm-yank-pop
        :inv "M-h" #'other-window
        :inv "M-z" #'evil-collection-vterm-toggle-send-escape
        :inv "M-u" 'evil-scroll-up
        :inv "M-v" 'evil-scroll-down)
  )

(after! vterm
  (setq vterm-max-scrollback 10000)
  ;; (setq x-gtk-use-native-input nil) ;; 2025-08-10 Important with ibus korean input

  ;; kime 환경변수 설정 (기존 코드 유지)
  (add-to-list 'vterm-environment "GTK_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "QT_IM_MODULE=fcitx5")
  (add-to-list 'vterm-environment "XMODIFIERS=@im=fcitx5")

  (defun my/vterm-setup-gtk-use-native-input ()
    "Setup native input for vterm buffer"
    (interactive)
    (when (eq major-mode 'vterm-mode)
      (setq-local x-gtk-use-native-input t))))


;;;; ACP (Agent Client Protocol)

;; https://agentclientprotocol.com/
;; https://github.com/xenodium/agent-shell/issues/27

(progn
  (require 'shell-maker)
  (require 'acp)
  (require 'agent-shell)

  ;; Ensure claude-code-acp is in exec-path for Termux
  (when IS-TERMUX
    (add-to-list 'exec-path "/data/data/com.termux/files/usr/bin"))

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :login t))
  ;; (setq agent-shell-openai-authentication
  ;;       (agent-shell-openai-make-authentication :login t))

  (setq agent-shell--transcript-file-path-function #'agent-shell--default-transcript-file-path)
  (setq agent-shell-header-style nil)

  (require 'agent-shell-manager)
  (setq agent-shell-manager-side 'bottom)  ; Options: 'left, 'right, 'top, 'bottom
  (map! :n "s-;" #'agent-shell-manager-toggle)
  (map! :map agent-shell-mode-map
        :inv "M-h" #'other-window
        :inv "M-RET" #'+default/newline
        :inv "M-<return>" #'+default/newline
        :inv "DEL" #'evil-delete-backward-char-and-join
        ;; :inv "M-<return>" #'+default/newline
        :in "C-RET" #'shell-maker-submit
        :in "C-<return>" #'shell-maker-submit
        )

  (require 'agent-shell-sidebar)
  (setq agent-shell-sidebar-width "25%"
        agent-shell-sidebar-minimum-width 80
        agent-shell-sidebar-maximum-width "50%"
        agent-shell-sidebar-position 'right
        agent-shell-sidebar-locked t
        ;; agent-shell-sidebar-default-config (agent-shell-anthropic-make-claude-code-config)
        )

  ;; agent-shell 버퍼를 실제 버퍼로 표시 (버퍼 목록에서 보이게)
  (add-hook 'agent-shell-mode-hook #'doom-mark-buffer-as-real-h)
  )

;;;; Efrit (Steve Yegge's AI Assistant) - efrit-ko fork

;; AI-powered autonomous Emacs assistant (junghan0611/efrit-ko)
;; Features:
;; - efrit-chat: Multi-turn conversational interface
;; - efrit-do: Natural language command execution
;; - efrit-do-async: Async command execution with session management
;; - efrit-remote-queue: AI agent communication channel
;; - OpenRouter backend support (efrit-ko fork)

(use-package! efrit
  :commands (efrit-chat efrit-do efrit-do-async efrit-unified-do
             efrit-streamlined-send efrit-remote-queue-start)
  :init
  ;; API Backend: 'anthropic or 'openrouter
  ;; Set ANTHROPIC_API_KEY or OPENROUTER_API_KEY environment variable
  (setq efrit-api-backend 'anthropic)  ; or 'openrouter

  ;; Model selection (works with both backends)
  ;; Anthropic: claude-sonnet-4-20250514, claude-3-5-sonnet-20241022
  ;; OpenRouter: anthropic/claude-sonnet-4, anthropic/claude-3.5-sonnet
  (setq efrit-model "claude-sonnet-4-20250514")
  (setq efrit-max-tokens 8192)

  ;; Data directory under doom user dir
  (setq efrit-data-directory (expand-file-name ".efrit" doom-user-dir))

  ;; OpenRouter specific settings (only used when efrit-api-backend is 'openrouter)
  (setq efrit-openrouter-site-url "https://github.com/junghan0611/efrit-ko")
  (setq efrit-openrouter-site-name "Efrit-KO")

  :config
  ;; Performance settings
  (setq efrit-performance-cache-ttl 300) ; Cache for 5 minutes
  (setq efrit-performance-max-sessions 10)
  (setq efrit-async-max-work-log-entries 50)
)

;;;; Beads

(use-package! beads
  :defer 2
  :commands (beads beads-list beads-ready beads-show beads-create)
  ;; :bind ("C-c b" . beads)
  :hook (doom-after-init . beads-eldoc-mode))  ; Enable eldoc support

;;;; Efrit + Beads Keybindings

(map! :leader
      (:prefix ("3" . "efrit/beads")
       :desc "beads" "3" #'beads
       :desc "Chat" "c" #'efrit-chat
       :desc "Do (sync)" "d" #'efrit-do
       :desc "Do (async)" "D" #'efrit-do-async
       :desc "Unified" "u" #'efrit-unified-do
       :desc "Streamlined" "s" #'efrit-streamlined-send
       :desc "Start queue" "q" #'efrit-remote-queue-start
       :desc "Queue status" "Q" #'efrit-remote-queue-status
       :desc "Dashboard" "b" #'efrit-dashboard
       :desc "Performance" "p" #'efrit-performance-show-stats))

;;;; TODO MCP (Model Context Protocol)

;; (unless IS-TERMUX
;;   (when (display-graphic-p) ; gui
;;     (use-package! mcp-server-lib
;;       :after org
;;       :config
;;       (mcp-server-lib-install))

;;     (use-package! elisp-dev-mcp
;;       :after mcp-server-lib
;;       :commands (elisp-dev-mcp-enable elisp-dev-mcp-disable)
;;       :config
;;       (setq mcp-server-lib-log-level 'info))  ;; 필요시 'debug로 변경

;;     (use-package! org-mcp
;;       :after mcp-server-lib
;;       :config
;;       (setq org-mcp-allowed-files
;;             (append
;;              (directory-files-recursively "~/org/" "\\.org$")
;;              (directory-files-recursively "~/claude-memory/" "\\.org$")))
;;       (org-mcp-enable)
;;       ;; Start the server automatically when Emacs starts
;;       (mcp-server-lib-start))
;;     )
;;   )

(provide 'ai-agent-shell)
;;; ai-agent-shell.el ends here
