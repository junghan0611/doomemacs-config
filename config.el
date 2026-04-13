;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This is a file generated from a literate programing source file located at
;; https://gitlab.com/zzamboni/dot-doom/-/blob/master/doom.org
;; You should make any changes there and regenerate it from Emacs org-mode
;; using org-babel-tangle (C-c C-v t)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(remove-hook! (text-mode prog-mode conf-mode) #'display-line-numbers-mode)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Global Unset Keys

(map! "<f2>" nil
      "M-\\" nil  ; delete-horizontal-space
      "M-a" nil   ; forward-sentence - use evil motion instead
      "M-c" nil   ; capitalize-word
      "M-e" nil)  ; backward-sentence - use evil motion instead

;;; Load 'Per-Machine' - User Configs
;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.

(let ((per-machine-filename (concat doom-user-dir "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;; GENERAL SETTINGS

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)

;; /doom/high-school-macos-emacs-dev-env/doom/init.el
(setq-default x-stretch-cursor t) ; make the cursor wide over tabs, etc.
(setq undo-limit 80000000) ; Raise undo-limit to 80Mb
(setq truncate-string-ellipsis "…") ; Unicode ellispis are nicer than "...", and also save /precious/ space

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'emacs-lisp-mode)

;; Set initial buffer to org
(setq initial-major-mode #'emacs-lisp-mode); text-mode

;;; Leader key

;; Over-ride or add to Doom Emacs default key bindings
;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; 'M-m', '\,' 'SPC m' for localleader
(setq doom-localleader-key ","
      doom-localleader-alt-key "C-,")

(defun my/call-localleader ()
  (interactive)
  (setq unread-command-events (listify-key-sequence ",")))

(map! :leader (:desc "+major-mode" "m" #'my/call-localleader))

(after! evil
  ;; (global-set-key (kbd "M-m") #'my/call-localleader)
  (evil-define-key '(normal visual) prog-mode-map (kbd "C-,") 'my/call-localleader))

;;; better default

;; 'tags-completion-at-point-function' break ten-glossary
(setq-default completion-at-point-functions nil) ; important

;; (setq-default display-line-numbers-width-start t) ; doom's default t
(setq inhibit-compacting-font-caches t)

;; Stop asking abount following symlinks to version controlled files
(setq vc-follow-symlinks t)

(progn
  (global-auto-revert-mode 1) ;; 기본 활성화 (필수)
  (setq auto-revert-verbose nil)
  ;; VC 정보도 업데이트 (magit 상태 반영)
  (setq auto-revert-check-vc-info t)
  ;; 간격 (notify 사용시 fallback용)
  (setq auto-revert-interval 5)

  (let ((device (string-trim
                 (shell-command-to-string "cat ~/.current-device 2>/dev/null"))))
    (cond
     ((string-equal-ignore-case device "termux")
      (setq auto-revert-use-notify nil
            auto-revert-avoid-polling nil))
     ((cl-member device '("nuc" "laptop" "thinkpad") :test #'string-equal-ignore-case)
      ;; OS 파일 알림 사용 (polling보다 빠르고 효율적)
      (setq auto-revert-use-notify t
            auto-revert-avoid-polling t))))
  )

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; Denote 23.9. Speed up backlinks’ buffer creation?
;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setq xref-search-program
      (cond ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
            ((executable-find "ugrep") 'ugrep) (t 'grep)))

;;; pass + auth (gptel 등에서 API 키 접근을 위해 즉시 로드)
;;
;; GPG 에이전트 캐시 설정 (암호 입력 빈도 줄이기):
;;   ~/.gnupg/gpg-agent.conf 파일에 다음 추가 후 `gpgconf --kill gpg-agent` 실행
;;     default-cache-ttl 31536000   ; 1년 (사실상 영구)
;;     max-cache-ttl 31536000
;;     allow-preset-passphrase      ; 외부 도구가 암호 프리셋 가능
;;     pinentry-program /usr/bin/pinentry-gnome3
;;
;; auth-sources 선택:
;;   - password-store: `pass` CLI 기반, 디렉터리 구조 (~/.password-store/)
;;   - authinfo.gpg: 전통적 Emacs 방식, 단일 파일 (~/.authinfo.gpg)
;;   - 둘 다 사용 시: (setq auth-sources '(password-store "~/.authinfo.gpg"))
;;
;; gptel API 키 접근 방식:
;;   - password-store-get 함수로 직접 가져옴 (auth-source 우회)
;;   - 예: (password-store-get "api/anthropic/personal")

(require 'password-store)
(setq pass-username-field "login"
      password-store-password-length 24)

(use-package! password-store-menu
  :after password-store
  :custom (password-store-menu-key "C-c C-p")
  :config
  (password-store-menu-enable))

;; (setq auth-sources '(password-store "~/.authinfo.gpg"))
;; (setq auth-sources '("~/.authinfo.gpg")
;;       auth-source-cache-expiry nil) ; default is 7200 (2h)

;;; Load libraries via require (prevents duplicate loading)

(add-load-path! "lisp/")

(load! "+user-info")  ; no provide, keep load!

(require 'ui-config)
(require 'evil-config)
(require 'korean-input-config)
(require 'time-config)
(require 'completion-config)

(require 'module-emacs-config)

(require 'org-config)
(require 'denote-config)
(require 'denote-silo-config)
(require 'denote-export-config)
(require 'org-functions)
(require 'denote-functions)

(require 'unicode-config)
(require 'editing-config)

(require 'ai-gptel)
(require 'ai-agent-shell)            ; acp 설정
(require 'ai-pi-agent)               ; Pi coding agent 설정
(require 'ai-bot-config)             ; Telegram 봇 통합

(require 'ai-stt-eca-whisper)
(require 'ai-tts-edge)

(require 'modeline-config)
(require 'tab-bar-config)

(require 'prog-mode-config)

;; (require 'sks-hub-nav)                 ; SKS Hub Zig 상태머신 네비게이션
;; (require 'android-config)              ; Android/Kotlin 개발 환경
(require 'utils-config)
(require 'project-config)
(load! "lisp/ai-gptel-local-proxy" nil t) ; 로컬 전용 프록시 (없으면 무시)
(require 'elfeed-config)             ; elfeed + gptel 인라인 요약/번역 + remember
(require 'zotero-config)             ; zotero translation server (조건부 로딩)
;; (require 'ai-orchestration)          ; efrit/beads (조건부 로딩)
(require 'tmux-config)               ; tmux + claude code orchestration
;; (require 'zellij-config)             ; zellij terminal multiplexer
(require 'search-config)             ; recent-rgrep 등 검색 도구
(require 'keybindings-config)
(require 'keybindings-denote-config)
;; (require 'casual-config)               ; casual transient UI (<f12>)
(require 'termux-config)
(require 'tty-config)                 ; 터미널(TTY) 통합: term-keys, kitty-graphics, clipboard
;; (require 'eaf-config)                ; EAF (조건부 로딩)

(require 'present-config)

(require 'functions)

;;; overide doomemacs

;;;; fortune

;; not work on termux
;; (unless IS-TERMUX
;;   (require 'fortune)
;;   (setq fortune-always-compile nil)
;;   (setq fortune-dir (concat root-path "usr/share/games/fortunes/advice"))
;;   (setq fortune-file (concat root-path "usr/share/games/fortunes/advice")))

;; term-keys, clipboard, kitty-graphics → lisp/tty-config.el 로 이동

;;; TODO Custom Integration

;;;; MU4e

;; (after! mu4e
;;   (setq mu4e-maildir "~/Maildir"
;;         mu4e-get-mail-command "mbsync -a"
;;         mu4e-update-interval (* 60 60 3)))  ; 3H 마다 자동 동기화

;;;; DONT  Notmuch 이메일 설정

(after! notmuch
  ;; 다중 계정 설정
  (setq notmuch-identities
        '("jhkim2@goqual.com"
          "junghanacs@gmail.com"))

  ;; FCC (보낸 메일 저장 위치)
  (setq notmuch-fcc-dirs
        '(("jhkim2@goqual.com" . "work/[Gmail]/&yVwwYA-")
          ("junghanacs@gmail.com" . "personal/[Gmail]/Sent Mail")))

  ;; 메일 발송 설정
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-f-is-evil t)

  ;; 동기화 명령
  (setq +notmuch-sync-backend "mbsync -a")

  ;; 저장된 검색
  (setq notmuch-saved-searches
        '((:name "📧 Work Inbox"
           :query "tag:inbox AND to:jhkim2@goqual.com"
           :key "w")
          (:name "📧 Personal Inbox"
           :query "tag:inbox AND to:junghanacs@gmail.com"
           :key "p")
          (:name "📬 Unread"
           :query "tag:unread"
           :key "u")
          (:name "📤 Sent"
           :query "tag:sent"
           :key "s")
          (:name "🗓️ Today"
           :query "date:today"
           :key "t"))))

;;;; Notification System (dunst via notify-send)

(defun my/notify (title message &optional urgency duration)
  "Send system notification via notify-send (dunst).

TITLE: notification title
MESSAGE: notification body
URGENCY: low, normal, critical (default: normal)
DURATION: milliseconds (default: 4000)

Works in both GUI and terminal (emacs -nw) environments.
Requires notify-send (libnotify) and dunst daemon.

Returns t on success, nil if notify-send is not available."
  (when (executable-find "notify-send")
    (let ((urgency (or urgency "normal"))
          (duration (or duration 4000)))
      (ignore-errors
        (call-process "notify-send" nil 0 nil
                      "-u" urgency
                      "-t" (format "%d" duration)
                      title
                      message))
      t)))
;;   )

;;; DONT py3status integration (ElleNajit)

;; (with-eval-after-load 'org-clock
;;   (add-hook 'org-clock-in-hook #'junghan/update-org-clocked-in-task-file)
;;   (add-hook 'org-clock-out-hook #'junghan/update-org-clocked-in-task-file)
;;   (add-hook 'org-after-todo-state-change-hook #'junghan/update-org-clocked-in-task-file)

;;   ;; Update every minute
;;   (run-at-time "1 min" 60 #'junghan/update-org-clocked-in-task-file))

;;; Workflow Shared (인간/에이전트 공유 — denote 이후 로드)

(after! denote (require 'workflow-shared))

;;; END
