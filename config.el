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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(remove-hook! (text-mode conf-mode) #'display-line-numbers-mode)

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

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;;; Load libraries
(load! "+user-info")
(load! "lisp/ui-config")
(load! "lisp/evil-config")
(load! "lisp/korean-input")
(load! "lisp/completion-config")
(load! "lisp/org-config")
(load! "lisp/denote-config")
(load! "lisp/denote-silo")
(load! "lisp/denote-export")
(load! "lisp/ai-gptel")
(load! "lisp/ai-agent-shell")

;;; Load 'Per-Machine' - User Configs
;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.

(let ((per-machine-filename (concat doom-user-dir "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;; GENERAL SETTINGS

(use-package! server
  :unless (display-graphic-p)
  :after-call doom-first-input-hook doom-first-file-hook focus-out-hook
  :defer 1
  :config
  (setq server-name "starter")
  (unless (server-running-p)
    (server-start)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)

;; /doom/high-school-macos-emacs-dev-env/doom/init.el
(setq-default x-stretch-cursor t) ; make the cursor wide over tabs, etc.
(setq undo-limit 80000000) ; Raise undo-limit to 80Mb
(setq truncate-string-ellipsis "…") ; Unicode ellispis are nicer than "...", and also save /precious/ space

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

;;; Input System : Hangul

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(set-clipboard-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; 멀티바이트 모드 활성화 (필요시)
(set-buffer-multibyte t)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3)

(setq system-time-locale "C") ;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.

(when IS-TERMUX
  (setenv "LANG" "C.UTF-8")
  (setenv "LC_ALL" "C.UTF-8"))

;; 2. 입력 메서드 시각적 피드백 최소화 (모바일 최적화)
(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

;; 3. 한영 전환 키 바인딩 (Emacs 입력 메서드 전용)
;; 안드로이드 IME 한영 전환 사용 안 함!
(global-set-key (kbd "C-\\") 'toggle-input-method)  ; Emacs 기본 (가장 중요!)

;; KKP (Kitty Keyboard Protocol) 한영 전환 키
;; +korean-input-fix.el에서 Alt_R → <Hangul> 매핑 처리
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)  ; GUI 호환
(global-set-key (kbd "<Hangul>") 'toggle-input-method) ; 한글 키 (Alt_R)

;; Termux/모바일 전용: 추가 토글 키
(when IS-TERMUX
  (global-set-key (kbd "M-SPC") 'toggle-input-method) ; Alt+Space (긴급용)
  (global-set-key (kbd "C-c \\") 'toggle-input-method) ; 최후 수단

  ;; 선택적: 특정 입력 메서드로 즉시 전환
  (global-set-key (kbd "C-c k")
    (lambda () (interactive) (set-input-method "korean-hangul")))
  (global-set-key (kbd "C-c e")
    (lambda () (interactive) (deactivate-input-method))))

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+

(unless (string-equal system-type "android")
;;;###autoload
  (defun my/set-emoji-symbol-font ()
    (interactive)

    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))

    (when (display-graphic-p) ; gui
      (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
      (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
      (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
      (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top
      )

    (unless (display-graphic-p) ; terminal
      ;; 터미널에서는 Noto Color Emoji 사용 (컬러 이모지 지원시)
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil)
      ;; 폴백 폰트 설정 (Noto Emoji가 없는 경우)
      ;; (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans Mono") nil 'append)

      ;; 터미널에서 폰트 스케일 조정 (이모지 크기 일정하게)
      (setq face-font-rescale-alist
            '(("Noto Color Emoji" . 0.9)
              ("Noto Emoji" . 0.9)
              ("Symbola" . 0.9)))

      ;; 이모지 문자의 너비를 2로 고정 (double-width)
      ;; 주요 이모지 범위들
      (dolist (range '((#x1F300 . #x1F6FF)  ; Misc Symbols and Pictographs
                       (#x1F700 . #x1F77F)  ; Alchemical Symbols
                       (#x1F780 . #x1F7FF)  ; Geometric Shapes Extended
                       (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
                       (#x1FA00 . #x1FA6F)  ; Chess Symbols
                       (#x1FA70 . #x1FAFF)  ; Symbols and Pictographs Extended-A
                       (#x2600 . #x26FF)    ; Miscellaneous Symbols
                       (#x2700 . #x27BF)    ; Dingbats
                       (#xFE00 . #xFE0F)    ; Variation Selectors
                       (#x1F000 . #x1F02F)  ; Mahjong Tiles
                       (#x1F030 . #x1F09F)  ; Domino Tiles
                       (#x1F0A0 . #x1F0FF))) ; Playing Cards
        (set-char-table-range char-width-table range 2))
      ;; 특정 이모지들을 유니코드 코드포인트로 너비 설정
      (dolist (codepoint '(#x1F600 #x1F603 #x1F604 #x1F601 #x1F606 #x1F605 #x1F602 #x1F923 #x1F60A #x1F607
                           #x1F642 #x1F643 #x1F609 #x1F60C #x1F60D #x1F970 #x1F618 #x1F617 #x1F619 #x1F61A
                           #x1F60B #x1F61B #x1F61C #x1F92A #x1F61D #x1F911 #x1F917 #x1F92D #x1F92B #x1F914
                           #x1F525 #x1F4AF #x2728 #x2B50 #x1F31F #x1F4AB #x1F308 #x2600 #x1F31E #x1F31D
                           #x2764 #x1F9E1 #x1F49B #x1F49A #x1F499 #x1F49C #x1F5A4 #x1F90D #x1F90E #x1F494
                           #x2705 #x274C #x2B55 #x1F534 #x1F7E0 #x1F7E1 #x1F7E2 #x1F535 #x1F7E3 #x26AB
                           #x26AA #x1F7E4 #x1F536 #x1F537 #x1F538 #x1F539 #x1F53A #x1F53B #x1F4A0 #x1F532))
        (set-char-table-range char-width-table codepoint 2)))

    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend))

  (add-hook 'after-setting-font-hook #'my/set-emoji-symbol-font))

;;; better default

;; 'tags-completion-at-point-function' break ten-glossary
(setq-default completion-at-point-functions nil) ; important

;; (setq-default display-line-numbers-width-start t) ; doom's default t
(setq inhibit-compacting-font-caches t)

;; Stop asking abount following symlinks to version controlled files
(setq vc-follow-symlinks t)

(global-auto-revert-mode 1) ; doom nil
(setq auto-revert-interval 10)

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; Denote 23.9. Speed up backlinks’ buffer creation?
;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setq xref-search-program
      (cond ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
            ((executable-find "ugrep") 'ugrep) (t 'grep)))

;;; overide doomemacs

(setq bookmark-default-file "~/emacs-bookmarks.el")
(setq bookmark-use-annotations nil)
(setq bookmark-automatically-show-annotations t)

(progn
  (require 'dabbrev)
  (setq dabbrev-abbrev-char-regexp "[가-힣A-Za-z-_]")
  (setq dabbrev-upcase-means-case-search nil) ; default t
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']"))
  )
  )
  :config (setq remember-data-file (my/org-remember-file)))
  (setq claude-code-notification-function #'my-claude-notify-with-sound)

  ;; optional IDE integration with Monet
  (when (locate-library "monet")
    (require 'monet)
    (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
    (monet-mode 1))

  (set-popup-rule! "^\\*claude" :vslot -15 :width 90 :side 'right :ttl t :select t :quit nil :modeline t)
  ;; (set-popup-rule! "^\\*claude" :vslot -15 :size 0.4 :side 'bottom :ttl t :select t :quit nil :modeline t)

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
                (setq-local vterm-max-scrollback 100000))))
  )


;;;; claude-code-ide

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

  (after! vterm
    (define-key vterm-mode-map (kbd "M-RET") 'claude-code-ide-insert-newline)
    (define-key vterm-mode-map (kbd "C-g") 'claude-code-ide-send-escape))
  ) ; optionally enable Emacs MCP tools
(add-hook 'doom-first-input-hook #'my/doom-themes-toggle)

;;;; flymake

(remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;; Elfeed

(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  (setq elfeed-search-filter "@6-months-ago") ; "" "@1-month-ago +unread"
  ;; (setq elfeed-search-title-max-width 90) ; default 70
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  )

;;;; eglot configuration

(progn
  (map! (:map eglot-mode-map
         :after eglot
         "C-c r" 'eglot-rename
         "C-c d" 'eldoc
         "C-c f" 'flymake-show-buffer-diagnostics
         "C-c 0" 'eglot-inlay-hints-mode
         "M-RET" 'eglot-code-actions)

        ;; FIXME need new keybindings
        ;; (:map 'flymake-mode-map
        ;;       "C-n" #'flymake-goto-next-error
        ;;       "C-p" #'flymake-goto-prev-error)
        )

  ;; (setq eglot-send-changes-idle-time 0.5)
  (setq flymake-no-changes-timeout nil)

  (add-hook! 'eglot-managed-mode-hook
    (eglot-inlay-hints-mode -1))
  )

;;;; fortune

;; not work on termux
;; (unless IS-TERMUX
;;   (require 'fortune)
;;   (setq fortune-always-compile nil)
;;   (setq fortune-dir (concat root-path "usr/share/games/fortunes/advice"))
;;   (setq fortune-file (concat root-path "usr/share/games/fortunes/advice")))

;;;; xclip

;; (use-package! xclip
;;   :unless window-system
;;   :config
;;   (unless (display-graphic-p) ; terminal
;;     (cond
;;      ((executable-find "termux-setup-storage")
;;       (setq xclip-method 'termux-clipboard-get)))
;;     (xclip-mode 1)))

;;;; TERMUX

(when IS-TERMUX

  (global-set-key (kbd "<M-SPC>") 'toggle-input-method)
  (global-set-key
   (kbd "M-<backtab>")
   (lambda ()
     (interactive)
     (other-window -1))))

;;;; term-keys

(use-package! clipetty
  :hook (after-init . global-clipetty-mode)
  :config
  (setq clipetty-assume-nested-mux nil))

;;;; Terminal Mode
(unless (display-graphic-p) ; terminal
  (setq visible-cursor nil)
  (xterm-mouse-mode -1) ; important
  (setq fast-but-imprecise-scrolling nil)
  (setq hscroll-step 0)
  (show-paren-mode -1)
  )

;;;; git / magit

;; Enforce git commit conventions.
;; See: http://chris.beams.io/posts/git-commit
(setq git-commit-summary-max-length 72) ; defaults to Github's max commit message length

(use-package! magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

;;; tramp

(progn
  (require 'tramp)
  (tramp-set-completion-function "ssh"
                                 '(
                                   (tramp-parse-sconfig "~/.ssh/config"))))

;;;; load functions

(load! "+functions")

;;; termux-fixes
;; Fix async issues in Termux/Android

(when IS-TERMUX
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-warning-on-missing-source nil)
  (setq async-bytecomp-allowed-packages nil)
  (setq process-connection-type nil)
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.6))

;;; bugfix treesit

(after! treesit
  (setq treesit-extra-load-path (list (concat doom-profile-data-dir "/tree-sitter/"))))

;;; denote-silo
;; 동적 Silo 관리는 +denote-silo-dynamic.el에서 처리됨
;; (after! denote
;;   (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/")))

;;; denote-export system

(load! "+denote-export")

;;; TODO Custom Integration

;;;; MU4e

(after! mu4e
  (setq mu4e-maildir "~/Maildir"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval (* 60 60 3)))  ; 3H 마다 자동 동기화

;;;; DONT  Notmuch 이메일 설정

;; (after! notmuch
;;   ;; 다중 계정 설정
;;   (setq notmuch-identities
;;         '("jhkim2@goqual.com"
;;           "junghanacs@gmail.com"))

;;   ;; FCC (보낸 메일 저장 위치)
;;   (setq notmuch-fcc-dirs
;;         '(("jhkim2@goqual.com" . "work/[Gmail]/&yVwwYA-")
;;           ("junghanacs@gmail.com" . "personal/[Gmail]/Sent Mail")))

;;   ;; 메일 발송 설정
;;   (setq message-send-mail-function 'message-send-mail-with-sendmail
;;         sendmail-program "/usr/bin/msmtp"
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-sendmail-f-is-evil t)

;;   ;; 동기화 명령
;;   (setq +notmuch-sync-backend "mbsync -a")

;;   ;; 저장된 검색
;;   (setq notmuch-saved-searches
;;         '((:name "📧 Work Inbox"
;;            :query "tag:inbox AND to:jhkim2@goqual.com"
;;            :key "w")
;;           (:name "📧 Personal Inbox"
;;            :query "tag:inbox AND to:junghanacs@gmail.com"
;;            :key "p")
;;           (:name "📬 Unread"
;;            :query "tag:unread"
;;            :key "u")
;;           (:name "📤 Sent"
;;            :query "tag:sent"
;;            :key "s")
;;           (:name "🗓️ Today"
;;            :query "date:today"
;;            :key "t"))))

;;;; pass + auth

(after! pass
  (setq pass-username-field "login"
        password-store-password-length 24))

(use-package! password-store-menu
  :defer 2
  :commands (password-store-menu-enable)
  :custom (password-store-menu-key "C-c C-p")
  :config
  (password-store-menu-enable))

(setq auth-sources '(password-store "~/.authinfo.gpg"))

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


;;; Load "+keybindings"

(load! "+keybindings")

;;; py3status integration (ElleNajit)

(with-eval-after-load 'org-clock
  (add-hook 'org-clock-in-hook #'junghan/update-org-clocked-in-task-file)
  (add-hook 'org-clock-out-hook #'junghan/update-org-clocked-in-task-file)
  (add-hook 'org-after-todo-state-change-hook #'junghan/update-org-clocked-in-task-file)

  ;; Update every minute
  (run-at-time "1 min" 60 #'junghan/update-org-clocked-in-task-file))

;;;; evil + hangul

;; 4. Evil 모드 연동: 자동 한영 전환
(after! evil
  ;; 버퍼별 입력 메서드 상태 저장
  (defvar-local my/saved-input-method nil
    "Normal 모드 진입 전 입력 메서드 상태")

  (defun my/evil-normal-state-korean-off (&rest _)
    "Normal 모드 진입: 한글 OFF, 상태 저장"
    (when (and (boundp 'current-input-method) current-input-method)
      (setq my/saved-input-method current-input-method)
      (deactivate-input-method)))

  (defun my/evil-insert-state-korean-restore ()
    "Insert 모드 진입: 이전 한글 상태 복원"
    (when (and my/saved-input-method
               (not current-input-method))
      (activate-input-method my/saved-input-method)))

  ;; Hook 등록
  (add-hook 'evil-normal-state-entry-hook #'my/evil-normal-state-korean-off)
  (add-hook 'evil-insert-state-entry-hook #'my/evil-insert-state-korean-restore)

  ;; Evil escape 후에도 확실히 끄기
  (advice-add 'evil-normal-state :after #'my/evil-normal-state-korean-off)

  ;; Shift+Space 메시지 (motion/normal/visual 모드에서)
  (mapc (lambda (mode)
          (let ((keymap (intern (format "evil-%s-state-map" mode))))
            (define-key (symbol-value keymap) [?\S- ]
                        #'(lambda () (interactive)
                            (message
                             (format "Input method is disabled in %s state." evil-state))))))
        '(motion normal visual))
  )

;; 5. Emacs 입력 메서드 추가 최적화
(with-eval-after-load 'quail
  ;; 한글 입력 모드 표시 (모드라인)
  (setq-default mode-line-mule-info
    '((:eval (if current-input-method
                 (propertize " [한] " 'face '(:foreground "green"))
               " [En] "))))

  ;; 2벌식 기본 사용 (3벌식 원하면 변경)
  ;; (setq default-korean-keyboard "390") ; 3벌식 최종
  )

;; 6. 안드로이드 Emacs 특화 설정 (해당시)
(when (string-equal system-type "android")
  ;; Android Emacs의 IME 간섭 차단
  (setq overriding-text-conversion-style nil)
  (setq-default text-conversion-style nil))

;;; TODO TERMUX

(when IS-TERMUX
  ;; 7. GUI 폰트 설정 (Sarasa Term K Nerd)

  ;; Termux X11 GUI에서 Nerd Font 아이콘 제대로 표시
  ;; Fold4 7.6" 2176x1812 (373 PPI, DPI 180 기준) 최적화
  ;; (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 14)
  ;;       doom-variable-pitch-font (font-spec :family "Sarasa Term K Nerd Font" :size 14))

  ;; 8. 배터리 효율 최적화 설정 (Termux X11 GUI)
  ;; 작성: 2025-11-08

  ;; Auto-save 간격 늘리기 (디스크 I/O 감소)
  (setq auto-save-interval 300)        ; 300 타이핑마다
  (setq auto-save-timeout 30)          ; 30초마다

  ;; GC 임계값 증가 (가비지 컬렉션 빈도 감소)
  (setq gc-cons-threshold (* 50 1024 1024))  ; 50MB

  ;; 스크롤 최적화
  (setq scroll-conservatively 101)
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position t)

  ;; 폰트 렌더링 최적화
  (setq inhibit-compacting-font-caches t)

  ;; 파일 변경 감지 간격 늘리기
  (setq auto-revert-interval 5)  ; 5초

  ;; 알람/비프음 비활성화 (하드웨어 절전)
  (setq ring-bell-function 'ignore)

  ;; GUI 전용 최적화
  (when (display-graphic-p)
    ;; 커서 깜빡임 비활성화 (배터리 절약)
    (blink-cursor-mode -1)

    (message "GUI 모드: 배터리 최적화 활성화 ✓"))

  ;; Termux extra-keys 방향키 설정
  ;; Termux 환경에서 방향키가 제대로 동작하도록 보장
  (when (and (not (display-graphic-p))
             (or (getenv "TERMUX_VERSION")
                 (string-match-p "termux" (or (getenv "PREFIX") ""))))

    ;; Termux는 ESC O 시퀀스를 전송 (Application Keypad Mode)
    ;; input-decode-map과 function-key-map 모두에 매핑 (더 강력)
    (defun termux-fix-arrow-keys ()
      "Fix arrow keys for Termux extra-keys."
      ;; input-decode-map (우선순위 높음)
      (define-key input-decode-map "\eOA" [up])
      (define-key input-decode-map "\eOB" [down])
      (define-key input-decode-map "\eOC" [right])
      (define-key input-decode-map "\eOD" [left])
      (define-key input-decode-map "\e[A" [up])
      (define-key input-decode-map "\e[B" [down])
      (define-key input-decode-map "\e[C" [right])
      (define-key input-decode-map "\e[D" [left])
      ;; function-key-map (호환성)
      (define-key function-key-map "\eOA" [up])
      (define-key function-key-map "\eOB" [down])
      (define-key function-key-map "\eOC" [right])
      (define-key function-key-map "\eOD" [left])
      (define-key function-key-map "\e[A" [up])
      (define-key function-key-map "\e[B" [down])
      (define-key function-key-map "\e[C" [right])
      (define-key function-key-map "\e[D" [left])
      ;; local-function-key-map (로컬)
      (define-key local-function-key-map "\eOA" [up])
      (define-key local-function-key-map "\eOB" [down])
      (define-key local-function-key-map "\eOC" [right])
      (define-key local-function-key-map "\eOD" [left]))

    ;; 즉시 적용
    (termux-fix-arrow-keys)

    ;; 터미널 초기화 후에도 적용 (tty-setup-hook)
    (add-hook 'tty-setup-hook #'termux-fix-arrow-keys)

    ;; evil-mode 로드 후에도 적용 (evil이 키를 오버라이드할 수 있음)
    (after! evil
      (termux-fix-arrow-keys))

    (message "Termux 방향키 ESC O 시퀀스 매핑 완료 ✓"))
  )

;;; END
