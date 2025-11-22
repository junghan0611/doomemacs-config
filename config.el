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
(load! "lisp/korean-input")
(load! "lisp/org-config")
(load! "lisp/denote-silo")
(load! "lisp/denote-export")
(load! "lisp/ai-gptel")

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

;;; startup and dashboard

;; (setq initial-scratch-message user-initial-scratch-message)

;; ;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; ;; elisp or note something down (that isn't worth an entry in my notes). I can
;; ;; do both in `lisp-interaction-mode'.
;; (setq doom-scratch-initial-major-mode 'emacs-lisp-mode)

;;;; Dashboard - Terminal Optimized

(setq +doom-dashboard-ascii-banner-fn nil)

;; Dashboard 위젯 구성
(setq +doom-dashboard-functions
      '(my/dashboard-widget-fortune ;; fortune
        doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

;; Fortune 위젯: Kevin Kelly 명언 또는 fortune 명령어 출력
(defun my/dashboard-widget-fortune ()
  "Display fortune quote or Kevin Kelly's wisdom."
  (let* ((quotestring
          (if (executable-find "fortune")
              ;; fortune 명령어가 있으면 사용, termux는 -c 옵션이 없으므로 조건부 처리
              (let ((fortune-cmd (if IS-TERMUX
                                     "fortune"  ; termux: simple fortune
                                   "fortune -c 90% advice 10% ."))) ; other: with -c option
                (string-join
                 (mapcar
                  (lambda (l) (concat "\n " (string-fill l 72)))
                  (string-lines
                   (shell-command-to-string fortune-cmd)))))
            ;; fortune 없으면 Kevin Kelly 기본 명언
            "\n The only way to fight against getting old is to remain astonished.
                                                      - Kevin Kelly")))
    (+doom-dashboard--center
     (- +doom-dashboard--width 2)
     (insert quotestring "\n"))))

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

;;;; dired

(after! dired
  (setq dired-make-directory-clickable t) ; Emacs 29.1, doom t
  (setq dired-free-space nil) ; Emacs 29.1, doom first

  ;; Better dired flags:
  ;; `-l' is mandatory
  ;; `-a' shows all files
  ;; `-h' uses human-readable sizes
  ;; `-F' appends file-type classifiers to file names (for better highlighting)
  ;; -g     like -l, but do not list owner
  (setq dired-listing-switches "-AGFhgv --group-directories-first --time-style=long-iso") ;; doom "-ahl -v --group-directories-first"
  (setq dired-recursive-copies 'always ; doom 'always
        dired-dwim-target t) ; doom t
  (setq dired-ls-F-marks-symlinks nil ; doom nil -F marks links with @
        delete-by-moving-to-trash t) ; doom nil

  (setq dired-use-ls-dired t)  ; doom t
  (setq dired-do-revert-buffer t) ; doom nil
  ;; (setq dired-clean-confirm-killing-deleted-buffers t) ; doom nil
  ;; (setq dired-kill-when-opening-new-dired-buffer t) ; doom nil

  (require 'wdired)
  (setq wdired-allow-to-change-permissions t) ; doom nil
  (setq wdired-create-parent-directories t)

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (setq-local truncate-lines t) ; Do not wrap lines
              ;; (visual-line-mode -1)
              (hl-line-mode 1)))
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-omit-mode)

  (evil-define-key 'normal dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode
    (kbd "C-c l") 'org-store-link
    (kbd "C-x /") 'dired-narrow-regexp
    (kbd ".") 'consult-line
    ;; (kbd "K") 'dired-kill-subdir
    (kbd "K") 'dired-do-kill-lines
    ;; (kbd "F") 'evil-avy-goto-line-below ;; 2024-01-25 useful
    (kbd "h") 'dired-up-directory
    (kbd "RET") 'dired-find-file
    (kbd "l") 'dired-find-file
    (kbd "S-<return>") 'dired-find-file-other-window
    ;; evil-force-normal-state
    (kbd "q") 'casual-dired-tmenu
    (kbd "S-SPC") 'dired-toggle-marks
    )
  )

;;;; visual-line-mode

(add-hook 'backtrace-mode-hook 'display-line-numbers-mode)
(add-hook 'backtrace-mode-hook 'visual-line-mode)

;;;; pulse-line

(progn
  ;; add visual pulse when changing focus, like beacon but built-in
  ;; from from https://karthinks.com/software/batteries-included-with-emacs/
  (require 'pulse)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command
           '(scroll-up-command scroll-down-command ace-window recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))

;;;; which-key

(after! which-key
  (setq
   which-key-max-description-length 29 ; doom 27, spacemacs 36
   which-key-idle-delay 0.4
   which-key-idle-secondary-delay 0.01
   ;;  which-key-ellipsis ".."
   ;;  which-key-allow-multiple-replacements nil
   ;;  which-key-use-C-h-commands t) ; paging key maps
   ))

;;;; popup-rule

;; from prot's dotfiles : important
(add-to-list
 'display-buffer-alist
 `("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

;;;; dabbrev

(progn
  (require 'dabbrev)
  (setq dabbrev-abbrev-char-regexp "[가-힣A-Za-z-_]")
  (setq dabbrev-ignored-buffer-regexps
        '("\\` "
          "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
          "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-upcase-means-case-search nil) ; default t

  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)
  ;; (setq dabbrev-check-all-buffers t) ;; default t
  ;; (setq cape-dabbrev-check-other-buffers t) ; enable when dabbrev on init.el
  )

;;; completion

;;;; corfu

;; 2024-09-13 기본 설정, jump-out-of-pair 추가
;; Tab 이 자동 완성이면 괄호 점프랑 충돌 난다. C-j/k C-n/p 는 직관적인 기본 설정이므로 건들이지 않는다.

(after! corfu
  ;; (setq corfu-auto-delay 0.5) ; doom 0.24
  (setq corfu-auto-prefix 4) ; doom 2, default 3
  ;; (setq corfu-preselect 'valid) ; doom 'prompt
  ;; (setq tab-always-indent t) ; for jump-out-of-pair - doom 'complete
  (setq +corfu-want-minibuffer-completion nil) ; doom t

  (setq +corfu-want-tab-prefer-expand-snippets nil)
  (setq +corfu-want-tab-prefer-navigating-snippets nil)
  (setq +corfu-want-tab-prefer-navigating-org-tables nil)

  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (eq emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  ;; IMO, modern editors have trained a bad habit into us all: a burning need for
  ;; completion all the time -- as we type, as we breathe, as we pray to the
  ;; ancient ones -- but how often do you *really* need that information? I say
  ;; rarely. So opt for manual completion:
  ;; doom/hlissner-dot-doom/config.el
  ;; (setq corfu-auto nil)

  ;; default 'C-S-s'
  (define-key corfu-map (kbd "M-.") '+corfu-move-to-minibuffer)
  )

;;;; vertico-map

(after! consult
  ;; (setq consult--customize-alist nil)

  (consult-customize
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   :preview-key '("C-SPC" :debounce 0.3 "<up>" "<down>" "M-j" "M-k"))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '("C-SPC"
                  :debounce 0.3 "<up>" "<down>" "M-j" "M-k"))
  )

(after! vertico
  (map! :map vertico-map
        "M-j" #'vertico-next
        "M-k" #'vertico-previous

        ;; M-d: 문자 하나씩 삭제 (DEL은 기본 동작 유지: 디렉토리 단위 삭제)
        "M-d" #'delete-backward-char))

;;; evil

(after! evil
  ;; C-h is backspace in insert state
  ;; (setq evil-want-C-h-delete t) ; default nil
  (setq evil-want-C-w-delete t) ; default t
  (setq evil-want-C-u-scroll t) ; default t

  ;; use C-i / C-o  evil-jump-backward/forward
  ;; (setq evil-want-C-i-jump t) ; default nil

  ;;  /home/junghan/sync/man/dotsamples/vanilla/mpereira-dotfiles-evil-clojure/configuration.org
  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
  ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
  ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
  ;; '(foo.bar/baz)', which I don't like.
  ;; (setq-default evil-symbol-word-search t)
  ;; (setq evil-jumps-cross-buffers nil)
  (setq evil-want-Y-yank-to-eol t) ; doom t

  ;; 'Important' Prevent the cursor from moving beyond the end of line.
  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil) ; nil is better - default t
  (setq evil-move-beyond-eol nil) ; default nil

  (setq +evil-want-o/O-to-continue-comments nil) ; doom t
  (setq +default-want-RET-continue-comments nil) ; doom t

  (setq evil-want-fine-undo t) ; doom 'nil

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil) ; default t
  ;; Don't create a kill entry on every visual movement.
  ;; More details: https://emacs.stackexchange.com/a/15054:
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-motion-state-map "M" nil)

    ;; Replace Emacs Tabs key bindings with Workspace key bindings
    ;; replace "." search with consul-line in Evil normal state
    ;; use default "/" evil search

    ;; disable evil macro
    (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

    ;; o :: ace-link-info 이거면 충분하다.
    (define-key evil-insert-state-map (kbd "C-]") 'forward-char) ; very useful

    ;; =C-w= 'insert 'evil-delete-backward-word
    ;; =C-w= 'visual 'evil-window-map
    ;; use evil bindings $ ^

    ;; M-d region delete and C-d char delete
    (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)

    ;; Don't put overwritten text in the kill ring
    ;; evil-delete-char -> delete-forward-char
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char)
    )

  ;; evil-org
  (with-eval-after-load 'evil-org
    ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-d") 'delete-forward-char)
    (evil-define-key 'normal 'evil-org-mode-map "x" 'delete-forward-char)
    ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-k") 'org-kill-line)
    ;; (evil-define-key 'insert 'org-mode-map (kbd "C-k") 'org-kill-line)
    (evil-define-key 'normal 'evil-org-mode-map "X" 'delete-backward-char))
  )

;; ,. as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence ",.") ;; "jk"
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay 1.0) ;; 0.5, default 0.1
  (evil-escape-mode 1)

  ;; 모바일 타이핑 최적화
  (when IS-TERMUX
    (setq evil-escape-delay 0.8)))

(after! smartparens
  ;; 2023-09-14 global 로 사용하다보니 거슬린다. 잠시만. 글로벌을 빼면 어떤가?
  ;; ("\\\\(" . "\\\\)") ;; emacs regexp parens
  ;; ("\\{"   . "\\}")   ;; latex literal braces in math mode
  ;; ("\\("   . "\\)")   ;; capture parens in regexp in various languages
  ;; ("\\\""  . "\\\"")  ;; escaped quotes in strings
  ;; ("/*"    . "*/")    ;; C-like multi-line comment
  ;; ("\""    . "\"")    ;; string double quotes
  ;; ("'"     . "'")     ;; string single quotes/character quotes
  ;; ("("     . ")")     ;; parens (yay lisp)
  ;; ("["     . "]")     ;; brackets
  ;; ("{"     . "}")     ;; braces (a.k.a. curly brackets)
  ;; ("`"     . "`")     ;; latex strings. tap twice for latex double quotes

  ;; Unbind `M-s' (set by paredit keybindings above) because it's bound
  ;; to some handy occur related functions
  ;; (define-key sp-keymap (kbd "M-s") nil)

  ;; org 모드에서 거슬린다. 제거. 굳.
  (sp-local-pair 'org-mode "(" ")" :actions '(rem)) ; for denote completion
  (sp-local-pair 'org-mode "[" "]" :actions '(rem)) ; temporarly
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "`" "`" :actions '(rem))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'org-mode "/" "/" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "~" "~" :actions '(rem))

  ;; markdown 에서도 삭제
  (sp-local-pair 'markdown-mode "(" ")" :actions '(rem))
  (sp-local-pair 'markdown-mode "'" "'" :actions '(rem))
  (sp-local-pair 'markdown-mode "`" "`" :actions '(rem))
  (sp-local-pair 'markdown-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'markdown-mode "/" "/" :actions '(rem))

  ;; pair management
  (sp-with-modes
      '(minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "(" nil :wrap "C-("))
  (sp-with-modes 'markdown-mode (sp-local-pair "**" "***"))
  (sp-with-modes
      'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))

  (sp-with-modes
      'org-mode
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "$$" "$$"))
  )

;;;; tempel

(use-package! tempel
  :bind
  (("M-+" . tempel-complete) ;; Alternative tempel-expand
   ("M-*" . tempel-insert))
  :init
  (setq tempel-path (expand-file-name "var/tempel-templates.eld" doom-user-dir)))

(use-package! tempel-collection
  :after tempel)

;;;; imenu-list

;; Show an outline summary of the current buffer.
(use-package! imenu-list
  :init
  (add-hook 'imenu-list-major-mode-hook #'toggle-truncate-lines)
  (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-position 'left)
  (setq imenu-list-idle-update-delay 1.0) ; default 1.0
  (setq imenu-list-size 45) ; default 0.3
  :config
  ;;;###autoload
  (defun spacemacs/imenu-list-smart-focus ()
    "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
    (interactive)
    (if (get-buffer-window imenu-list-buffer-name t)
        (imenu-list-show)
      (imenu-list-smart-toggle)))
  (after! winum
    (define-key
     winum-keymap
     [remap winum-select-window-8]
     #'spacemacs/imenu-list-smart-focus)))

;;;; buildin annotation with remember

(use-package! remember
  :commands remember
  :init
  (setq
   remember-notes-initial-major-mode 'org-mode
   remember-notes-auto-save-visited-file-name t)
  :config (setq remember-data-file (my/org-remember-file)))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;; (require 'ob-tangle)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: (after! org
;; MOVED-TO-lisp/org-config.el:   (message "after org - config")
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (load-file (concat doom-user-dir "lisp/org-funcs.el"))
;; MOVED-TO-lisp/org-config.el:   ;; (load-file (concat doom-user-dir "lisp/org-config.el"))
;; MOVED-TO-lisp/org-config.el:   ;; (+org-init-keybinds-h) -> 2024-06-01 여기 키바인딩 관련 부분 뒤에서 다시 잡아줌
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-attach-use-inheritance nil) ; selective
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (progn
;; MOVED-TO-lisp/org-config.el:     (setq org-capture-bookmark nil)
;; MOVED-TO-lisp/org-config.el:     (setq org-edit-src-content-indentation 0) ; default 2
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:     )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-id-locations-file
;; MOVED-TO-lisp/org-config.el:         (file-name-concat org-directory (concat "." system-name "-orgids"))) ; ".org-id-locations"))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; overide here! important
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-insert-heading-respect-content nil) ; doom t
;; MOVED-TO-lisp/org-config.el:   ;; org-indent-mode 사용하면 org-hide-leading-stars 자동 on
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-hide-leading-stars nil) ; doom t
;; MOVED-TO-lisp/org-config.el:   )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: (after! org
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-todo-keywords : whhone
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (progn
;; MOVED-TO-lisp/org-config.el:     ;; https://whhone.com/emacs-config/
;; MOVED-TO-lisp/org-config.el:     (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "DONT(o)")))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:     (with-no-warnings
;; MOVED-TO-lisp/org-config.el:       (custom-declare-face '+org-todo-todo  '((t (:inherit (bold error org-todo)))) "")
;; MOVED-TO-lisp/org-config.el:       (custom-declare-face '+org-todo-next  '((t (:inherit (bold warning org-todo)))) "")
;; MOVED-TO-lisp/org-config.el:       (custom-declare-face '+org-todo-done  '((t (:inherit (bold success org-todo)))) "")
;; MOVED-TO-lisp/org-config.el:       (custom-declare-face '+org-todo-dont '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
;; MOVED-TO-lisp/org-config.el:       )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:     (setq org-todo-keyword-faces
;; MOVED-TO-lisp/org-config.el:           '(("TODO" . +org-todo-todo) ;; red
;; MOVED-TO-lisp/org-config.el:             ("DONE" . +org-todo-done) ;; green
;; MOVED-TO-lisp/org-config.el:             ("NEXT" . +org-todo-next) ;; yellow
;; MOVED-TO-lisp/org-config.el:             ("DONT" . +org-todo-dont) ;; green
;; MOVED-TO-lisp/org-config.el:             ))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:     ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; MOVED-TO-lisp/org-config.el:     (setq org-agenda-custom-commands
;; MOVED-TO-lisp/org-config.el:           '(("n" "Agenda / NEXT"
;; MOVED-TO-lisp/org-config.el:              ((agenda "" nil)
;; MOVED-TO-lisp/org-config.el:               (tags "INBOX+LEVEL=2|CATEGORY=\"Inbox\"+LEVEL=1")
;; MOVED-TO-lisp/org-config.el:               (todo "NEXT" nil)
;; MOVED-TO-lisp/org-config.el:               ;; (todo "TODO" nil) ;; 2024-03-18 add
;; MOVED-TO-lisp/org-config.el:               ) nil)
;; MOVED-TO-lisp/org-config.el:             (" " "Agenda and all TODOs" ; default' view
;; MOVED-TO-lisp/org-config.el:              ((agenda "")
;; MOVED-TO-lisp/org-config.el:               (alltodo "")))))
;; MOVED-TO-lisp/org-config.el:     )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; DONT custom agenda files
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; ;; (setq org-agenda-files org-user-agenda-files)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-diary-file (my/org-diary-file))
;; MOVED-TO-lisp/org-config.el:   (setq org-default-notes-file (my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; doom-emacs capture files : absolute path
;; MOVED-TO-lisp/org-config.el:   (setq +org-capture-todo-file (my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el:   (setq +org-capture-notes-file (my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el:   (setq +org-capture-changelog-file (my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el:   (setq +org-capture-projects-file (my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el:   (setq +org-capture-journal-file (my/org-diary-file))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-agenda
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-sticky t) ; default nil
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Shift the agenda to show the previous 3 days and the next 7 days for
;; MOVED-TO-lisp/org-config.el:   ;; better context on your week. The past is less important than the future.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-span 'day) ; default 'week, doom 10
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Hide all scheduled todo.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-todo-ignore-scheduled 'all)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Ignores "far" deadline TODO items from TODO list.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-todo-ignore-deadlines 'far)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Hide all scheduled todo, from tags search view, like tags-todo.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-tags-todo-honor-ignore-options t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Hide all done todo in agenda
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-skip-scheduled-if-done t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Hide task until the scheduled date.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-log-into-drawer t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-log-done 'time)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (setcdr (assoc 'note org-log-note-headings) "%d")
;; MOVED-TO-lisp/org-config.el:   ;; Interstitial Journaling: add note to CLOCK entry after clocking out
;; MOVED-TO-lisp/org-config.el:   ;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
;; MOVED-TO-lisp/org-config.el:   (setq org-log-note-clock-out t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; 4 priorities to model Eisenhower's matrix.
;; MOVED-TO-lisp/org-config.el:   ;; - [#A] means +important +urgent
;; MOVED-TO-lisp/org-config.el:   ;; - [#B] means +important -urgent
;; MOVED-TO-lisp/org-config.el:   ;; - [#C] means -important +urgent
;; MOVED-TO-lisp/org-config.el:   ;; - [#D] means -important -urgent
;; MOVED-TO-lisp/org-config.el:   (setq org-priority-default 68
;; MOVED-TO-lisp/org-config.el:         org-priority-lowest 68)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; diary-file
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq diary-file (concat doom-user-dir "diary"))
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-include-diary t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-agenda-log-mode and clock-mode
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Show all agenda dates - even if they are empty
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-show-all-dates t)
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-start-with-log-mode t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; Agenda log mode items to display (closed clock : default)
;; MOVED-TO-lisp/org-config.el:   ;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
;; MOVED-TO-lisp/org-config.el:   ;; closed    Show entries that have been closed on that day.
;; MOVED-TO-lisp/org-config.el:   ;; clock     Show entries that have received clocked time on that day.
;; MOVED-TO-lisp/org-config.el:   ;; state     Show all logged state changes.
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-agenda-log-mode-items '(closed clock state))
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-log-mode-add-notes nil)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-sort-notime-is-late t) ; Org 9.4
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (org-clock-auto-clockout-insinuate) ; auto-clockout
;; MOVED-TO-lisp/org-config.el:   ;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
;; MOVED-TO-lisp/org-config.el:   (setq org-clock-string-limit 30) ; default 0
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; org-clock-persist for share with machines
;; MOVED-TO-lisp/org-config.el:   (setq org-clock-persist-query-save t)
;; MOVED-TO-lisp/org-config.el:   (setq org-clock-persist-query-resume t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; current  Only the time in the current instance of the clock
;; MOVED-TO-lisp/org-config.el:   ;; today    All time clocked into this task today
;; MOVED-TO-lisp/org-config.el:   ;; repeat   All time clocked into this task since last repeat
;; MOVED-TO-lisp/org-config.el:   ;; all      All time ever recorded for this task
;; MOVED-TO-lisp/org-config.el:   ;; auto     Automatically, either all, or repeat for repeating tasks
;; MOVED-TO-lisp/org-config.el:   (setq org-clock-mode-line-entry t)
;; MOVED-TO-lisp/org-config.el:   (setq org-clock-mode-line-line-total 'auto) ; default nil
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; global Effort estimate values
;; MOVED-TO-lisp/org-config.el:   ;; global STYLE property values for completion
;; MOVED-TO-lisp/org-config.el:   (setq org-global-properties
;; MOVED-TO-lisp/org-config.el:         (quote
;; MOVED-TO-lisp/org-config.el:          (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
;; MOVED-TO-lisp/org-config.el:           ("STYLE_ALL" . "habit"))))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-tag and category
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-auto-align-tags nil) ; default t, use doom's custom
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-tags-column 0) ; default -77
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-show-inherited-tags nil)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-tag-alist (quote (
;; MOVED-TO-lisp/org-config.el:                               (:startgroup) ;; Action
;; MOVED-TO-lisp/org-config.el:                               ("later" . ?.)
;; MOVED-TO-lisp/org-config.el:                               ("now" . ?,)
;; MOVED-TO-lisp/org-config.el:                               (:endgroup)
;; MOVED-TO-lisp/org-config.el:                               ("important" . ?i) ; 별도 처리
;; MOVED-TO-lisp/org-config.el:                               ("waiting" . ?w)
;; MOVED-TO-lisp/org-config.el:                               ("next" . ?n)
;; MOVED-TO-lisp/org-config.el:                               ("hold" . ?h)
;; MOVED-TO-lisp/org-config.el:                               ;; ("crypt" . ?E)
;; MOVED-TO-lisp/org-config.el:                               ("note" . ?o)
;; MOVED-TO-lisp/org-config.el:                               ("noexport" . ?x)
;; MOVED-TO-lisp/org-config.el:                               ("nonum" . ?u)
;; MOVED-TO-lisp/org-config.el:                               ("ATTACH" . ?a)
;; MOVED-TO-lisp/org-config.el:                               ("latest" . ?l) ;; latest version
;; MOVED-TO-lisp/org-config.el:                               )))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-to-list 'org-tags-exclude-from-inheritance "projects") ; projects 왜 구분했었지?
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-agenda-custom-commands
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; ol-doi ol-w3m ol-bbdb ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
;; MOVED-TO-lisp/org-config.el:   ;; ol-eww ol-bibtex
;; MOVED-TO-lisp/org-config.el:   ;; Adapted from http://stackoverflow.com/a/12751732/584121
;; MOVED-TO-lisp/org-config.el:   ;; (require 'org-protocol)
;; MOVED-TO-lisp/org-config.el:   (setq org-protocol-default-template-key "L")
;; MOVED-TO-lisp/org-config.el:   (setq org-modules `(org-habit org-protocol))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-agenda-prefix-format
;; MOVED-TO-lisp/org-config.el:   ;;       '((agenda  . " %i %-14:c%?-12t% s")
;; MOVED-TO-lisp/org-config.el:   ;;         (todo  . " %i %-14:c")
;; MOVED-TO-lisp/org-config.el:   ;;         (tags  . " %i %-14:c")
;; MOVED-TO-lisp/org-config.el:   ;;         (search . " %i %-14:c")))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; https://www.pygopar.com/creating-new-columns-in-org-agenda
;; MOVED-TO-lisp/org-config.el:   ;; Originally from here: https://stackoverflow.com/a/59001859/2178312
;; MOVED-TO-lisp/org-config.el:   (defun gopar/get-schedule-or-deadline-if-available ()
;; MOVED-TO-lisp/org-config.el:     (let ((scheduled (org-get-scheduled-time (point)))
;; MOVED-TO-lisp/org-config.el:           (deadline (org-get-deadline-time (point))))
;; MOVED-TO-lisp/org-config.el:       (if (not (or scheduled deadline))
;; MOVED-TO-lisp/org-config.el:           (format " ")
;; MOVED-TO-lisp/org-config.el:         ;; (format "🗓️ ")
;; MOVED-TO-lisp/org-config.el:         "   ")))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-prefix-format
;; MOVED-TO-lisp/org-config.el:         '((agenda . " %-4e %i %-12:c%?-12t% s ")
;; MOVED-TO-lisp/org-config.el:           (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
;; MOVED-TO-lisp/org-config.el:           (tags . " %i %-12:c")
;; MOVED-TO-lisp/org-config.el:           (search . " %i %-12:c")))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (when IS-TERMUX
;; MOVED-TO-lisp/org-config.el:     (setq org-agenda-prefix-format
;; MOVED-TO-lisp/org-config.el:           '((agenda  . " %i %?-12t% s")
;; MOVED-TO-lisp/org-config.el:             (todo  . " %i ")
;; MOVED-TO-lisp/org-config.el:             (tags  . " %i ")
;; MOVED-TO-lisp/org-config.el:             (search . " %i "))))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-category-icon-alist nil)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-agenda-hide-tags-regexp
;; MOVED-TO-lisp/org-config.el:         "agenda\\|DONT\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-hook 'org-agenda-finalize-hook
;; MOVED-TO-lisp/org-config.el:             (lambda ()
;; MOVED-TO-lisp/org-config.el:               ;; (setq-local line-spacing 0.2)
;; MOVED-TO-lisp/org-config.el:               (define-key org-agenda-mode-map [(double-mouse-1)] 'org-agenda-goto-mouse)))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (defun cc/org-agenda-goto-now ()
;; MOVED-TO-lisp/org-config.el:     "Redo agenda view and move point to current time '← now'"
;; MOVED-TO-lisp/org-config.el:     (interactive)
;; MOVED-TO-lisp/org-config.el:     (org-agenda-redo)
;; MOVED-TO-lisp/org-config.el:     (org-agenda-goto-today)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:     (if window-system
;; MOVED-TO-lisp/org-config.el:         (search-forward "← now ─")
;; MOVED-TO-lisp/org-config.el:       (search-forward "now -"))
;; MOVED-TO-lisp/org-config.el:     )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-hook 'org-agenda-mode-hook
;; MOVED-TO-lisp/org-config.el:             (lambda ()
;; MOVED-TO-lisp/org-config.el:               (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
;; MOVED-TO-lisp/org-config.el:               (define-key org-agenda-mode-map (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)
;; MOVED-TO-lisp/org-config.el:               (define-key org-agenda-mode-map (kbd "DEL") #'evil-switch-to-windows-last-buffer)
;; MOVED-TO-lisp/org-config.el:               ;; (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
;; MOVED-TO-lisp/org-config.el:               ;; (define-key org-agenda-mode-map (kbd "M-P") 'ash/org-pomodoro-til-meeting)
;; MOVED-TO-lisp/org-config.el:               (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-archive-location "archives/%s_archive::")
;; MOVED-TO-lisp/org-config.el:   (setq org-archive-location (file-name-concat org-directory "archives/%s::"))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; nil 이면 C-c C-o 으로 접근한다.
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-mouse-1-follows-link t) ; default 450
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-capture-template-dir (concat doom-user-dir "captures/"))
;; MOVED-TO-lisp/org-config.el:   (setq org-datetree-add-timestamp t)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; Simple is Better
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
;; MOVED-TO-lisp/org-config.el:   (setq org-capture-templates nil)
;; MOVED-TO-lisp/org-config.el:   (add-to-list
;; MOVED-TO-lisp/org-config.el:    'org-capture-templates
;; MOVED-TO-lisp/org-config.el:    `("i" "Inbox" entry (file+headline ,(my/org-inbox-file) "Inbox")
;; MOVED-TO-lisp/org-config.el:      "* %?\n%i\n%a"))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-to-list
;; MOVED-TO-lisp/org-config.el:    'org-capture-templates
;; MOVED-TO-lisp/org-config.el:    `("I" "Inbox (Work)" entry (file+headline ,(my/org-inbox-file) "Inbox")
;; MOVED-TO-lisp/org-config.el:      "* %? :work:\n%i\n%a"))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-to-list
;; MOVED-TO-lisp/org-config.el:    'org-capture-templates
;; MOVED-TO-lisp/org-config.el:    `("p" "Project /w template" entry (file+headline ,(my/org-inbox-file) "Projects")
;; MOVED-TO-lisp/org-config.el:      (file ,(concat org-capture-template-dir "project.capture"))))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (add-to-list
;; MOVED-TO-lisp/org-config.el:   ;;  'org-capture-templates
;; MOVED-TO-lisp/org-config.el:   ;;  `("l" "links" entry (file ,(my/org-links-file))
;; MOVED-TO-lisp/org-config.el:   ;;    "* TODO %(org-cliplink-capture)" :immediate-finish t))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-to-list
;; MOVED-TO-lisp/org-config.el:    'org-capture-templates
;; MOVED-TO-lisp/org-config.el:    `("T" "Personal Todo /w clock-in" entry (file ,(my/org-inbox-file))
;; MOVED-TO-lisp/org-config.el:      "* TODO [#C] %?\n%T\n%a\n" :clock-in t :clock-resume t))
;; MOVED-TO-lisp/org-config.el:   )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; org-journal
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: (progn
;; MOVED-TO-lisp/org-config.el:   (require 'org-journal)
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-dir (concat user-org-directory "journal"))
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-date-format "%Y-%m-%d %A") ; Week%W:
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-journal-time-format "%R ")
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-enable-agenda-integration t) ; default nil
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-file-type 'weekly)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil
;; MOVED-TO-lisp/org-config.el:   )
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: ;;;; citar
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el: (progn
;; MOVED-TO-lisp/org-config.el:   (require 'citar)
;; MOVED-TO-lisp/org-config.el:   ;; MOVE to +denote-export.el
;; MOVED-TO-lisp/org-config.el:   ;; (setq citar-bibliography config-bibfiles)
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-cite-global-bibliography config-bibfiles)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; ;; use #+cite_export: csl apa.csl
;; MOVED-TO-lisp/org-config.el:   ;; (setq org-cite-csl-styles-dir (concat org-directory ".csl"))
;; MOVED-TO-lisp/org-config.el:   ;; (setq citar-citeproc-csl-styles-dir (concat org-directory ".csl"))
;; MOVED-TO-lisp/org-config.el:   ;; ;; (setq citar-citeproc-csl-locales-dir "~/.csl/locales")
;; MOVED-TO-lisp/org-config.el:   ;; (setq citar-citeproc-csl-style "apa.csl") ; ieee.csl
;; MOVED-TO-lisp/org-config.el:   ;; (setq citar-symbol-separator " ")
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   ;; (require 'citar-citeproc)
;; MOVED-TO-lisp/org-config.el:   ;; (setq citar-format-reference-function 'citar-citeproc-format-reference)
;; MOVED-TO-lisp/org-config.el:   (setq citar-format-reference-function 'citar-format-reference)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (setq
;; MOVED-TO-lisp/org-config.el:    citar-templates
;; MOVED-TO-lisp/org-config.el:    '((main . ;; [${urldate:10}]
;; MOVED-TO-lisp/org-config.el:       "[${dateadded:10}] \{${datemodified:10}\} ${author editor:20} ${translator:8} (${date year issued:4}) @${=key= id:16} ${title:68} ")  ; 2024-09-12 김정한
;; MOVED-TO-lisp/org-config.el:      (suffix
;; MOVED-TO-lisp/org-config.el:       . "${shorttitle:25} ${=type=:10} ${namea:16} ${url:20} ${tags keywords:*}") ; 2024-11-17 add url
;; MOVED-TO-lisp/org-config.el:      (preview
;; MOVED-TO-lisp/org-config.el:       .
;; MOVED-TO-lisp/org-config.el:       "${title} :${year issued date:4}\n- ${author} ${translator} ${namea}\n- ${abstract}\n- ${shorttitle}") ; citar-copy-reference
;; MOVED-TO-lisp/org-config.el:      (note . "#+title: ${author translator:10}, ${title}")))
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
;; MOVED-TO-lisp/org-config.el:   (setq bibtex-dialect 'biblatex)
;; MOVED-TO-lisp/org-config.el:   (setq bibtex-align-at-equal-sign t)
;; MOVED-TO-lisp/org-config.el:   (setq bibtex-text-indentation 20)
;; MOVED-TO-lisp/org-config.el: 
;; MOVED-TO-lisp/org-config.el:   (with-eval-after-load 'savehist
;; MOVED-TO-lisp/org-config.el:     (add-to-list 'savehist-additional-variables 'citar-history))
;; MOVED-TO-lisp/org-config.el:   )

;;;;; denote confuguration

(use-package! denote
  :demand t
  :commands
  (denote denote-create-note denote-insert-link denote-show-backlinks-buffer denote-link-ol-store)
  :init
  (setq denote-directory org-directory)
  (require 'denote-org)
  (require 'denote-silo)
  (require 'denote-sequence)
  ;; (require 'denote-journal)
  (require 'denote-org)
  (require 'denote-markdown)

  (setq denote-file-type 'org)
  (setq denote-sort-components '(signature title keywords identifier))
  (setq denote-backlinks-show-context nil)
  (setq denote-sort-keywords t)
  (setq denote-infer-keywords t)
  (setq denote-excluded-directories-regexp "screenshot")
  (setq denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+hugo_lastmod: %2$s
#+date:       %2$s
#+identifier: %4$s
#+export_file_name: %4$s.md
#+description:
#+hugo_tags: temp
#+hugo_categories: Noname
#+print_bibliography:\n* History\n- %2$s\n* Related-Notes\n\n")

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
  (setq denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech

  ;; More functionality
  (setq denote-org-store-link-to-heading nil ; default t
        denote-rename-confirmations nil ; default '(rewrite-front-matter modify-file-name)
        denote-save-buffers t) ; default nil
  (add-hook 'org-mode-hook (lambda ()
                             (setq denote-rename-buffer-backlinks-indicator "¶")
                             (setq denote-rename-buffer-format "%t%b")
                             (denote-rename-buffer-mode +1)))

  ;; for claude memory integration
  ;; 동적 Silo 관리는 +denote-silo-dynamic.el에서 처리됨
  ;; (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/"))

  ;; (use-package! consult-notes
  ;;   :defer 2
  ;;   :commands (consult-notes consult-notes-search-in-all-notes)
  ;;   :config
  ;;   (setq consult-notes-denote-display-id t)
  ;;   (setq consult-notes-denote-dir t)
  ;;   (setq consult-notes-denote-title-margin 2) ; 24
  ;;   (consult-notes-denote-mode 1)
  ;;   )

  (use-package! citar-denote
    :demand t ;; Ensure minor mode is loaded
    :bind (:map org-mode-map
           ("C-c B" . citar-insert-citation)
           :map minibuffer-local-map
           ("M-r" . vertico-repeat))
    :commands
    (citar-create-note citar-open-notes citar-denote-open citar-denote-add-citekey)
    :init
    (require 'bibtex)
    (require 'citar)
    :custom
    ;; (citar-open-always-create-notes t)
    ;; (citar-denote-signature t)
    (citar-denote-file-type 'org)
    (citar-denote-subdir t)
    (citar-denote-keyword "bib")
    (citar-denote-title-format "author-year-title") ; default title
    (citar-denote-use-bib-keywords nil)
    (citar-denote-title-format-authors 1)
    (citar-denote-title-format-andstr "and")
    :config
    ;; FIXME for denote-obsidian
    (setq citar-denote-file-types
          `((org
             :reference-format "#+reference:  %s\n"
             :reference-regex "^#\\+reference\\s-*:")
            (markdown-obsidian ;; 2025-02-03
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-yaml
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-toml
             :reference-format "reference  = %s\n"
             :reference-regex "^reference\\s-*=")
            (text
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")))
    (citar-denote-mode))
  )

;;;; denote-explore

(use-package! denote-explore)

;;;; denote-search

(use-package! denote-search)

;;;; Ten with etags

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; (use-package! ten
;;   :defer 2
;;   ;; :hook ((org-mode Info-mode) . ten-font-lock-mode) ;; text-mode
;;   :init
;;   (setq ten-exclude-regexps '("/\\."))
;;   :config
;;   (require 'consult-ten)
;;   (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
;;   )

;;;; gptel

(use-package! gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-temperature 0.3) ; gptel 1.0, Perplexity 0.2
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4

  (load! "+gptel")

  (with-eval-after-load 'gptel-org
    (defun gptel-org-toggle-branching-context ()
      "Toggle gptel context between doc and subheading."
      (interactive)
      (if gptel-org-branching-context
          (progn
            (setq-local gptel-org-branching-context nil)
            (message "Context: whole doc"))
        (setq-local gptel-org-branching-context t)
        (message "Context: subheading")))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user: "
          (alist-get 'org-mode gptel-response-prefix-alist) "@assistant:\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
    (setq-default gptel-org-branching-context t))
  )

;;;; claude-code

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

;;;; doom-modeline

(setq doom-modeline-time nil)
(setq doom-modeline-time-icon nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-support-imenu t)
(setq doom-modeline-enable-word-count nil)
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mod)) ; org-mode

(after! doom-modeline
  (setq doom-modeline-icon nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)

  (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 4)

  (setq doom-modeline-persp-name t) ; doom nil
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ; default 'auto

  (setq doom-modeline-repl t)
  (setq doom-modeline-github t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-hud nil))

;;;; outli

(use-package! outli
  :defer 1
  :init (setq outli-speed-commands nil)
  :hook (prog-mode . outli-mode)
  :config
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
  (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))
  )

;;;; themes

;; doom-themes
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic nil) ; if nil, italics is universally disabled

;; 터미널에서 테마 색상 충돌 방지
(unless (display-graphic-p)
  ;; 터미널에서 배경색 투명도 유지
  (setq-default frame-background-mode 'dark)
  ;; 터미널 색상 팔레트 활용
  (setq xterm-color-preserve-properties t)

  ;; Ghostty 터미널 전용 설정
  (cond
   ;; xterm-ghostty terminfo 사용시
   ((string-match "ghostty" (or (getenv "TERM") ""))
    ;; Ghostty는 24비트 트루컬러 지원 (이미 terminfo에 정의됨)
    (setenv "COLORTERM" "truecolor")
    ;; 배경 투명도 유지
    (set-face-background 'default "unspecified-bg" nil)
    ;; 터미널 자체 색상 테마 우선
    (setq-default terminal-ansi-color-vector
                  [unspecified "#282a36" "#ff5555" "#50fa7b" "#f1fa8c"
                               "#6272a4" "#ff79c6" "#8be9fd" "#f8f8f2"])
    ;; Ghostty는 256색상 이상 지원 (terminfo pairs=0x7fff)
    (setq xterm-color-use-bold-for-bright nil)
    ;; Ghostty 최적화 설정
    (add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-direct"))
    )

   ;; 일반 256color 터미널
   ((string-match "256color" (or (getenv "TERM") ""))
    (setq xterm-color-names-bright
          ["#3B4252" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#E5E9F0"])
    (setq xterm-color-names
          ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#D8DEE9"]))))

(defun my/doom-themes-toggle ()
  (interactive)
  (setq doom-theme 'doom-one)
  (doom-themes-visual-bell-config)
  (load-theme doom-theme t))
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

  (setq agent-shell-qwen-authentication
        (agent-shell-qwen-make-authentication :login t))

  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :login t))
  ;; (setq agent-shell-openai-authentication
  ;;       (agent-shell-openai-make-authentication :login t))

  (setq agent-shell--transcript-file-path-function #'agent-shell--default-transcript-file-path)
  (setq agent-shell-header-style nil)

  (require 'agent-shell-manager)
  (setq agent-shell-manager-side 'bottom)  ; Options: 'left, 'right, 'top, 'bottom
  (map! :n "s-;" #'agent-shell-manager-toggle)
  (map! :map agent-shell-mode-map :inv "M-h" #'other-window)

  (require 'agent-shell-sidebar)
  (setq agent-shell-sidebar-width "25%"
        agent-shell-sidebar-minimum-width 80
        agent-shell-sidebar-maximum-width "50%"
        agent-shell-sidebar-position 'right
        agent-shell-sidebar-locked t
        agent-shell-sidebar-default-config (agent-shell-anthropic-make-claude-code-config))

  ;; agent-shell 버퍼를 실제 버퍼로 표시 (버퍼 목록에서 보이게)
  (add-hook 'agent-shell-mode-hook #'doom-mark-buffer-as-real-h)
  )

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
