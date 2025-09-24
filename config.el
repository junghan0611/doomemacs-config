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
(load! "+user-info")

;;; Load 'Per-Machine' - User Configs
;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.

(let ((per-machine-filename (concat doom-user-dir "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;; Load 'user-keys'

(let ((user-keys-filename (concat doom-user-dir "user-keys.el")))
  (when (file-exists-p user-keys-filename)
    (load-file user-keys-filename)))

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

;;; Leader key

;; Over-ride or add to Doom Emacs default key bindings
;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153
;; 'M-m', '\,' 'SPC m' for localleader
(setq doom-localleader-key ","
      doom-localleader-alt-key "C-,")

(defun my/call-localleader ()
  (interactive)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8-unix)

  (set-selection-coding-system 'utf-8) ;; important
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8)

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  (setq-default line-spacing 3)

  ;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
  (setq system-time-locale "C")

  (setq input-method-verbose-flag nil
        input-method-highlight-flag nil)

  (global-set-key (kbd "<S-SPC>") 'toggle-input-method)
  ;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
  (global-set-key (kbd "<Hangul>") 'toggle-input-method)
  ;; (global-unset-key (kbd "S-SPC"))

  (unless (string-equal system-type "android")
;;;###autoload
    (defun my/set-emoji-symbol-font ()
      (interactive)

      ;; 터미널에서 폰트 스케일 조정 (이모지 크기 일정하게)
      (unless (display-graphic-p)
        (setq face-font-rescale-alist
              '(("Noto Color Emoji" . 0.9)
                ("Noto Emoji" . 0.9)
                ("Symbola" . 0.9))))

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
        (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Color Emoji") 'append)
        ;; 폴백 폰트 설정 (Noto Emoji가 없는 경우)
        (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans Mono") nil 'append)
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
  )

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
        "M-k" #'vertico-previous))

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
  (evil-escape-mode 1))

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

;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-19: Check the `templates'
(use-package! tempel
  :bind
  (("M-+" . tempel-complete) ;; Alternative tempel-expand
   ("M-*" . tempel-insert))
  :config
  ;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift
  (setq tempel-path (expand-file-name "/var/tempel-templates.eld" doom-user-dir))

  ;; Use concrete keys because of org mode
  ;; "M-RET" #'tempel-done
  ;; "M-{" #'tempel-previous
  ;; "M-}" #'tempel-next
  ;; "M-<up>" #'tempel-previous
  ;; "M-<down>" #'tempel-next

  ;; 2023-10-19 disable my custom
  (define-key tempel-map (kbd "RET") #'tempel-done)
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)

  (use-package! tempel-collection))

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

;;;; org

;; (require 'ob-tangle)

(after! org
  (message "after org - config")

  ;; (load-file (concat doom-user-dir "lisp/org-funcs.el"))
  ;; (load-file (concat doom-user-dir "lisp/org-config.el"))
  ;; (+org-init-keybinds-h) -> 2024-06-01 여기 키바인딩 관련 부분 뒤에서 다시 잡아줌
  ;; (setq org-attach-use-inheritance nil) ; selective

  (progn
    (setq org-capture-bookmark nil)
    (setq org-edit-src-content-indentation 0) ; default 2

    )

  (setq org-id-locations-file
        (file-name-concat org-directory (concat "." system-name "-orgids"))) ; ".org-id-locations"))

  ;; overide here! important
  ;; (setq org-insert-heading-respect-content nil) ; doom t
  ;; org-indent-mode 사용하면 org-hide-leading-stars 자동 on
  ;; (setq org-hide-leading-stars nil) ; doom t
  )

(after! org

;;;; org-todo-keywords : whhone

  (progn
    ;; https://whhone.com/emacs-config/
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "DONT(o)")))

    (with-no-warnings
      (custom-declare-face '+org-todo-todo  '((t (:inherit (bold error org-todo)))) "")
      (custom-declare-face '+org-todo-next  '((t (:inherit (bold warning org-todo)))) "")
      (custom-declare-face '+org-todo-done  '((t (:inherit (bold success org-todo)))) "")
      (custom-declare-face '+org-todo-dont '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
      )

    (setq org-todo-keyword-faces
          '(("TODO" . +org-todo-todo) ;; red
            ("DONE" . +org-todo-done) ;; green
            ("NEXT" . +org-todo-next) ;; yellow
            ("DONT" . +org-todo-dont) ;; green
            ))

    ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    (setq org-agenda-custom-commands
          '(("n" "Agenda / NEXT"
             ((agenda "" nil)
              (tags "INBOX+LEVEL=2|CATEGORY=\"Inbox\"+LEVEL=1")
              (todo "NEXT" nil)
              ;; (todo "TODO" nil) ;; 2024-03-18 add
              ) nil)
            (" " "Agenda and all TODOs" ; default' view
             ((agenda "")
              (alltodo "")))))
    )

;;;; DONT custom agenda files

  ;; ;; (setq org-agenda-files org-user-agenda-files)

  (setq org-agenda-diary-file (my/org-diary-file))
  (setq org-default-notes-file (my/org-inbox-file))

  ;; doom-emacs capture files : absolute path
  (setq +org-capture-todo-file (my/org-inbox-file))
  (setq +org-capture-notes-file (my/org-inbox-file))
  (setq +org-capture-changelog-file (my/org-inbox-file))
  (setq +org-capture-projects-file (my/org-inbox-file))
  (setq +org-capture-journal-file (my/org-diary-file))

;;;; org-agenda

  ;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
  (setq org-agenda-sticky t) ; default nil

  ;; Shift the agenda to show the previous 3 days and the next 7 days for
  ;; better context on your week. The past is less important than the future.
  (setq org-agenda-span 'day) ; default 'week, doom 10

  ;; Hide all scheduled todo.
  (setq org-agenda-todo-ignore-scheduled 'all)

  ;; Ignores "far" deadline TODO items from TODO list.
  (setq org-agenda-todo-ignore-deadlines 'far)

  ;; Hide all scheduled todo, from tags search view, like tags-todo.
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; Hide all done todo in agenda
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Hide task until the scheduled date.
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  (setq org-log-into-drawer t)

  (setq org-log-done 'time)

  ;; (setcdr (assoc 'note org-log-note-headings) "%d")
  ;; Interstitial Journaling: add note to CLOCK entry after clocking out
  ;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
  (setq org-log-note-clock-out t)

  ;; 4 priorities to model Eisenhower's matrix.
  ;; - [#A] means +important +urgent
  ;; - [#B] means +important -urgent
  ;; - [#C] means -important +urgent
  ;; - [#D] means -important -urgent
  (setq org-priority-default 68
        org-priority-lowest 68)

;;;; diary-file

  (setq diary-file (concat doom-user-dir "diary"))
  (setq org-agenda-include-diary t)

;;;; org-agenda-log-mode and clock-mode

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-with-log-mode t)

  ;; Agenda log mode items to display (closed clock : default)
  ;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
  ;; closed    Show entries that have been closed on that day.
  ;; clock     Show entries that have received clocked time on that day.
  ;; state     Show all logged state changes.
  ;; (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-log-mode-add-notes nil)

  ;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

  ;; (org-clock-auto-clockout-insinuate) ; auto-clockout
  ;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
  (setq org-clock-string-limit 30) ; default 0

  ;; org-clock-persist for share with machines
  (setq org-clock-persist-query-save t)
  (setq org-clock-persist-query-resume t)

  ;; current  Only the time in the current instance of the clock
  ;; today    All time clocked into this task today
  ;; repeat   All time clocked into this task since last repeat
  ;; all      All time ever recorded for this task
  ;; auto     Automatically, either all, or repeat for repeating tasks
  (setq org-clock-mode-line-entry t)
  (setq org-clock-mode-line-line-total 'auto) ; default nil

  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties
        (quote
         (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
          ("STYLE_ALL" . "habit"))))

;;;; org-tag and category

  ;; (setq org-auto-align-tags nil) ; default t, use doom's custom
  ;; (setq org-tags-column 0) ; default -77
  (setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column
  (setq org-agenda-show-inherited-tags nil)

  (setq org-tag-alist (quote (
                              (:startgroup) ;; Action
                              ("later" . ?.)
                              ("now" . ?,)
                              (:endgroup)
                              ("important" . ?i) ; 별도 처리
                              ("waiting" . ?w)
                              ("next" . ?n)
                              ("hold" . ?h)
                              ;; ("crypt" . ?E)
                              ("note" . ?o)
                              ("noexport" . ?x)
                              ("nonum" . ?u)
                              ("ATTACH" . ?a)
                              ("latest" . ?l) ;; latest version
                              )))

  (add-to-list 'org-tags-exclude-from-inheritance "projects") ; projects 왜 구분했었지?

;;;; org-agenda-custom-commands

  ;; ol-doi ol-w3m ol-bbdb ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
  ;; ol-eww ol-bibtex
  ;; Adapted from http://stackoverflow.com/a/12751732/584121
  ;; (require 'org-protocol)
  (setq org-protocol-default-template-key "L")
  (setq org-modules `(org-habit org-protocol))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda  . " %i %-14:c%?-12t% s")
  ;;         (todo  . " %i %-14:c")
  ;;         (tags  . " %i %-14:c")
  ;;         (search . " %i %-14:c")))

  ;; https://www.pygopar.com/creating-new-columns-in-org-agenda
  ;; Originally from here: https://stackoverflow.com/a/59001859/2178312
  (defun gopar/get-schedule-or-deadline-if-available ()
    (let ((scheduled (org-get-scheduled-time (point)))
          (deadline (org-get-deadline-time (point))))
      (if (not (or scheduled deadline))
          (format " ")
        ;; (format "🗓️ ")
        "   ")))

  (setq org-agenda-prefix-format
        '((agenda . " %-4e %i %-12:c%?-12t% s ")
          (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))

  (when IS-TERMUX
    (setq org-agenda-prefix-format
          '((agenda  . " %i %?-12t% s")
            (todo  . " %i ")
            (tags  . " %i ")
            (search . " %i "))))

  (setq org-agenda-category-icon-alist nil)

  (setq org-agenda-hide-tags-regexp
        "agenda\\|DONT\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              ;; (setq-local line-spacing 0.2)
              (define-key org-agenda-mode-map [(double-mouse-1)] 'org-agenda-goto-mouse)))

  (defun cc/org-agenda-goto-now ()
    "Redo agenda view and move point to current time '← now'"
    (interactive)
    (org-agenda-redo)
    (org-agenda-goto-today)

    (if window-system
        (search-forward "← now ─")
      (search-forward "now -"))
    )

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)
              (define-key org-agenda-mode-map (kbd "DEL") #'evil-switch-to-windows-last-buffer)
              ;; (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
              ;; (define-key org-agenda-mode-map (kbd "M-P") 'ash/org-pomodoro-til-meeting)
              (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))

  ;; (setq org-archive-location "archives/%s_archive::")
  (setq org-archive-location (file-name-concat org-directory "archives/%s::"))

  ;; nil 이면 C-c C-o 으로 접근한다.
  ;; (setq org-mouse-1-follows-link t) ; default 450

  (setq org-capture-template-dir (concat doom-user-dir "captures/"))
  (setq org-datetree-add-timestamp t)

;;;; Simple is Better

  ;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
  (setq org-capture-templates nil)
  (add-to-list
   'org-capture-templates
   `("i" "Inbox" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %?\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("I" "Inbox (Work)" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %? :work:\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("p" "Project /w template" entry (file+headline ,(my/org-inbox-file) "Projects")
     (file ,(concat org-capture-template-dir "project.capture"))))

  ;; (add-to-list
  ;;  'org-capture-templates
  ;;  `("l" "links" entry (file ,(my/org-links-file))
  ;;    "* TODO %(org-cliplink-capture)" :immediate-finish t))

  (add-to-list
   'org-capture-templates
   `("T" "Personal Todo /w clock-in" entry (file ,(my/org-inbox-file))
     "* TODO [#C] %?\n%T\n%a\n" :clock-in t :clock-resume t))
  )

(with-eval-after-load 'org
  (require 'ox-hugo)

  ;; (setq org-hugo-base-dir (file-truename "~/git/blog/"))
  (setq org-hugo-base-dir user-hugo-blog-dir) ;; 2024-10-07 fix quartz

  (setq org-hugo-auto-set-lastmod t
        org-hugo-suppress-lastmod-period 3600.0) ; 3600.0 1h, (86400.0) 24h, (172800.0) 48h
  (setq org-hugo-front-matter-format 'yaml)

  ;; My blog is created using Hugo and ox-hugo. It generates better markdown than what you would get using org-md-export!
  ;; It works well out-of-the-box. However, extra configuration is required to embed video.
  ;; In ox-hugo, uses #+begin_video to generate the <video> HTML5 tag (details in ox-hugo/issues/274).
  ;; In Hugo config, set markup.goldmark.renderer.unsafe to true (details in discourse.gohugo.io).
  (add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

  (setq org-hugo-section "notes") ; 2024-04-26 change
  (setq org-hugo-paired-shortcodes "mermaid callout cards details tabs") ; hint sidenote

  ;; https://ox-hugo.scripter.co/doc/formatting/
  ;; if org-hugo-use-code-for-kbd is non-nil
  ;; Requires CSS to render the <kbd> tag as something special.
  ;; eg: ~kbd~
  ;; (setq org-hugo-use-code-for-kbd t)

  ;; https://ox-hugo.scripter.co/doc/linking-numbered-elements/

  ;; org-export-dictionary 에 Figure, Table 에 한글 번역을 넣으면
  ;; 한글로 바뀌어 export 될 것이다.
  (setq org-hugo-link-desc-insert-type t)

  ;; 내보낼 때는 fill-column 끈다.
  (setq org-hugo-preserve-filling nil) ; important

  (setq org-hugo-allow-spaces-in-tags t) ; default t
  (setq org-hugo-prefer-hyphen-in-tags t) ; default t

  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "images") ; imgs
  ;; (setq org-hugo-default-static-subdirectory-for-externals "~/git/temp/notes.junghanacs.com/quartz/static/images") ; imgs

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

  ;; In that normal example of the sidenote, ox-hugo trims the whitespace around
  ;; the sidenote block. That is configured by customizing the
  ;; org-hugo-special-block-type-properties variable:
  (progn
    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("details" :raw t)))
  ;; (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

  ;; If this property is set to an empty string, this heading will not be auto-inserted.
  ;; default value is 'References'
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References")

  (defun my/insert-white-space ()
    (interactive)
    (insert " "))

  (defun +org-export-remove-white-space (text _backend _info)
    "Remove zero width spaces from TEXT."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string " " "" text)))
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-white-space t)
  (evil-define-key '(insert normal) text-mode-map (kbd "M-m") #'my/insert-white-space)
  )

;;;; org-journal

(progn
  (require 'org-journal)
  (setq org-journal-dir (concat user-org-directory "journal"))
  (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
  (setq org-journal-date-format "%Y-%m-%d %A") ; Week%W:

  ;; (setq org-journal-time-format "%R ")
  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")

  (setq org-journal-enable-agenda-integration t) ; default nil
  (setq org-journal-file-type 'weekly)

  (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil
  )

;;;; citar

(progn
  (require 'citar)
  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)

  ;; use #+cite_export: csl apa.csl
  (setq org-cite-csl-styles-dir (concat org-directory ".csl"))
  (setq citar-citeproc-csl-styles-dir (concat org-directory ".csl"))
  ;; (setq citar-citeproc-csl-locales-dir "~/.csl/locales")
  (setq citar-citeproc-csl-style "apa.csl") ; ieee.csl
  (setq citar-symbol-separator " ")

  ;; (require 'citar-citeproc)
  ;; (setq citar-format-reference-function 'citar-citeproc-format-reference)
  (setq citar-format-reference-function 'citar-format-reference)

  (setq
   citar-templates
   '((main . ;; [${urldate:10}]
      "[${dateadded:10}] \{${datemodified:10}\} ${author editor:20} ${translator:8} (${date year issued:4}) @${=key= id:16} ${title:68} ")  ; 2024-09-12 김정한
     (suffix
      . "${shorttitle:25} ${=type=:10} ${namea:16} ${url:20} ${tags keywords:*}") ; 2024-11-17 add url
     (preview
      .
      "${title} :${year issued date:4}\n- ${author} ${translator} ${namea}\n- ${abstract}\n- ${shorttitle}") ; citar-copy-reference
     (note . "#+title: ${author translator:10}, ${title}")))

  (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'citar-history))
  )

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
  (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/"))

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

  ;; (set-popup-rule! "^\\*claude" :vslot -15 :width 90 :side 'right :ttl t :select t :quit nil :modeline t)
  (set-popup-rule! "^\\*claude" :vslot -15 :size 0.4 :side 'bottom :ttl t :select t :quit nil :modeline t)

  (claude-code-mode)

  (add-hook 'claude-code-start-hook
            (lambda ()
              ;; Only increase scrollback for vterm backend
              (when (eq claude-code-terminal-backend 'vterm)
                ;; (setq-local x-gtk-use-native-input t)
                (define-key claude-code-command-map (kbd "M-RET") 'claude-code--vterm-send-alt-return)
                (define-key vterm-mode-map (kbd "M-RET") 'claude-code--vterm-send-alt-return)
                (setq-local vterm-max-scrollback 100000))))
  )


;;;; claude-code-ide

(use-package! claude-code-ide
  :init
  ;; Open Claude at the bottom with custom height
  (setq claude-code-ide-window-side 'bottom
        claude-code-ide-window-width 80
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

  (add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!
  ;; (add-hook 'org-mode-hook 'outli-mode)
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

;;;; jinx for spell

(use-package! jinx
  :config
  (setq jinx-delay 0.5) ; default 0.2
  ;; (dolist (hook '(text-mode-hook conf-mode-hook)) ; prog-mode-hook
  ;;   (add-hook hook #'jinx-mode))

  ;; (add-hook 'org-mode-hook #'jinx-mode)
  ;; (add-hook 'prog-mode-hook #'jinx-mode) ; 주석
  (setq jinx-languages "ko")
  ;; (setq jinx-exclude-regexps
  ;;       '((t "[A-Za-z]" "[']")))
  (setq jinx-exclude-regexps
        '((emacs-lisp-mode "Package-Requires:.*$")
          (t "[A-Za-z]" "[']" "[A-Z]+\\>" "-+\\>" "\\w*?[0-9]\\w*\\>" "[a-z]+://\\S-+" "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" "\\(?:Local Variables\\|End\\):\\s-*$" "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))

  ;; C-; embark-dwim
  ;; C-: 점 앞의 철자가 틀린 단어에 대한 수정을 트리거합니다.
  ;; C-u M-$전체 버퍼에 대한 수정을 트리거합니다.
  (keymap-global-set "C-:" #'jinx-correct)
  (keymap-global-set "C-M-$" #'jinx-languages)

  ;; /tecosaur-dot-doom/config.org
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! ispell
    (global-set-key [remap ispell-word] #'jinx-correct))
  )

;; (setq rmh-elfeed-org-files '("path/to/your/elfeed/file.org")) ; default ~/org/elfeed.org
;; gc copy-link
(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq elfeed-search-filter "@6-months-ago") ;;  "", "@1-month-ago +unread"
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

;;;; vterm for TERMUX

(when IS-TERMUX
  ;; (after! vterm
  ;;   ;; (setq vterm-shell (concat root-path "usr/bin/zsh"))
  ;;   )

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

;; (use-package! term-keys
;;   :unless window-system
;;   :config
;;   (unless (display-graphic-p) ; terminal
;;     (term-keys-mode t)))

;; README /doomemacs-junghan0611/lisp/doom-ui.el
;; Terminal Mode
(unless (display-graphic-p) ; terminal
  (setq visible-cursor nil)
  (xterm-mouse-mode -1) ; important
  (setq fast-but-imprecise-scrolling nil)
  (setq hscroll-step 0)
  (show-paren-mode -1)
  )

;;; tramp

(progn
  (require 'tramp)
  (tramp-set-completion-function "ssh"
                                 '(
                                   (tramp-parse-sconfig "~/.ssh/config"))))

;;; global-unset-key

(global-unset-key (kbd "<f2>"))

(global-unset-key (kbd "M-a"))  ; unset forward-sentence -> use ')'
(global-unset-key (kbd "M-c"))  ; unset capitalize-word
(global-unset-key (kbd "M-e"))  ; unset backward-sentence -> use '('

;;; Emacs Keys

(global-set-key (kbd "C-M-;") 'pp-eval-expression) ; unbinded key
(global-set-key (kbd "C-M-'") 'eldoc-toggle) ; unbinded key

;;;; embark

(global-set-key (kbd "M-y") #'consult-yank-pop) ; yank-pop
(global-set-key (kbd "M-o") 'embark-act) ;; spacemacs bindings
(global-set-key (kbd "M-O") 'embark-dwim) ;; good alternative: M-.

(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'my/org-store-link-id-optional)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; persp-mode and projectile in different prefixes
;; (setq! persp-keymap-prefix (kbd "C-c w"))
;; (after! projectile
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(global-set-key (kbd "M-u") 'evil-scroll-up)
(global-set-key (kbd "M-v") 'evil-scroll-down)

;;;; Extra Fn-key

(when (locate-library "imenu-list")
  (global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
  (global-set-key (kbd "M-<f8>") 'spacemacs/imenu-list-smart-focus))

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."

  "b" #'org-cite-insert

  "c" #'citar-open

  "d" #'citar-denote-dwim
  ;; "e" #'citar-open-entry
  "e" #'citar-denote-open-reference-entry

  "a" #'citar-denote-add-reference
  "1" #'citar-denote-find-citation ;; grep [cite @xxx]

  "i" #'citar-insert-citation
  "n" #'citar-create-note
  "o" #'citar-denote-open-note
  "O" #'citar-open-links

  "f" #'citar-denote-find-reference
  "l" #'citar-denote-link-reference

  "s" #'citar-denote-create-silo-note
  "k" #'citar-denote-remove-reference
  )

(defvar-keymap ews-denote-map
  :doc "Denote keybindings."
  "b" ews-bibliography-map
  "B" #'denote-org-backlinks-for-heading
  "d" #'denote-create-note

  "f" #'+default/find-in-notes ; find-files
  ;;   "F" #'+default/browse-notes

  "i" #'denote-org-dblock-insert-links
  "I" #'denote-org-dblock-insert-backlinks

  "l" #'denote-link-or-create
  "L" #'denote-link-after-creating-with-command

  "n" #'consult-notes

  "G" #'consult-notes-search-in-all-notes

  "s" #'denote-silo-open-or-create
  "S" #'denote-silo-select-silo-then-command

  "t" #'denote-type

  "r" #'denote-region ; "contents" mnemonic
  ;; "R" #'denote-rename-file-using-front-matter
  "," #'denote-rename-file-using-front-matter
  "-" #'denote-show-backlinks-buffer

  "SPC" #'org-journal-open-current-journal-file

  "j" #'org-journal-new-entry
  "u" #'org-transclusion-mode

  "k" #'denote-rename-file-keywords
  "z" #'denote-rename-file-signature

  "M-f" #'denote-find-link
  "M-b" #'denote-find-backlink
  )
(keymap-set global-map "C-c n" ews-denote-map)
(keymap-set global-map "M-e" ews-denote-map) ; ews-denote-map

;;; key

;;;; Top-menu M-x

;; 심볼 검색 현재 폴더
;; v expand-region
;; 토글 버퍼

(map! :leader
      "SPC" nil
      ;; "." nil
      ;; "," nil
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Search for symbol in cwd" "(" #'+default/search-cwd-symbol-at-point

      ;; :desc "Find file in project" "." #'projectile-find-file
      ;; :desc "Find file in cwd" "," #'my/consult-fd
      ;; :desc "consult-buffer" "`" #'consult-buffer
      ;; :desc "Eval expression" "M-;" #'pp-eval-expression
      )

;;;; Replace Doom `/' highlight with buffer-search

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer) ;; / -> .

;;;; 'v' er/expand-region

(map! :leader
      :desc "er/expand-region" "v" #'er/expand-region
      ;; :desc "expand-menu" "V" #'expand-transient
      )

;;;; window

;; doom-leader-map w C-S-w 'ace-swap-window

;;;; 'n' +notes denote

(map! :leader
      (:prefix ("n" . "notes")
               "g" #'+default/org-notes-search ; grep
               "d" ews-denote-map
               "SPC" #'org-journal-open-current-journal-file
               "L" #'my/org-store-link-id-optional
               "u" #'org-transclusion-mode
               ))

;;;; 'i' insert

(map! :leader
      (:prefix "i"
       :desc "time-stamp" "1" #'time-stamp
       ))

;;;; vterm-mode-map

(after! vterm
  ;; Compile Vterm without asking.
  (setq vterm-always-compile-module t)
  (map! :map vterm-mode-map "M-y" #'vterm-yank-pop))

;;;; 'w' window


(map! :leader
      :prefix "w"
      "1" nil "2" nil "3" nil "4" nil "5" nil "6" nil "7" nil "8" nil "9" nil "0" nil "-" nil "b" nil "d" nil "r" nil "R" nil "m" nil "<" nil ">" nil "_" nil "|" nil
      "C-=" nil "C-_" nil "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-k" nil "C-l" nil "C-w" nil "C-n" nil "C-o" nil "C-p" nil "C-q" nil "C-r" nil "C-s" nil "C-t" nil "C-u" nil "C-v" nil "C-x" nil "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-<down>" nil "C-<left>" nil "C-<right>" nil "C-<up>" nil
      "TAB" #'evil-window-prev
      "." #'window-transient
      "c" #'window-cleanup+
      "g" #'golden-ratio

      ;; "D" #'delete-window ; block delete workspace
      "M" #'ace-swap-window
      ;; "W" #'ace-window
      "m" #'toggle-maximize-buffer
      "=" #'balance-windows-area
      :desc "window-vsplit" "/" #'evil-window-vsplit
      ;; :desc "window-vsplit" "v" #'evil-window-vsplit
      ;; :desc "window-vsplit-follow" "V" #'+evil/window-vsplit-and-follow
      :desc "window-layout-toggle" "-" 'spacemacs/window-layout-toggle
      :desc "delete-other-window" "O" 'delete-other-windows)

;;;; mode-map

(map! :map cdlatex-mode-map
      :i "TAB" #'cdlatex-tab)

(map! (:map org-mode-map
       "<f12>" #'org-transclusion-mode
       :ni "C-c H" #'org-insert-heading
       :ni "C-c S" #'org-insert-subheading
       :i "C-n" #'next-line
       :i "C-p" #'previous-line
       :n "C-S-p" #'outline-up-heading
       :n "C-j" #'org-forward-heading-same-level
       :n "C-k" #'org-backward-heading-same-level
       :n "C-n" #'org-next-visible-heading
       :n "C-p" #'org-previous-visible-heading
       :n "zu" #'outline-up-heading
       "C-c d"  #'cape-dict
       :i "<tab>"  #'completion-at-point ; 2025-02-03
       :i "TAB"  #'completion-at-point
       "M--" #'denote-find-backlink
       ))

(map! (:map org-journal-mode-map
       :n "]f"  #'org-journal-next-entry
       :n "[f"  #'org-journal-previous-entry
       :n "C-n" #'org-next-visible-heading ; overide
       :n "C-p" #'org-previous-visible-heading)
      (:map org-journal-search-mode-map
            "C-n" #'org-journal-search-next
            "C-p" #'org-journal-search-previous))

(map! (:map outline-mode-map
       :n "C-n" #'outline-next-heading
       :n "C-p" #'outline-previous-heading
       :i "C-n" #'next-line
       :i "C-p" #'previous-line
       :n "C-S-p" #'outline-up-heading
       :n "zu" #'outline-up-heading)
      )

;; BUG Reset Here! modules/config/default/+emacs-bindings.el
(map!
 (:after smartparens
  :map smartparens-mode-map

  ;; Doom's Default - /modules/config/default/+emacs-bindings.el
  "C-M-a"           #'sp-beginning-of-sexp
  "C-M-e"           #'sp-end-of-sexp
  "C-M-f"           #'sp-forward-sexp
  "C-M-b"           #'sp-backward-sexp
  "C-M-n"           #'sp-next-sexp
  "C-M-p"           #'sp-previous-sexp
  "C-M-u"           #'sp-up-sexp
  "C-M-d"           #'sp-down-sexp
  "C-M-k"           #'sp-kill-sexp
  "C-M-t"           #'sp-transpose-sexp
  "C-M-<backspace>" #'sp-splice-sexp

  "C-<right>" #'sp-forward-slurp-sexp
  "C-<left>" #'sp-forward-barf-sexp
  "M-<left>" #'sp-backward-slurp-sexp
  "M-<right>" #'sp-backward-barf-sexp

  "M-<up>"  #'sp-splice-sexp-killing-backward
  "M-<down>" #'sp-splice-sexp-killing-forward

  "C-c (" #'sp-wrap-round
  ;; "C-c [" #'sp-wrap-square ; conflict org-mode-map
  ;; "C-c {" #'sp-wrap-curly
  ))

;;; Custom EVIL Keys

(map! :i "M-l" #'sp-forward-slurp-sexp ; downcase-word
      :i "M-h" #'sp-forward-barf-sexp  ; mark-paragraph
      ;; :v "s" #'evil-surround-region
      ;; "s-b" #'consult-buffer
      ;; "s-=" #'text-scale-increase
      ;; "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter)) ; nop
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter)) ; nop
      ;; :n "DEL" #'previous-buffer
      :n "DEL" #'evil-switch-to-windows-last-buffer ; BACKSPACE
      ;; :n "s-e" #'+scroll-line-down-other-window
      ;; :n "s-y" #'+scroll-line-up-other-window
      :i "M-/" #'hippie-expand
      :n "g SPC" #'evil-jump-to-tag
      :i "C-v" #'evil-paste-after ; evil-quoted-insert : 'C-q'
      :n "[ g" #'+vc-gutter/previous-hunk ; remap diff-hl-previous-hunk
      :n "] g" #'+vc-gutter/next-hunk ; remap diff-hl-next-hunk

      :m "8" #'evil-ex-search-word-forward ; default *
      :m "3" #'evil-ex-search-word-backward ; default #
      :m "4" #'evil-end-of-line ; default $
      :m "0" #'evil-beginning-of-line

      ;; :m "C-i" #'evil-jump-forward ;; evil-want-C-i-jump - evil-maps.el
      :n "g ]" #'evil-jump-forward
      :n "g [" #'evil-jump-backward
      ;; :n "g RET" #'tabgo
      )

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

(after! denote
  (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/")))


;;;; Notmuch 이메일 설정

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

;;; user-keybindings
