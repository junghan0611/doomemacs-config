;;; $DOOMDIR/lisp/ui-config.el --- UI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; UI 설정
;; - Dashboard
;; - Doom Modeline
;; - Themes
;; - Outli (code outline)
;; - Visual effects (pulse-line)
;; - Which-key

;;; Code:

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
           '(scroll-up-command evil-window-right evil-window-left scroll-down-command ace-window recenter-top-bottom other-window))
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

;;;; themes

;; doom-themes
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic nil) ; if nil, italics is universally disabled

;; 터미널 배경 투명화 함수
(defun my/terminal-transparent-background (&optional _theme)
  "터미널에서 Emacs 배경을 투명하게 설정.
Ghostty 등 터미널의 배경색/투명도를 그대로 사용하게 함.
_THEME 인자는 `enable-theme-functions' 호환용."
  (unless (display-graphic-p)
    ;; 모든 터미널 프레임에 적용
    (dolist (frame (frame-list))
      (unless (display-graphic-p frame)
        (set-face-background 'default "unspecified-bg" frame)
        (set-face-background 'line-number "unspecified-bg" frame)
        (set-face-background 'line-number-current-line "unspecified-bg" frame)
        (set-face-background 'fringe "unspecified-bg" frame)))))

;; 터미널 기본 설정
(unless (display-graphic-p)
  ;; 터미널에서 dark 테마 기본 사용
  (setq-default frame-background-mode 'dark)

  ;; Ghostty 터미널 전용 설정
  (cond
   ;; xterm-ghostty terminfo 사용시
   ((string-match "ghostty" (or (getenv "TERM") ""))
    ;; Ghostty는 24비트 트루컬러 지원
    (setenv "COLORTERM" "truecolor")
    ;; Ghostty는 256색상 이상 지원
    (setq xterm-color-use-bold-for-bright nil)
    ;; Ghostty 최적화 설정
    (add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-direct")))

   ;; 일반 256color 터미널
   ((string-match "256color" (or (getenv "TERM") ""))
    (setq xterm-color-names-bright
          ["#3B4252" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#E5E9F0"])
    (setq xterm-color-names
          ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B"
           "#81A1C1" "#B48EAD" "#88C0D0" "#D8DEE9"])))

  ;; enable-theme-functions 사용 (Emacs 29+, doom-load-theme-hook보다 확실)
  ;; depth 100 = 테마 로드 완료 후 가장 마지막에 실행
  (add-hook 'enable-theme-functions #'my/terminal-transparent-background 100)

  ;; 새 프레임 생성시에도 적용
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (unless (display-graphic-p frame)
                (run-with-timer 0.1 nil #'my/terminal-transparent-background)))))

;;;; modus-themes

(use-package! doric-themes
  :commands (doric-themes-load-random))

(use-package! ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1))

(use-package! modus-themes
  :init
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
  :config
  (setq modus-themes-italic-constructs nil))

;;;###autoload
(defun my/themes-toggle ()
  (interactive)
  ;; Finally, load your theme of choice (or a random one with
  (ef-themes-load-random-dark)
  ;; 터미널에서 테마 로드 후 배경 투명화 (확실한 적용)
  (unless (display-graphic-p)
    (run-with-timer 0.05 nil #'my/terminal-transparent-background)))

(add-hook! 'doom-first-input-hook #'my/themes-toggle)

;;;; spacious-padding

(use-package! spacious-padding
  :if window-system ; important
  :hook (server-after-make-frame . spacious-padding-mode)
  :init
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  (setq spacious-padding-widths
        '(:header-line-width 4
          :mode-line-width 4 ; 6
          :tab-width 4 ; sync mode-line-width for keycast-tab-bar
          :internal-border-width 20 ; 15
          :right-divider-width 30 ; 30
          :scroll-bar-width 8
          :fringe-width 8
          ))
  (add-hook! 'doom-load-theme-hook #'spacious-padding-mode)
  :config
  ;; (when (fboundp 'tooltip-mode) (tooltip-mode 1))
  ;; (when (fboundp 'tool-bar-mode) (tool-bar-mode 1))
  ;; (when (display-graphic-p) ; gui
  ;;   (menu-bar-mode +1)) ; disable <f10>
  (spacious-padding-mode +1))

;;;; popup

(progn
  ;; Completely disable management of the mode-line in popups:
  (remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h) ; important

  (set-popup-rules!
    '(
      ("*Ilist*" :size 40 :side left :modeline t :select nil :quit nil) ; imenu-list 40
      ("^\\*eww.*" :size 82 :side left :modeline t :select t :quit nil :ttl t) ; jh
      )
    )
  )

;;; provide

(provide 'ui-config)

;;; ui-config.el ends here
