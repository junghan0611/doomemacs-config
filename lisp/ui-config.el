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

;;;; outli

(use-package! outli
  :defer 1
  :init
  (setq outli-speed-commands nil)
  (add-hook 'prog-mode-hook #'outli-mode)
  (add-hook 'conf-mode-hook #'outli-mode)
  :config
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(zig-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-ts-mode "##" ?# t))
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

;; TODO 터미널에서 테마 색상 충돌 방지가 되는거야?
(unless (display-graphic-p)
  ;; 터미널에서 배경색 투명도 유지
  (setq-default frame-background-mode 'dark)

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

;;;; modus-themes

(use-package! doric-themes)

(use-package! modus-themes
  :init
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)))

(defun my/themes-toggle ()
  (interactive)
  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'modus-vivendi-tinted))

(add-hook 'doom-first-input-hook #'my/themes-toggle)

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
