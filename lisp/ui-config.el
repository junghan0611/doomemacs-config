;;; $DOOMDIR/lisp/ui-config.el --- UI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

;; 새 :ui dashboard 모듈 사용 (+dashboard-* 접두사)
(setq +dashboard-ascii-banner-fn nil)

;; Dashboard 위젯 구성
(setq +dashboard-functions
      '(my/dashboard-widget-fortune ;; fortune
        +dashboard-widget-banner
        +dashboard-widget-shortmenu
        +dashboard-widget-loaded
        +dashboard-widget-footer))

;; Fortune 위젯: Kevin Kelly 명언 또는 fortune 명령어 출력
(defun my/dashboard-widget-fortune ()
  "Display fortune quote or Kevin Kelly's wisdom."
  (let* ((quotestring
          (if (executable-find "fortune")
              ;; fortune 명령어가 있으면 사용, termux는 -c 옵션이 없으므로 조건부 처리
              (let ((fortune-cmd (if IS-TERMUX
                                     "fortune"  ; termux: simple fortune
                                   "fortune ~/.fortunes/advice"))) ; NixOS: ~/.fortunes/advice (Kevin Kelly)
                (string-join
                 (mapcar
                  (lambda (l) (concat "\n " (string-fill l 72)))
                  (string-lines
                   (shell-command-to-string fortune-cmd)))))
            ;; fortune 없으면 Kevin Kelly 기본 명언
            "\n The only way to fight against getting old is to remain astonished.
                                                      - Kevin Kelly")))
    ;; 새 dashboard의 +dashboard-insert-centered 사용 (pixel-width 기반 센터링)
    (+dashboard-insert-centered quotestring)))

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

;;;; Terminal True-color & Theme 설정
;; 24bit true-color 해결: emacsclient wrapper (shell.nix의 e 함수)에서
;; TERM을 *-direct terminfo로 전환하여 init_tty가 16M colors 인식.
;; ref: tecosaur doom config.org "Emacs client wrapper"
;;
;; 테마 reload는 여전히 필요:
;; Emacs daemon은 GUI 프레임으로 시작하므로, emacsclient -nw로
;; 터미널 프레임 생성 시 테마 face를 터미널 기준으로 재계산해야 함.
;; (unless (display-graphic-p)) 로 감싸면 안 됨 — daemon 로드 시 graphic-p = t
(defun my/setup-terminal-frame (&optional frame)
  "터미널 FRAME의 테마를 재적용하여 face를 재계산."
  (interactive)
  (let ((f (or frame (selected-frame))))
    (unless (display-graphic-p f)
      (with-selected-frame f
        (setq frame-background-mode 'dark)
        (run-with-timer 0.2 nil
                        (lambda ()
                          (let ((theme (car custom-enabled-themes)))
                            (when theme
                              (load-theme theme t)))))))))

(add-hook 'after-make-frame-functions #'my/setup-terminal-frame)
(unless (display-graphic-p)
  (my/setup-terminal-frame (selected-frame)))

;; 터미널 배경 투명화 — 터미널 자체 배경색을 사용하도록 설정
(defun my/terminal-transparent-background (&optional _theme)
  "터미널에서 Emacs UI 배경을 투명(unspecified)으로 설정.
_THEME 인자는 `enable-theme-functions' 호환용."
  (interactive)
  (unless (display-graphic-p)
    (dolist (frame (frame-list))
      (unless (display-graphic-p frame)
        (dolist (face '(default
                        fringe
                        line-number
                        line-number-current-line
                        mode-line
                        mode-line-active
                        mode-line-inactive
                        tab-bar
                        tab-line
                        tab-bar-tab
                        header-line))
          (when (facep face)
            (set-face-background face "unspecified-bg" frame)))))))

;;;; modus-themes

(use-package! doric-themes
  :commands (doric-themes-load-random))

(use-package! ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1))

(use-package! modus-themes
  :init
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
  :config
  (setq modus-themes-italic-constructs nil))

;;;###autoload
(defun my/themes-toggle ()
  (interactive)
  ;; (ef-themes-load-random-dark)
  ;; (modus-themes-select 'modus-vivendi-tinted)
  ;; (modus-themes-load-theme 'ef-maris-dark)
  (modus-themes-load-theme 'modus-vivendi-tinted)
  ;; 터미널에서 테마 로드 후 배경 투명화 (확실한 적용)
  (unless (display-graphic-p)
    (run-with-timer 0.05 nil #'my/terminal-transparent-background)))

(add-hook! 'doom-first-buffer-hook #'my/themes-toggle)

;;;; winpulse - window focus flash

(use-package! winpulse
  :config
  (winpulse-mode +1))

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
          :mode-line-width 4
          :tab-bar-width 2
          :tab-line-width 2
          :internal-border-width 15
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

;;;; :ui vc-gutter diff-hl

(after! diff-hl
  (setq diff-hl-disable-on-remote t) ; default nil
  ;; flydiff (실시간 diff) 비활성 — after-change-functions 훅 + 1s 타이머
  ;; 제거. diff-hl 자체는 유지되어 save/vc-refresh 시 gutter 갱신.
  (remove-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
  (remove-hook 'diff-hl-flydiff-mode-hook #'+vc-gutter-init-flydiff-mode-h))

;;;; Mouse buttons

;; from /tecosaur-dot-doom/config.org
(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;;;; Window title

;; from /tecosaur-dot-doom/config.org
(setq frame-title-format
      '(""
        (:eval
         (if (string-match-p (regexp-quote (or (bound-and-true-p org-roam-directory) "\u0000"))
                             (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (when-let ((project-name (and (featurep 'projectile) (projectile-project-name))))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◈ %s" "  ◇  %s") project-name))))))

;;; provide

(provide 'ui-config)

;;; ui-config.el ends here
