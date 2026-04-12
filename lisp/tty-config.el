;;; $DOOMDIR/lisp/tty-config.el --- Terminal (TTY) environment config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 터미널(TTY) Emacs를 위한 통합 설정.
;; - term-keys: WezTerm 키 시퀀스 (F1~F12, Hangul, S-SPC 등)
;; - kitty-graphics: 터미널 이미지 표시 (Kitty/Sixel protocol)
;; - clipboard: OSC 52 (Doom tty +osc → clipetty)
;;
;; vterm/eat 같은 터미널 에뮬레이터 설정과는 별개.
;; 이 파일은 "Emacs 자체가 TTY에서 돌아갈 때" 필요한 설정.

;;; Code:

;;;; term-keys — 키 시퀀스 전달

(use-package! term-keys
  :config
  (require 'term-keys-wezterm)
  ;; 데몬 모드: GUI로 시작해도 TTY 클라이언트 접속 시 term-keys 활성화
  (term-keys-mode t))

;; term-keys 시퀀스 \x1b\x1f (prefix) = C-M-_ → undo-fu가 가로채는 문제 방지
;; Doom이 doom-first-buffer에서 undo-fu-mode 활성화 → keymap 재생성되므로
;; after! 대신 모드 활성화 후 항상 unbind
(add-hook! 'undo-fu-mode-hook
  (define-key undo-fu-mode-map (kbd "C-M-_") nil))

;; term-keys conf 재생성:
;; (with-temp-buffer (insert (term-keys/wezterm-conf))
;;                   (write-region (point-min) (point-max) "~/term-keys-wezterm.lua"))
;; (with-temp-buffer (insert (term-keys/kitty-conf))
;;                   (write-region (point-min) (point-max) "~/term-keys-kitty.conf"))

;;;; kitty-graphics — 터미널 이미지

(use-package! kitty-graphics
  :defer t
  :init
  ;; TTY 프레임에서만 활성화, tmux 안에서는 비활성화
  ;; daemon 모드: GUI로 시작해도 TTY 클라이언트 접속 시 활성화
  (defun +kitty-graphics-maybe-enable ()
    "Enable kitty-graphics-mode only in TTY without tmux."
    (when (and (not (display-graphic-p))
               (not (getenv "TMUX")))
      (require 'kitty-graphics)
      (kitty-graphics-mode 1)))
  (add-hook 'tty-setup-hook #'+kitty-graphics-maybe-enable)
  ;; 비데몬 TTY 직접 실행 시에도 동작
  (unless (daemonp)
    (add-hook 'doom-first-buffer-hook #'+kitty-graphics-maybe-enable))
  :config
  (setq kitty-gfx-max-width 100
        kitty-gfx-max-height 35))

;;;; clipboard — OSC 52

;; Doom (tty +osc) → clipetty 자동 활성화. 별도 설정 불필요.
;; WezTerm: OSC 52 기본 지원 (설정 없음)
;; tmux: set -g set-clipboard on / allow-passthrough on (shell.nix)

;; (use-package! xclip
;;   :config
;;   (unless (display-graphic-p)
;;     (xclip-mode 1)))

;; (use-package! clipetty
;;   :hook (after-init . global-clipetty-mode)
;;   :config
;;   (setq clipetty-assume-nested-mux nil))

(provide 'tty-config)
;;; tty-config.el ends here
