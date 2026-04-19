;;; $DOOMDIR/lisp/tty-config.el --- Terminal (TTY) environment config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 터미널(TTY) Emacs를 위한 통합 설정.
;; - term-keys: WezTerm 키 시퀀스 (F1~F12, Hangul, S-SPC 등)
;; - kitty-graphics: 터미널 이미지 표시 (Kitty/Sixel protocol)
;; - clipboard: OSC 52 (Emacs 내장 xterm.el, clipetty 제거)
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

;;;; clipboard — OSC 52 (Emacs 내장 xterm.el)

;; clipetty 제거 — write-region + terminal-name=/dev/tty 문제 회피.
;; Emacs 29+ 내장 xterm.el의 gui-backend-set-selection이 send-string-to-terminal로
;; OSC 52를 보내므로 /dev/tty 문제 없음.
;; ref: llmlog 20260413T124341 (emacs terminal-name /dev/tty 문제 조사)
;;
;; WezTerm: OSC 52 기본 지원 (설정 없음)
;; tmux: set -g set-clipboard on / allow-passthrough on (shell.nix)
(unless (display-graphic-p)
  (set-terminal-parameter nil 'xterm--set-selection t))

;;;; Evil cursor shape (DECSCUSR) — Normal=block, Insert=bar

(unless (display-graphic-p)
  (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e[6 q")))
  (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\e[2 q"))))

;;;; Visual tweaks — GUI 감성의 터미널 분할선/모드라인

;; 창 분할 세로선: ASCII '|' → U+2502 '│' (Box Drawings Light Vertical)
;; 인접 글자와 붙지 않는 얇고 매끈한 경계 — TTY에서 GUI에 가까운 느낌.
;; ref: https://www.masteringemacs.org/article/slimmer-emacs-kitty
;;
;; Emacs 디스플레이 테이블 해석: window > buffer > standard (첫 non-nil 테이블).
;; org/markdown 처럼 buffer-display-table 을 자기 용도로 세팅하는 모드에서는
;; standard-display-table 의 vertical-border 슬롯이 묻힌다. 그래서 버퍼 로컬
;; 테이블이 이미 있을 땐 그 슬롯을 직접 덮어써 준다.
(unless (display-graphic-p)
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?│))
  (defun +tty-patch-vertical-border ()
    "Patch vertical-border slot on current buffer's display-table, if any."
    (when buffer-display-table
      (set-display-table-slot buffer-display-table
                              'vertical-border
                              (make-glyph-code ?│))))
  (add-hook 'after-change-major-mode-hook #'+tty-patch-vertical-border))

;; 모드라인 끝 trailing space 제거 — GUI/TTY 공통 무해
(setq mode-line-end-spaces nil)

;;;; Lightweight TTY — 에이전트 프론트엔드는 키보드-only, 가벼움 우선

;; 마우스 이벤트 처리 제거 — 에이전트 프론트엔드는 키보드로만 운용
;; show-paren-mode 비활성 — org 읽기/쓰기 위주, paren highlight 비용 절감
(unless (display-graphic-p)
  (xterm-mouse-mode -1)
  (show-paren-mode -1))

(provide 'tty-config)
;;; tty-config.el ends here
