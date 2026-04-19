;;; $DOOMDIR/lisp/tty-config.el --- Terminal (TTY) environment config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 터미널(TTY) Emacs를 위한 통합 설정.
;; - term-keys: WezTerm 키 시퀀스 (F1~F12, Hangul, S-SPC 등)
;; - clipboard: OSC 52 (Emacs 내장 xterm.el, clipetty 제거)
;; - 성능: xterm-set-window-title off, mouse/paren off 등 경량화
;;
;; vterm/eat 같은 터미널 에뮬레이터 설정과는 별개.
;; 이 파일은 "Emacs 자체가 TTY에서 돌아갈 때" 필요한 설정.
;;
;; kitty-graphics 는 제외 — post-command-hook 에 +window-scroll/size/
;; buffer-change-functions + 11개 org/image advice 를 건다. 에이전트
;; 프론트엔드는 텍스트 중심이라 비용 대비 이득 없음. 필요시 수동으로
;; M-x kitty-graphics-mode 토글.

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

;;;; TTY 설정 진입점 — 타이밍 정렬

;; `display-graphic-p` 는 파일 로드 시점에 신뢰 불가.
;;   - daemon 로드 시점: frame 자체가 없음 → 항상 t 반환
;;   - non-daemon 로드 시점: Doom 이 이후 hook 단계에서 TTY 설정을 켬
;;     * xterm-mouse-mode → tty-setup-hook (modules/os/tty/config.el)
;;     * show-paren-mode  → doom-first-buffer-hook (doom-ui.el)
;; 따라서 `(unless (display-graphic-p) ...)` 를 top-level 에서 실행하면
;; Doom 이 뒤에 다시 켜 덮어쓴다. 모든 TTY 설정은 hook 으로 미루고,
;; Doom 의 같은 hook 보다 뒤에 돌도록 :append 로 붙인다.
;;
;; frame-local 설정(OSC 52, display-table, cursor shape, FE0F 숨김)도
;; tty-setup-hook 에서 수행해 daemon + GUI/TTY 혼합 환경을 정상 처리.

(defun +tty-setup ()
  "TTY frame 전용 세팅. tty-setup-hook / doom-first-buffer-hook 에서 호출."
  (when (not (display-graphic-p))
    ;; OSC 52 selection (frame-local) — Emacs 29+ 내장 xterm.el 경로
    (set-terminal-parameter nil 'xterm--set-selection t)
    ;; Emacs → 터미널 윈도우 타이틀 OSC 0/2 송출 비활성.
    ;; tmux/WezTerm 이 이미 타이틀 관리하므로 중복. profile 에서 6% 점유.
    (setq-default xterm-set-window-title nil)
    ;; 가벼움 — 에이전트 프론트엔드는 키보드-only
    (xterm-mouse-mode -1)
    (show-paren-mode -1)
    ;; vertical-border: ASCII '|' → U+2502 '│' (GUI 감성의 얇은 경계)
    (set-display-table-slot standard-display-table
                            'vertical-border (make-glyph-code ?│))
    ;; VS-16 (U+FE0F) 숨김 — per-grapheme-cluster 렌더 경로 차단
    ;; WezTerm cell_widths(per-codepoint)만으론 드리프트 커버 불가.
    ;; 버퍼 원문은 보존, 화면 송출에서만 제거.
    ;; ref: llmlog 20260417T173916
    (aset standard-display-table #xFE0F (vector))))

(add-hook 'tty-setup-hook #'+tty-setup 'append)
(add-hook 'doom-first-buffer-hook #'+tty-setup 'append)

;;;; buffer-local vertical-border 패치

;; org/markdown 등은 buffer-display-table 을 자기 용도로 세팅한다.
;; 그러면 standard-display-table 의 vertical-border 슬롯이 묻히므로,
;; 버퍼 로컬 테이블이 생성된 후 그 슬롯을 직접 덮어쓴다.
;; 참고: display-table 해석 우선순위 — window > buffer > standard.
(defun +tty-patch-vertical-border ()
  "Patch vertical-border slot on current buffer's display-table, if any."
  (when (and (not (display-graphic-p)) buffer-display-table)
    (set-display-table-slot buffer-display-table
                            'vertical-border (make-glyph-code ?│))))
(add-hook 'after-change-major-mode-hook #'+tty-patch-vertical-border)

;;;; Evil cursor shape (DECSCUSR) — Normal=block, Insert=bar

;; hook 은 GUI/TTY 공용으로 등록하되, 실행 시점에 current frame 이
;; TTY 일 때만 escape 시퀀스를 송출한다. daemon + GUI/TTY 혼합 대응.
(defun +tty-evil-cursor-insert ()
  (unless (display-graphic-p) (send-string-to-terminal "\e[6 q")))
(defun +tty-evil-cursor-normal ()
  (unless (display-graphic-p) (send-string-to-terminal "\e[2 q")))
(add-hook 'evil-insert-state-entry-hook #'+tty-evil-cursor-insert)
(add-hook 'evil-insert-state-exit-hook  #'+tty-evil-cursor-normal)

;;;; 모드라인 trailing space 제거 — GUI/TTY 공통 무해

(setq mode-line-end-spaces nil)

(provide 'tty-config)
;;; tty-config.el ends here
