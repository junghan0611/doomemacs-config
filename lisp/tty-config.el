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
;; frame-local 설정(OSC 52, display-table, cursor shape)은 tty-setup-hook 에서
;; 수행해 daemon + GUI/TTY 혼합 환경을 정상 처리.

(defun +tty-wezterm-p ()
  "현재 TTY가 WezTerm 경로면 t.

TERM_PROGRAM=WezTerm 을 1차 기준으로 사용한다. 노트북 TTY 경로를 우선 겨냥한
최소 워크어라운드이며, native terminal/Termux 는 추후 별도 검토한다."
  (string-equal (getenv "TERM_PROGRAM") "WezTerm"))

(defun +tty-wezterm-strip-vs16 ()
  "WezTerm TTY 버퍼에서 VS-16 auto-composition 을 끈다.

WezTerm #7570 회피용 최소 조치:
- FE0F 는 화면에서만 숨김 (원문 보존)
- FE0F 트리거 auto-composition 만 버퍼 로컬로 비활성화

BMP→SMP remap 은 여기서 하지 않는다. 흔한 케이스는 telega 쪽 display-table
우회가 담당하고, 일반 텍스트는 base glyph 로 안전하게 남긴다."
  (when (and (not (display-graphic-p)) (+tty-wezterm-p))
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (aset standard-display-table #xFE0F [])
    (setq-local composition-function-table
                (copy-sequence composition-function-table))
    (set-char-table-range composition-function-table #xFE0F nil)))

(defun +tty-safe-ellipsis-setup ()
  "TTY에서 prompt truncation ellipsis만 ASCII로 고정한다."
  ;; consult/minibuffer 경로의 `…' 폭 드리프트만 최소 수정으로 회피.
  (setq truncate-string-ellipsis "..."))

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
    (+tty-safe-ellipsis-setup)
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    ;; vertical-border: ASCII '|' → U+2502 '│' (GUI 감성의 얇은 경계)
    (set-display-table-slot standard-display-table
                            'vertical-border (make-glyph-code ?│))
    (+tty-wezterm-strip-vs16)))

(add-hook 'tty-setup-hook #'+tty-setup 'append)
(add-hook 'doom-first-buffer-hook #'+tty-setup 'append)

;;;; consult prompt ellipsis — TTY에서 ASCII로 고정

(after! consult
  (defun +tty-consult-left-truncate-file-a (orig-fn file)
    "TTY에서 consult 경로 축약 ellipsis를 ASCII로 치환한다."
    (let ((out (funcall orig-fn file)))
      (if (display-graphic-p)
          out
        (replace-regexp-in-string "…" "..." out t t))))

  (defun +tty-consult-directory-prompt-a (orig-fn prompt dir)
    "TTY에서 consult 디렉토리 prompt의 hardcoded ellipsis를 ASCII로 치환한다."
    (pcase-let ((`(,prompt-str ,paths ,edir) (funcall orig-fn prompt dir)))
      (if (display-graphic-p)
          (list prompt-str paths edir)
        (list (replace-regexp-in-string "…" "..." prompt-str t t)
              paths edir))))

  (advice-add 'consult--left-truncate-file :around #'+tty-consult-left-truncate-file-a)
  (advice-add 'consult--directory-prompt :around #'+tty-consult-directory-prompt-a))

;;;; buffer-local vertical-border / VS-16 패치

;; org/markdown 등은 buffer-display-table 을 자기 용도로 세팅한다.
;; 그러면 standard-display-table 의 vertical-border 슬롯이 묻히므로,
;; 버퍼 로컬 테이블이 생성된 후 그 슬롯을 직접 덮어쓴다.
;; 참고: display-table 해석 우선순위 — window > buffer > standard.
(defun +tty-patch-buffer-locals ()
  "Patch buffer-local display/composition settings for TTY buffers."
  (when (and (not (display-graphic-p)) buffer-display-table)
    (set-display-table-slot buffer-display-table
                            'vertical-border (make-glyph-code ?│)))
  (+tty-wezterm-strip-vs16))
(add-hook 'after-change-major-mode-hook #'+tty-patch-buffer-locals)

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
