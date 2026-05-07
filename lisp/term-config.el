;;; $DOOMDIR/lisp/term-config.el --- Terminal emulator config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Emacs 내부 터미널 에뮬레이터 설정.
;;
;; Doom의 :term 모듈에서 관리하는 영역:
;; - eshell
;; - vterm
;; - ghostel (실험적)
;;
;; tty-config.el 이 "Emacs 자체가 TTY에서 돌아갈 때"의 환경을 다룬다면,
;; 이 파일은 Emacs 안에서 구동되는 터미널 버퍼/에뮬레이터를 다룬다.

;;; Code:

(use-package! ghostel
  :commands (ghostel)
  :if (and (fboundp 'module-load)
           module-file-suffix
           (not (bound-and-true-p IS-TERMUX)))
  :init
  ;; Experimental ghostty/libghostty-vt surface for terminal-path checks.
  ;; Keep Doom's vterm as the stable baseline; use ghostel only when needed.
  (setq ghostel-module-auto-install 'ask)
  :config
  (setq ghostel-shell (or explicit-shell-file-name
                          shell-file-name
                          (getenv "SHELL")
                          "/bin/zsh"))
  ;; Default is xterm-ghostty. If remote terminfo gets noisy, temporarily use:
  ;; (setq ghostel-term "xterm-256color")
  ;; Keep side effects opt-in during evaluation.
  ;; (setq ghostel-enable-osc52 t)
  ;; (setq ghostel-tramp-shell-integration t)
  )

(use-package! evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(provide 'term-config)
;;; term-config.el ends here
