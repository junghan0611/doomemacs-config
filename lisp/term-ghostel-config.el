;;; $DOOMDIR/lisp/term-ghostel-config.el --- Ghostty-backed Emacs terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;;; Commentary:

;; Experimental Ghostty/libghostty-vt backed terminal for Emacs.
;;
;; This does not replace vterm.  Keep Doom's :term vterm as the stable
;; baseline, and use ghostel as an opt-in terminal surface for checking:
;;
;; - Claude Code / pi / tmux repaint behavior
;; - TRAMP and SSH terminal behavior
;; - xterm-ghostty terminfo compatibility
;; - Unicode, Hangul, Nerd Font, Kitty protocol behavior inside Emacs
;;
;; Do not enable this on Termux/Android until the native module path and
;; platform ABI are verified separately.

;;; Code:

(defun +ghostel-supported-p ()
  "Return non-nil when this Emacs can plausibly load ghostel."
  (and (fboundp 'module-load)
       module-file-suffix
       (not (bound-and-true-p IS-TERMUX))))

(when (+ghostel-supported-p)
  (use-package! ghostel
    :commands (ghostel)
    :init
    ;; Ask on first use.  Other useful values:
    ;; - download: use a prebuilt module when available
    ;; - compile: build from source with Zig
    ;; - nil: disable automatic module installation
    (setq ghostel-module-auto-install 'ask)

    :config
    (setq ghostel-shell (or (getenv "SHELL") "/bin/zsh"))

    ;; Default is xterm-ghostty.  Keep it while evaluating Claude Code,
    ;; tmux, TRAMP, SSH and modern TUI fast paths.  If remote terminfo
    ;; becomes noisy, temporarily lower this to xterm-256color.
    ;; (setq ghostel-term "xterm-256color")

    ;; Keep remote/clipboard side effects opt-in during the first pass.
    ;; (setq ghostel-enable-osc52 t)
    ;; (setq ghostel-tramp-shell-integration t)
    ))

(provide 'term-ghostel-config)
;;; term-ghostel-config.el ends here
