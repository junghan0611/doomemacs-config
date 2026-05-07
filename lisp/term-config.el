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
  :commands (ghostel ghostel-project my/pi-ghostel-start)
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

  ;;; project.el integration — C-x p p picks "Ghostel" alongside Doom defaults.
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)

  ;;; Whitelist additional elisp callbacks for `ghostel_cmd' shell helper.
  ;; Default: find-file, find-file-other-window, dired, dired-other-window, message.
  ;; Add Doom-specific commands we want callable from inside the terminal.
  (with-eval-after-load 'ghostel
    (dolist (cmd '(("magit-status-setup-buffer" magit-status-setup-buffer)
                   ("dired-jump"                 dired-jump)
                   ("+default/find-file-under-here" +default/find-file-under-here)))
      (add-to-list 'ghostel-eval-cmds cmd)))

  ;;; OSC 9;4 progress protocol — claude code, codex, pi CLI all emit this.
  ;; spinner.el ships with magit; default to spinner if loadable, else text.
  (when (locate-library "spinner")
    (setq ghostel-progress-function #'ghostel-spinner-progress
          ghostel-spinner-type 'progress-bar)))

(use-package! evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  ;; Route insert-state ESC to the PTY in every ghostel buffer.
  ;; Claude Code, codex, pi all expose their own vim-style mode that
  ;; expects ESC to reach them; alt-screen apps (vim, htop) need it too.
  ;; Use `,.' (evil-escape) for Emacs evil normal state, `C-c C-t' for
  ;; scrollback (`ghostel-copy-mode').  `evil-ghostel-toggle-send-escape'
  ;; switches modes per buffer when needed.
  (setq evil-ghostel-escape 'terminal))


;;; Pi CLI in ghostel — replacing pi-coding-agent package on -nw

(defcustom my/pi-ghostel-args
  '("--entwurf-control" "--emacs-agent-socket" "pi")
  "Args passed to the `pi' CLI when launched via `my/pi-ghostel-start'."
  :type '(repeat string)
  :group 'ghostel)

(defun my/pi-ghostel--inject-env ()
  "Inject env vars expected by pi when running inside ghostel.
Transiently hooked by `my/pi-ghostel-start' onto `ghostel-pre-spawn-hook'."
  (setenv "PI_EMACS_AGENT_SOCKET" "pi"))

(defun my/pi-ghostel-start (&optional new-buffer)
  "Open a ghostel buffer at project root and start `pi' inside.
With prefix arg NEW-BUFFER, force a fresh ghostel buffer.

Replaces the `pi-coding-agent' Emacs package on TTY (-nw) instances:
ghostel handles the terminal mirroring, OSC 9;4 progress, OSC 8 hyperlinks,
OSC 133 prompt markers, and Korean IME (via our fork).  Pi just runs as
its own CLI."
  (interactive "P")
  ;; Ensure `ghostel-pre-spawn-hook' and friends are defined before we let-bind.
  (require 'ghostel)
  (let ((ghostel-pre-spawn-hook
         (cons #'my/pi-ghostel--inject-env ghostel-pre-spawn-hook)))
    (if new-buffer
        (let ((current-prefix-arg '(4)))
          (call-interactively #'ghostel-project))
      (call-interactively #'ghostel-project)))
  (let ((buf (current-buffer))
        (cmd (concat "pi "
                     (mapconcat #'shell-quote-argument
                                my/pi-ghostel-args " ")
                     "\n")))
    (run-with-timer
     0.4 nil
     (lambda ()
       (when (and (buffer-live-p buf)
                  (with-current-buffer buf
                    (derived-mode-p 'ghostel-mode)))
         (with-current-buffer buf
           (ghostel-send-string cmd)))))))

(map! :leader
      :desc "Pi (ghostel)" "j SPC" #'my/pi-ghostel-start)

(provide 'term-config)
;;; term-config.el ends here
