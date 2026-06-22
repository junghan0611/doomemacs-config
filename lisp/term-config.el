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

;; Experimental — on-demand only.  Loaded lazily via `:commands`/`:defer';
;; invoke explicitly through `M-x ghostel`, `M-x ghostel-project`, or
;; `M-x my/pi-ghostel-start' when you want it.  No leader binding and no
;; project-switch entry on purpose — vterm stays the stable daily driver,
;; ghostel is a "I want to test something" detour.  Re-enable the
;; commented hooks below once daily-use signal accumulates.
(use-package! ghostel
  :defer t
  :commands (ghostel ghostel-project my/pi-ghostel-start)
  :hook ((ghostel-mode . doom-mark-buffer-as-real-h)
         ;; Opt-in Emacs Lisp IME integration (hangul2/quail commit-forward
         ;; + redraw-defer during composition).  Must be on before
         ;; `toggle-input-method' so its activate-hook wraps the live IME.
         (ghostel-mode . ghostel-ime-mode))
  :if (and (fboundp 'module-load)
           module-file-suffix
           (not (bound-and-true-p my/termux-p)))
  :init
  (setq ghostel-module-auto-install 'ask)
  :config
  (setq ghostel-shell (or explicit-shell-file-name
                          shell-file-name
                          (getenv "SHELL")
                          "/bin/bash"))
  ;; Default is xterm-ghostty. If remote terminfo gets noisy, temporarily use:
  ;; (setq ghostel-term "xterm-256color")
  ;; Keep side effects opt-in during evaluation.
  ;; (setq ghostel-enable-osc52 t)
  ;; (setq ghostel-tramp-shell-integration t)

  ;;; project.el integration — disabled while ghostel is experimental.
  ;; Re-enable once daily-use signal accumulates.
  ;; (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)

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

;; Leader binding parked while ghostel is experimental.
;; Invoke via `M-x my/pi-ghostel-start' when you want to test it.
;; (map! :leader
;;       :desc "Pi (ghostel)" "j SPC" #'my/pi-ghostel-start)


;;; term-sessions — browse zmx live sessions from Emacs

;; zmx (neurosnap) owns the session lifecycle/PTYs/history; Emacs is just a
;; client that shells out for control ops and attaches through a frontend.
;; This is the Emacs counterpart of the `zx' fzf picker in ~/.bashrc.local:
;; `term-sessions-consult-session' lists every live session (cc/cx/agy/pi/...)
;; with a `zmx history' preview and attaches the chosen one in one step.
;;
;; Session names are plain (`cc.<dir>', not prefixed), so leave
;; `term-sessions-zmx-session-prefix' nil to see all of them.  `zmx' resolves
;; via PATH (~/.local/bin until the nixos-config rebuild lands a system zmx).
(use-package! term-sessions
  :defer t
  :commands (term-sessions-consult-session
             term-sessions-open
             term-sessions-list
             term-sessions-history
             term-sessions-kill)
  :init
  ;; Set the frontend in `:init', BEFORE the package loads.  `term-sessions-open'
  ;; reads `term-sessions-preferred-frontend' at open time and ignores any
  ;; stored spec frontend, so a `:config' setq (which only runs *after* the
  ;; autoloaded command has already triggered load) is too late and races the
  ;; `term' defcustom default.  ghostel, not vterm: ghostel's `ghostel-mode-hook'
  ;; fires `ghostel-ime-mode' (see the ghostel use-package above), so Korean
  ;; input works inside attached sessions — vterm has no equivalent IME path
  ;; here.  `evil-ghostel-mode' also routes ESC to the PTY, which is what
  ;; attaching to a live claude/pi session wants anyway.
  (setq term-sessions-preferred-frontend 'ghostel)
  (map! :leader
        (:prefix ("j" . "pi-agent")
         :desc "zmx sessions (consult)" "z" #'term-sessions-consult-session)))


;;; zmx agent-harness launchers — Emacs analog of zcc/zcx/zagy/zep

;; The shell launchers in ~/.bashrc.local run
;;   zmx attach "<prefix>.<basename PWD>" <command>
;; to create-or-attach a per-project session running an agent harness.
;; `term-sessions-open' passes COMMAND straight to `zmx attach'
;; (`term-sessions--attach-args'), so the same one-handle semantics — create
;; with the harness command on first open, reattach as-is afterwards — fall out
;; of it.  `term-sessions-run' is the wrong tool: it runs a one-shot job
;; *inside* a session and captures output, not an interactive TUI attach.
(defvar my/zmx-harnesses
  '((claude . (:prefix "cc"  :command "claude"))
    (codex  . (:prefix "cx"  :command "codex"))
    (agy    . (:prefix "agy" :command "agy"))
    (pi     . (:prefix "pi"  :command "bash -lc \"source $HOME/.bashrc.local; pit\"")))
  "Agent harnesses launchable as zmx sessions via `my/zmx-launch'.
Each entry maps a harness key to a plist with `:prefix' (the zmx
session-name prefix, matching ~/.bashrc.local) and `:command' (passed to
`zmx attach' for session creation).  `:command' is parsed with
`split-string-and-unquote', which honours double quotes but not single
quotes — quote multi-word arguments with \\\"...\\\".

The `pi' command runs through `bash -lc' so the `pit' shell function — and
the `_pi_garden_pi' it calls — are sourced from ~/.bashrc.local.  zmx exec's
its argv directly, so a bare `pit' would not resolve; the wrapper is what
the `zpi' launcher in ~/.bashrc.local does.  The model and flags
\(--model openai-codex/gpt-5.5 --entwurf-control --emacs-agent-socket server)
live inside `pit' there, not in this file.")

(defun my/zmx--project-root ()
  "Return the current project root, or `default-directory' when outside one."
  (or (when-let* ((proj (project-current)))
        (project-root proj))
      default-directory))

(defun my/zmx-launch (harness)
  "Create-or-attach a zmx session running agent HARNESS at the project root.
HARNESS is a key in `my/zmx-harnesses'.  The session is named
\"<prefix>.<project>\" and attached through the ghostel frontend, mirroring
the zcc/zcx/zagy/zep launchers in ~/.bashrc.local."
  (interactive
   (list (intern (completing-read "zmx harness: "
                                  (mapcar #'car my/zmx-harnesses) nil t))))
  (let* ((spec (map-elt my/zmx-harnesses harness))
         (root (my/zmx--project-root))
         (name (concat (plist-get spec :prefix) "."
                       (file-name-nondirectory (directory-file-name root))))
         (command (plist-get spec :command))
         (default-directory root))
    (term-sessions-open name command)))

(map! :leader
      (:prefix ("j" . "pi-agent")
       :desc "zmx launch harness" "a" #'my/zmx-launch))

(provide 'term-config)
;;; term-config.el ends here
