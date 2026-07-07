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

;; ghostel is the daily in-Emacs terminal *and* the agent-tool surface (vterm is
;; retired, commented out in init.el).  Since 2026-07 the official `:term
;; ghostel' module (enabled with `+everywhere' in init.el) owns the base
;; `use-package! ghostel': mode-line-invisible, line-number disable, solaire,
;; persp buffer-name guard, and comint/compile/eshell integration + `evil-ghostel'.
;; This file is now an *override layer* — only what the module does not wire —
;; plus our own entry points (`my/ghostel-toggle'/`-here', `my/pi-ghostel-start',
;; zmx launchers) further below.  We version-manage the package via `unpin!'
;; (packages.el), so ghostel tracks dakra/main rather than the module's stale pin.

(after! ghostel
  ;; ghostel buffers are long-lived agent sessions (claude/codex/pi): treat them
  ;; as real so `doom/kill-this-buffer' and real-buffer cycling respect them.
  ;; Declarative, mirroring how `app/irc' registers `circe-mode'.
  (add-to-list 'doom-real-buffer-modes 'ghostel-mode)
  (setq ghostel-module-auto-install 'ask)
  (setq ghostel-shell (or explicit-shell-file-name
                          shell-file-name
                          (getenv "SHELL")
                          "/bin/bash"))
  ;; Default term is xterm-ghostty.  If remote terminfo gets noisy, temporarily:
  ;; (setq ghostel-term "xterm-256color")
  ;; Opt-in side effects, left off by default:
  ;; (setq ghostel-enable-osc52 t)
  ;; (setq ghostel-tramp-shell-integration t)

  ;; Whitelist Doom commands callable from inside the terminal via `ghostel_cmd'.
  ;; Defaults already include find-file(-other-window), dired(-other-window), message.
  (dolist (cmd '(("magit-status-setup-buffer" magit-status-setup-buffer)
                 ("dired-jump"                 dired-jump)
                 ("+default/find-file-under-here" +default/find-file-under-here)))
    (add-to-list 'ghostel-eval-cmds cmd))

  ;; OSC 9;4 progress protocol — claude code, codex, pi CLI all emit this.
  ;; spinner.el ships with magit; fall back to text when unavailable.
  (when (locate-library "spinner")
    (setq ghostel-progress-function #'ghostel-spinner-progress
          ghostel-spinner-type 'progress-bar)))

;; Korean Emacs-Lisp input method — bundled in the ghostel package but not wired
;; by the module.  README's documented opt-in; must be on before
;; `toggle-input-method' so its activate-hook wraps the live IME.  The fix that
;; lets hangul compose in the read-only terminal buffer (PR #510) is now upstream.
(use-package! ghostel-ime
  :hook (ghostel-mode . ghostel-ime-mode))

;; evil-ghostel: the module enables the mode under `:editor evil' + `+everywhere'.
;; We only route insert-state ESC to the PTY — Claude Code, codex, pi each expose
;; their own vim-style mode that expects ESC, and alt-screen apps (vim, htop) need
;; it too.  `,.' (evil-escape) reaches Emacs normal state; `C-c C-t' opens
;; scrollback (`ghostel-copy-mode'); `evil-ghostel-toggle-send-escape' flips it
;; per buffer when needed.
(after! evil-ghostel
  (setq evil-ghostel-escape 'terminal))


;;; toggle / here — our replacements for `+vterm/*' and the module's `+ghostel/*'

;; The `:term ghostel' module ships `+ghostel/toggle'/`+ghostel/here', but we
;; keep our own: the module's `+ghostel/toggle' references `buffer-name' as a
;; variable (void-variable in its prefix-arg branch) and enters neither evil
;; insert state nor the Korean IME — both of which an agent terminal needs the
;; moment it appears.  ghostel displays same-window by default, so the popup
;; behaviour comes from the `set-popup-rule!' below matching the popup buffer
;; name; `my/ghostel-toggle' then shows/hides the popup window.
;;
;; Unlike vterm's popup (`:ttl 0', killed on close), we keep `:ttl nil' and
;; toggle by hiding the window — ghostel here is an *agent tool*, so a popup
;; toggle must never kill the running session (claude/codex/pi).

(set-popup-rule! "^\\*doom:ghostel-popup:"
  :size 0.30 :vslot -4 :select t :quit nil :ttl nil)

(defun my/ghostel--popup-name ()
  "Return the per-workspace ghostel popup buffer name.
Mirrors `+vterm/toggle's persp-scoped naming so each workspace keeps
its own popup terminal."
  (format "*doom:ghostel-popup:%s*"
          (if (bound-and-true-p persp-mode)
              (safe-persp-name (get-current-persp))
            "main")))

(defun my/ghostel--enter-insert (buffer)
  "Select BUFFER's window and enter evil insert state.
ghostel here is an agent terminal: it must be ready for input the moment
it appears.  evil also keeps the input method *off* in normal state, so
the Korean IME (`ghostel-ime-mode') only engages once we are in insert —
re-showing a hidden popup that was left in normal state would otherwise
swallow Hangul toggles.  No-op when evil is absent or already in insert."
  (when-let* ((win (and (buffer-live-p buffer) (get-buffer-window buffer))))
    (with-selected-window win
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-insert-state-p)))
        (evil-insert-state)))))

(defun my/ghostel--at-project-root (arg display-fn)
  "Set PROOT to the project root and call DISPLAY-FN there.
With prefix ARG non-nil, stay in `default-directory' instead of the
project root.  Ghostel analog of `+vterm--configure-project-root-and-display'."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load ghostel"))
  (require 'ghostel)
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory (if arg default-directory project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

(defun my/ghostel-toggle (arg)
  "Toggle a ghostel terminal popup window at the project root.
With prefix ARG, recreate the popup buffer in the current project's root.
Returns the ghostel buffer.  The ghostel counterpart of `+vterm/toggle'."
  (interactive "P")
  (my/ghostel--at-project-root
   arg
   (lambda ()
     (let ((bname (my/ghostel--popup-name)))
       (when arg
         (let ((buffer (get-buffer bname))
               (window (get-buffer-window bname)))
           (when (buffer-live-p buffer)
             (let (kill-buffer-query-functions)
               (kill-buffer buffer)))
           (when (window-live-p window)
             (delete-window window))))
       (if-let* ((win (get-buffer-window bname)))
           ;; Visible -> hide.  `delete-window' (not popup `:ttl' kill) keeps
           ;; the agent session alive behind the curtain.
           (delete-window win)
         ;; Reuse the public `ghostel' entry so identity, IME, evil-ghostel,
         ;; and process startup match a normal ghostel buffer; the popup rule
         ;; on BNAME makes `display-buffer' float it.
         (let ((ghostel-buffer-name bname))
           (ghostel))
         ;; Enter insert on every show (create *and* re-show) so the terminal
         ;; is ready and the Korean IME path is live.
         (my/ghostel--enter-insert (get-buffer bname)))
       (get-buffer bname)))))

(defun my/ghostel-here (arg)
  "Open a ghostel terminal in the current window at the project root.
With prefix ARG, use `default-directory' instead of the project root.
The ghostel counterpart of `+vterm/here'."
  (interactive "P")
  (my/ghostel--at-project-root
   arg
   (lambda ()
     ;; Bind `display-buffer-alist' to nil to bypass popup rules and force
     ;; same-window display, exactly as `+vterm/here' does.  Plain `ghostel'
     ;; (single reusable buffer) honours the ambient `default-directory'.
     (let (display-buffer-alist)
       (ghostel))
     (my/ghostel--enter-insert (current-buffer)))))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Toggle ghostel popup" "t" #'my/ghostel-toggle
       :desc "Ghostel here"         "T" #'my/ghostel-here))


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
