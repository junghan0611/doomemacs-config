;;; $DOOMDIR/lisp/tmux-config.el --- tmux control surface for ghostel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; tmux runs *inside* ghostel: agents live in tmux so a crashed Emacs does not
;; take them down, and the same session can be picked up from a bare terminal.
;; The shell side is SSOT and Emacs owns none of it — `tm' opens a session named
;; after the directory, `tml' picks one with fzf
;; (nixos-config: users/junghan/modules/shell.nix), and `~/.config/tmux/tmux.conf'
;; binds the in-terminal keys.  This file adds only what the terminal cannot
;; reach: Emacs-side keys for the tmux client living in a ghostel buffer.
;;
;; Why the keys have to be `C-c t ...' and not the leader: in ghostel's semi-char
;; mode `SPC' is sent to the PTY, so `doom-leader' never fires inside a terminal
;; buffer.  What comes back to Emacs is exactly `ghostel-keymap-exceptions'
;; (`C-c', `C-x', `M-x', ...), so `C-c' is the only prefix available in the
;; buffer where tmux actually is.
;;
;; Why not emamux: `emamux:in-tmux-p' tests `(getenv "TMUX")' — it assumes Emacs
;; itself runs inside tmux and drives a sibling pane.  Here the nesting is the
;; other way round, so every emamux entry point aborts.  Removed 2026-08-30.
;;
;; What the terminal already does, and this file deliberately does not duplicate:
;;   M-[ / M-]     previous/next window   (see `lisp/term-config.el' for M-[)
;;   M-\           next pane
;;   M-c s / M-c S session picker (fzf popup) / choose-tree
;;   M-c Enter     copy mode
;;
;; Session <-> buffer mapping is owned here rather than sniffed: a tmux client
;; is only visible as a pty in `tmux list-clients', and ghostel's pty is native
;; (`ghostel-use-native-pty'), so it is not reachable from Emacs.  `my/tmux-attach'
;; therefore records the session in `my/tmux-session' when it opens the buffer.

;;; Code:

(require 'seq)
(require 'map)

;;;; Session <-> ghostel buffer

(defvar-local my/tmux-session nil
  "Name of the tmux session this ghostel buffer is attached to.
Set by `my/tmux-attach'.  Nil in any other buffer.")

(defun my/tmux--run (&rest args)
  "Run tmux with ARGS and return stdout, or nil when tmux failed."
  (with-temp-buffer
    (when (= 0 (apply #'call-process "tmux" nil t nil args))
      (buffer-string))))

(defun my/tmux--lines (&rest args)
  "Run tmux with ARGS and return stdout as a list of non-empty lines."
  (seq-remove #'string-empty-p
              (split-string (or (apply #'my/tmux--run args) "") "\n")))

(defun my/tmux--sessions ()
  "Return an alist of (LABEL . SESSION) for every live tmux session.
LABEL mirrors what `tml' shows: name, window count, attach mark, path."
  (mapcar (lambda (line)
            (let ((fields (split-string line "\t")))
              (cons (format "%-20s %-8s %s"
                            (nth 0 fields) (nth 1 fields)
                            (abbreviate-file-name (or (nth 2 fields) "")))
                    (nth 0 fields))))
          (my/tmux--lines
           "list-sessions" "-F"
           "#{session_name}\t#{session_windows}w #{?session_attached,●,}\t#{session_path}")))

(defun my/tmux--windows (session)
  "Return an alist of (LABEL . INDEX) for the windows of SESSION."
  (mapcar (lambda (line)
            (let ((fields (split-string line "\t")))
              (cons (format "%-4s %-20s %s"
                            (nth 0 fields) (nth 1 fields) (nth 2 fields))
                    (nth 0 fields))))
          (my/tmux--lines
           "list-windows" "-t" (concat "=" session) "-F"
           "#{window_index}#{window_flags}\t#{window_name}\t#{pane_current_command}")))

(defun my/tmux--window-index (label)
  "Strip tmux window flags (*, -, Z, ...) from LABEL, leaving the index."
  (and label (car (split-string label "[^0-9]" t))))

(defun my/tmux--session (&optional prompt-always)
  "Return the tmux session for the current context.
Uses `my/tmux-session' when the buffer has one; otherwise, or when
PROMPT-ALWAYS is non-nil, asks.  Signals when no session is running."
  (or (and (not prompt-always) my/tmux-session)
      (let ((sessions (my/tmux--sessions)))
        (unless sessions
          (user-error "No tmux session running (open one with `tm' in a shell)"))
        (cdr (assoc (completing-read "tmux session: " sessions nil t)
                    sessions)))))

(defun my/tmux--buffer-name (session)
  "Return the ghostel buffer name dedicated to SESSION."
  (format "*tmux:%s*" session))

;;;; Commands

;;;###autoload
(defun my/tmux-attach (session)
  "Attach tmux SESSION in its own ghostel buffer, or switch to it if open.
The Emacs-side counterpart of `tml': one buffer per session, so switching
sessions is ordinary buffer switching and `tmux switch-client' — which would
need the client pty Emacs cannot see — is never required."
  (interactive (list (my/tmux--session 'prompt)))
  (let* ((name (my/tmux--buffer-name session))
         (buf (get-buffer name)))
    (unless (and buf (buffer-live-p buf)
                 (with-current-buffer buf (process-live-p ghostel--process)))
      (let ((ghostel-buffer-name name))
        (setq buf (ghostel t)))
      (with-current-buffer buf
        (setq my/tmux-session session)
        ;; -A attaches an existing session instead of erroring, matching `tm'.
        (ghostel-send-string
         (format "tmux new-session -A -s %s\n" (shell-quote-argument session)))))
    (pop-to-buffer buf)))

;;;###autoload
(defun my/tmux-window-switch ()
  "Pick a window of this buffer's tmux session and select it.
`select-window' needs no client target — every client attached to the session
follows — so this works no matter which buffer or terminal is attached."
  (interactive)
  (let* ((session (my/tmux--session))
         (windows (my/tmux--windows session)))
    (unless windows
      (user-error "tmux session %s has no windows" session))
    (let* ((label (completing-read (format "tmux %s window: " session) windows nil t))
           (index (my/tmux--window-index (cdr (assoc label windows)))))
      (my/tmux--run "select-window" "-t" (format "=%s:%s" session index))
      (message "tmux %s → window %s" session index))))

;;;###autoload
(defun my/tmux-window-next ()
  "Select the next window of this buffer's tmux session (Emacs-side `M-]')."
  (interactive)
  (my/tmux--run "next-window" "-t" (concat "=" (my/tmux--session))))

;;;###autoload
(defun my/tmux-window-previous ()
  "Select the previous window of this buffer's tmux session (Emacs-side `M-[')."
  (interactive)
  (my/tmux--run "previous-window" "-t" (concat "=" (my/tmux--session))))

;;;###autoload
(defun my/tmux-window-new (&optional name)
  "Create a window in this buffer's tmux session, keeping the current path.
The Emacs-side counterpart of `M-c c'.  With a prefix argument, ask for a
window NAME."
  (interactive (list (when current-prefix-arg (read-string "Window name: "))))
  (let* ((session (my/tmux--session))
         ;; `-c "#{pane_current_path}"' would expand against the *calling*
         ;; client, not the target session — measured 2026-08-30, it picked up
         ;; the caller's shell path.  Resolve the target's path first.
         (target (concat "=" session ":"))
         (path (string-trim
                (or (my/tmux--run "display-message" "-p" "-t" target
                                  "#{pane_current_path}")
                    "")))
         (args (append (list "new-window" "-t" (concat "=" session))
                       (unless (string-empty-p path) (list "-c" path))
                       (when name (list "-n" name))
                       (list "-P" "-F" "#{window_index}:#{window_name}"))))
    (message "tmux %s → new window %s"
             session (string-trim (or (apply #'my/tmux--run args) "?")))))

;;;###autoload
(defun my/tmux-window-kill ()
  "Kill the current window of this buffer's tmux session, after confirming.
Anything running in that window dies with it, so this always asks."
  (interactive)
  (let* ((session (my/tmux--session))
         (target (concat "=" session ":"))
         (what (string-trim
                (or (my/tmux--run "display-message" "-p" "-t" target
                                  "#{window_index}:#{window_name} (#{pane_current_command})")
                    "?"))))
    (when (yes-or-no-p (format "Kill tmux window %s:%s? " session what))
      (my/tmux--run "kill-window" "-t" target)
      (message "Killed tmux %s:%s" session what))))

;;;###autoload
(defun my/tmux-list-sessions ()
  "Show the live tmux sessions, the way `tml' lists them."
  (interactive)
  (let ((sessions (my/tmux--sessions)))
    (if sessions
        (message "%s" (mapconcat #'car sessions "\n"))
      (message "No tmux session running"))))

;;;; Herd — which agents are actually doing something

;; The question this answers is narrow on purpose: is anything still running in
;; that session, or is it three agents sitting idle and worth killing?  It is a
;; resource decision, not a workflow one — "waiting for approval" is deliberately
;; not modeled (agents run yolo here, so that state barely exists).
;;
;; The signal is `window_activity': tmux's timestamp of the last *pane output* in
;; a window.  What resets it, measured 2026-08-30 by writing straight to
;; `pane_tty' (send-keys would have echoed and polluted the reading):
;;
;;   a printed line            -> reset      a spinner redrawing in place -> reset
;;   \033[?25l\033[?25h (invisible) -> reset      an OSC 2 title change        -> reset
;;   nothing at all            -> keeps growing, second for second
;;
;; So it counts *bytes arriving*, not visible change — anything the program emits
;; keeps the window alive.  `monitor-activity' being off does not affect it; that
;; option only gates the alert flag.  Do not substitute `session_activity', which
;; also counts client interaction: a session silent for 16h still reported 2s
;; because a client was attached to it.
;;
;; That leaves one worry — an app that repaints on a timer would look busy
;; forever.  Sampled the whole board twice, 30s apart: all 18 silent panes came
;; back at exactly +30s, so nothing here (pi, Claude Code, shells) repaints while
;; parked.  The converse also held: every working agent sat pinned at 0s, because
;; these CLIs animate a spinner while they think.  A long silent tool call is
;; therefore the only realistic false `idle', which is what the generous
;; threshold below is for.
;;
;; Parsing `pane_title' was the other candidate (agent CLIs do broadcast state
;; there via OSC 0/2 — `✳ …' while Claude Code works, `● …: done' / `π - …' from
;; pi).  It is shown as context but never used to classify: the vocabulary is per
;; CLI and changes without notice, which is exactly how the previous version of
;; this file rotted.  Idle time is CLI-neutral.

(defconst my/tmux-herd-idle-threshold 60
  "Seconds of silence after which an agent window counts as idle.
Agents that are working redraw constantly — every actively running window
measured on 2026-08-30 sat at 0-2s, while ones parked at a prompt were minutes
to hours old.  60 is deliberately generous: a long silent tool call should read
as running, and the exact idle time is on screen anyway for the close calls.")

(defconst my/tmux-herd-shells '("bash" "zsh" "fish" "sh" "-bash" "-zsh")
  "Foreground commands that mean no agent is running in that window.")

(defun my/tmux--herd-windows ()
  "Return one plist per tmux window: :session :index :cmd :idle :title :dead."
  (let ((now (float-time)))
    (mapcar (lambda (line)
              (let ((f (split-string line "\t")))
                (list :session (nth 0 f)
                      :index   (nth 1 f)
                      :cmd     (nth 2 f)
                      :idle    (round (- now (string-to-number (nth 3 f))))
                      :dead    (equal (nth 4 f) "1")
                      :title   (or (nth 5 f) ""))))
            (my/tmux--lines
             "list-panes" "-a" "-F"
             (concat "#{session_name}\t#{window_index}\t#{pane_current_command}\t"
                     "#{window_activity}\t#{pane_dead}\t#{pane_title}")))))

(defun my/tmux--herd-window-state (win)
  "Classify WIN as `dead', `free' (no agent), `run', or `idle'."
  (cond ((plist-get win :dead) 'dead)
        ((member (plist-get win :cmd) my/tmux-herd-shells) 'free)
        ((< (plist-get win :idle) my/tmux-herd-idle-threshold) 'run)
        (t 'idle)))

(defun my/tmux--herd-sessions ()
  "Group windows by session.  Returns (SESSION STATE IDLE WINDOWS AGENTS PATH).
STATE is `run' when any window is still producing output, `idle' when agents are
loaded but all silent, and `free' when nothing but shells is left."
  (let ((paths (mapcar (lambda (line)
                         (let ((f (split-string line "\t")))
                           (cons (nth 0 f) (nth 1 f))))
                       (my/tmux--lines "list-sessions" "-F"
                                       "#{session_name}\t#{session_path}")))
        (by-session (make-hash-table :test #'equal)))
    (dolist (win (my/tmux--herd-windows))
      (push win (gethash (plist-get win :session) by-session)))
    (sort
     (map-apply
      (lambda (session wins)
        (let* ((states (mapcar #'my/tmux--herd-window-state wins))
               (agents (seq-remove (lambda (s) (memq s '(free dead))) states)))
          (list session
                (cond ((memq 'run states) 'run)
                      (agents 'idle)
                      (t 'free))
                (apply #'min (mapcar (lambda (w) (plist-get w :idle)) wins))
                (length wins)
                (length agents)
                (or (cdr (assoc session paths)) ""))))
      by-session)
     (lambda (a b)
       ;; Running first, then longest-silent — the kill candidates sink together.
       (let ((rank (lambda (row) (pcase (nth 1 row) ('run 0) ('idle 1) (_ 2)))))
         (if (= (funcall rank a) (funcall rank b))
             (> (nth 2 a) (nth 2 b))
           (< (funcall rank a) (funcall rank b))))))))

(defun my/tmux--herd-format-idle (seconds)
  "Render SECONDS as a compact age."
  (cond ((< seconds 60) (format "%ds" seconds))
        ((< seconds 3600) (format "%dm" (/ seconds 60)))
        ((< seconds 86400) (format "%dh" (/ seconds 3600)))
        (t (format "%dd" (/ seconds 86400)))))

(defun my/tmux--herd-entries ()
  "Build `tabulated-list-entries' for the herd view."
  (mapcar
   (pcase-lambda (`(,session ,state ,idle ,windows ,agents ,path))
     (list session
           (vector
            (pcase state
              ('run  (propertize "run"  'face 'success))
              ('idle (propertize "idle" 'face 'warning))
              (_     (propertize "free" 'face 'shadow)))
            (my/tmux--herd-format-idle idle)
            session
            (format "%d/%d" agents windows)
            (abbreviate-file-name path))))
   (my/tmux--herd-sessions)))

(defvar-keymap my/tmux-herd-mode-map
  :doc "Keymap for `my/tmux-herd-mode'."
  "RET" #'my/tmux-herd-attach
  "w"   #'my/tmux-herd-windows
  "D"   #'my/tmux-herd-kill)

(define-derived-mode my/tmux-herd-mode tabulated-list-mode "tmux-herd"
  "Session-level view of what is still running under tmux."
  (setq tabulated-list-format
        [("St" 5 t) ("Quiet" 7 t) ("Session" 22 t) ("Agents" 7 t) ("Path" 40 t)]
        tabulated-list-sort-key nil
        tabulated-list-entries #'my/tmux--herd-entries)
  (tabulated-list-init-header))

;; `tabulated-list-mode' lands in evil's motion state, where `RET'/`w' are
;; already taken (`evil-ret', `evil-forward-word-begin'), so the plain keymap
;; above never fires under evil.  Kill is `D' rather than the `k' dired habit
;; would suggest: `k' is evil's line-up, and shadowing it in a list buffer is
;; how you delete the wrong session.
(map! :map my/tmux-herd-mode-map
      :nm "RET" #'my/tmux-herd-attach
      :nm "w"   #'my/tmux-herd-windows
      :nm "D"   #'my/tmux-herd-kill
      :nm "gr"  #'tabulated-list-revert)

(defun my/tmux-herd--session-at-point ()
  "Return the session on the current herd line, or signal."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun my/tmux-herd-attach ()
  "Open the session on this line in its ghostel buffer."
  (interactive)
  (my/tmux-attach (my/tmux-herd--session-at-point)))

(defun my/tmux-herd-windows ()
  "Show the windows of the session on this line, with their idle times."
  (interactive)
  (let ((session (my/tmux-herd--session-at-point)))
    (message
     "%s"
     (mapconcat
      (lambda (win)
        (format "%s:%-3s %-8s %-6s %s"
                session (plist-get win :index) (plist-get win :cmd)
                (my/tmux--herd-format-idle (plist-get win :idle))
                (plist-get win :title)))
      (seq-filter (lambda (w) (equal (plist-get w :session) session))
                  (my/tmux--herd-windows))
      "\n"))))

(defun my/tmux-herd-kill ()
  "Kill the tmux session on this line, after confirming.
Also kills its ghostel buffer, so no buffer is left attached to nothing."
  (interactive)
  (let ((session (my/tmux-herd--session-at-point)))
    (when (yes-or-no-p (format "Kill tmux session %s (and anything running in it)? "
                               session))
      (my/tmux--run "kill-session" "-t" (concat "=" session))
      (when-let* ((buf (get-buffer (my/tmux--buffer-name session))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (tabulated-list-revert)
      (message "Killed tmux session %s" session))))

;;;###autoload
(defun my/tmux-herd ()
  "Show which tmux sessions still have an agent doing something.
`RET' opens a session, `w' lists its windows, `k' kills it, `g' refreshes."
  (interactive)
  (let ((buf (get-buffer-create "*tmux-herd*")))
    (with-current-buffer buf
      (my/tmux-herd-mode)
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;;; Keybindings

;; Inside a ghostel buffer only `ghostel-keymap-exceptions' reaches Emacs, so
;; these live under `C-c t'.  `C-c C-t' is ghostel's own copy mode — left alone.
(after! ghostel
  (define-key ghostel-mode-map (kbd "C-c t w") #'my/tmux-window-switch)
  (define-key ghostel-mode-map (kbd "C-c t n") #'my/tmux-window-next)
  (define-key ghostel-mode-map (kbd "C-c t p") #'my/tmux-window-previous)
  (define-key ghostel-mode-map (kbd "C-c t s") #'my/tmux-attach)
  (define-key ghostel-mode-map (kbd "C-c t l") #'my/tmux-list-sessions)
  (define-key ghostel-mode-map (kbd "C-c t h") #'my/tmux-herd)
  (define-key ghostel-mode-map (kbd "C-c t c") #'my/tmux-window-new)
  (define-key ghostel-mode-map (kbd "C-c t k") #'my/tmux-window-kill))

;; Outside a terminal buffer the leader works normally.  Keys only — naming a
;; prefix here would bind a fresh keymap over Doom's (see AGENTS.md § map!).
(map! :leader
      (:prefix "\\"
       (:prefix "t"
        :desc "Attach session"     "t" #'my/tmux-attach
        :desc "Switch window"      "w" #'my/tmux-window-switch
        :desc "Next window"        "n" #'my/tmux-window-next
        :desc "Previous window"    "p" #'my/tmux-window-previous
        :desc "List sessions"      "l" #'my/tmux-list-sessions
        :desc "Herd (status board)" "h" #'my/tmux-herd
        :desc "New window"         "c" #'my/tmux-window-new
        :desc "Kill window"        "k" #'my/tmux-window-kill)))

(provide 'tmux-config)
;;; tmux-config.el ends here
