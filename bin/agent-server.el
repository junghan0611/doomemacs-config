;;; agent-server.el --- Minimal Emacs daemon for OpenClaw agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Lightweight Emacs daemon for OpenClaw agent access via emacsclient.
;; Based on denote-export.el — loads only essential packages from Doom's
;; straight build directory. No UI, no themes, no keybindings.
;;
;; Architecture:
;;   [Docker: OpenClaw] → emacsclient (Nix store mount)
;;     → Unix socket /run/emacs/agent-server
;;       → [Host: emacs --daemon=agent-server --load agent-server.el]
;;
;; The agent uses `emacs_eval` (algal/openclaw-emacs-tools) to:
;;   1. Call predefined functions below
;;   2. Define new functions at runtime (self-extending REPL)
;;   3. Stable functions graduate into this file
;;
;; Usage:
;;   # Start daemon
;;   emacs --daemon=agent-server --load ~/repos/gh/doomemacs-config/bin/agent-server.el
;;
;;   # Test from host
;;   emacsclient -s agent-server --eval '(agent-server-status)'
;;
;;   # Test from Docker (with Nix store mount)
;;   /nix/store/.../emacsclient -s /run/emacs/agent-server --eval '(agent-server-status)'
;;
;;   # Stop
;;   emacsclient -s agent-server --eval '(kill-emacs)'

;;; Code:

;;;; Configuration

(defvar agent-server-name "agent-server"
  "Server name for the agent daemon.")

(defvar agent-server-version "0.1.0"
  "Version of agent-server.el.")

;; Minimize startup
(defvar agent-server--original-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t)

;; Headless — no UI
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)

;; Safe file-local variables only
(setq enable-local-variables :safe)
(setq enable-local-eval nil)
(setq enable-dir-local-variables t)

;; No backup/auto-save files (Doom disables these, but we're not Doom)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(message "[agent-server] Starting v%s ..." agent-server-version)

;;;; Doom Straight Package Loading Infrastructure
;; Reused from denote-export.el — loads packages from Doom's build dir
;; without starting Doom itself.

(defvar doom-user-dir
  (let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
    (if script-dir
        (expand-file-name ".." script-dir)
      (expand-file-name "~/repos/gh/doomemacs-config"))))

(defvar doom-emacs-dir
  (or (getenv "EMACSDIR")
      (expand-file-name "~/.emacs.d")))

;; Load Doom core (minimal — for straight.el paths)
(when (file-directory-p doom-emacs-dir)
  (setq user-emacs-directory doom-emacs-dir)
  (load (expand-file-name "lisp/doom.el" doom-emacs-dir) nil t)
  (load (expand-file-name "lisp/doom-start.el" doom-emacs-dir) nil t))

;; Disable package.el — Doom uses straight.el
(setq package-enable-at-startup nil)
(setq package-archives nil)
(fset 'package-initialize #'ignore)

;;;; Native Compilation

(when (featurep 'native-compile)
  (let ((doom-eln-dir (expand-file-name ".local/cache/eln/" doom-emacs-dir)))
    (when (file-directory-p doom-eln-dir)
      (let ((eln-subdirs (directory-files doom-eln-dir t "^[0-9]" t)))
        (dolist (dir eln-subdirs)
          (when (file-directory-p dir)
            (add-to-list 'native-comp-eln-load-path dir))))
      (message "[agent-server] ✓ native-comp eln paths added"))))

;;;; Load Path Setup

(defun agent-server--find-straight-build-dir ()
  "Find the straight build directory (version-agnostic)."
  (let ((base (expand-file-name ".local/straight/" doom-emacs-dir)))
    (when (file-directory-p base)
      (let ((dirs (seq-filter #'file-directory-p
                              (directory-files base t "^build-"))))
        (when dirs
          (car (sort dirs #'file-newer-than-file-p)))))))

(let ((build-dir (agent-server--find-straight-build-dir))
      (repos-dir (expand-file-name ".local/straight/repos/" doom-emacs-dir)))
  ;; Build dir first (has .elc/.eln)
  (when (and build-dir (file-directory-p build-dir))
    (message "[agent-server] Loading from: %s" build-dir)
    (dolist (pkg-dir (directory-files build-dir t "^[^.]" t))
      (when (and (file-directory-p pkg-dir)
                 (not (string-suffix-p ".el" pkg-dir)))
        (add-to-list 'load-path pkg-dir))))
  ;; Repos dir as fallback
  (when (file-directory-p repos-dir)
    (dolist (repo-dir (directory-files repos-dir t "^[^.]" t))
      (when (file-directory-p repo-dir)
        (add-to-list 'load-path repo-dir t)))))

;; Prioritize Doom's org over built-in
(let ((build-dir (agent-server--find-straight-build-dir)))
  (when build-dir
    (let ((org-dir (expand-file-name "org" build-dir)))
      (when (file-directory-p org-dir)
        (push org-dir load-path)))))

;;;; Essential Packages

(message "[agent-server] Loading packages...")

;; Core
(require 'org)
(require 'ox)
(require 'cl-lib)
(require 'seq)
(require 'json)

;; From Doom straight
(or (require 'dash nil t) (message "[agent-server] WARN: dash not found"))
(or (require 'denote nil t) (message "[agent-server] WARN: denote not found"))
(or (require 'citar nil t) (message "[agent-server] WARN: citar not found"))
(or (require 'parsebib nil t) (message "[agent-server] WARN: parsebib not found"))

;; Org-cite
(require 'oc nil t)
(require 'oc-basic nil t)
(require 'oc-csl nil t)

;; Denote extensions
(require 'denote-regexp nil t)
(condition-case nil (require 'denote-org) (error nil))
(condition-case nil (require 'denote-explore) (error nil))

(message "[agent-server] ✓ Packages loaded (org %s, denote %s)"
         (org-version)
         (if (featurep 'denote) (denote-retrieve-title-or-filename
                                  (expand-file-name "~/org/") nil)
           "N/A"))

;;;; Directory & Bibliography Configuration

;; NOTE 2026-03-01: ~/org → ~/sync/org 심볼릭 링크 환경.
;; Doom은 find-file-visit-truename=t로 buffer-file-name이 실제 경로로 resolve됨.
;; agent-server는 Doom이 아니라 find-file-visit-truename=nil (기본값) →
;; buffer-file-name이 ~/org/... 그대로 → expand-file-name으로 충분.
;;
;; "워크플로우 공유(Workflow Sharing)":
;; 인간(Doom)과 에이전트(agent-server)가 동일한 ~/org를 읽고 쓴다.
;; botlog/는 에이전트 rw — 봇로그 문서, botlog/agenda/ reverse-datetree 기록.
;; 인간의 org-agenda가 botlog/agenda/를 포함하므로,
;; 에이전트 활동이 인간의 agenda 뷰에 자연스럽게 나타난다.
(defvar org-directory (expand-file-name "~/org"))
(setq denote-directory (expand-file-name "~/org/"))

;; Load user info (+user-info.el)
(let ((user-info (expand-file-name "+user-info.el" doom-user-dir)))
  (when (file-exists-p user-info)
    (load user-info nil t)
    (message "[agent-server] ✓ User info loaded")))

;; Bibliography
(when (boundp 'config-bibfiles)
  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)
  (when (fboundp 'citar-refresh)
    (condition-case nil (citar-refresh) (error nil)))
  (message "[agent-server] ✓ Bibliography: %d files" (length config-bibfiles)))

;;;; Denote Silo (multi-directory support)

;; denote-silo-extras for multi-directory notes
(condition-case nil
    (progn
      (require 'denote-silo-extras nil t)
      (message "[agent-server] ✓ denote-silo loaded"))
  (error nil))

;;;; Security: Path Access Control
;; CRITICAL: emacs daemon runs on HOST — Docker ro mounts are bypassed.
;; All file operations must enforce access control here.

(defvar agent-server-read-paths
  '("/home/junghan/org/"
    "/home/junghan/repos/gh/"
    "/home/junghan/repos/work/"
    "/home/junghan/repos/3rd/")
  "Paths the agent can READ from.")

(defvar agent-server-write-paths
  '("/home/junghan/org/botlog/"
    "/home/junghan/repos/gh/self-tracking-data/")
  "Paths the agent can WRITE to. Must match Docker rw mounts.")

(defun agent-server--path-allowed-p (file mode)
  "Check if FILE access is allowed for MODE (read or write).
Returns t if allowed, signals error if not."
  (let* ((expanded (expand-file-name file))
         (paths (pcase mode
                  ('read (append agent-server-write-paths agent-server-read-paths))
                  ('write agent-server-write-paths)
                  (_ (error "Unknown mode: %s" mode)))))
    (unless (cl-some (lambda (prefix)
                       (string-prefix-p (expand-file-name prefix) expanded))
                     paths)
      (error "ACCESS DENIED: %s not allowed for %s. Allowed: %S"
             expanded mode paths))
    t))

;; Override dangerous built-ins when called from agent context
;; NOTE: These protect against (write-region ... "/etc/passwd") via emacs_eval
(defvar agent-server--original-write-region (symbol-function 'write-region))
(defvar agent-server--write-guard-enabled t
  "When non-nil, write-region checks agent-server-write-paths.")

(defun agent-server--guarded-write-region (start end filename &rest args)
  "Write-region with path guard. Only allows writes to agent-server-write-paths."
  (when agent-server--write-guard-enabled
    (agent-server--path-allowed-p filename 'write))
  (apply agent-server--original-write-region start end filename args))

;; Install guard — uncomment to enforce globally (strict mode)
;; (fset 'write-region #'agent-server--guarded-write-region)
;; NOTE: Global override disabled for now — internal packages (org, denote)
;; also use write-region. Enable per-function guards instead.

;;;; Agent Functions
;; These are the stable API that the agent calls via emacs_eval.
;; New functions can be defined at runtime via emacs_eval — when stable,
;; they graduate into this section.

(defun agent-server-status ()
  "Return server status as a plist string."
  (format "(:version %S :server %S :org-version %S :denote-dir %S :packages (%s) :uptime %S)"
          agent-server-version
          agent-server-name
          (org-version)
          denote-directory
          (mapconcat #'symbol-name
                     (seq-filter (lambda (f) (featurep f))
                                 '(org denote citar oc oc-csl denote-org denote-explore))
                     " ")
          (emacs-uptime)))

(defun agent-org-read-file (file)
  "Read org FILE and return its contents as string.
FILE should be an absolute path."
  (agent-server--path-allowed-p file 'read)
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))
    (format "ERROR: File not found: %s" file)))

(defun agent-org-get-headings (file &optional max-level)
  "Return headings from org FILE as a list of (LEVEL TITLE) pairs.
MAX-LEVEL limits depth (default: all levels)."
  (agent-server--path-allowed-p file 'read)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let (headings)
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (hl)
            (let ((level (org-element-property :level hl))
                  (title (org-element-property :raw-value hl)))
              (when (or (null max-level) (<= level max-level))
                (push (list level title) headings)))))
        (nreverse headings)))))

(defun agent-org-get-properties (file)
  "Return file-level properties (#+KEY: VALUE) from org FILE as alist."
  (agent-server--path-allowed-p file 'read)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let (props)
        (dolist (key '("TITLE" "DATE" "FILETAGS" "IDENTIFIER" "REFERENCE"))
          (let ((val (org-with-point-at (point-min)
                       (when (re-search-forward
                              (format "^#\\+%s:\\s-+\\(.+\\)$" key) nil t)
                         (match-string-no-properties 1)))))
            (when val
              (push (cons key val) props))))
        (nreverse props)))))

(defun agent-denote-search (query &optional type)
  "Search Denote notes by QUERY.
TYPE can be 'title, 'tag, or 'fulltext (default: 'title).
Returns list of (ID TITLE TAGS FILE)."
  (let ((files (denote-directory-files))
        (type (or type 'title))
        results)
    (dolist (file files)
      (let* ((id (denote-retrieve-filename-identifier file))
             (title (or (denote-retrieve-title-value file
                          (denote-filetype-heuristics file))
                        ""))
             (tags (denote-extract-keywords-from-path file))
             (match
              (pcase type
                ('title (string-match-p (regexp-quote query) title))
                ('tag (member query tags))
                ('fulltext
                 (when (file-exists-p file)
                   (with-temp-buffer
                     (insert-file-contents file nil 0 50000) ; first 50KB
                     (goto-char (point-min))
                     (search-forward query nil t)))))))
        (when match
          (push (list id title tags file) results))))
    (nreverse results)))

(defun agent-citar-lookup (query &optional max-results)
  "Search bibliography for QUERY. Returns up to MAX-RESULTS (default 10)."
  (if (not (featurep 'citar))
      "ERROR: citar not loaded"
    (let ((max-results (or max-results 10))
          (entries (citar--format-candidates))
          results)
      (maphash (lambda (key value)
                 (when (and (< (length results) max-results)
                            (string-match-p (regexp-quote query) key))
                   (push key results)))
               entries)
      (nreverse results))))

(defun agent-org-dblock-update (file)
  "Update all dynamic blocks in org FILE.
Requires write access — dblock update modifies and saves the file."
  (agent-server--path-allowed-p file 'write)
  (if (not (file-exists-p file))
      (format "ERROR: File not found: %s" file)
    (let ((buf (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buf
            (unless (derived-mode-p 'org-mode) (org-mode))
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "^#\\+BEGIN:" nil t)
                  (progn
                    (goto-char (point-min))
                    (org-update-all-dblocks)
                    (save-buffer)
                    (format "OK: Updated dblocks in %s" (file-name-nondirectory file)))
                (format "SKIP: No dblocks in %s" (file-name-nondirectory file)))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

;;;; Memory Management

(setq gc-cons-threshold (* 128 1024 1024)) ; 128MB — lighter than export server
(garbage-collect)
(message "[agent-server] gc-cons-threshold: 128MB")

;;;; Server Ready

(setq agent-server-ready t)

(message "[agent-server] ========================================")
(message "[agent-server] READY — v%s" agent-server-version)
(message "[agent-server] Server: %s" agent-server-name)
(message "[agent-server] Org: %s" (org-version))
(message "[agent-server] Denote dir: %s" denote-directory)
(message "[agent-server] API: agent-server-status, agent-org-read-file,")
(message "[agent-server]      agent-org-get-headings, agent-org-get-properties,")
(message "[agent-server]      agent-denote-search, agent-citar-lookup,")
(message "[agent-server]      agent-org-dblock-update")
(message "[agent-server] REPL: emacs_eval for runtime extension")
(message "[agent-server] ========================================")

;; Server socket (daemon mode auto-creates, but explicit for clarity)
(unless noninteractive
  (server-start)
  (message "[agent-server] Socket created"))

(provide 'agent-server)
;;; agent-server.el ends here
