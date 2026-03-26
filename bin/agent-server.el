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

;; org-element 캐시 비활성화 — 멀티 에이전트 환경
;; 인간(Doom)과 다른 에이전트가 같은 org 파일을 동시 접근.
;; persistent 캐시가 있으면 stale 데이터를 읽을 수 있음.
;; agent-server는 항상 디스크에서 최신 상태를 읽어야 한다.
(setq org-element-use-cache nil)
(setq org-element-cache-persistent nil)

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

;; NOTE: doom.el, doom-start.el 로드하면 Doom 전체 초기화 실행 →
;; 기존 Doom GUI 서버와 충돌 ("Emacs server already running").
;; load-path만 필요하므로 doom 코어 로드하지 않음.
;; straight build 디렉토리에서 직접 load-path 구성.
(when (file-directory-p doom-emacs-dir)
  (setq user-emacs-directory doom-emacs-dir))

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

;; 워크플로우 공유: 인간(Doom)과 동일한 agenda 설정
(let ((shared (expand-file-name "lisp/workflow-shared.el" doom-user-dir)))
  (when (file-exists-p shared)
    (load shared nil t)
    (message "[agent-server] ✓ workflow-shared loaded (agenda-files: %d)"
             (length org-agenda-files))))

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
    "/home/junghan/repos/gh/self-tracking-data/"
    "/home/junghan/repos/gh/naver-saiculture/")
  "Paths the agent can WRITE to. Must match Docker rw mounts.")

(defvar agent-server-denote-rename-paths
  '("/home/junghan/org/")
  "Paths where denote rename (front-matter → filename sync) is allowed.
Rename only changes filename, not file content. Weaker than write.")

(defun agent-server--path-allowed-p (file mode)
  "Check if FILE access is allowed for MODE (read, write, or rename).
Returns t if allowed, signals error if not."
  (let* ((expanded (expand-file-name file))
         (paths (pcase mode
                  ('read (append agent-server-write-paths
                                 agent-server-denote-rename-paths
                                 agent-server-read-paths))
                  ('write agent-server-write-paths)
                  ('rename (append agent-server-write-paths
                                   agent-server-denote-rename-paths))
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
;;
;; ⚠️ 활성화 시 주의: agent-denote-add-* 함수들이 save-buffer 호출 →
;; write-region 경유 → write 모드 체크. ~/org/notes/ 등은 write-paths에 없으므로
;; ERROR. 활성화하려면 guarded-write-region에서 denote-append-paths도 허용하거나,
;; agent-denote-add-* 내부에서 write-guard-enabled을 let-bind로 nil 처리해야 함.

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

(defun agent-denote-rename-by-front-matter (file)
  "FILE의 front-matter(#+title, #+filetags 등)를 읽어 파일명을 자동 변경.
`denote-rename-confirmations'를 nil로 바인딩하여 y-or-n-p 프롬프트 없이 실행.
에이전트가 headless 환경에서 안전하게 호출할 수 있다.

Rename 후 검증: front-matter의 #+title, #+filetags 와 파일명이 일치하는지 확인.
불일치 시 결과에 WARN 포함."
  (agent-server--path-allowed-p file 'rename)
  (if (not (file-exists-p file))
      (format "ERROR: File not found: %s" file)
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t)
          (denote-kill-buffers 'on-rename))
      (condition-case err
          (let* ((new-name (denote-rename-file-using-front-matter file))
                 (result-file (or new-name file))
                 (result-name (file-name-nondirectory result-file))
                 ;; Post-rename verification
                 (warnings nil))
            ;; Check #+title matches filename title part
            (when (file-exists-p result-file)
              (with-temp-buffer
                (insert-file-contents result-file nil 0 4096)
                (goto-char (point-min))
                ;; Verify #+identifier matches filename ID
                (when (re-search-forward "^#\\+identifier:\\s-+\\([0-9T]+\\)" nil t)
                  (let ((fm-id (match-string 1))
                        (fn-id (denote-retrieve-filename-identifier result-file)))
                    (unless (equal fm-id fn-id)
                      (push (format "identifier mismatch: fm=%s fn=%s" fm-id fn-id)
                            warnings))))))
            (if warnings
                (format "OK(WARN): %s → %s | %s"
                        (file-name-nondirectory file) result-name
                        (string-join warnings "; "))
              (format "OK: %s → %s"
                      (file-name-nondirectory file) result-name)))
        (user-error (format "SKIP: %s — %s"
                            (file-name-nondirectory file)
                            (error-message-string err)))
        (error (format "ERROR: %s — %s"
                       (file-name-nondirectory file)
                       (error-message-string err)))))))

(defun agent-denote-rename-bulk (directory)
  "DIRECTORY 내 모든 denote 파일의 front-matter 기반 일괄 rename.
결과를 (renamed skipped errors) 카운트로 반환."
  (agent-server--path-allowed-p directory 'rename)
  (let ((files (directory-files-recursively directory "\\.org$"))
        (renamed 0) (skipped 0) (errors 0)
        (denote-rename-confirmations nil)
        (denote-save-buffers t)
        (denote-kill-buffers 'on-rename))
    (dolist (file files)
      (when (denote-file-has-identifier-p file)
        (condition-case nil
            (progn
              (denote-rename-file-using-front-matter file)
              (cl-incf renamed))
          (user-error (cl-incf skipped))
          (error (cl-incf errors)))))
    (format "Done: %d renamed, %d skipped, %d errors (total: %d files)"
            renamed skipped errors (length files))))

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

(defun agent-being-data (&optional as-json)
  "Return current user 'being data' as a plist (or JSON with AS-JSON non-nil).
Delegates to `workflow-shared-being-data'; data is computed once at server
startup via `workflow-shared-compute-being-data' and then cached.

Example return value:
  (:notes 3328 :journal-days 1477 :garden 2178 :bib 671
   :notes-formatted \"3,328\" :journal-days-formatted \"1,477\"
   :garden-formatted \"2,178\" :bib-formatted \"671\"
   :computed-at \"2026-03-26 Thu 10:00\")

Usage:
  emacsclient -s agent-server --eval '(agent-being-data)'
  emacsclient -s agent-server --eval '(agent-being-data t)'  ; JSON"
  (if (fboundp 'workflow-shared-being-data)
      (workflow-shared-being-data as-json)
    (message "[agent-server] WARNING: workflow-shared not loaded")
    nil))

;;;; Org Agenda API

(defun agent-org-agenda--ensure-journal ()
  "현재 주 + 이전 2주 journal 파일을 org-agenda-files에 추가.
geworfen이 ±14일 지원하므로 3주치 필요 (현재 주 + 이전 2주).
org-journal 미로드 환경에서 직접 경로 계산.
~/org → ~/sync/org 심볼릭 링크 환경에서 중복 등록 방지."
  (let* ((now (current-time))
         (dow (string-to-number (format-time-string "%u" now)))) ; 1=Mon..7=Sun
    ;; 0, 7, 14일 전의 월요일 기준 journal 파일 등록
    (dolist (days-back '(0 7 14))
      (let* ((past (time-subtract now (days-to-time days-back)))
             (past-dow (string-to-number (format-time-string "%u" past)))
             (monday (time-subtract past (days-to-time (1- past-dow))))
             (week-file (format-time-string
                         "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org" monday))
             (journal-path (expand-file-name
                            (concat "journal/" week-file) org-directory))
             (journal-true (file-truename journal-path))
             (already (cl-some (lambda (f) (string= (file-truename f) journal-true))
                               org-agenda-files)))
        (when (and (file-exists-p journal-path) (not already))
          (add-to-list 'org-agenda-files journal-path t)
          (message "[agent-server] journal added: %s"
                   (file-name-nondirectory journal-path)))))))

(defun agent-org-agenda--refresh-buffers ()
  "Agenda 파일 버퍼를 디스크에서 갱신. 외부 변경 반영.
버퍼가 없으면 열어서 최신 상태로 로드 (첫 호출 시 필요).
디렉토리 항목은 내부 .org 파일로 확장.
~/org → ~/sync/org symlink 환경: file-truename으로 버퍼 탐색."
  (let ((files (cl-mapcan
                (lambda (entry)
                  (if (file-directory-p entry)
                      (directory-files entry t "\\.org$")
                    (list entry)))
                org-agenda-files)))
    (dolist (file files)
      (when (and (file-exists-p file)
                 (not (file-directory-p file)))
        (let* ((true-path (file-truename file))
               (buf (or (get-file-buffer file)
                        (get-file-buffer true-path))))
          (if buf
              (with-current-buffer buf
                (when (not (buffer-modified-p))
                  (revert-buffer t t t)))
            (find-file-noselect true-path)))))))

(defun agent-org-agenda-day (&optional date)
  "오늘(또는 DATE) 일간 agenda 뷰를 clean text로 반환.
DATE는 \"-1\" (어제), \"+3\" (3일 후), \"2026-03-01\" 등.
Human + Agent + Diary 통합 타임라인.

호출마다:
  1. journal 파일을 org-agenda-files에 추가 (없으면)
  2. 열린 버퍼를 디스크에서 갱신 (외부 변경 반영)
  3. wide window에서 agenda 생성 (텍스트 잘림 방지)"
  ;; 1. journal 등록 + 버퍼 갱신
  (agent-org-agenda--ensure-journal)
  (agent-org-agenda--refresh-buffers)
  ;; 2. agenda 생성 — headless에서 frame-width=80이라 텍스트 잘림 방지
  (set-frame-width nil 300)
  (let ((org-agenda-sticky nil)
        (org-agenda-window-setup 'current-window)
        (org-agenda-tags-column 140)  ;; 태그를 140컬럼에 좌측정렬 (일관된 위치)
        ;; 카테고리 전체 출력: Agent(T), Agent(O), Human 등
        (org-agenda-prefix-format
         '((agenda  . " %i %-10:c%?-12t% s")
           (todo    . " %i %-10:c")
           (tags    . " %i %-10:c")
           (search  . " %i %-10:c")))
        (day (cond
              ((null date) nil)
              ((string-match "^[+-]?[0-9]+$" date)
               (+ (org-today) (string-to-number date)))
              (t (org-time-string-to-absolute date)))))
    (org-agenda-list nil day 1)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer)
      content)))

(defun agent-org-agenda-week (&optional date)
  "DATE 기준 주간 agenda 뷰를 clean text로 반환."
  (agent-org-agenda--ensure-journal)
  (agent-org-agenda--refresh-buffers)
  (set-frame-width nil 300)
  (let ((org-agenda-sticky nil)
        (org-agenda-window-setup 'current-window)
        (org-agenda-tags-column -300)
        (org-agenda-prefix-format
         '((agenda  . " %i %-10:c%?-12t% s")
           (todo    . " %i %-10:c")
           (tags    . " %i %-10:c")
           (search  . " %i %-10:c")))
        (day (cond
              ((null date) nil)
              ((string-match "^[+-]?[0-9]+$" date)
               (+ (org-today) (string-to-number date)))
              (t (org-time-string-to-absolute date)))))
    (org-agenda-list nil day 7)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer)
      content)))

(defun agent-org-agenda-tags (match)
  "태그 MATCH 조건으로 필터링된 agenda 뷰 반환.
예: \"commit\", \"pi|botlog\", \"+emacs-draft\"."
  (let ((org-agenda-sticky nil)
        (org-agenda-window-setup 'current-window))
    (org-tags-view nil match)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer)
      content)))

;;;; Denote Operations
;; Structured append-only operations for denote files.
;; Counterpart: emacs skill (SKILL.md) documents these for agents.
;; See: [[denote:20260322T080400][에이전트 denote 오퍼레이션 프로토콜 설계]]

(defvar agent-server-denote-append-paths
  '("/home/junghan/org/")
  "Paths where agent-denote-add-* functions can APPEND content.
Separate from `agent-server-write-paths' — append-only is safer than overwrite.")

(defun agent-server--denote-append-allowed-p (file)
  "Check if FILE allows denote append operations.
FILE must be a denote file (has identifier) under `agent-server-denote-append-paths'."
  (let ((expanded (expand-file-name file)))
    (unless (and (denote-file-has-identifier-p expanded)
                 (cl-some (lambda (prefix)
                            (string-prefix-p (expand-file-name prefix) expanded))
                          agent-server-denote-append-paths))
      (error "APPEND DENIED: %s is not a denote file under allowed paths" expanded))
    t))

(defun agent-denote-keywords ()
  "Return sorted list of all denote keywords currently in use.
Agents should query this before creating new tags to avoid fragmentation.
Note: scans all files — may be slow on first call (3000+ notes)."
  (denote-keywords))

(defun agent-denote-add-history (id content)
  "Add a timestamped entry to the History section of denote file ID.

ID is a denote identifier (e.g. \"20260302T191200\").
CONTENT is the text after the timestamp (e.g. \"@pi-claude — 작업 완료\").

The entry is inserted as the first item under the '* 히스토리' or '* History'
heading, so the newest entry is always on top.

Returns OK/ERROR string.  Creates the heading if CREATE-IF-MISSING."
  (let* ((file (denote-get-path-by-id id)))
    (if (not file)
        (format "ERROR: No denote file for ID %s" id)
      (agent-server--denote-append-allowed-p file)
      (condition-case err
          (let ((buf (find-file-noselect file))
                (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
                (history-re "^\\* \\(?:히스토리\\|History\\)"))
            (unwind-protect
                (with-current-buffer buf
                  (unless (derived-mode-p 'org-mode) (org-mode))
                  (save-excursion
                    (goto-char (point-min))
                    (if (not (re-search-forward history-re nil t))
                        ;; No history heading — create one after front-matter
                        (progn
                          (goto-char (point-min))
                          ;; Skip front-matter (find first blank line after keywords)
                          (if (re-search-forward "^$" nil t)
                              (forward-line 1)
                            (goto-char (point-max)))
                          (insert (format "\n* 히스토리\n- %s %s\n" timestamp content))
                          (save-buffer)
                          (format "OK: Created 히스토리 heading + entry in %s"
                                  (file-name-nondirectory file)))
                      ;; Found history heading — insert after it
                      (forward-line 1)
                      (insert (format "- %s %s\n" timestamp content))
                      (save-buffer)
                      (format "OK: Added history entry to %s"
                              (file-name-nondirectory file)))))
              (when (buffer-live-p buf)
                (kill-buffer buf))))
        (error (format "ERROR: %s — %s" id (error-message-string err)))))))

(defun agent-denote-add-heading (id heading content &optional after-heading)
  "Add a level-1 heading to denote file ID.

HEADING is the heading text (without the leading '* ').
CONTENT is the body text under the heading.
AFTER-HEADING, if given, is an existing heading text to insert after
\(uses `org-end-of-subtree' to find the section end\).
If nil, appends at end of file.

Returns OK/ERROR string."
  (let* ((file (denote-get-path-by-id id)))
    (if (not file)
        (format "ERROR: No denote file for ID %s" id)
      (agent-server--denote-append-allowed-p file)
      (condition-case err
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (unless (derived-mode-p 'org-mode) (org-mode))
                  (save-excursion
                    (if after-heading
                        ;; Find the target heading and go to end of its subtree
                        (progn
                          (goto-char (point-min))
                          (let ((re (format "^\\* %s\\(?:[ \t]\\|$\\)" (regexp-quote after-heading))))
                            (if (not (re-search-forward re nil t))
                                (error "Heading not found: %s" after-heading)
                              (org-end-of-subtree t)
                              (unless (bolp) (insert "\n")))))
                      ;; No after-heading — go to end of file
                      (goto-char (point-max))
                      (unless (bolp) (insert "\n")))
                    (insert (format "\n* %s\n\n%s\n" heading content))
                    (save-buffer)
                    (format "OK: Added heading '%s' to %s"
                            heading (file-name-nondirectory file))))
              (when (buffer-live-p buf)
                (kill-buffer buf))))
        (error (format "ERROR: %s — %s" id (error-message-string err)))))))

(defun agent-denote-add-link (id target-id description)
  "Add a denote link to file ID, pointing to TARGET-ID.

Looks for a '관련' or '관련 노트' or 'Related' heading.
If found, appends the link there.  If not, creates '** 관련' at end of file.

DESCRIPTION is the link text.  Returns OK/ERROR string."
  (let* ((file (denote-get-path-by-id id)))
    (if (not file)
        (format "ERROR: No denote file for ID %s" id)
      (agent-server--denote-append-allowed-p file)
      ;; Verify target exists
      (let ((target-file (denote-get-path-by-id target-id)))
        (if (not target-file)
            (format "ERROR: Target denote file not found for ID %s" target-id)
          (condition-case err
              (let ((buf (find-file-noselect file)))
                (unwind-protect
                    (with-current-buffer buf
                      (unless (derived-mode-p 'org-mode) (org-mode))
                      (save-excursion
                        (goto-char (point-min))
                        (let ((link-text (format "- [[denote:%s][%s]]\n" target-id description))
                              (related-re "^\\*+ \\(?:관련\\(?: 노트\\)?\\|Related\\)"))
                          (if (re-search-forward related-re nil t)
                              ;; Found related heading — append after last item
                              (progn
                                (org-end-of-subtree t)
                                (unless (bolp) (insert "\n"))
                                (insert link-text))
                            ;; No related heading — create at end
                            (goto-char (point-max))
                            (unless (bolp) (insert "\n"))
                            (insert (format "\n** 관련\n%s" link-text)))
                          (save-buffer)
                          (format "OK: Added link to %s in %s"
                                  target-id (file-name-nondirectory file)))))
                  (when (buffer-live-p buf)
                    (kill-buffer buf))))
            (error (format "ERROR: %s — %s" id (error-message-string err)))))))))

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
(message "[agent-server]      agent-denote-rename-by-front-matter,")
(message "[agent-server]      agent-denote-rename-bulk,")
(message "[agent-server]      agent-org-dblock-update,")
(message "[agent-server]      agent-org-agenda-day, agent-org-agenda-week,")
(message "[agent-server]      agent-org-agenda-tags,")
(message "[agent-server]      agent-denote-keywords, agent-denote-add-history,")
(message "[agent-server]      agent-denote-add-heading, agent-denote-add-link")
(message "[agent-server] REPL: emacs_eval for runtime extension")
(message "[agent-server] ========================================")

;; Server socket (daemon mode auto-creates, but explicit for clarity)
(unless noninteractive
  (server-start)
  (message "[agent-server] Socket created"))

(provide 'agent-server)
;;; agent-server.el ends here
