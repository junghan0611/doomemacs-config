#!/usr/bin/env emacs --script
;;; denote-export.el --- Unified Denote export & dblock server -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Unified Emacs server/batch for Denote operations:
;; - Hugo export (daemon mode with emacsclient)
;; - Dblock update (batch or daemon mode)
;;
;; Server name: "denote-export-server"
;;
;; Usage (Export - daemon mode):
;;   emacs --daemon=denote-export-server --load denote-export.el
;;   emacsclient -s denote-export-server --eval '(denote-export-file "file.org")'
;;   emacsclient -s denote-export-server --eval '(kill-emacs)'
;;
;; Usage (Dblock - batch mode):
;;   emacs --batch --load denote-export.el -- dblock ~/org/meta
;;
;; Usage (Dblock - daemon mode):
;;   emacs --daemon=denote-dblock --load denote-export.el
;;   emacsclient -s denote-dblock --eval '(denote-dblock-update-file "file.org")'

;;; Code:

;;;; Configuration

;; Server configuration
(defvar denote-export-server-name "denote-export-server"
  "Unique name for the Denote export server to avoid conflicts.")

;; Minimize startup time (will be reset after initialization)
(defvar denote-export--original-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t)

;; Disable UI elements (for daemon)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)

;; Disable file-local variables interactive prompts
(setq enable-local-variables :safe)  ; Only allow safe local variables
(setq enable-local-eval nil)         ; Never eval file-local code
(setq enable-dir-local-variables t)  ; Keep dir-locals support

;; No backup/auto-save/lockfiles (headless daemon)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Enable debug on error
(setq debug-on-error t)

;; Disable author export (prevents "John Doe" default)
(setq org-export-with-author nil)

(message "[Server] Starting Denote Export Server: %s" denote-export-server-name)

;;;; Doom Emacs Integration

;; Determine DOOMDIR
(defvar doom-user-dir
  (let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
    (if script-dir
        (expand-file-name ".." script-dir)
      (expand-file-name "~/repos/gh/doomemacs-config"))))

(message "[Server] DOOM_USER_DIR: %s" doom-user-dir)

;; Find Doom Emacs installation
(defvar doom-emacs-dir
  (or (getenv "EMACSDIR")
      (expand-file-name "~/doomemacs-starter")
      (expand-file-name "~/.config/emacs")))

;; Load Doom core (minimal)
(when (file-directory-p doom-emacs-dir)
  (setq user-emacs-directory doom-emacs-dir)
  (load (expand-file-name "lisp/doom.el" doom-emacs-dir) nil t)
  (load (expand-file-name "lisp/doom-start.el" doom-emacs-dir) nil t))

;; CRITICAL: Disable package.el completely to prevent ELPA usage
(setq package-enable-at-startup nil)
(setq package-archives nil)
(fset 'package-initialize #'ignore)
(fset 'package-install (lambda (&rest _) (error "ELPA is disabled! Use Doom's straight packages only")))
(fset 'package-refresh-contents (lambda (&rest _) (error "ELPA is disabled!")))

;;;; Native Compilation Support

;; --quick omits Doom's eln cache path from native-comp-eln-load-path.
;; Without this, packages like ox-hugo load as bytecode even when .eln exists.
(when (featurep 'native-compile)
  (let ((doom-eln-dir (expand-file-name ".local/cache/eln/" doom-emacs-dir)))
    (when (file-directory-p doom-eln-dir)
      (let ((eln-subdirs (directory-files doom-eln-dir t "^[0-9]" t)))
        (dolist (dir eln-subdirs)
          (when (file-directory-p dir)
            (add-to-list 'native-comp-eln-load-path dir))))
      (message "[Server] ✓ native-comp eln paths added from: %s" doom-eln-dir))))

;;;; Package Loading (straight.el)

;; Helper function to find straight build directory (version-agnostic)
(defun find-straight-build-dir ()
  "Find the straight build directory, handling different Emacs versions."
  (let ((straight-build-base (expand-file-name ".local/straight/" doom-emacs-dir)))
    (when (file-directory-p straight-build-base)
      (let ((build-dirs (seq-filter #'file-directory-p
                                    (directory-files straight-build-base t "^build-"))))
        (when build-dirs
          ;; Return the most recent build directory (exclude cache files)
          (car (sort build-dirs #'file-newer-than-file-p)))))))

;; Add Doom's straight paths to load-path
;; Build dir first (has .elc → enables native-comp .eln lookup)
;; Repos dir as fallback (raw .el only, append to end)
(let ((build-dir (find-straight-build-dir))
      (repos-dir (expand-file-name ".local/straight/repos/" doom-emacs-dir)))
  (when (and build-dir (file-directory-p build-dir))
    (message "[Server] Loading packages from: %s" build-dir)
    (let ((pkg-dirs (directory-files build-dir t "^[^.]" t)))
      (dolist (pkg-dir pkg-dirs)
        (when (and (file-directory-p pkg-dir)
                   (not (string-suffix-p ".el" pkg-dir)))
          (add-to-list 'load-path pkg-dir)))))
  (when (file-directory-p repos-dir)
    (let ((repo-dirs (directory-files repos-dir t "^[^.]" t)))
      (dolist (repo-dir repo-dirs)
        (when (file-directory-p repo-dir)
          (add-to-list 'load-path repo-dir t))))))

(message "[Server] Loading essential packages from Doom's straight...")

;;;; Required Packages

;; CRITICAL: Ensure Doom's org is loaded before built-in org
;; In batch mode, built-in org can be loaded first causing version mismatch
(let ((build-dir (find-straight-build-dir)))
  (when build-dir
    (let ((org-dir (expand-file-name "org" build-dir)))
      (when (file-directory-p org-dir)
        ;; Add to front of load-path to override built-in
        (push org-dir load-path)
        (message "[Server] Prioritized Doom's org: %s" org-dir)))))

;; Essential requires
(require 'org)
(require 'ox)
(require 'cl-lib)
(require 'seq)  ;; CRITICAL: Required for seq-filter in dblock functions
(require 'json)
(require 'browse-url)

;; Load required packages from Doom's straight repos
(message "[Server] Loading packages from Doom...")
(or (require 'dash nil t) (error "dash not found in Doom"))
(or (require 'denote nil t) (error "denote not found in Doom"))
(or (require 'ox-hugo nil t) (error "ox-hugo not found in Doom"))
(or (require 'citar nil t) (error "citar not found in Doom"))
(or (require 'parsebib nil t) (error "parsebib not found in Doom"))

;; Verify native-comp status for critical export functions
(when (featurep 'native-compile)
  (let ((checks '((org-hugo-export-to-md . "ox-hugo")
                  (org-export-as . "ox")
                  (org-element-parse-buffer . "org-element")))
        (native-count 0) (total 0))
    (dolist (check checks)
      (setq total (1+ total))
      (let ((native-p (and (fboundp (car check))
                           (subr-native-elisp-p (symbol-function (car check))))))
        (when native-p (setq native-count (1+ native-count)))
        (message "[Server] native-comp %s: %s"
                 (cdr check) (if native-p "✓ native" "✗ bytecode"))))
    (message "[Server] native-comp: %d/%d critical functions" native-count total)))

;; CRITICAL: Load org-cite and citar for bibliography support
(require 'oc)           ;; org-cite
(require 'oc-basic)     ;; basic citation processor
(require 'oc-csl)       ;; CSL citation processor

;; Chart package (optional)
(require 'chart nil t)

;; denote-regexp (required by denote-explore)
(require 'denote-regexp nil t)

;; denote-org for dblock functions (formerly denote-org-extras)
(condition-case err
    (progn
      (require 'denote-org)
      (message "[Server] ✓ denote-org loaded (dblock functions available)"))
  (error
   (message "[Server] WARNING: denote-org failed to load: %S" err)))

;; denote-explore (for macros like denote-explore-count-notes)
(condition-case err
    (progn
      (require 'denote-explore)
      (message "[Server] ✓ denote-explore loaded successfully"))
  (error
   (message "[Server] WARNING: denote-explore failed to load: %S" err)
   (defun denote-explore-count-notes () "0")
   (defun denote-explore-count-keywords () "0")))

(message "[Server] ✓ All required packages loaded from Doom")

;;;; Directory Configuration

;; Set denote-directory
(defvar org-directory (expand-file-name "~/org"))
(setq denote-directory (expand-file-name "~/org/"))
(message "[Server] Set denote-directory: %s" denote-directory)

;; Save original doom-user-dir
(defvar original-doom-user-dir doom-user-dir)

;; Load user info
(let ((user-info (expand-file-name "+user-info.el" original-doom-user-dir)))
  (if (file-exists-p user-info)
      (load user-info)
    (message "[Server] WARNING: +user-info.el not found")))

;; Set defaults if not defined
(unless (boundp 'user-hugo-blog-dir)
  (defvar user-hugo-blog-dir (expand-file-name "~/repos/gh/notes/")))

(unless (boundp 'user-org-directory)
  (defvar user-org-directory (expand-file-name "~/org/")))

(unless (boundp 'org-hugo-base-dir)
  (setq org-hugo-base-dir user-hugo-blog-dir))

(unless (boundp 'org-hugo-section)
  (setq org-hugo-section "notes"))

;; Force expand tilde
(when (and org-hugo-base-dir (string-prefix-p "~" org-hugo-base-dir))
  (setq org-hugo-base-dir (expand-file-name org-hugo-base-dir)))

;; Load export configuration
(let ((export-config (expand-file-name "lisp/denote-export-config.el" original-doom-user-dir)))
  (if (file-exists-p export-config)
      (load export-config)
    (error "[Server] Export config not found: %s" export-config)))

(message "[Server] Loaded export configuration")

;;;; Bibliography/Citation Setup
;; NOTE: org-cite settings are in lisp/denote-export-config.el (loaded above)
;; - org-cite-csl-link-cites t
;; - org-cite-export-processors

(require 'oc)
(require 'oc-basic)
(require 'oc-csl)

;; Configure bibliography paths (config-bibfiles defined in +user-info.el)
(when (boundp 'config-bibfiles)
  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)
  (message "[Server] Bibliography files: %S" config-bibfiles)
  (when (fboundp 'citar-refresh)
    (condition-case err
        (citar-refresh)
      (error
       (message "[Server] WARNING: citar-refresh failed: %S" err)))))

;; Verify citar is loaded
(unless (featurep 'citar)
  (message "[Server] WARNING: citar not loaded!"))

(message "[Server] Bibliography initialized")

;;;; Macro Expansion Setup

(require 'org-macro)

(defun batch-expand-macros-before-export (backend)
  "Expand all org macros before export."
  (condition-case err
      (progn
        (org-macro-initialize-templates)
        (org-macro-replace-all org-macro-templates))
    (error
     (message "[Server] WARNING: Macro expansion failed: %S" err))))

(add-hook 'org-export-before-processing-hook
          #'batch-expand-macros-before-export)

(when (featurep 'denote-explore)
  (add-to-list 'org-macro-templates
               '("denote-explore-count-notes" . "(eval (denote-explore-count-notes))") t)
  (add-to-list 'org-macro-templates
               '("denote-explore-count-keywords" . "(eval (denote-explore-count-keywords))") t))

(message "[Server] Macro expansion configured")

;;;; Helper Functions

(defun extract-denote-id-from-filename (filename)
  "Extract Denote ID (YYYYMMDDTHHMMSS) from FILENAME.
Returns nil if no ID found."
  (let ((basename (file-name-nondirectory filename)))
    (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" basename)
      (match-string 1 basename))))

(defun get-org-hugo-section-from-path (filepath)
  "Determine org-hugo-section from FILEPATH directory.
Returns meta, bib, notes, botlog, or test based on parent directory name."
  (let ((parent-dir (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory filepath)))))
    (cond
     ((string= parent-dir "meta") "meta")
     ((string= parent-dir "bib") "bib")
     ((string= parent-dir "notes") "notes")
     ((string= parent-dir "botlog") "botlog")
     ((string= parent-dir "test") "test")
     (t "notes")))) ; default to notes

;;;; Frontmatter Validation

(defun denote-export--validate-frontmatter ()
  "Validate and fix org frontmatter in current buffer before export.
Fixes:
- Missing #+hugo_lastmod: → copy from #+date:
- Missing #+identifier: → extract from filename
Returns list of applied fixes for logging."
  (let ((fixes nil)
        (modified nil))
    (save-excursion
      ;; 1. hugo_lastmod 누락 → date에서 복사
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+hugo_lastmod:" nil t)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+date:\\s-+\\(.+\\)$" nil t)
          (let ((date-val (match-string 1)))
            (goto-char (point-min))
            (if (re-search-forward "^#\\+filetags:.*$" nil t)
                ;; filetags 다음에 삽입
                (progn
                  (end-of-line)
                  (insert "\n#+hugo_lastmod: " date-val))
              ;; filetags 없으면 date 다음에 삽입
              (goto-char (point-min))
              (when (re-search-forward "^#\\+date:.*$" nil t)
                (end-of-line)
                (insert "\n#+hugo_lastmod: " date-val)))
            (push (format "added hugo_lastmod from date: %s" date-val) fixes)
            (setq modified t)))))
    ;; 수정된 경우 저장
    (when modified
      (save-buffer)
      (message "[Validate] Fixed %d issue(s): %s"
               (length fixes)
               (string-join fixes ", ")))
    fixes))

;;;; Export Functions

;; Counter for periodic garbage collection
(defvar denote-export-file-counter 0
  "Counter for tracking number of files processed by this server.")

(defvar denote-export-gc-interval 50
  "Run garbage collection every N files to prevent memory buildup.
With gc-cons-threshold at 256MB, manual GC is a safety net for RSS bounds.")

(defun denote-export-file (file)
  "Export single org FILE to Hugo markdown.
This function is called via emacsclient."
  ;; Increment file counter and check for GC
  (setq denote-export-file-counter (1+ denote-export-file-counter))
  (when (zerop (mod denote-export-file-counter denote-export-gc-interval))
    (message "[Server] [GC] Running garbage collection at file #%d..." denote-export-file-counter)
    (garbage-collect)
    (message "[Server] [GC] Done"))

  (let ((buf nil)
        (result nil))
    (condition-case err
        (let* ((dir (file-name-directory file))
               (dir-locals-file (expand-file-name ".dir-locals.el" dir))
               (dir-locals-settings nil)
               (denote-id (extract-denote-id-from-filename file)))

          ;; Verify Denote ID exists
          (unless denote-id
            (error "No Denote ID found in filename: %s" file))

          ;; Parse .dir-locals.el if exists
          (when (file-exists-p dir-locals-file)
            (with-temp-buffer
              (insert-file-contents dir-locals-file)
              (let ((locals (read (current-buffer))))
                (setq dir-locals-settings (cdr (assoc 'org-mode locals))))))

          ;; Open file and save buffer reference for cleanup
          (setq buf (find-file-noselect file))

          (unwind-protect
              (with-current-buffer buf
                ;; CRITICAL: Ensure org-mode is active (always call to reinitialize)
                (org-mode)

                ;; Validate and fix frontmatter before export
                (denote-export--validate-frontmatter)

                ;; Apply dir-locals settings
                (when dir-locals-settings
                  (dolist (setting dir-locals-settings)
                    (when (and (consp setting)
                               (not (eq (car setting) 'eval)))
                      (let* ((var-name (car setting))
                             (original-value (cdr setting))
                             (expanded-value (if (and (stringp original-value)
                                                      (string-prefix-p "~" original-value))
                                                 (expand-file-name original-value)
                                               original-value)))
                        ;; Use setq-local to properly override global settings
                        (set (make-local-variable var-name) expanded-value)
                        (set var-name expanded-value)))))

                ;; CRITICAL: Force set org-hugo-section from directory path
                ;; This overrides .dir-locals.el to ensure correct section
                (let ((computed-section (get-org-hugo-section-from-path file)))
                  ;; Set both local and global to ensure ox-hugo picks it up
                  (set (make-local-variable 'org-hugo-section) computed-section)
                  (setq org-hugo-section computed-section)
                  (message "[Server] File: %s" (file-name-nondirectory file))
                  (message "[Server] Computed section: %s" computed-section)
                  (message "[Server] Actual org-hugo-section: %s" org-hugo-section))

                ;; Verify required variables
                (unless org-hugo-base-dir
                  (error "org-hugo-base-dir is nil!"))
                (unless org-hugo-section
                  (error "org-hugo-section is nil!"))

                ;; Force expand tilde before export
                (when (and org-hugo-base-dir (string-prefix-p "~" org-hugo-base-dir))
                  (setq-local org-hugo-base-dir (expand-file-name org-hugo-base-dir)))

                (message "[Server] Before export - section: %s, base-dir: %s"
                         org-hugo-section org-hugo-base-dir)

                ;; Export
                (let* ((export-result (condition-case export-err
                                          (org-hugo-export-to-md)
                                        (error
                                         (format "ERROR:%s:%s" file (error-message-string export-err)))))
                       (final-result export-result))

                  ;; If export succeeded, rename to Denote ID and add date to frontmatter
                  (when (and (stringp export-result)
                             (not (string-prefix-p "ERROR:" export-result))
                             (file-exists-p export-result))
                    (let* ((result-dir (file-name-directory export-result))
                           (target-file (expand-file-name (concat denote-id ".md") result-dir)))
                      ;; Rename exported file to Denote ID
                      (rename-file export-result target-file t)

                      ;; Add date to frontmatter from Denote ID (only if not present)
                      ;; Format: 20230521T215600 -> 2023-05-21
                      (when (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" denote-id)
                        (let ((date-str (format "%s-%s-%s"
                                                (match-string 1 denote-id)
                                                (match-string 2 denote-id)
                                                (match-string 3 denote-id))))
                          (with-temp-file target-file
                            (insert-file-contents target-file)
                            (goto-char (point-min))
                            ;; Find frontmatter boundaries
                            (when (re-search-forward "^---\n" nil t)
                              (let ((frontmatter-start (point)))
                                (when (re-search-forward "^---\n" nil t)
                                  ;; Check if date: already exists in frontmatter
                                  (goto-char frontmatter-start)
                                  (unless (re-search-forward "^date:" (match-end 0) t)
                                    ;; No date field found, add it
                                    (goto-char (match-beginning 0))
                                    (insert (format "date: %s\n" date-str)))))))))

                      (setq final-result target-file)))

                  (setq result
                        (if (stringp final-result)
                            (if (string-prefix-p "ERROR:" final-result)
                                final-result
                              (format "SUCCESS:%s:%s" file final-result))
                          (format "ERROR:%s:Export returned nil" file)))))
            ;; CLEANUP: Always kill buffer, even on error
            (when (buffer-live-p buf)
              (kill-buffer buf))))
      (error
       (message "[Server] ✗ Error: %s" (error-message-string err))
       ;; Ensure buffer cleanup on outer error too
       (when (and buf (buffer-live-p buf))
         (kill-buffer buf))
       (setq result nil)))
    result))

;; Batch export function - process list of files independently
(defun denote-export-batch-files (files &optional log-file)
  "Export list of FILES in sequence, logging to LOG-FILE.
Each server processes its own list independently."
  (let ((total (length files))
        (success 0)
        (errors 0)
        (counter 0)
        (start-time (current-time))
        (log-buffer (when log-file (find-file-noselect log-file))))

    (message "[Batch] Starting batch export of %d files" total)

    (dolist (file files)
      (setq counter (1+ counter))
      (let* ((basename (file-name-nondirectory file))
             (progress-msg (format "[%3d/%d] Processing: %s" counter total basename))
             (result (denote-export-file file)))

        ;; Log progress
        (message "%s" progress-msg)
        (when log-buffer
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert progress-msg "\n")
            (insert (format "  Result: %s\n" result))))

        ;; Count success/error
        (if (and result (string-prefix-p "SUCCESS:" result))
            (setq success (1+ success))
          (setq errors (1+ errors)))))

    (let* ((end-time (current-time))
           (duration (float-time (time-subtract end-time start-time)))
           (speed (if (> duration 0) (/ total duration) 0))
           (summary (format "Batch completed: %d success, %d errors, %.1fs (%.3f files/sec)"
                            success errors duration speed)))

      (message "[Batch] %s" summary)
      (when log-buffer
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert "\n" summary "\n")
          (save-buffer)))

      summary)))

;; Directory export function - finds and exports all .org files in directory
(defun denote-export-directory (directory &optional shard-id total-shards)
  "Export all .org files in DIRECTORY.
If SHARD-ID and TOTAL-SHARDS are provided, only process SHARD-ID's portion
\(for parallel processing across multiple daemons).
This function handles filenames internally, avoiding shell quoting issues with NBSP."
  (let* ((all-files (directory-files directory t "\\.org$" t))
         (total-count (length all-files))
         (org-files (if (and shard-id total-shards)
                        ;; Shard files for parallel processing
                        (let* ((shard-size (ceiling (/ (float total-count) total-shards)))
                               (start-idx (truncate (* (1- shard-id) shard-size)))
                               (end-idx (min (+ start-idx shard-size) total-count)))
                          (cl-subseq all-files start-idx end-idx))
                      ;; Process all files
                      all-files))
         (file-count (length org-files))
         (success 0)
         (errors 0))

    (if (and shard-id total-shards)
        (message "[Directory] Shard %d/%d: processing %d/%d files from: %s"
                 shard-id total-shards file-count total-count directory)
      (message "[Directory] Starting export of %d files from: %s" file-count directory))
    (message "[Directory] NBSP(U+00A0) safe: processing filenames in Elisp only")

    (dolist (file org-files)
      (let* ((basename (file-name-nondirectory file))
             (result (denote-export-file file)))

        (if (and result (string-prefix-p "SUCCESS:" result))
            (progn
              (message "✓ %s" basename)
              (setq success (1+ success)))
          (message "✗ %s" basename)
          (setq errors (1+ errors)))))

    (let ((summary (format "SUCCESS:%d,ERRORS:%d,TOTAL:%d" success errors file-count)))
      (message "[Directory] Export completed: %s" summary)
      summary)))

;;;; Dblock Functions

(defun denote-dblock-update-file (file)
  "Update all dblocks in FILE and save."
  (message "[DBLOCK] Processing: %s" file)
  (let ((start-time (float-time))
        (buf nil))
    (condition-case err
        (progn
          (setq buf (find-file-noselect file))
          (with-current-buffer buf
            ;; CRITICAL: Ensure org-mode is active for dblock functions
            (unless (derived-mode-p 'org-mode)
              (org-mode))
            ;; Check if file has dblocks
            (save-excursion
              (goto-char (point-min))
              (if (re-search-forward "^#\\+BEGIN:" nil t)  ; Match any BEGIN block
                  (progn
                    (message "[DBLOCK] Found in %s" (file-name-nondirectory file))
                    (goto-char (point-min))
                    (org-update-all-dblocks)
                    (save-buffer)
                    (message "[DBLOCK] ✓ Updated in %.2fs" (- (float-time) start-time)))
                (message "[DBLOCK] SKIP: No dblock in %s" (file-name-nondirectory file)))))
          (when (buffer-live-p buf)
            (kill-buffer buf)))
      (error
       (message "[DBLOCK] ✗ Failed to update %s: %s" file err)
       (when (and buf (buffer-live-p buf))
         (kill-buffer buf))))))

(defun denote-dblock-update-directory (directory)
  "Update all dblocks in DIRECTORY recursively."
  (let* ((all-files (directory-files-recursively directory "\\.org\\'"))
         ;; Filter out Emacs lock files (.#filename)
         (org-files (seq-filter (lambda (f)
                                  (not (string-match-p "/\\.#" f)))
                                all-files))
         (total (length org-files))
         (counter 0)
         (files-with-dblocks 0)
         (start-time (float-time)))

    (message "[DBLOCK] Found %d org files in %s (excluded %d lock files)"
             total directory (- (length all-files) total))
    (message "========================================")

    (dolist (file org-files)
      (setq counter (1+ counter))
      (message "[%d/%d] Processing..." counter total)

      ;; Quick pre-check: does file have dblock?
      (condition-case nil
          (when (with-temp-buffer
                  (insert-file-contents file nil 0 5000) ; Check first 5KB
                  (goto-char (point-min))
                  (re-search-forward "^#\\+BEGIN:" nil t))  ; Match any BEGIN block
            (setq files-with-dblocks (1+ files-with-dblocks))
            (denote-dblock-update-file file))
        (file-missing (message "[DBLOCK] SKIP: File not found: %s" file))))

    (let ((duration (- (float-time) start-time)))
      (message "========================================")
      (message "[DBLOCK] SUMMARY: Completed %d/%d files" counter total)
      (message "[DBLOCK] SUMMARY: Files with dblocks: %d" files-with-dblocks)
      (message "[DBLOCK] SUMMARY: Total time: %.2fs (%.3f files/sec)"
               duration
               (if (> duration 0) (/ (float counter) duration) 0))
      (message "========================================"))))

;;;; Memory Management

;; CRITICAL: Reset gc-cons-threshold to reasonable value after initialization
;; 256MB reduces auto-GC frequency during org-export heavy allocation
;; Manual GC every 50 files keeps memory bounded
(setq gc-cons-threshold (* 256 1024 1024))
(garbage-collect)  ; Force initial GC
(message "[Server] gc-cons-threshold reset to 256MB, initial GC done")

;;;; Server Startup (for daemon mode)

;; Set ready flag AFTER all initialization
(setq denote-export-server-ready t)

(message "[Server] ========================================")
(message "[Server] Denote Export Server FULLY READY!")
(message "[Server] Server name: %s" denote-export-server-name)
(message "[Server] Export function: denote-export-file")
(message "[Server] Dblock function: denote-dblock-update-file")
(message "[Server] Ready flag: %s" denote-export-server-ready)
(message "[Server] GC interval: every %d files" denote-export-gc-interval)
(message "[Server] ========================================")

;; Keep server running (for daemon mode)
;; NOTE: server-name is automatically set to daemon-name when started with --daemon=name
(unless noninteractive
  (server-start)
  (message "[Server] Server socket created"))

(provide 'denote-export)
;;; denote-export.el ends here
