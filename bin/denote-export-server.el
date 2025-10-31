#!/usr/bin/env emacs --script
;;; denote-export-server.el --- Emacs server for Denote export -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Emacs daemon for fast Denote → Hugo export via emacsclient.
;;
;; Server name: "denote-export-server"
;;
;; Usage:
;;   # Start server
;;   emacs --daemon=denote-export-server --load denote-export-server.el
;;
;;   # Export via client
;;   emacsclient -s denote-export-server --eval '(export-file "file.org")'
;;
;;   # Stop server
;;   emacsclient -s denote-export-server --eval '(kill-emacs)'

;;; Code:

;; Server configuration
(defvar denote-export-server-name "denote-export-server"
  "Unique name for the Denote export server to avoid conflicts.")

;; Minimize startup time
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

;; Enable debug on error
(setq debug-on-error t)

;; Disable author export (prevents "John Doe" default)
(setq org-export-with-author nil)

(message "[Server] Starting Denote Export Server: %s" denote-export-server-name)

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

;; Initialize package system
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Add ELPA archives if needed
(unless (assoc "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))

;; Helper function to require/install packages
(defun ensure-package (package)
  "Ensure PACKAGE is installed and loaded."
  (unless (require package nil t)
    (message "[Server] Installing %s..." package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)
    (require package)))

(message "[Server] Loading essential packages...")

;; Essential requires
(require 'org)
(require 'ox)

;; Install required packages
(ensure-package 'ox-hugo)
(ensure-package 'dash)
(ensure-package 'denote)
(ensure-package 'citar)

;; CRITICAL: Load org-cite and citar for bibliography support
(require 'oc)           ;; org-cite
(require 'oc-csl)       ;; CSL citation processor
(require 'citar)        ;; citar frontend

(message "[Server] Loading denote-explore dependencies...")

;; Built-in packages
(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'browse-url)

;; Chart package (optional)
(condition-case err
    (require 'chart)
  (error
   (message "[Server] WARNING: chart not available")))

;; denote-regexp
(unless (require 'denote-regexp nil t)
  (let ((denote-regexp-paths (list
                              (expand-file-name ".local/straight/build-30.1.90/denote-regexp" doom-emacs-dir)
                              (expand-file-name ".local/straight/repos/denote-regexp" doom-emacs-dir))))
    (dolist (path denote-regexp-paths)
      (when (file-directory-p path)
        (add-to-list 'load-path path)))
    (condition-case err
        (require 'denote-regexp)
      (error
       (message "[Server] WARNING: Could not load denote-regexp")))))

;; denote-explore
(unless (require 'denote-explore nil t)
  (let ((search-paths (list
                       (expand-file-name ".local/elpa/denote-explore" doom-emacs-dir)
                       (expand-file-name ".local/straight/repos/denote-explore" doom-emacs-dir)
                       (expand-file-name ".local/straight/build-30.1.90/denote-explore" doom-emacs-dir))))
    (dolist (path search-paths)
      (when (file-directory-p path)
        (add-to-list 'load-path path)))
    (condition-case err
        (require 'denote-explore)
      (error
       (message "[Server] WARNING: denote-explore not loaded")
       (defun denote-explore-count-notes () "0")
       (defun denote-explore-count-keywords () "0")))))

;; Set denote-directory
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
(let ((export-config (expand-file-name "+denote-export.el" original-doom-user-dir)))
  (if (file-exists-p export-config)
      (load export-config)
    (error "[Server] Export config not found: %s" export-config)))

(message "[Server] Loaded export configuration")

;; ========== Bibliography/Citation 초기화 ==========
(require 'oc)
(require 'oc-basic)
(require 'oc-csl)

(setq org-cite-export-processors
      '((html . (csl "apa.csl"))
        (latex . biblatex)
        (t . (basic))))

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

;; ========== 매크로 확장 설정 ==========
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

;; ========== Helper Functions ==========
(defun extract-denote-id-from-filename (filename)
  "Extract Denote ID (YYYYMMDDTHHMMSS) from FILENAME.
Returns nil if no ID found."
  (let ((basename (file-name-nondirectory filename)))
    (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" basename)
      (match-string 1 basename))))

(defun get-org-hugo-section-from-path (filepath)
  "Determine org-hugo-section from FILEPATH directory.
Returns meta, bib, notes, or test based on parent directory name."
  (let ((parent-dir (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory filepath)))))
    (cond
     ((string= parent-dir "meta") "meta")
     ((string= parent-dir "bib") "bib")
     ((string= parent-dir "notes") "notes")
     ((string= parent-dir "test") "test")
     (t "notes")))) ; default to notes

;; ========== Export Function ==========
(defun denote-export-file (file)
  "Export single org FILE to Hugo markdown.
This function is called via emacsclient."
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

        ;; Open file and apply settings
        (with-current-buffer (find-file-noselect file)
          ;; CRITICAL: Ensure org-mode is active (always call to reinitialize)
          (org-mode)

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
          (let* ((result (condition-case export-err
                             (org-hugo-export-to-md)
                           (error
                            (format "ERROR:%s:%s" file (error-message-string export-err)))))
                 (final-result result))

            ;; If export succeeded, rename to Denote ID and add date to frontmatter
            (when (and (stringp result)
                       (not (string-prefix-p "ERROR:" result))
                       (file-exists-p result))
              (let* ((result-dir (file-name-directory result))
                     (target-file (expand-file-name (concat denote-id ".md") result-dir)))
                ;; Rename exported file to Denote ID
                (rename-file result target-file t)

                ;; Add date to frontmatter from Denote ID
                ;; Format: 20230521T215600 -> 2023-05-21
                (when (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" denote-id)
                  (let ((date-str (format "%s-%s-%s"
                                        (match-string 1 denote-id)
                                        (match-string 2 denote-id)
                                        (match-string 3 denote-id))))
                    (with-temp-file target-file
                      (insert-file-contents target-file)
                      (goto-char (point-min))
                      ;; Find end of frontmatter (second ---)
                      (when (re-search-forward "^---\n" nil t)
                        (when (re-search-forward "^---\n" nil t)
                          (forward-line -1)
                          ;; Insert date before closing ---
                          (insert (format "date: %s\n" date-str)))))))

                (setq final-result target-file)))

            (kill-buffer)
            (if (stringp final-result)
                (if (string-prefix-p "ERROR:" final-result)
                    ;; Error case
                    final-result
                  ;; Success case - return formatted success message
                  (format "SUCCESS:%s:%s" file final-result))
              ;; Export returned nil
              (format "ERROR:%s:Export returned nil" file))))
        )
    (error
     (message "[Server] ✗ Error: %s" (error-message-string err))
     nil)))

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
(for parallel processing across multiple daemons).
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

;; Set ready flag AFTER all initialization
(setq denote-export-server-ready t)

(message "[Server] ========================================")
(message "[Server] Denote Export Server FULLY READY!")
(message "[Server] Server name: %s" denote-export-server-name)
(message "[Server] Export function: denote-export-file")
(message "[Server] Ready flag: %s" denote-export-server-ready)
(message "[Server] ========================================")

;; Keep server running
;; NOTE: server-name is automatically set to daemon-name when started with --daemon=name
;; We don't need to set it explicitly here
(server-start)

(message "[Server] Server socket created")

;;; denote-export-server.el ends here
