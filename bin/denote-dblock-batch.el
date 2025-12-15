#!/usr/bin/env -S emacs --script
;;; denote-dblock-batch.el --- Batch mode dblock update for Denote notes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Batch mode script for updating dynamic blocks in Denote notes.
;; Primarily used for meta notes with denote-backlinks.
;;
;; Usage:
;;   emacs --batch --load denote-dblock-batch.el DIRECTORY
;;   ./denote-dblock-batch.el ~/org/meta

;;; Code:

;; Minimize startup time
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t)

;; Enable debug on error
(setq debug-on-error t)

;; Determine DOOMDIR - use script location
(defvar doom-user-dir
  (let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
    (if script-dir
        (expand-file-name ".." script-dir)
      (expand-file-name "~/repos/gh/doomemacs-config"))))

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

;; Helper function to find straight build directory (version-agnostic)
(defun find-straight-build-dir ()
  "Find the straight build directory, handling different Emacs versions."
  (let ((straight-build-base (expand-file-name ".local/straight/" doom-emacs-dir)))
    (when (file-directory-p straight-build-base)
      (let ((build-dirs (directory-files straight-build-base t "^build-")))
        (when build-dirs
          ;; Return the most recent build directory
          (car (sort build-dirs #'file-newer-than-file-p)))))))

;; Add Doom's straight paths to load-path AND load autoloads
(let ((build-dir (find-straight-build-dir))
      (repos-dir (expand-file-name ".local/straight/repos/" doom-emacs-dir)))
  (when (and build-dir (file-directory-p build-dir))
    ;; Add all package directories from build dir
    (let ((pkg-dirs (directory-files build-dir t "^[^.]" t)))
      (dolist (pkg-dir pkg-dirs)
        (when (and (file-directory-p pkg-dir)
                   (not (string-suffix-p ".el" pkg-dir)))
          (add-to-list 'load-path pkg-dir)
          ;; Load autoloads file if exists
          (let ((autoload-file (expand-file-name 
                                (concat (file-name-nondirectory pkg-dir) "-autoloads.el")
                                pkg-dir)))
            (when (file-exists-p autoload-file)
              (load autoload-file nil t)))))))
  (when (file-directory-p repos-dir)
    ;; Add all repos as fallback
    (let ((repo-dirs (directory-files repos-dir t "^[^.]" t)))
      (dolist (repo-dir repo-dirs)
        (when (file-directory-p repo-dir)
          (add-to-list 'load-path repo-dir))))))

;; Essential requires (built-in)
(require 'org)

;; Load required packages from Doom's straight repos
(message "[DBLOCK] Loading packages from Doom...")
(or (require 'dash nil t) (error "dash not found in Doom"))
(or (require 'denote nil t) (error "denote not found in Doom"))

;; CRITICAL: Load denote-org-extras for dblock functions
;; This provides org-dblock-write:denote-links and org-dblock-write:denote-backlinks
(condition-case err
    (progn
      (require 'denote-org-extras)
      (message "[OK] denote-org-extras loaded (dblock functions available)"))
  (error
   (message "[WARN] denote-org-extras not available: %s" err)))

;; Load denote-explore for extended backlinks support
(require 'denote-regexp nil t)

(condition-case err
    (progn
      (require 'denote-explore)
      (message "[OK] denote-explore loaded"))
  (error
   (message "[WARN] denote-explore not available: %s" err)))

(message "[OK] All required packages loaded from Doom")

;; Set denote directory from command line or default
(defvar org-directory (expand-file-name "~/org"))
(setq denote-directory (expand-file-name "~/org"))

;; Verify dblock functions are available
(unless (fboundp 'org-dblock-write:denote-links)
  (message "[ERROR] org-dblock-write:denote-links not defined!")
  (message "[ERROR] denote-org or denote-org-extras not loaded properly"))

;; Parse .dir-locals.el if exists (for denote-directory override)
(let ((dir-locals-file (expand-file-name ".dir-locals.el" org-directory)))
  (when (file-exists-p dir-locals-file)
    (with-temp-buffer
      (insert-file-contents dir-locals-file)
      (goto-char (point-min))
      (when (re-search-forward "(denote-directory . \"\\([^\"]+\\)\")" nil t)
        (let ((dir (match-string 1)))
          ;; Expand tilde in batch mode
          (when (string-prefix-p "~" dir)
            (setq dir (expand-file-name dir)))
          (setq denote-directory dir)
          (message "[CONFIG] denote-directory = %s" denote-directory))))))

(message "[START] Denote dblock batch update")
(message "[DOOMDIR] %s" doom-user-dir)
(message "[DENOTE] %s" denote-directory)

;;;; Main dblock update function

(defun denote-dblock-update-file (file)
  "Update all dblocks in FILE and save."
  (message "[PROCESSING] %s" file)
  (let ((start-time (float-time)))
    (condition-case err
        (with-current-buffer (find-file-noselect file)
          ;; Check if file has dblocks
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^#\\+BEGIN:" nil t)  ; Match any BEGIN block
                (progn
                  (message "[DBLOCK] Found in %s" (file-name-nondirectory file))
                  (goto-char (point-min))
                  (org-update-all-dblocks)
                  (save-buffer)
                  (message "[OK] Updated in %.2fs" (- (float-time) start-time)))
              (message "[SKIP] No dblock in %s" (file-name-nondirectory file))))
          (kill-buffer))
      (error
       (message "[ERROR] Failed to update %s: %s" file err)))))

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
    
    (message "[SCAN] Found %d org files in %s (excluded %d lock files)"
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
        (file-missing (message "[SKIP] File not found: %s" file))))
    
    (let ((duration (- (float-time) start-time)))
      (message "========================================")
      (message "[SUMMARY] Completed: %d/%d files" counter total)
      (message "[SUMMARY] Files with dblocks: %d" files-with-dblocks)
      (message "[SUMMARY] Total time: %.2fs (%.3f files/sec)" 
               duration 
               (/ (float counter) duration))
      (message "========================================"))))

;;;; Entry point

(let ((target-dir (or (car command-line-args-left)
                      (expand-file-name "~/org/meta"))))
  (unless (file-directory-p target-dir)
    (error "Directory not found: %s" target-dir))
  
  (message "[TARGET] %s" target-dir)
  (denote-dblock-update-directory target-dir)
  (message "[DONE] Batch update completed"))

;; Exit cleanly
(kill-emacs 0)

;;; denote-dblock-batch.el ends here
