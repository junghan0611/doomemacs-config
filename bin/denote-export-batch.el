#!/usr/bin/env -S emacs --script
;;; denote-export-batch.el --- Batch mode export for Denote notes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Batch mode script for exporting Denote notes to Hugo markdown.
;; Used by parallel export script (denote-export-parallel.sh).
;;
;; Usage:
;;   emacs --batch --load denote-export-batch.el FILE1.org FILE2.org ...
;;
;; Or via shebang:
;;   ./denote-export-batch.el FILE1.org FILE2.org ...

;;; Code:

;; Minimize startup time
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer t)

;; Enable debug on error
(setq debug-on-error t)

;; Determine DOOMDIR - use script location instead of env var
;; This script is in: <doomdir>/bin/denote-export-batch.el
(defvar doom-user-dir
  (let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
    (if script-dir
        ;; Go up one level from bin/ to get doomdir
        (expand-file-name ".." script-dir)
      ;; Fallback
      (expand-file-name "~/repos/gh/doomemacs-config"))))

;; (message "DEBUG: doom-user-dir = %s" doom-user-dir)

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
    (message "%s not found, attempting to install..." package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)
    (require package)))

;; Essential requires
(require 'org)
(require 'ox)

;; Install required packages
(ensure-package 'ox-hugo)

;; Install denote-explore dependencies in order
;; (message "Installing denote-explore dependencies...")

;; Built-in packages (should always be available)
(require 'cl-lib)
(require 'json)
(require 'browse-url)

;; External packages that need installation
(ensure-package 'dash)
(ensure-package 'denote)
(ensure-package 'citar)  ;; Bibliography support

;; Chart package (may need separate installation)
(condition-case err
    (require 'chart)
  (error
   (message "chart not available, attempting to install...")
   (condition-case install-err
       (progn
         (unless package-archive-contents
           (package-refresh-contents))
         (package-install 'chart)
         (require 'chart))
     (error
      (message "WARNING: Could not install chart package: %S" install-err)
      (message "  denote-explore may have limited functionality")))))

;; denote-regexp (separate package in straight)
(unless (require 'denote-regexp nil t)
  ;; (message "denote-regexp not found, searching...")
  (let ((denote-regexp-paths (list
                              (expand-file-name ".local/straight/build-30.1.90/denote-regexp" doom-emacs-dir)
                              (expand-file-name ".local/straight/repos/denote-regexp" doom-emacs-dir))))
    (dolist (path denote-regexp-paths)
      (when (file-directory-p path)
        ;; (message "Found denote-regexp in: %s" path)
        (add-to-list 'load-path path)))
    (condition-case err
        (require 'denote-regexp)
      (error
       (message "WARNING: Could not load denote-regexp: %S" err)))))

;; Try to require denote-explore (for macros like denote-explore-count-notes)
(unless (require 'denote-explore nil t)
  ;; (message "denote-explore not found, searching in package directories...")

  ;; Search in multiple possible locations
  (let ((search-paths (list
                       ;; Check doom-emacs-dir first
                       (expand-file-name ".local/elpa/denote-explore" doom-emacs-dir)
                       (expand-file-name ".local/straight/repos/denote-explore" doom-emacs-dir)
                       (expand-file-name ".local/straight/build-30.1.90/denote-explore" doom-emacs-dir)
                       ;; Fallback to user-emacs-directory
                       (expand-file-name "denote-explore" (concat user-emacs-directory ".local/elpa/"))
                       (expand-file-name "denote-explore" (concat user-emacs-directory "elpa/"))))
        (found nil))

    ;; Try to find existing installation
    (dolist (path search-paths)
      (when (and (not found) (file-directory-p path))
        ;; (message "Found denote-explore in: %s" path)
        (add-to-list 'load-path path)
        (setq found t)))

    ;; Try to load (denote must be already loaded)
    (if found
        (condition-case err
            (progn
              (require 'denote-explore)
              ;; (message "Successfully loaded denote-explore")
              )
          (error
           (message "WARNING: Failed to load denote-explore: %S" err)
           (defun denote-explore-count-notes () "0")
           (defun denote-explore-count-keywords () "0")))
      ;; Not found in any path - use stub
      (message "WARNING: denote-explore not found")
      (defun denote-explore-count-notes () "0")
      (defun denote-explore-count-keywords () "0"))))

;; Set denote-directory immediately after loading denote (critical for link resolution!)
(setq denote-directory (expand-file-name "~/org/"))
;; (message "Set denote-directory: %s" denote-directory)

;; Save original doom-user-dir before loading +user-info
(defvar original-doom-user-dir doom-user-dir)
;; (message "DEBUG: original-doom-user-dir = %s" original-doom-user-dir)

;; Load user info (required for user-hugo-blog-dir)
(let ((user-info (expand-file-name "+user-info.el" original-doom-user-dir)))
  (if (file-exists-p user-info)
      (load user-info)
    (message "Warning: +user-info.el not found")))

;; Set defaults if not defined
(unless (boundp 'user-hugo-blog-dir)
  (defvar user-hugo-blog-dir (expand-file-name "~/repos/gh/notes/")))

(unless (boundp 'user-org-directory)
  (defvar user-org-directory (expand-file-name "~/org/")))

;; Ensure org-hugo-base-dir and section are set
(unless (boundp 'org-hugo-base-dir)
  (setq org-hugo-base-dir user-hugo-blog-dir))

(unless (boundp 'org-hugo-section)
  (setq org-hugo-section "test"))

;; Force expand tilde in org-hugo-base-dir (critical!)
(when (and org-hugo-base-dir (string-prefix-p "~" org-hugo-base-dir))
  (setq-local org-hugo-base-dir (expand-file-name org-hugo-base-dir)))

;; Load export configuration (use original-doom-user-dir)
(let ((export-config (expand-file-name "+denote-export.el" original-doom-user-dir)))
  (if (file-exists-p export-config)
      (load export-config)
    (error "Export config not found: %s" export-config)))

;; ========== Bibliography/Citation 초기화 ==========
;; Require org-cite and citeproc for citation processing
(require 'oc)  ;; org-cite
(require 'oc-basic)  ;; basic citation processor
(require 'oc-csl)  ;; CSL citation processor (if available)

;; Configure org-cite export processors
(setq org-cite-export-processors
      '((html . (csl "apa.csl"))
        (latex . biblatex)
        (t . (basic))))

;; Initialize citar if available
(when (and (featurep 'citar) (boundp 'config-bibfiles))
  ;; Force bibliography hash table initialization
  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)

  ;; Initialize citar cache (this creates the hash tables)
  (when (fboundp 'citar-refresh)
    (condition-case err
        (citar-refresh)
      (error
       (message "WARNING: citar-refresh failed: %S" err)))))

;; ========== 매크로 확장 설정 ==========
(require 'org-macro)

;; Hook: export 전 매크로 확장
(defun batch-expand-macros-before-export (backend)
  "Expand all org macros before export in batch mode."
  (condition-case err
      (progn
        ;; (message "  [Hook] Expanding macros...")
        (org-macro-initialize-templates)
        (org-macro-replace-all org-macro-templates)
        ;; (message "  [Hook] Macros expanded")
        )
    (error
     (message "WARNING: Macro expansion failed: %S" err))))

(add-hook 'org-export-before-processing-hook
          #'batch-expand-macros-before-export)

;; denote-explore 매크로가 로드되었다면 명시적 등록
(when (featurep 'denote-explore)
  ;; 매크로 템플릿에 denote-explore 함수들 추가
  (add-to-list 'org-macro-templates
               '("denote-explore-count-notes" . "(eval (denote-explore-count-notes))") t)
  (add-to-list 'org-macro-templates
               '("denote-explore-count-keywords" . "(eval (denote-explore-count-keywords))") t))

;; (message "Macro expansion configured for batch export")
;; ========== 매크로 확장 설정 끝 ==========

;; Export function
(defun batch-export-file (file)
  "Export single org FILE to Hugo markdown in batch mode."
  (condition-case err
      (let* ((dir (file-name-directory file))
             (dir-locals-file (expand-file-name ".dir-locals.el" dir))
             (dir-locals-settings nil))

        ;; (message "Exporting: %s" file)

        ;; Parse .dir-locals.el if exists
        (when (file-exists-p dir-locals-file)
          ;; (message "  Loading dir-locals: %s" dir-locals-file)
          (with-temp-buffer
            (insert-file-contents dir-locals-file)
            (let ((locals (read (current-buffer))))
              (setq dir-locals-settings (cdr (assoc 'org-mode locals))))))

        ;; Open file and apply settings in buffer context
        (with-current-buffer (find-file-noselect file)
          ;; Apply dir-locals settings to THIS buffer
          (when dir-locals-settings
            (dolist (setting dir-locals-settings)
              (when (and (consp setting)
                         (not (eq (car setting) 'eval)))
                ;; Expand tilde in paths
                (let* ((var-name (car setting))
                       (original-value (cdr setting))
                       (expanded-value (if (and (stringp original-value)
                                                (string-prefix-p "~" original-value))
                                           (expand-file-name original-value)
                                         original-value)))
                  ;; (message "    Setting %s: %s -> %s" var-name original-value expanded-value)
                  (set (make-local-variable var-name) expanded-value)
                  ;; (message "    Verified %s = %s" var-name (symbol-value var-name))
                  ))))

          ;; Debug: show final values (only if needed)
          ;; (message "  Final Section: %s" (or org-hugo-section "NOT SET"))
          ;; (message "  Final Base: %s" (or org-hugo-base-dir "NOT SET"))
          ;; (message "  Final Base (expanded): %s"
          ;;          (when org-hugo-base-dir (expand-file-name org-hugo-base-dir)))


          ;; Verify all required variables are set
          (unless org-hugo-base-dir
            (error "org-hugo-base-dir is nil!"))
          (unless org-hugo-section
            (error "org-hugo-section is nil!"))

          ;; CRITICAL: Force expand tilde before export
          (when (and org-hugo-base-dir (string-prefix-p "~" org-hugo-base-dir))
            (setq-local org-hugo-base-dir (expand-file-name org-hugo-base-dir)))

          ;; Export with detailed error tracking
          (let ((result (condition-case export-err
                            (org-hugo-export-to-md)
                          (error
                           (message "✗ Error during export: %s" (error-message-string export-err))
                           (message "   Backtrace: %S" export-err)
                           (message "   buffer-file-name: %s" (buffer-file-name))
                           (message "   org-hugo-base-dir: %s" org-hugo-base-dir)
                           (message "   org-hugo-section: %s" org-hugo-section)
                           nil))))
            (if result
                (message "✓ Exported: %s → %s" file result)
              (message "✗ Export failed: %s" file)))
          (kill-buffer)))
    (error
     (message "✗ Error exporting %s: %s" file (error-message-string err))
     nil)))

;; Main execution
(let ((files command-line-args-left))
  (if files
      (progn
        ;; Silently process files (progress tracked by parallel script)
        (mapc #'batch-export-file files))
    (message "No files specified for export.")))

;; Cleanup
(setq command-line-args-left nil)

;;; denote-export-batch.el ends here
