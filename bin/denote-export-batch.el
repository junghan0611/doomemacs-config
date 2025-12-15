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

;; CRITICAL: Disable package.el completely to prevent ELPA usage
(setq package-enable-at-startup nil)
(setq package-archives nil)
(fset 'package-initialize #'ignore)
(fset 'package-install #'ignore)
(fset 'package-refresh-contents #'ignore)
(fset 'package-install-file #'ignore)
(fset 'package-install-from-archive #'ignore)

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

;; Verify package is available in load-path
(defun verify-package-available (package)
  "Verify PACKAGE is available in load-path before loading."
  (let ((found nil))
    (dolist (dir load-path)
      (when (file-exists-p (expand-file-name (format "%s.el" package) dir))
        (setq found t)))
    (unless found
      (error "Package %s not found in Doom's straight repos. Check installation." package))
    found))

;; Essential requires (built-in)
(require 'org)
(require 'ox)
(require 'cl-lib)
(require 'json)
(require 'browse-url)

;; Load required packages from Doom's straight repos
;; If any package is missing, we want a clear error (NOT silent ELPA install)
(message "Loading packages from Doom's straight repos...")
(or (require 'dash nil t) (error "dash not found in Doom"))
(or (require 'denote nil t) (error "denote not found in Doom"))
(or (require 'ox-hugo nil t) (error "ox-hugo not found in Doom"))
(or (require 'citar nil t) (error "citar not found in Doom"))
(or (require 'parsebib nil t) (error "parsebib not found in Doom"))

;; Chart package (optional)
(require 'chart nil t)

;; denote-regexp (required by denote-explore)
(require 'denote-regexp nil t)

;; denote-explore (for macros like denote-explore-count-notes)
(condition-case err
    (progn
      (require 'denote-explore)
      (message "✓ denote-explore loaded successfully"))
  (error
   (message "WARNING: denote-explore failed to load: %S" err)
   (defun denote-explore-count-notes () "0")
   (defun denote-explore-count-keywords () "0")))

(message "✓ All required packages loaded from Doom")

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
(let ((export-config (expand-file-name "lisp/denote-export.el" original-doom-user-dir)))
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
