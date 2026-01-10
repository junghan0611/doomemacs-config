;;; $DOOMDIR/lisp/module-emacs-config.el --- module-emacs Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; dired             ; making dired pretty [functional]
;; electric          ; smarter, keyword-based electric-indent
;; eww               ; the internet is gross
;; ibuffer           ; +icons - interactive buffer management
;; undo              ; persistent, smarter undo for your inevitable mistakes
;; vc                ; version-control and Emacs, sitting in a tree

;;; Code:

;;;; dired

(after! dired
  (setq dired-make-directory-clickable t) ; Emacs 29.1, doom t
  (setq dired-free-space nil) ; Emacs 29.1, doom first

  ;; Better dired flags:
  ;; `-l' is mandatory
  ;; `-a' shows all files
  ;; `-h' uses human-readable sizes
  ;; `-F' appends file-type classifiers to file names (for better highlighting)
  ;; -g     like -l, but do not list owner
  (setq dired-listing-switches "-AGFhgv --group-directories-first --time-style=long-iso") ;; doom "-ahl -v --group-directories-first"
  (setq dired-recursive-copies 'always ; doom 'always
        dired-dwim-target t) ; doom t
  (setq dired-ls-F-marks-symlinks nil ; doom nil -F marks links with @
        delete-by-moving-to-trash t) ; doom nil

  (setq dired-use-ls-dired t)  ; doom t

  (require 'dired-aux)
  (setq dired-do-revert-buffer t) ; doom nil
  ;; (setq dired-clean-confirm-killing-deleted-buffers t) ; doom nil
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-omit-mode)

  (require 'wdired)
  (setq wdired-allow-to-change-permissions t) ; doom nil
  (setq wdired-create-parent-directories t)

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              ;; (visual-line-mode -1)
              (setq-local truncate-lines t) ; Do not wrap lines
              (display-line-numbers-mode -1)
              (hl-line-mode 1)))

  ;; prot-dired-grep-marked-files
  ;; (require 'prot-dired)
  )

;;;; dired-preview

(use-package! dired-preview
  :after dired
  :commands dired-preview
  :init
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20)) ;; => 1048576
  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (width . 0.5)))
  ;; default' dired-preview-display-action-alist-dwim
  (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
  )

;;; provide

(provide 'module-emacs-config)

;;; module-emacs-config.el ends here
