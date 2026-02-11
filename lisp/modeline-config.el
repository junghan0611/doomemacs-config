;;; $DOOMDIR/lisp/modeline-config.el --- Modeline Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Code:

;;;; doom-modeline

(require 'doom-modeline)

(progn
  (doom-modeline-def-modeline
    'main
    '(eldoc
      bar
      persp-name
      ;; workspace-name - conflict tab-bar
      window-number
      modals
      input-method
      matches
      follow
      buffer-info
      remote-host
      buffer-position
      word-count
      parrot
      selection-info)
    '(compilation
      objed-state
      misc-info
      battery
      grip
      irc
      mu4e
      gnus
      github
      debug
      repl
      lsp
      minor-modes
      indent-info
      buffer-encoding
      major-mode
      process
      vcs
      check
      time))

  (setq doom-modeline-time nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-minor-modes nil)
  ;; (setq doom-modeline-battery nil)
  ;; (setq Info-breadcrumbs-in-mode-line-mode nil)
  (setq doom-modeline-support-imenu nil)

  ;; UTF-8 is default encoding remove it from modeline frap-dot-doom/ui-old.el
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                            (eq buffer-file-coding-system 'utf-8)))))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mod)) ; org-mode

  (setq doom-modeline-modal-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon (display-graphic-p))
  (setq doom-modeline-buffer-modification-icon (display-graphic-p))

  (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 4)

  (setq doom-modeline-persp-name t) ; doom nil
  (setq doom-modeline-window-width-limit (- fill-column 5))

  (setq doom-modeline-repl t)
  (setq doom-modeline-github t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project) ; default 'auto

  (remove-hook 'display-time-mode-hook #'doom-modeline-override-time)
  (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)
  )

;;; provide

(provide 'modeline-config)

;;; modeline-config.el ends here
