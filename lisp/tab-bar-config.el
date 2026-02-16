;;; $DOOMDIR/lisp/tab-bar-config.el --- Time and Calendar Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:


;;; Code:

;;;;; celestial-mode-line

(use-package! celestial-mode-line
  :after time
  :init
  (setq celestial-mode-line-update-interval 3600) ; default 60
  (setq celestial-mode-line-sunrise-sunset-alist
        '((sunrise . "🌅") (sunset . "🌃")))
  (setq celestial-mode-line-phase-representation-alist
        '((0 . "🌚") (1 . "🌛") (2 . "🌝") (3 . "🌜")))
  :config (celestial-mode-line-start-timer)
  )

;;;; custom tab-bar global-mode-string

(progn
  (require 'tab-bar)

  ;; 2025-01-26
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)

  (setq tab-bar-format
        '( ;; tab-bar-format-history
          tab-bar-format-tabs
          tab-bar-separator
          tab-bar-format-add-tab
          tab-bar-format-align-right
          tab-bar-format-global
          ))

;;;;###autoload
  (defun my/load-global-mode-string ()
    (interactive)

    ;; (message "my/load-global-mode-string")
    (when (not (bound-and-true-p display-time-mode))
      (display-time-mode t))

    ;; (when (fboundp 'display-time-mode)
    ;;   (display-time-mode t))

    (setq global-mode-string (remove 'display-time-string global-mode-string))
    (setq global-mode-string '("" celestial-mode-line-string display-time-string))

    (tab-bar-mode +1))

  (add-hook 'doom-after-init-hook #'my/load-global-mode-string 80)
  (add-hook 'doom-after-reload-hook #'my/load-global-mode-string)
  )

;;;;; DONT keycast tab-bar

;; (use-package! keycast
;;   :config
;;   ;; (setq keycast-tab-bar-minimal-width 50) ; 40
;;   (setq keycast-tab-bar-format "%10s%k%c%r")

;;   (dolist (input '(self-insert-command org-self-insert-command))
;;     (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
;;   (dolist (event
;;            '(mouse-event-p mouse-movement-p
;;              mwheel-scroll
;;              handle-select-window
;;              mouse-set-point
;;              mouse-drag-region
;;              dired-next-line ; j
;;              dired-previous-line ; k
;;              next-line
;;              previous-line
;;              evil-next-line ; j
;;              evil-previous-line ; k
;;              evil-forward-char ; l
;;              evil-backward-char ; h
;;              pixel-scroll-interpolate-up ; <prior> page-up
;;              pixel-scroll-interpolate-down ; <next> page-down

;;              pixel-scroll-precision
;;              evil-jump-item
;;              evil-mouse-drag-region ;; double click

;;              org-cycle
;;              keyboard-quit
;;              save-buffer
;;              ;; block-toggle-input-method
;;              ;; toggle-input-method

;;              ;; evil-formal-state
;;              ;; evil-force-normal-state

;;              ;; 2023-10-02 Added for clojure-dev
;;              ;; lsp-ui-doc--handle-mouse-movement
;;              ignore-preserving-kill-region
;;              ;; pdf-view-text-region
;;              ;; pdf-view-mouse-set-region
;;              ;; mouse-set-region
;;              ))
;;     (add-to-list 'keycast-substitute-alist `(,event nil)))
;;   )

;;;; 탭 네비게이션 키바인딩

(with-eval-after-load 'tab-bar
  ;; gb / gB
  (define-key evil-motion-state-map "gb" 'tab-next)
  (define-key evil-motion-state-map "gB" 'tab-previous)
  (define-key evil-normal-state-map "gb" 'tab-next)
  (define-key evil-normal-state-map "gB" 'tab-previous)

  (define-key evil-motion-state-map "gh" 'menu-bar-open)
  (define-key evil-normal-state-map "gh" 'menu-bar-open)

  ;; Ctrl + Number
  (global-set-key (kbd "s-\\") 'tab-bar-switch-to-tab)
  (global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
  (global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab))

;;; provide

(provide 'tab-bar-config)

;;; tab-bar-config.el ends here
