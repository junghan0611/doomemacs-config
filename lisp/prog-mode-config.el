;;; $DOOMDIR/lisp/prog-mode-config.el --- prog-mode Config -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Code:
;;;; Emacs Lisp Mode

(add-hook! emacs-lisp-mode-hook
  (defun emacs-lisp--mode-setup-h ()
   ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
   ;; with a tab width of 8. Any smaller and the indentation will be
   ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
   ;; safe to ignore this setting otherwise.
   ;; (setq-local tab-width 8)
   (setq-local comment-column 0)
   (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
   (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))

;;;; aggressive-indent

(use-package! aggressive-indent
  :if window-system
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook 'aggressive-indent-mode)
  (add-hook 'racket-mode-hook 'aggressive-indent-mode)
  (add-hook 'hy-mode-hook 'aggressive-indent-mode)
  )

;;;; lsp-mode - lsp-ui-mode - lsp-treemacs

;; lsp 관련 설정 메뉴들. 느리게 만드는 범인중 십중팔구 LSP가 관련되어져 있다고 함.
;; 해당 튜닝도 구글링을 통해서 찾았다.
;; (setq lsp-file-watch-threshold (* 1024 1024))
;; (setq read-process-output-max (* 1024 1024))

(progn
  (after! lsp-mode
    (setq
     ;; https://emacs-lsp.github.io/lsp-mode/page/settings/headerline/
     lsp-headerline-breadcrumb-enable t ; doom nil
     lsp-headerline-breadcrumb-icons-enable nil
     ;; lsp-headerline-breadcrumb-segments '(symbols) ; namespace & symbols, no file path

     lsp-imenu-index-function #'lsp-imenu-create-categorized-index ;; 2025-03-26 doom 'lsp-imenu-create-uncategorized-index

     lsp-idle-delay 0.2  ; smooth LSP features response
     ;; lsp-eldoc-enable-hover nil ; default t - disable all hover actions
     ;; lsp-modeline-code-actions-segments '(count icon)
     ;; lsp-navigation 'both ; default 'both ; 'simple or 'peek
     ;; lsp-modeline-diagnostics-enable nil
     ;; lsp-modeline-code-actions-enable nil
     )
    )

  (after! lsp-ui
    (setq
     ;; lsp-ui-doc-use-webkit nil ; default nil
     ;; lsp-ui-doc-winum-ignore t ; default t
     lsp-ui-sideline-enable nil ; doom t - disable sideline for less distraction
     lsp-ui-sideline-diagnostic-max-line-length 20 ; default 100
     ;; lsp-ui-doc-enable nil ;; doom t - disable all doc popups
     treemacs-space-between-root-nodes nil  ;; doom nil
     ;; lsp-log-io t  ; default nil - Log client-server json communication
     lsp-ui-peek-enable t ; doom t
     ))

  (when (modulep! :ui treemacs +lsp)
    (setq lsp-treemacs-error-list-current-project-only t)
    (lsp-treemacs-sync-mode +1))
  )

;;;; Flycheck

(after! flycheck
  (setq flycheck-global-modes '(not emacs-lisp-mode org-mode markdown-mode gfm-mode))
  (setq flycheck-checker-error-threshold 1000) ; need more than default of 400
  )

(remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)

(progn
  (setq flycheck-help-echo-function nil ; default 'flycheck-help-echo-all-error-messages
        flycheck-display-errors-function nil ; default 'flycheck-display-error-messages
        )

  (after! flycheck
    (ignore-errors
      (define-key flycheck-mode-map flycheck-keymap-prefix nil))
    (setq flycheck-keymap-prefix nil)

    (add-hook! flycheck-mode
      (defun disable-flycheck-popup-buffer ()
        (setq flycheck-display-errors-function #'ignore)))
    (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
    )

  (after! elisp-mode
    (add-hook! 'doom-scratch-buffer-created-hook
      (defun flycheck-off ()
        (flycheck-mode -1))))
  )

;;;; linenote

(use-package! org-linenote
  :init
  (setq org-linenote-default-extension ".md"))

;;;; flymake

;; (remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;; eglot configuration

;; (progn
;;   (map! (:map eglot-mode-map
;;          :after eglot
;;          "C-c r" 'eglot-rename
;;          "C-c d" 'eldoc
;;          "C-c f" 'flymake-show-buffer-diagnostics
;;          "C-c 0" 'eglot-inlay-hints-mode
;;          "M-RET" 'eglot-code-actions)

;;         ;; FIXME need new keybindings
;;         ;; (:map 'flymake-mode-map
;;         ;;       "C-n" #'flymake-goto-next-error
;;         ;;       "C-p" #'flymake-goto-prev-error)
;;         )

;;   ;; (setq eglot-send-changes-idle-time 0.5)
;;   (setq flymake-no-changes-timeout nil)

;;   (add-hook! 'eglot-managed-mode-hook
;;     (eglot-inlay-hints-mode -1))
;;   )

;;;; keybindings

;; (when (locate-library "consult-flycheck")
;;   (global-set-key (kbd "M-g f") 'consult-flycheck)
;;   ;; (global-set-key (kbd "M-g F") 'consult-flymake)
;;   )

;;;; bugfix treesit

  (after! treesit
    (setq treesit-extra-load-path (list (concat doom-profile-data-dir "/tree-sitter/"))))

;;;; Provide

(provide 'prog-mode-config)

;;; prog-mode-config.el ends here
