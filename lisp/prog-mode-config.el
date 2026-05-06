;;; $DOOMDIR/lisp/prog-mode-config.el --- prog-mode Config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

;;;; flymake

(remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;; DONT Flycheck

;; (progn
;;   (after! flycheck
;;     (setq flycheck-global-modes '(not emacs-lisp-mode org-mode markdown-mode gfm-mode))
;;     (setq flycheck-checker-error-threshold 1000) ; need more than default of 400
;;     )

;;   (remove-hook 'doom-first-buffer-hook #'global-flycheck-mode)

;;   (progn
;;     (setq flycheck-help-echo-function nil ; default 'flycheck-help-echo-all-error-messages
;;           flycheck-display-errors-function nil ; default 'flycheck-display-error-messages
;;           )

;;     (after! flycheck
;;       (ignore-errors
;;         (define-key flycheck-mode-map flycheck-keymap-prefix nil))
;;       (setq flycheck-keymap-prefix nil)

;;       (add-hook! flycheck-mode
;;         (defun disable-flycheck-popup-buffer ()
;;           (setq flycheck-display-errors-function #'ignore)))
;;       (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-package)
;;       )

;;     (after! elisp-mode
;;       (add-hook! 'doom-scratch-buffer-created-hook
;;         (defun flycheck-off ()
;;           (flycheck-mode -1))))
;;     )
;;   )

;;;; linenote

;; (use-package! org-linenote
;;   :init
;;   (setq org-linenote-default-extension ".md"))

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

;;;; treesit

(with-eval-after-load 'treesit
  (setq treesit-auto-install-grammar 'always)
  ;; history가 cache 경로를 가지면 Doom advice를 우회하므로 초기화
  (setq treesit--install-language-grammar-out-dir-history nil))

;;;; Provide

(provide 'prog-mode-config)

;;; prog-mode-config.el ends here
