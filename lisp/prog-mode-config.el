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

;;;; flymake

(remove-hook! (prog-mode text-mode) #'flymake-mode)

;;;; eglot configuration

(progn
  (map! (:map eglot-mode-map
         :after eglot
         "C-c r" 'eglot-rename
         "C-c d" 'eldoc
         "C-c f" 'flymake-show-buffer-diagnostics
         "C-c 0" 'eglot-inlay-hints-mode
         "M-RET" 'eglot-code-actions)

        ;; FIXME need new keybindings
        ;; (:map 'flymake-mode-map
        ;;       "C-n" #'flymake-goto-next-error
        ;;       "C-p" #'flymake-goto-prev-error)
        )

  ;; (setq eglot-send-changes-idle-time 0.5)
  (setq flymake-no-changes-timeout nil)

  (add-hook! 'eglot-managed-mode-hook
    (eglot-inlay-hints-mode -1))
  )

;;;; bugfix treesit

(after! treesit
  (setq treesit-extra-load-path (list (concat doom-profile-data-dir "/tree-sitter/"))))

;;;; Provide

(provide 'prog-mode-config)

;;; prog-mode-config.el ends here
