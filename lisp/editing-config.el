;;; $DOOMDIR/lisp/editing-config.el --- EDITING configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;;; Code:

;;;; olivetti

(use-package! olivetti
  :after org
  :custom
  ;; (olivetti-body-width 0.7) ; nil
  (olivetti-minimum-body-width 90) ; for compatibility fill-column 80
  (olivetti-recall-visual-line-mode-entry-state t))

;;;; Ten with etags

;; gavinok-dotfiles/init.el
;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (use-package etags-regen
;;   :when (executable-find "etags")
;;   :custom (etags-regen-tags-file "/tmp/TAGS")
;;   :commands etags-regen-mode
;;   :bind (("C-c t" . complete-tag)
;;          ("C-c M-." . my/goto-etags))
;;   :init
;;   (defvar etags-regen-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; ;; eww-mode nov-mode -- conflict face 켜면 안된다.

(when (locate-library "ten")
  (require 'ten)
  (setq ten-glossary-file-extensions '("org" "md" "txt"))
  (setq ten-glossary-exclude-regexps '("/\\."))
  (setq ten-tags-file-default user-ten-tags-file)
  ;;   ;; :bind (("M-c t" . complete-tag)
  ;;   ;;        ("C-c M-." . my/goto-etags))
  (add-hook 'org-mode-hook 'ten-font-lock-mode) ;; never! for all text-mode
  (add-hook 'Info-mode-hook 'ten-font-lock-mode)
  (with-eval-after-load 'consult
    (require 'consult-ten)
    (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
    )
  )

;;;; outli

(use-package! outli
  :defer 1
  :init
  (setq outli-speed-commands nil)
  (add-hook 'prog-mode-hook #'outli-mode)
  (add-hook 'conf-mode-hook #'outli-mode)
  :config
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(zig-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
  (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))
  )


;;;; centered-cursor-mode

(use-package! centered-cursor-mode
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :init
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               mouse-set-region
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region))
  :config
  (map! :map ccm-map
        :inv "M-\\" #'other-window
        :inv "M-u" #'evil-scroll-up
        :inv "M-v" #'evil-scroll-down)
  )

;;;; redacted

(use-package! redacted
  :defer t
  :commands (redacted-mode))

;;;; logos

(use-package! logos
  :defer 2
  :commands (logos-focus-mode)
  :init
  ;; If you want to use outlines instead of page breaks (the ^L):
  (setq logos-outlines-are-pages t)
  ;; This is the default value for the outlines:
  ;; (setq logos-outline-regexp-alist
  ;;       `((emacs-lisp-mode . "^;;;;+ ")
  ;;         (org-mode . "^\\*+ +")
  ;;         (markdown-mode . "^\\#+ +")
  ;;         (t . ,(if (boundp 'outline-regexp) outline-regexp logos--page-delimiter))))

  (setq logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;;+ ")
                                     (org-mode . "^\\*+ +")
                                     (markdown-mode . "^\\#+ +")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-cursor nil)
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-header-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch nil) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)
  (setq logos-outlines-are-pages t)
  :config
  ;; I don't need to do `with-eval-after-load' for the `modus-themes' as
  ;; I always load them before other relevant potentially packages.
  ;; (add-hook 'modus-themes-after-load-theme-hook #'logos-update-fringe-in-buffers)
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    )
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top
  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top)

  ;; Also consider adding keys to `logos-focus-mode-map'.  They will take
  ;; effect when `logos-focus-mode' is enabled.
  )

;;;; focus on paragraph

(use-package! focus
  :after org
  :config
  (add-to-list 'focus-mode-to-thing '(org-mode . paragraph)))


;;;; provide

(provide 'editing-config)

;;; editing-config end here
