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


;;;; provide

(provide 'editing-config)

;;; editing-config end here
