;;; $DOOMDIR/lisp/editing-config.el --- EDITING configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; [2025-01] Terminal Focus Issue - 아래 패키지들이 터미널 모드에서 문제 발생
;; SSH + Ghostty/터미널에서 창 포커스 전환 시 "I" "O" 문자가 입력되는 현상
;; 원인: Focus reporting (DECSET 1004) 관련 ESC[I / ESC[O 시퀀스 충돌
;; 제거된 패키지: olivetti, redacted, logos, focus, centered-cursor-mode
;; 터미널 Emacs 호환성을 위해 제거 결정 (정확한 원인 패키지는 미확인)

;;; Code:

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

;;; pinentry

;; GPG Pinentry 모드 설정
;;
;; IMPORTANT: epa-pinentry-mode는 반드시 nil로 설정해야 함!
;;
;; 이유:
;; - 'loopback: GPG agent를 우회하여 Emacs가 직접 passphrase 처리
;;   → GPG agent 캐시 무시, Emacs 시작할 때마다 passphrase 입력 필요
;;   → NixOS gpg-agent.conf의 캐시 설정(1년)이 무효화됨
;;
;; - nil: GPG agent와 협력하여 passphrase 처리
;;   → GPG agent 캐시 활용, 캐시 기간 동안 재입력 불필요
;;   → Emacs minibuffer에서 입력, 시스템 전체 캐시 공유
;;
;; 참고:
;; - pinentry-start: Emacs를 GPG agent의 pinentry로 등록
;; - 시스템 gpg-agent.conf 설정을 존중하는 것이 올바른 방식

(use-package! pinentry
  :config
  (setq epa-pinentry-mode nil)  ; GPG agent 캐시 활용 (loopback 사용 금지!)
  (pinentry-start))

;;;; imenu-list

;;;###autoload
(defun spacemacs/imenu-list-smart-focus ()
  "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (imenu-list-show)
    (imenu-list-smart-toggle)))

;; Show an outline summary of the current buffer.
(use-package! imenu-list
  :init
  (add-hook 'imenu-list-major-mode-hook #'toggle-truncate-lines)
  ;; (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-size 40)
  (setq imenu-list-auto-resize nil))

;;;; imenu-list-mode-map

(after! imenu-list
  (map! :map imenu-list-major-mode-map
        :n "f"      #'hs-toggle-hiding
        :n "g"      #'imenu-list-refresh
        :n "r"      #'imenu-list-refresh
        :n "d"      #'imenu-list-display-dwim
        :n "RET"    #'imenu-list-ret-dwim
        :n "u"      #'imenu-list-up-level
        :n "z u"    #'imenu-list-up-level ; outline-up-heading
        :n "^"      #'imenu-list-up-level  ; dired style
        :n "C-S-p"  #'imenu-list-up-level  ; sync org-mode markdown-mode
        :n "M-j"    #'imenu-list-next-entry-same-level
        :n "M-k"    #'imenu-list-previous-entry-same-level
        :n "M-n"    #'evil-next-line
        :n "M-p"    #'evil-previous-line))

;;;; provide

(provide 'editing-config)

;;; editing-config end here
