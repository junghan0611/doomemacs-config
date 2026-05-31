;;; $DOOMDIR/lisp/module-emacs-config.el --- module-emacs Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

;;;; dired + dirvish — Doom 기본 (DOOM-NATIVE)

;; Doom :emacs dired 모듈은 +dirvish 플래그 없이도 dirvish 를 자동으로
;; minor augmentation 으로 끼워 넣는다 (preview, icons, mode-line 정보).
;; 풀 dirvish 인터페이스 (Ranger 풍 멀티 패널) 가 필요하면 init.el 에서
;; `(dired +dirvish)' 로 플래그 활성. 지금은 기본만 사용.
;;
;; 진입:
;;   `SPC f j' / `SPC f d' / `M-x dired RET' → 자동으로 dirvish UI 적용
;;   `<f8>'                                  → dirvish-side 사이드바
;;
;; 핵심 키 (dirvish-mode-map 안):
;;   ?            dirvish-dispatch — 모든 기능 진입점 (transient menu)
;;   F            dirvish-layout-toggle — 1/2/3 panel
;;   TAB / gl     dirvish-subtree-toggle
;;   h / l        dired-up-directory / dired-find-file
;;   y l/n/p/r/y  yank submap (path/name 복사)
;;   M-n          dirvish-narrow (filter)
;;   M-m          dirvish-mark-menu
;;
;; 보호된 키 (lisp/keybindings-config.el 에서 unbind):
;;   M-e — denote core keymap
;;   M-s — 글로벌 search prefix
;;   각각 dispatch (?) 메뉴 또는 M-x 로 접근.
;;
;; 비활성:
;;   neotree, treemacs — dired/dirvish 로 대체. SPC f * 시리즈가 더 빠름.
;;   diredfl  — denote 와 충돌 (packages.el)

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
  ;; (remove-hook 'dired-mode-hook 'dired-omit-mode)

  ;; Plain Dired buffers should start in the compact view too.
  ;; Dirvish has its own `dirvish-hide-details' switch below for Dirvish
  ;; sessions; keep this hook as the built-in Dired fallback.
  (defun my/dired-enable-hide-details-h ()
    "Enable compact Dired listings by default."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook #'my/dired-enable-hide-details-h)

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

(after! dirvish
  ;; Doom narrows this to '(dirvish dirvish-side).  Include plain `dired'
  ;; as well, otherwise Dirvish-controlled Dired entry points can reopen with
  ;; details shown even when `dired-hide-details-mode' is on our hook.
  (setq dirvish-hide-details '(dired dirvish dirvish-fd dirvish-side)))

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
