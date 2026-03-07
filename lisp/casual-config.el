;;; $DOOMDIR/lisp/casual-config.el --- Casual Transient UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Casual: 각 모드별 Transient UI를 <f12>로 통일 접근.
;; 지원 모드에서 <f12> 누르면 해당 모드의 casual 메뉴가 열림.
;; https://github.com/kickingvegas/casual

;;; Code:

;;;; <f12> — 모드별 casual-*-tmenu 바인딩

(use-package! casual
  :defer t
  :config
  ;; 각 모드 맵에 <f12> → casual-*-tmenu 바인딩
  (map!
   (:after dired
    :map dired-mode-map
    "<f12>" #'casual-dired-tmenu)

   (:after calc
    :map calc-mode-map
    "<f12>" #'casual-calc-tmenu)

   (:after ibuffer
    :map ibuffer-mode-map
    "<f12>" #'casual-ibuffer-tmenu)

   (:after info
    :map Info-mode-map
    "<f12>" #'casual-info-tmenu)

   (:after bookmark
    :map bookmark-bmenu-mode-map
    "<f12>" #'casual-bookmarks-tmenu)

   (:after re-builder
    :map reb-mode-map
    "<f12>" #'casual-re-builder-tmenu)

   (:after org-agenda
    :map org-agenda-mode-map
    "<f12>" #'casual-agenda-tmenu)

   (:after org
    :map org-mode-map
    "<f12>" #'casual-org-tmenu)

   (:after eww
    :map eww-mode-map
    "<f12>" #'casual-eww-tmenu)

   (:after compile
    :map compilation-mode-map
    "<f12>" #'casual-compile-tmenu)

   (:after man
    :map Man-mode-map
    "<f12>" #'casual-man-tmenu)

   (:after esh-mode
    :map eshell-mode-map
    "<f12>" #'casual-eshell-tmenu)

   (:after ediff
    :map ediff-mode-map
    "<f12>" #'casual-ediff-tmenu)

   (:after image-mode
    :map image-mode-map
    "<f12>" #'casual-image-tmenu)

   (:after calendar
    :map calendar-mode-map
    "<f12>" #'casual-calendar-tmenu)

   (:after help-mode
    :map help-mode-map
    "<f12>" #'casual-help-tmenu)

   (:after css-mode
    :map css-mode-map
    "<f12>" #'casual-css-tmenu)

   (:after csv-mode
    :map csv-mode-map
    "<f12>" #'casual-csv-tmenu)

   (:after sgml-mode
    :map html-mode-map
    "<f12>" #'casual-html-tmenu)

   (:after make-mode
    :map makefile-mode-map
    "<f12>" #'casual-make-tmenu)

   (:after elisp-mode
    :map emacs-lisp-mode-map
    "<f12>" #'casual-elisp-tmenu)

   (:after bibtex
    :map bibtex-mode-map
    "<f12>" #'casual-bibtex-tmenu)

   (:after isearch
    :map isearch-mode-map
    "<f12>" #'casual-isearch-tmenu))

  ;; editkit: 범용 편집 메뉴 (모드 무관, C-<f12>)
  (map! "C-<f12>" #'casual-editkit-main-tmenu))

(provide 'casual-config)
;;; casual-config.el ends here
