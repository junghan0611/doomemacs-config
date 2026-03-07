;;; $DOOMDIR/lisp/casual-config.el --- Casual Transient UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Casual: 각 모드별 Transient UI를 <f12>로 통일 접근.
;; 지원 모드에서 <f12> 누르면 해당 모드의 casual 메뉴가 열림.
;; M-<f12>는 casual-editkit (범용 편집, 모드 무관).
;; https://github.com/kickingvegas/casual
;;
;; 각 바인딩은 해당 모드가 로드된 후 설정됨 (after!).
;; casual 서브 패키지는 autoload되어 있어 별도 require 불필요.
;;
;; 바닐라 Emacs 참고 (Emacs 29.1+ keymap-set 사용):
;;   (keymap-set dired-mode-map "<f12>" #'casual-dired-tmenu)
;;   (keymap-set calc-mode-map "<f12>" #'casual-calc-tmenu)
;; Doom에서는 map! + after! 패턴을 사용.

;;; Code:

;;;; <f12> — 모드별 casual-*-tmenu 바인딩

(after! dired
  (map! :map dired-mode-map "<f12>" #'casual-dired-tmenu))

(after! calc
  (map! :map calc-mode-map "<f12>" #'casual-calc-tmenu)
  (map! :map calc-alg-map "<f12>" #'casual-calc-tmenu))

(after! ibuffer
  (map! :map ibuffer-mode-map "<f12>" #'casual-ibuffer-tmenu))

(after! info
  (map! :map Info-mode-map "<f12>" #'casual-info-tmenu))

(after! bookmark
  (map! :map bookmark-bmenu-mode-map "<f12>" #'casual-bookmarks-tmenu))

(after! re-builder
  (map! :map reb-mode-map "<f12>" #'casual-re-builder-tmenu))

(after! org-agenda
  (map! :map org-agenda-mode-map "<f12>" #'casual-agenda-tmenu))

(after! org
  (map! :map org-mode-map "<f12>" #'casual-org-tmenu))

(after! eww
  (map! :map eww-mode-map "<f12>" #'casual-eww-tmenu))

(after! compile
  (map! :map compilation-mode-map "<f12>" #'casual-compile-tmenu))

(after! man
  (map! :map Man-mode-map "<f12>" #'casual-man-tmenu))

(after! esh-mode
  (map! :map eshell-mode-map "<f12>" #'casual-eshell-tmenu))

;; ediff/image: keymap이 모드 활성화 시에만 생성 → hook으로 바인딩
(add-hook! 'ediff-keymap-setup-hook
  (define-key ediff-mode-map (kbd "<f12>") #'casual-ediff-tmenu))

(add-hook! 'image-mode-hook
  (define-key image-mode-map (kbd "<f12>") #'casual-image-tmenu))

(after! calendar
  (map! :map calendar-mode-map "<f12>" #'casual-calendar-tmenu))

(after! help-mode
  (map! :map help-mode-map "<f12>" #'casual-help-tmenu))

(after! css-mode
  (map! :map css-mode-map "<f12>" #'casual-css-tmenu))

(after! csv-mode
  (map! :map csv-mode-map "<f12>" #'casual-csv-tmenu))

(after! sgml-mode
  (map! :map html-mode-map "<f12>" #'casual-html-tmenu))

(after! make-mode
  (map! :map makefile-mode-map "<f12>" #'casual-make-tmenu))

(after! elisp-mode
  (map! :map emacs-lisp-mode-map "<f12>" #'casual-elisp-tmenu))

(after! bibtex
  (map! :map bibtex-mode-map "<f12>" #'casual-bibtex-tmenu))

(after! isearch
  (map! :map isearch-mode-map "<f12>" #'casual-isearch-tmenu))

;;;; M-<f12> — casual-editkit (범용 편집, 모드 무관)

(map! "M-<f12>" #'casual-editkit-main-tmenu)

(provide 'casual-config)
;;; casual-config.el ends here
