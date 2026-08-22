;;; $DOOMDIR/lisp/casual-config.el --- Casual Transient UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Casual: 모드별 Transient 메뉴.  https://github.com/kickingvegas/casual
;;
;; 2026-08-22: 모드별 <f12> 26줄을 upstream `casual-init' 한 줄로 갈아치웠다.
;; upstream 이 이미 같은 일을 하고 있었고, 우리 목록은 새 모듈이 나올 때마다
;; 뒤처지기만 했다.
;;
;; <f1> 은 전역이다.  `casual-editkit-init' 이 primary 를 `keymap-global-set' 으로
;; 박으므로 (casual-editkit.el:43) Emacs 기본 <f1> 도움말 접두는 사라진다 —
;; 도움말은 C-h / SPC h 로 그대로.  모드 메뉴가 있는 곳은 그 메뉴가, 없는 곳은
;; `casual-editkit-main-tmenu' 가 뜬다.
;;
;; M-<f1> 은 upstream 이 secondary 로 돌려놓은 모드에만 있다 — bibtex, elisp,
;; css, csv, html.  나머지 모드에서는 비어 있다.
;;
;; 비용: `casual-init' 은 `casual-<mode>-init' 을 전부 부르고 그 autoload 들이
;; calc, bibtex, eww, man, esh-mode, ediff, re-builder, cus-edit 를 끌고 들어온다.
;; 측정 2026-08-22 batch — 콜드 3.6s / 주요 모듈 선로딩 warm 2.6s, features +239.
;; 이 Doom 은 `use-package-always-defer' 가 nil 이라 `:defer' 를 안 쓰면 그 값을
;; 시작 때 그대로 낸다.  그래서 `:defer 5' — 시작 5초 뒤 idle 에 읽는다.
;; <f1> 은 그 뒤부터 산다.

;;; Code:

;;;; casual

(use-package! casual
  :defer 5
  :init
  ;; upstream 기본값은 C-o / M-m — 둘 다 Doom 에서 이미 임자가 있어 f1 축으로.
  (setq casual-keybinding-primary "<f1>")
  (setq casual-keybinding-secondary "M-<f1>")
  :config
  (casual-init))

;;;; Context Menu & Anju

;; [DISABLED] 마우스 지원 안 함 — 에이전트 인터페이스로 대체
;; context-menu, anju, org-mouse 모두 비활성
;; (context-menu-mode 1)
;; (after! anju (setq use-file-dialog nil))
;; (anju-init)
;; (keymap-global-set "C-x 1" #'anju-toggle-one-window)

;;;; provide

(provide 'casual-config)
;;; casual-config.el ends here
