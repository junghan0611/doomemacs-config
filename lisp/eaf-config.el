;;; $DOOMDIR/lisp/eaf-config.el --- EAF configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; EAF (Emacs Application Framework) 설정
;; - site-lisp에 클론된 경우에만 로드
;; - Evil 모드 통합 (SPC 키 처리)
;; - 한글 입력 지원 (x-gtk-use-native-input)

;;; Code:

(defvar eaf-directory "~/.emacs.d/site-lisp/emacs-application-framework/")

(when (file-directory-p eaf-directory)

;;;; Core

  (add-to-list 'load-path eaf-directory)

  ;; 기본 동작 덮어쓰기 방지 (require 전에 설정!)
  (setq eaf-find-file-advisor-enable nil   ; find-file 대체 안함
        eaf-dired-advisor-enable nil)      ; dired 대체 안함

  (require 'eaf)

;;;; Apps

  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-pyqterminal)
  ;; (require 'eaf-jupyter)         ; Qt 위젯 직접 통합 예시
  ;; (require 'eaf-vue-tailwindcss)

;;;; Doom + 한글 입력

  (add-hook 'eaf-mode-hook #'doom-mark-buffer-as-real-h)

  (defun my/eaf-setup-gtk-use-native-input ()
    "한글 입력을 위한 GTK native input 활성화."
    (when (eq major-mode 'eaf-mode)
      (setq-local x-gtk-use-native-input t)))
  (add-hook 'eaf-mode-hook #'my/eaf-setup-gtk-use-native-input)

;;;; Evil 통합

  (require 'eaf-evil)

  ;; eaf-evil-leader-key "C-SPC"

  ;; SPC 키: EAF 앱별로 다르게 처리
  ;; - browser (입력 포커스 없을 때): leader
  ;; - pdf-viewer, mind-elixir: leader
  ;; - 그 외 (pyqterminal 등): 그대로 SPC
  (define-key key-translation-map (kbd "SPC")
              (lambda (_prompt)
                (if (derived-mode-p 'eaf-mode)
                    (pcase eaf--buffer-app-name
                      ("browser" (if eaf-buffer-input-focus
                                     (kbd "SPC")
                                   (kbd eaf-evil-leader-key)))
                      ("pdf-viewer" (kbd eaf-evil-leader-key))
                      ("mind-elixir" (kbd eaf-evil-leader-key))
                      (_ (kbd "SPC")))
                  (kbd "SPC"))))

;;;; Browser 설정

  (setq eaf-browser-translate-language "ko"
        eaf-browser-continue-where-left-off t
        eaf-browser-default-search-engine "duckduckgo"
        eaf-browser-enable-adblocker "true")

  (eaf-bind-key nil "M-q" eaf-browser-keybinding)

;;;; pyqterminal 설정

  (setq eaf-pyqterminal-font-size 16
        eaf-pyqterminal-font-family "Sarasa Term K Nerd")

  ;; TODO: M-m local leader - EAF 키 처리 방식 조사 필요 (doom-xxx)

  ) ; end when

(provide 'eaf-config)
;;; eaf-config.el ends here
