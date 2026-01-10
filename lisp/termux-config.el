;;; $DOOMDIR/lisp/termux-config.el --- Termux Android Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:


;;; Code:


;;;; termux-fixes
;; Fix async issues in Termux/Android

(when IS-TERMUX
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-warning-on-missing-source nil)
  (setq async-bytecomp-allowed-packages nil)
  (setq process-connection-type nil)
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.6))

;;; TODO TERMUX

(when IS-TERMUX
  ;; 7. GUI 폰트 설정 (Sarasa Term K Nerd)

  ;; Termux X11 GUI에서 Nerd Font 아이콘 제대로 표시
  ;; Fold4 7.6" 2176x1812 (373 PPI, DPI 180 기준) 최적화
  ;; (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 14)
  ;;       doom-variable-pitch-font (font-spec :family "Sarasa Term K Nerd Font" :size 14))

  ;; 8. 배터리 효율 최적화 설정 (Termux X11 GUI)
  ;; 작성: 2025-11-08

  ;; Auto-save 간격 늘리기 (디스크 I/O 감소)
  (setq auto-save-interval 300)        ; 300 타이핑마다
  (setq auto-save-timeout 30)          ; 30초마다

  ;; GC 임계값 증가 (가비지 컬렉션 빈도 감소)
  (setq gc-cons-threshold (* 50 1024 1024))  ; 50MB

  ;; 스크롤 최적화
  (setq scroll-conservatively 101)
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position t)

  ;; 폰트 렌더링 최적화
  (setq inhibit-compacting-font-caches t)

  ;; 파일 변경 감지 간격 늘리기
  (setq auto-revert-interval 5)  ; 5초

  ;; 알람/비프음 비활성화 (하드웨어 절전)
  (setq ring-bell-function 'ignore)

  ;; Termux extra-keys 방향키 설정
  ;; Termux 환경에서 방향키가 제대로 동작하도록 보장
  (when (and (not (display-graphic-p))
             (or (getenv "TERMUX_VERSION")
                 (string-match-p "termux" (or (getenv "PREFIX") ""))))

    ;; Termux는 ESC O 시퀀스를 전송 (Application Keypad Mode)
    ;; input-decode-map과 function-key-map 모두에 매핑 (더 강력)
    (defun termux-fix-arrow-keys ()
      "Fix arrow keys for Termux extra-keys."
      ;; input-decode-map (우선순위 높음)
      (define-key input-decode-map "\eOA" [up])
      (define-key input-decode-map "\eOB" [down])
      (define-key input-decode-map "\eOC" [right])
      (define-key input-decode-map "\eOD" [left])
      (define-key input-decode-map "\e[A" [up])
      (define-key input-decode-map "\e[B" [down])
      (define-key input-decode-map "\e[C" [right])
      (define-key input-decode-map "\e[D" [left])
      ;; function-key-map (호환성)
      (define-key function-key-map "\eOA" [up])
      (define-key function-key-map "\eOB" [down])
      (define-key function-key-map "\eOC" [right])
      (define-key function-key-map "\eOD" [left])
      (define-key function-key-map "\e[A" [up])
      (define-key function-key-map "\e[B" [down])
      (define-key function-key-map "\e[C" [right])
      (define-key function-key-map "\e[D" [left])
      ;; local-function-key-map (로컬)
      (define-key local-function-key-map "\eOA" [up])
      (define-key local-function-key-map "\eOB" [down])
      (define-key local-function-key-map "\eOC" [right])
      (define-key local-function-key-map "\eOD" [left]))

    ;; 즉시 적용
    (termux-fix-arrow-keys)

    ;; 터미널 초기화 후에도 적용 (tty-setup-hook)
    (add-hook 'tty-setup-hook #'termux-fix-arrow-keys)

    ;; evil-mode 로드 후에도 적용 (evil이 키를 오버라이드할 수 있음)
    (after! evil
      (termux-fix-arrow-keys))

    (message "Termux 방향키 ESC O 시퀀스 매핑 완료 ✓"))
  )


;;; provide

(provide 'termux-config)

;;; termux-config.el ends here
