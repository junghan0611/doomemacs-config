;;; $DOOMDIR/lisp/defaults-config.el --- Better defaults -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 전역 기본값 — Emacs 표준 변수 튜닝을 한 곳에 모은다.
;; auto-revert, completion-at-point, symlinks, lockfiles, xref 등
;; 특정 패키지에 귀속되지 않는 general default 들.

;;; Code:

(require 'cl-lib)
(require 'autorevert)
(require 'xref)

;;;; Completion / font / symlinks

;; 'tags-completion-at-point-function' break ten-glossary
(setq-default completion-at-point-functions nil) ; important

(setq inhibit-compacting-font-caches t)

;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)

;;;; auto-revert

;; 인간 ↔ 에이전트 공동 편집 — 외부 write 감지해 버퍼 갱신.
;; inotify 기반(nuc/laptop/thinkpad)은 즉시, termux 는 polling fallback.

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; VC 정보 modeline 갱신 비활성 — revert 시마다 git status 호출.
;; 프로파일 auto-revert-buffers 6s 누적의 주원인. magit/diff-hl 로 대체.
(setq auto-revert-check-vc-info nil)

;; 간격 (notify 사용 시 fallback 용)
(setq auto-revert-interval 5)

(cond
 ((equal my/current-device "termux")
  (setq auto-revert-use-notify nil
        auto-revert-avoid-polling nil))
 ((member my/current-device '("nuc" "laptop" "thinkpad"))
  ;; OS 파일 알림 사용 (polling 보다 빠르고 효율적)
  (setq auto-revert-use-notify t
        auto-revert-avoid-polling t)))

;;;; kill-ring / lockfiles

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files — Emacs ↔ Emacs 규약. 외부 에이전트에 무용.
(setq create-lockfiles nil)

;;;; xref search program

;; Denote 23.9. Speed up backlinks’ buffer creation.
;; Prefer ripgrep, then ugrep, and fall back to regular grep.
(setq xref-search-program
      (cond ((or (executable-find "ripgrep") (executable-find "rg")) 'ripgrep)
            ((executable-find "ugrep") 'ugrep)
            (t 'grep)))

;;;; persp-mode 세션 자동저장 — user 데몬만

;; pi 같은 보조 데몬도 doom config 을 풀로드하여 동일한 persp-save-dir
;; (~/.config/emacs/.local/state/workspaces/autosave) 를 공유한다. 종료
;; 순서에 따라 user 의 워크스페이스가 빈 pi 세션에 덮어쓰일 위험이 있어
;; user 인스턴스에서만 자동저장을 활성화한다.
;; server-name 은 init.el 에서 EMACS_SERVER_NAME 환경변수로 결정.
(unless (equal server-name "user")
  (setq persp-auto-save-opt 0))

(provide 'defaults-config)
;;; defaults-config.el ends here
