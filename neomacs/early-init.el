;;; neomacs/early-init.el --- Neomacs vanilla minimal profile, early stage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Early init for the Neomacs vanilla minimal profile (issue #8).
;; Loaded before package.el activation and before the first frame exists.
;;
;; This profile is deliberately independent of Doom.  Nothing here may
;; assume `doom-*' variables, Doom modules, or an ELPA-populated
;; `package-user-dir'.  The goal is a profile that boots on a bare
;; standalone Neomacs binary with builtins only.

;;; Code:

;;;; Package system — off by default

;; The minimal set is builtin-only, so package.el activation is skipped at
;; startup.  Neomacs issue #121 (gnutls/ELPA) is probed explicitly instead,
;; via probe/probe-tls.el, so a broken TLS stack can never turn into a
;; startup failure here.
(setq package-enable-at-startup nil)

;;;; GC during startup

;; Neomacs runs a concurrent-mark GC that is still young (upstream tasks
;; 01/03, 2026-07).  Raising the threshold during startup keeps early
;; allocation churn out of the picture when a boot failure has to be
;; attributed to something else.
(setq gc-cons-threshold (* 256 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.2)))

;;;; Frame — no chrome before the first redisplay

;; Set on `default-frame-alist' rather than via the modes, so the renderer
;; never has to lay out a toolbar it will immediately discard.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      initial-scratch-message nil)

;;;; Native compilation

;; Neomacs has no native-comp; guard so the same file also runs under GNU
;; Emacs when a divergence has to be compared side by side.
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;;; early-init.el ends here
