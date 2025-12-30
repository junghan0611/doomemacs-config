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

;;; provide

(provide 'termux-config)

;;; termux-config.el ends here
