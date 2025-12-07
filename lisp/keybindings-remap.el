;;; $DOOMDIR/lisp/keybindings-remap.el --- Keybindings Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;;;; remap all C-c prefix keys to M-c?

;; static map
(define-key key-translation-map (kbd "M-c") (kbd "C-c"))

;;; provide

(provide 'keybindings-remap)

;;; ends here
