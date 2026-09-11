;;; $DOOMDIR/lisp/botschaft-config.el --- ChatGPT web conversation reader -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Botschaft reads the canonical server history without keeping a local store.
;; Its adapter is installed under the XDG data directory by botschaft's run.sh.

;;; Code:

;;;; Package wiring

(use-package! botschaft
  :commands (botschaft-projects botschaft-search botschaft-open)
  :config
  (let ((adapter-bin
         (expand-file-name
          "botschaft/cwa/src/.venv/bin/"
          (or (getenv "XDG_DATA_HOME") "~/.local/share"))))
    (setq botschaft-python (expand-file-name "python" adapter-bin)
          botschaft-cwa (expand-file-name "cwa" adapter-bin)
          botschaft-shim
          (expand-file-name "bin/cwaq"
                            (file-name-directory
                             (locate-library "botschaft"))))))

(provide 'botschaft-config)
;;; botschaft-config.el ends here
