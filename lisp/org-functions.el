;;; $DOOMDIR/lisp/org-functions.el --- Org Helper Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;;; Code:

(require 'org)

;;;; bh/insert-inactive-timestamp

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

;;;; ~/sync/emacs/emacs-fulllab-config/dotdoomemacs/+markdown.el

(progn

  (defun my/yank-as-org ()
    "Convert region of markdown text to org while yanking."
    (interactive)
    (let* ((_ (unless (executable-find "pandoc")
                (user-error "pandoc not found")))
           (beg (if evil-mode
                    (marker-position evil-visual-beginning)
                  (region-beginning)))
           (end (if evil-mode
                    (marker-position evil-visual-end)
                  (region-end)))
           (region-content (buffer-substring-no-properties beg end))
           (_ (print region-content))
           (converted-content
            (with-temp-buffer
              (insert region-content)
              (shell-command-on-region
               (point-min)
               (point-max)
               "pandoc --wrap=none -f markdown -t org" nil t)
              (buffer-string))))
      (kill-new converted-content)
      (message "yanked Markdown as Org")))

  (defun my/yank-as-markdown ()
    "Convert region of Org-mode to markdown while yanking."
    (interactive)
    (let* ((_ (unless (executable-find "pandoc")
                (user-error "pandoc not found")))
           (beg (if evil-mode
                    (marker-position evil-visual-beginning)
                  (region-beginning)))
           (end (if evil-mode
                    (marker-position evil-visual-end)
                  (region-end)))
           (region-content (buffer-substring-no-properties beg end))
           (_ (print region-content))
           (converted-content
            (with-temp-buffer
              (insert region-content)
              (shell-command-on-region
               (point-min)
               (point-max)
               "pandoc --wrap=none -f org -t gfm" nil t)
              (buffer-string))))
      (kill-new converted-content)
      (message "yanked Org as Markdown")))
  )


;;;; org-open-at-point-other-window

;; ohyecloudy-dot-doom/doom.d/config.org
(defun my/org-open-at-point-other-window ()
  (interactive)
  (let ((org-link-frame-setup (cons (cons 'file 'find-file-other-window) org-link-frame-setup)))
    (org-open-at-point)))

;;;;;; org-indent-src-block

(defun my/org-indent-src-block ()
  (interactive)
  (org-edit-special)
  (my/indent-buffer)
  (org-edit-src-exit))

;;;; Provide

(provide 'org-functions)

;;; org-functions.el ends here
