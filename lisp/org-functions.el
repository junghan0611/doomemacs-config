;;; $DOOMDIR/lisp/org-functions.el --- Org Helper Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

(defun bh/make-org-scratch ()
  (interactive)
  (find-file (concat org-directory "/scratch.org")))

(defun bh/make-markdown-scratch ()
  (interactive)
  (find-file (concat org-directory "/scratch.md")))

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

;;;;;; org-indent-src-block

(defun my/update-hugo-lastmod ()
  "Update #+hugo_lastmod on save. Add after #+filetags if missing."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((lastmod-line (format "#+hugo_lastmod: [%s]"
                                  (format-time-string "%Y-%m-%d %a %H:%M"))))
        (if (re-search-forward "^#\\+hugo_lastmod:.*$" nil t)
            ;; Exists: update in place
            (replace-match lastmod-line)
          ;; Missing: add after #+filetags
          (goto-char (point-min))
          (when (re-search-forward "^#\\+filetags:.*$" nil t)
            (end-of-line)
            (insert "\n" lastmod-line)))))))

;;;; my/org-link-to-headline — intra-document internal link

;; Insert a fuzzy `[[*Heading][desc]]' link to another headline in the same
;; buffer.  Unlike denote:ID links (cross-file), this stays inside one file, so
;; an agent handed the source file already holds the whole target — following
;; the link needs no extra fetch.  Used to thread a journal's time-stamped
;; headings into a "prior thought" graph along one timeline: a later heading
;; links back to an earlier one without rewriting the earlier text, keeping the
;; timestamps honest.
;;
;; Origin: reddit.com/r/emacs/comments/yjobc2/comment/iur16c7/

(defun my/org--headline-titles ()
  "Return raw titles of all headlines in the current buffer.
Titles drop the leading stars, TODO keyword, priority cookie and tags,
so a chosen title matches `[[*Heading]]' fuzzy search regardless of a
TODO/NEXT keyword on the target heading."
  (org-element-map (org-element-parse-buffer 'headline) 'headline
    (lambda (h) (org-element-property :raw-value h))))

(defun my/org-link-to-headline ()
  "Insert an internal link to a headline in the current buffer.
Prompt for a headline, then a description defaulting to the headline
text, and insert a fuzzy `[[*Heading][desc]]' link that jumps within
the file."
  (interactive)
  (let* ((titles (my/org--headline-titles))
         (choice (completing-read "Heading: " titles nil t))
         (desc (read-string "Description: " choice)))
    (org-insert-link nil (concat "*" choice) desc)))

;;;; Provide

(provide 'org-functions)

;;; org-functions.el ends here
