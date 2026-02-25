;;; $DOOMDIR/lisp/denote-config.el --- Denote Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (denote "3.0"))

;;; Commentary:

;; Denote 패키지 설정
;; - denote: 파일 기반 노트 시스템
;; - citar-denote: bibliography 연동
;; - denote-explore, denote-search

;;; Code:

;;;; denote configuration

(use-package! denote
  :demand t
  :commands
  (denote denote-create-note denote-insert-link denote-show-backlinks-buffer denote-link-ol-store)
  :init
  (setq denote-directory org-directory)
  (require 'denote-org)
  ;; (require 'denote-silo)
  (require 'denote-sequence)
  ;; (require 'denote-journal)
  (require 'denote-org)
  (require 'denote-markdown)

  (setq denote-file-type 'org)
  (setq denote-sort-components '(signature title keywords identifier))
  (setq denote-backlinks-show-context nil)
  (setq denote-sort-keywords t)
  (setq denote-infer-keywords t)
  (setq denote-excluded-directories-regexp "screenshot")
  (setq denote-org-front-matter
        "#+title:      %1$s
#+filetags:   %3$s
#+hugo_lastmod: %2$s
#+date:       %2$s
#+identifier: %4$s
#+export_file_name: %4$s.md
#+description:
#+hugo_tags: temp
#+hugo_categories: Noname
#+print_bibliography:\n* History\n- %2$s\n* Related-Notes\n\n")

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (setq denote-prompts '(subdirectory title keywords)) ; These are the minimum viable prompts for notes
  (setq denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech

  ;; More functionality
  (setq denote-org-store-link-to-heading nil ; default t
        denote-rename-confirmations nil ; default '(rewrite-front-matter modify-file-name)
        denote-save-buffers t) ; default nil
  (add-hook 'org-mode-hook (lambda ()
                             (setq denote-rename-buffer-backlinks-indicator "¶")
                             (setq denote-rename-buffer-format "%t%b")
                             (denote-rename-buffer-mode +1)))

  ;; for claude memory integration
  ;; 동적 Silo 관리는 +denote-silo-dynamic.el에서 처리됨
  ;; (add-to-list 'denote-silo-directories (expand-file-name "~/claude-memory/"))

  (use-package! consult-notes
    :commands (consult-notes consult-notes-search-in-all-notes)
    :config
    (setq consult-notes-denote-display-id t)
    (setq consult-notes-denote-dir t)
    (setq consult-notes-denote-title-margin 2) ; 24
    (consult-notes-denote-mode 1)
    )

  (use-package! consult-denote
    :config
    ;; Prefer `ripgrep' and `fd' variants when available
    (when (executable-find "fd")
      (setopt consult-denote-find-command #'consult-fd))
    (when (executable-find "rg")
      (setopt consult-denote-grep-command #'consult-ripgrep))
    (consult-denote-mode 1)
    )

  (use-package! citar-denote
    :demand t ;; Ensure minor mode is loaded
    :bind (:map org-mode-map
           ("C-c B" . citar-insert-citation)
           :map minibuffer-local-map
           ("M-r" . vertico-repeat))
    :commands
    (citar-create-note citar-open-notes citar-denote-open citar-denote-add-citekey)
    :init
    (require 'bibtex)
    (require 'citar)
    :custom
    ;; (citar-open-always-create-notes t)
    ;; (citar-denote-signature t)
    (citar-denote-file-type 'org)
    (citar-denote-subdir t)
    (citar-denote-keyword "bib")
    (citar-denote-title-format "author-year-title") ; default title
    (citar-denote-use-bib-keywords nil)
    (citar-denote-title-format-authors 1)
    (citar-denote-title-format-andstr "and")
    :config
    ;; FIXME for denote-obsidian
    (setq citar-denote-file-types
          `((org
             :reference-format "#+reference:  %s\n"
             :reference-regex "^#\\+reference\\s-*:")
            (markdown-obsidian ;; 2025-02-03
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-yaml
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")
            (markdown-toml
             :reference-format "reference  = %s\n"
             :reference-regex "^reference\\s-*=")
            (text
             :reference-format "reference:  %s\n"
             :reference-regex "^reference\\s-*:")))
    (citar-denote-mode))
  )

;;;; Dired dblock update

(progn
  (defun my/denote-dblock-update-file (file)
    "Update all dblocks in FILE and save."
    (let ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#\\+BEGIN:" nil t)
            (goto-char (point-min))
            (org-update-all-dblocks)
            (save-buffer)
            (message "[OK] Updated dblocks: %s" (file-name-nondirectory file)))))
      ;; Don't kill buffer if it was already open
      (unless (get-file-buffer file)
        (kill-buffer buf))))

;;;###autoload
  (defun my/denote-dblock-update-marked-files ()
    "Update dblocks in all marked files in Dired.
If no files are marked, update the file at point."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in Dired mode"))
    (let* ((files (dired-get-marked-files nil nil nil t))
           ;; Handle single file case (returns t as first element)
           (files (if (eq (car files) t)
                      (cdr files)
                    files))
           (org-files (seq-filter (lambda (f) (string-suffix-p ".org" f)) files))
           (total (length org-files))
           (count 0))
      (if (zerop total)
          (message "No org files selected")
        (message "Updating dblocks in %d files..." total)
        (dolist (file org-files)
          (setq count (1+ count))
          (message "[%d/%d] %s" count total (file-name-nondirectory file))
          (my/denote-dblock-update-file file))
        (message "Done! Updated %d files." total))))

;;;###autoload
  (defun my/denote-dblock-update-current-buffer ()
    "Update all dblocks in current buffer."
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in Org mode"))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+BEGIN:" nil t)
          (progn
            (goto-char (point-min))
            (org-update-all-dblocks)
            (message "Dblocks updated in %s" (buffer-name)))
        (message "No dblocks found in %s" (buffer-name)))))

  ;; Dired keybinding
  (after! dired
    (define-key dired-mode-map (kbd "C-c d u") #'my/denote-dblock-update-marked-files))
  )

;;;; denote-explore

(use-package! denote-explore)

;;;; denote-search

(use-package! denote-search)

;;;; Ten with etags

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

;; (use-package! ten
;;   :defer 2
;;   ;; :hook ((org-mode Info-mode) . ten-font-lock-mode) ;; text-mode
;;   :init
;;   (setq ten-exclude-regexps '("/\\."))
;;   :config
;;   (require 'consult-ten)
;;   (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
;;   )

;;;; denote-merge

(use-package! denote-merge
  :commands
  ( denote-merge-file
    denote-merge-region
    denote-merge-region-plain
    denote-merge-region-plain-indented
    denote-merge-region-org-src
    denote-merge-region-org-quote
    denote-merge-region-org-example
    denote-merge-region-markdown-quote
    denote-merge-region-markdown-fenced-block))

(provide 'denote-config)
;;; denote-config.el ends here
