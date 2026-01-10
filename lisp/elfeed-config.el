;;; $DOOMDIR/lisp/elfeed-config.el --- Elfeed configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Elfeed 커스텀 설정
;; - elfeed-tube 연동 (YouTube 자막)
;; - 컨텐츠 검색
;; - gptel 번역/요약 통합

;;; Code:

;;;; Elfeed

(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  (setq elfeed-search-filter "@1-year-ago") ; "@6-months-ago" "@1-month-ago +unread"
  ;; (setq elfeed-search-title-max-width 90) ; default 70
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  )

;;;; 본문 검색 (elfeed-deref 기반 - archive.gz 호환)

;; Doom Emacs는 elfeed 컨텐츠를 archive.gz로 압축 저장
;; cuckoo-search(ripgrep 직접 검색)는 호환 안됨
;; → elfeed-deref로 컨텐츠 읽어서 검색

(defun +elfeed-search-content (pattern)
  "Search elfeed entries by content PATTERN.
elfeed-deref를 사용하여 archive.gz에서도 컨텐츠를 읽음."
  (interactive "sSearch content: ")
  (unless (derived-mode-p 'elfeed-search-mode)
    (user-error "Not in elfeed-search buffer"))
  (let ((matches nil)
        (case-fold-search t))
    (dolist (entry elfeed-search-entries)
      (let ((content (elfeed-deref (elfeed-entry-content entry)))
            (title (elfeed-entry-title entry)))
        (when (or (and content (string-match-p pattern content))
                  (and title (string-match-p pattern title)))
          (push entry matches))))
    (if matches
        (progn
          (setq elfeed-search-entries (nreverse matches))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (entry elfeed-search-entries)
              (funcall elfeed-search-print-entry-function entry)
              (insert "\n")))
          (setq header-line-format
                (concat (elfeed-search--header) " [" pattern "]"))
          (goto-char (point-min))
          (message "Found %d entries matching '%s'" (length matches) pattern))
      (message "No matches for: %s" pattern))))

(defun +elfeed-search-content-clear ()
  "Clear content search and restore original filter."
  (interactive)
  (elfeed-search-update :force)
  (message "Filter cleared"))

;;;; org-web-tools

(use-package! org-web-tools)

;;;; Keybindings

(after! elfeed
  (map! :map elfeed-search-mode-map
        :niv "/" #'+elfeed-search-content       ; 본문 검색
        :niv "q" #'+elfeed-search-content-clear    ; 필터 초기화
        :localleader
        "/" #'+elfeed-search-content            ; 본문 검색
        "c" #'+elfeed-search-content-clear    ; 필터 초기화
        "t" #'org-web-tools-read-url-as-org
        "T" #'org-web-tools-convert-links-to-page-entries
       ))

;;; Provide

(provide 'elfeed-config)

;;; elfeed-config.el ends here
