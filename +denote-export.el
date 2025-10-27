;;; +denote-export.el --- Denote to Hugo Export System -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghanacs/doomemacs-config
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (denote "3.0") (ox-hugo "0.11"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Integrated export system for Denote notes to Hugo markdown.
;;
;; Features:
;; - Denote link conversion to Hugo relref
;; - Security filtering (ROT13, sensitive strings)
;; - Org-export optimized settings
;; - Parallel export support
;;
;; Migrated from emacs-fulllab-config:
;; - denote-hugo.el: Link conversion and export functions
;; - org-config.el: Org-export settings
;; - uniconfig.el: Security filters

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-hugo)
(require 'denote)

;;;; Section 0: ox-hugo Configuration
;; Migrated from config.el (line 942-1013)
;; IMPORTANT: Execute immediately, not with-eval-after-load

;; Base directory - uses user-hugo-blog-dir from +user-info.el
(when (boundp 'user-hugo-blog-dir)
  (setq org-hugo-base-dir user-hugo-blog-dir))

;; Lastmod settings
(setq org-hugo-auto-set-lastmod t
      org-hugo-suppress-lastmod-period 3600.0) ; 1h (86400.0=24h, 172800.0=48h)

;; Front matter format
(setq org-hugo-front-matter-format 'yaml)

;; Video support
;; Requires Hugo config: markup.goldmark.renderer.unsafe = true
(add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

;; Default section and shortcodes
(setq org-hugo-section "notes")
(setq org-hugo-paired-shortcodes "mermaid callout cards details tabs")

;; Link formatting
(setq org-hugo-link-desc-insert-type t)

;; Preserve filling - important for export
(setq org-hugo-preserve-filling nil)

;; Tag formatting
(setq org-hugo-allow-spaces-in-tags t)
(setq org-hugo-prefer-hyphen-in-tags t)

;; Static files directory
(setq org-hugo-default-static-subdirectory-for-externals "images")

;; Export creator string
(setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

;; Special blocks - raw content
(add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("details" :raw t))

;; Bibliography heading
(plist-put org-hugo-citations-plist :bibliography-section-heading "References")

;; Zero-width space handling
(defun my/insert-white-space ()
  "Insert zero-width space character."
  (interactive)
  (insert " "))

(defun +org-export-remove-white-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string " " "" text)))

(add-to-list 'org-export-filter-final-output-functions
             #'+org-export-remove-white-space t)

;; Keybinding for zero-width space (only in interactive mode)
(when (fboundp 'evil-define-key)
  (with-eval-after-load 'evil
    (evil-define-key '(insert normal) text-mode-map
      (kbd "M-m") #'my/insert-white-space)))

;;;; Section 1: Org-Export Settings
;; Migrated from org-config.el

(setq org-export-headline-levels 5)               ; default 3
(setq org-export-with-toc nil)                    ; default t - Hugo handles TOC
(setq org-export-exclude-tags
      '("private" "OFFICE" "FILE" "LOG" "CREDENTIAL"
        "REFILED" "LOCAL" "noexport" "ignore" "crypt"))

(setq org-publish-use-timestamps-flag t)          ; default t
(setq org-export-with-section-numbers t)          ; default t
(setq org-export-with-timestamps t)               ; default t
(setq org-export-with-todo-keywords t)            ; default t
(setq org-export-with-broken-links 'mark)         ; Mark broken links instead of error
(setq org-export-date-timestamp-format "%e %B %Y")
(setq org-export-use-babel nil)                   ; Faster export
(setq org-export-with-tags 'not-in-toc)

;;;; Section 2: Denote Link Conversion
;; Migrated from denote-hugo.el

(defun my/is-draft-file (path)
  "Check if the file at PATH is a draft."
  (string-match "_draft" path))

(defun my/is-md-file (path)
  "Check if the file at PATH belongs to the md directory."
  (string-match "/md" path))

(defun my/get-hugo-section-directory-from-path (path)
  "Extract Hugo section from PATH.
Checks for /notes, /journal, /talks, /meta, /bib directories."
  (let ((directories '("/test" "/notes" "/journal" "/talks" "/meta" "/bib"))
        (matched-dir nil))
    (dolist (dir directories)
      (when (string-match-p (regexp-quote dir) path)
        (setq matched-dir dir)))
    (if (not matched-dir)
        (setq matched-dir ""))
    matched-dir))

(defun my/denote-markdown-export (link desc)
  "Format the way Denote links are exported to markdown.
If LINK is considered private or a draft, return DESC.
If LINK is considered a public note, format it as a Hugo relative link.

IMPORTANT: Section is determined by org-hugo-section variable,
NOT by file path. This ensures URI permanence when files are moved."
  (condition-case err
      (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
             (path (nth 0 path-id))
             ;; Use org-hugo-section from .dir-locals.el (not path inference!)
             (section (or org-hugo-section "notes"))
             (id (nth 1 path-id))
             (query (nth 2 path-id))
             (exportfilename (format "%s.md" id))
             (content-dir (when org-hugo-base-dir
                            (concat (file-name-as-directory org-hugo-base-dir)
                                    (format "content/%s/" section))))
             (exportfilepath (when (and exportfilename content-dir)
                               (expand-file-name exportfilename content-dir)))
             (uri (cond
                   (query (format "%s/%s" exportfilename query))
                   (t (format "%s" exportfilename)))))
        ;; Return the formatted link
        (cond
         ;; For Digital Garden (Quartz)
         ((string-match "notes" org-hugo-base-dir)
          (if (or (my/is-draft-file path) (my/is-md-file path))
              (format "[%s]" desc)
            (if exportfilename
                (format "[%s]({{< relref \"%s/%s\" >}})" desc section exportfilename)
              (format "[%s]" desc))))

         ;; Fallback
         (t (format "[%s]" desc))))
    (error
     ;; If denote link resolution fails, return plain description
     (format "[%s]" desc))))

(defun my/denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export backend."
  (condition-case err
      (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
             (path (file-relative-name (nth 0 path-id)))
             (id (nth 1 path-id))
             (query (nth 2 path-id))
             (anchor (file-name-sans-extension path))
             (desc (cond
                    (description)
                    (query (format "denote:%s::%s" id query))
                    (t (concat "denote:" id)))))
        (cond
         ((eq format 'html)
          (if query
              (format "<a href=\"%s.html%s\">%s</a>" anchor query desc)
            (format "<a href=\"%s.html\">%s</a>" anchor desc)))
         ((eq format 'latex)
          (format "\\href{%s}{%s}"
                  (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path) desc))
         ((eq format 'texinfo) (format "@uref{%s,%s}" path desc))
         ((eq format 'ascii) (format "[%s] <denote:%s>" desc path))
         ((eq format 'md) (my/denote-markdown-export link desc))
         (t path)))
    (error
     ;; If link resolution fails, return description as plain text
     (or description link))))

;; Register denote link export handler
(org-link-set-parameters "denote" :export #'my/denote-link-ol-export)

;;;; Section 3: Security Filters
;; Migrated from uniconfig.el

;; Company name ROT13 filter
(when (and (boundp 'my-company-name) my-company-name)

  (defun my-org-hugo-rot13-company-name (text backend info)
    "Hugo로 내보낼 때 회사명을 ROT13으로 암호화합니다."
    (when (and my-company-name my-company-name-rot13
               (org-export-derived-backend-p backend 'hugo))
      (replace-regexp-in-string my-company-name
                                my-company-name-rot13
                                text t t)))

  (add-to-list 'org-export-filter-plain-text-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-src-block-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-link-functions
               'my-org-hugo-rot13-company-name)
  (add-to-list 'org-export-filter-keyword-functions
               'my-org-hugo-rot13-company-name))

;; Sensitive strings filter
(defvar my-sensitive-strings-map nil
  "민감한 문자열과 대체 문자열의 매핑을 저장하는 해시테이블")

(defvar my-sensitive-strings-file
  (expand-file-name "var/sensitive-strings.txt" doom-user-dir)
  "민감한 문자열들을 저장하는 파일 경로")

(defun my-generate-consistent-replacement (original-string)
  "원본 문자열을 일관된 대체 문자열로 변환합니다.
같은 입력에 대해 항상 같은 출력을 생성합니다."
  (let* ((hash (secure-hash 'sha256 original-string))
         (hex-chars (substring hash 0 (min 32 (* 2 (length original-string)))))
         (replacement ""))
    (dotimes (i (length original-string))
      (let ((orig-char (aref original-string i))
            (hex-index (* 2 (% i 16))))
        (cond
         ;; 숫자는 숫자로
         ((and (>= orig-char ?0) (<= orig-char ?9))
          (setq replacement
                (concat replacement
                        (char-to-string
                         (+ ?0 (% (string-to-number
                                   (substring hex-chars hex-index (1+ hex-index)) 16) 10))))))
         ;; 소문자는 소문자로
         ((and (>= orig-char ?a) (<= orig-char ?z))
          (setq replacement
                (concat replacement
                        (char-to-string
                         (+ ?a (% (string-to-number
                                   (substring hex-chars hex-index (1+ hex-index)) 16) 26))))))
         ;; 대문자는 대문자로
         ((and (>= orig-char ?A) (<= orig-char ?Z))
          (setq replacement
                (concat replacement
                        (char-to-string
                         (+ ?A (% (string-to-number
                                   (substring hex-chars hex-index (1+ hex-index)) 16) 26))))))
         ;; 기타 문자는 그대로
         (t
          (setq replacement (concat replacement (char-to-string orig-char)))))))
    replacement))

(defun my-load-sensitive-strings ()
  "민감한 문자열 파일을 로드합니다."
  (setq my-sensitive-strings-map (make-hash-table :test 'equal))
  (when (file-exists-p my-sensitive-strings-file)
    (with-temp-buffer
      (insert-file-contents my-sensitive-strings-file)
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (dolist (item data)
          (puthash (car item) (cdr item) my-sensitive-strings-map))))))

(defun my-save-sensitive-strings ()
  "민감한 문자열 매핑을 파일에 저장합니다."
  (let ((dir (file-name-directory my-sensitive-strings-file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (with-temp-file my-sensitive-strings-file
    (let ((data '()))
      (maphash (lambda (k v) (push (cons k v) data)) my-sensitive-strings-map)
      (insert (format "%S" data)))))

(defun my-add-sensitive-string (original)
  "새로운 민감한 문자열을 추가합니다."
  (interactive "s민감한 문자열 입력: ")
  (unless my-sensitive-strings-map
    (my-load-sensitive-strings))
  (unless (gethash original my-sensitive-strings-map)
    (let ((replacement (my-generate-consistent-replacement original)))
      (puthash original replacement my-sensitive-strings-map)
      (my-save-sensitive-strings)
      (message "추가됨: '%s' -> '%s'" original replacement))))

(defun my-org-hugo-filter-sensitive-strings (text backend info)
  "Hugo 내보내기 시 민감한 문자열들을 대체합니다."
  (when (org-export-derived-backend-p backend 'hugo)
    (unless my-sensitive-strings-map
      (my-load-sensitive-strings))
    (when my-sensitive-strings-map
      (maphash (lambda (original replacement)
                 (setq text (replace-regexp-in-string
                             (regexp-quote original) replacement text t t)))
               my-sensitive-strings-map))
    text))

;; Commented out by default - uncomment to enable
;; (add-to-list 'org-export-filter-plain-text-functions
;;              'my-org-hugo-filter-sensitive-strings)
;; (add-to-list 'org-export-filter-src-block-functions
;;              'my-org-hugo-filter-sensitive-strings)

;;;; Section 4: Export Functions
;; Migrated from denote-hugo.el

(defun my/org-update-all-dblocks ()
  "Update all dynamic blocks in current buffer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (org-update-all-dblocks)))

(defun my/org-update-all-dblocks-on-directory (directory)
  "Update all dynamic blocks in all Org files in DIRECTORY.
Used primarily for meta notes with denote backlinks."
  (interactive "DSelect directory: ")
  (let ((org-files (directory-files-recursively directory "\\.org\\'")))
    (dolist (org-file org-files)
      (message "Updating dblocks in: %s" org-file)
      (with-current-buffer (find-file-noselect org-file)
        (org-dblock-update 0)))))

(defun my/kill-all-buffers-except-toolbox ()
  "Kill all file-visiting buffers except special ones.
Helps cleanup after batch export."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (not (string-match-p "\\*" (buffer-name buffer))))
      (kill-buffer buffer))))

(defun my/org-hugo-export-directory (directory)
  "Export all Org files in DIRECTORY to Markdown using `org-hugo-export-to-md'.
Sequential processing - for parallel export, use external script."
  (interactive "DSelect directory: ")
  (let ((org-files (directory-files-recursively directory "\\.org\\'")))
    (dolist (org-file org-files)
      (message "Exporting: %s" org-file)
      (with-current-buffer (find-file-noselect org-file)
        (org-hugo-export-to-md))))

  (my/kill-all-buffers-except-toolbox)
  (garbage-collect)
  (setq which-key-replacement-alist nil))

(defun my/org-hugo-export-file (file)
  "Export single Org FILE to Markdown.
Used by parallel export script."
  (with-current-buffer (find-file-noselect file)
    (org-hugo-export-to-md)))

;;;; Section 5: Main Export Functions

(defun my/update-dblock-garden (dir)
  "Update dynamic blocks in DIR and save buffers."
  (interactive "DSelect directory: ")
  (message "Updating dblocks in: %s" dir)
  (my/org-update-all-dblocks-on-directory dir)
  (message "Saving org buffers...")
  (org-save-all-org-buffers)
  (setq which-key-replacement-alist nil))

(defun my/update-export-garden (dir)
  "Export all Org files in DIR to Hugo markdown."
  (interactive "DSelect directory: ")
  (message "Exporting files in: %s" dir)
  (my/org-hugo-export-directory dir))

;; Garden directory configuration
;; Override in per-machine.el if needed
(defvar garden-directory-lists
  '("~/org/meta" "~/org/bib" "~/org/notes")
  "List of directories to process for dblock updates and export.")

(defun my/update-dblock-garden-all ()
  "Update dynamic blocks in all garden directories."
  (interactive)
  (mapcar 'my/update-dblock-garden garden-directory-lists))

(defun my/update-export-garden-all ()
  "Export all garden directories sequentially."
  (interactive)
  (mapcar 'my/update-export-garden garden-directory-lists))

(defun my/update-dblock-export-garden-all ()
  "Update dblocks and export all garden directories sequentially.
For parallel export, use `my/update-dblock-export-garden-all-parallel'."
  (interactive)
  (my/update-dblock-garden-all)
  (my/update-export-garden-all))

;;;; Section 6: Parallel Export Integration

(defun my/update-dblock-export-garden-all-parallel ()
  "Update dblocks + parallel export using external script.
Significantly faster for large note collections (1,400+ files).

Prerequisites:
- GNU Parallel installed: apt install parallel
- Export script: ~/.config/doom/bin/denote-export-parallel.sh

Workflow:
1. Update dblocks in ~/org/meta (sequential, Emacs)
2. Parallel export via external script (8+ cores)
3. Post-processing in notes repo (clean-run.sh)"
  (interactive)

  ;; Step 1: Update dblocks (only ~/org/meta, sequential)
  (message "Step 1/3: Updating dblocks in ~/org/meta...")
  (my/update-dblock-garden "~/org/meta")

  ;; Step 2: Parallel export
  (message "Step 2/3: Starting parallel export...")
  (let ((script (expand-file-name "bin/denote-export-parallel.sh" doom-user-dir)))
    (if (file-exists-p script)
        (progn
          (shell-command script)
          (message "Parallel export completed."))
      (message "WARNING: Parallel export script not found: %s" script)
      (message "Falling back to sequential export...")
      (my/update-export-garden-all)))

  ;; Step 3: Reminder for post-processing
  (message "Step 3/3: Run clean-run.sh in ~/repos/gh/notes/ for post-processing"))

;;;; Section 7: Additional Hugo Functions
;; Helper functions for manual operations

(defun my/get-export-file-name-from-buffer ()
  "Retrieve the value of #+EXPORT_FILE_NAME from the current buffer."
  (save-excursion
    (goto-char 0)
    (if (re-search-forward "^#\\+export_file_name: \\(.*\\)$" nil t)
        (match-string 1)
      nil)))

(defun my/org-open-exported-markdown-in-hugo-content ()
  "Open the Markdown file exported from the current Org-mode buffer.
File is located in the `org-hugo-base-dir` content folder."
  (interactive)
  (let* ((exportfilename (my/get-export-file-name-from-buffer))
         (content-dir (concat (file-name-as-directory org-hugo-base-dir)
                              (format "content/%s/" org-hugo-section)))
         (exportfilepath (when (and exportfilename org-hugo-base-dir)
                           (expand-file-name exportfilename content-dir))))
    (if (and exportfilepath (file-exists-p exportfilepath))
        (find-file exportfilepath)
      (message "Markdown file not found: %s" exportfilepath))))

(defun my/insert-hugo-export-file-name ()
  "Add metadata to current org-mode file containing export file name.
Export File Name is the denote identifier."
  (interactive)
  (save-excursion
    (goto-char 0)
    (search-forward "identifier")
    (end-of-line)
    (insert (format
             "\n#+export_file_name: %s.md"
             (denote-retrieve-filename-identifier buffer-file-name)))))

;;;; TODO: pass CLI integration
;; Future enhancement: Integrate password-store for secret validation
;; (defun my/validate-secrets-before-export () ...)

(provide '+denote-export)
;;; +denote-export.el ends here
