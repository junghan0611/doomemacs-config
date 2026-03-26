;;; +denote-export-config.el --- Denote to Hugo Export System -*- lexical-binding: t; -*-

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

(setq org-hugo-export-with-toc nil) ; default nil

;; Lastmod settings
(setq org-hugo-auto-set-lastmod t
      org-hugo-suppress-lastmod-period 3600.0) ; 1h (86400.0=24h, 172800.0=48h)

;; Append and update time-stamps for
;; #+hugo_lastmod: Time-stamp: <>
;; org-hugo-auto-set-lastmod should be nil
(require 'time-stamp)
;; (add-hook 'write-file-functions 'time-stamp)
;; M-x time-stamp
;; Update last modified date for ox-hugo export
(setq time-stamp-active t
      time-stamp-start "#\\+hugo_lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%m-%d\]")

;; Front matter format
(setq org-hugo-front-matter-format 'yaml)

;; Video support
;; Requires Hugo config: markup.goldmark.renderer.unsafe = true
(add-to-list 'org-hugo-external-file-extensions-allowed-for-copying "webm")

;; Default section and shortcodes
(setq org-hugo-section "notes")
(setq org-hugo-paired-shortcodes "mermaid callout cards details tabs sidenote")

;; https://ox-hugo.scripter.co/doc/formatting/
;; if org-hugo-use-code-for-kbd is non-nil
;; Requires CSS to render the <kbd> tag as something special. eg: ~kbd~
;; (setq org-hugo-use-code-for-kbd t) ; default nil

;; Link formatting
;; (setq org-hugo-link-desc-insert-type t)

;; Preserve filling - important for export
(setq org-hugo-preserve-filling nil)
(setq org-hugo-delete-trailing-ws nil) ;; for quartz

;; Tag formatting
(setq org-hugo-allow-spaces-in-tags t)
(setq org-hugo-prefer-hyphen-in-tags t)

;; Static files directory
(setq org-hugo-default-static-subdirectory-for-externals "images")

;; Export creator string
(setq org-hugo-export-creator-string "Emacs + Org-mode + ox-hugo")

;; Special blocks - raw content
;; (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("details" :raw t))
(add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

;; Bibliography heading
;; If this property is set to an empty string, this heading will not be auto-inserted. default value is 'References'
(plist-put org-hugo-citations-plist :bibliography-section-heading "BIBLIOGRAPHY")

;; Citar and CSL configuration (for bibliography export)
(when (require 'citar nil t)
  (when (boundp 'config-bibfiles)
    (setq citar-bibliography config-bibfiles)
    (setq bibtex-files config-bibfiles)
    (setq org-cite-global-bibliography config-bibfiles)
    (setq citar-notes-paths (list (concat org-directory "bib/"))))

  ;; CSL styles directory
  (when (boundp 'org-directory)
    (setq org-cite-csl-styles-dir (concat org-directory ".csl"))
    (setq citar-citeproc-csl-styles-dir (concat org-directory ".csl")))

  ;; Setup export processor; default csl/citeproc-el, with biblatex for latex
  ;; after! is Doom-only; use with-eval-after-load for daemon compatibility
  (with-eval-after-load 'oc
    (require 'citar-citeproc)
    (setq bibtex-files config-bibfiles)
    (setq citar-format-reference-function 'citar-format-reference)
    (setq citar-citeproc-csl-style "apa.csl")
    ;; org-cite-csl-link-cites t = generate #citeproc_bib_item_N anchor links
    (setq org-cite-csl-link-cites t) ; important
    (setq org-cite-export-processors '((latex biblatex) (t csl))))
  )

;; Zero-width space handling
;; charset: unicode (Unicode (ISO10646)) code point in charset: 0xA0
(defun my/insert-white-space ()
  "Insert zero-width space character."
  (interactive)
  (insert " "))

;; NBSP removal disabled — my/org-fix-cjk-emphasis inserts NBSP that must
;; survive into markdown output for remark/Quartz to recognize emphasis.
;; No manually-inserted NBSP exists in org files (verified 0 occurrences).
;; (defun +org-export-remove-white-space (text _backend _info)
;;   "Remove zero width spaces from TEXT."
;;   (unless (org-export-derived-backend-p 'org)
;;     (replace-regexp-in-string " " "" text)))
;; (add-to-list 'org-export-filter-final-output-functions
;;              #'+org-export-remove-white-space t)

;; Keybinding for zero-width space (only in interactive mode)
(when (fboundp 'evil-define-key)
  (with-eval-after-load 'evil
    (evil-define-key '(insert normal) text-mode-map
      (kbd "M-m") #'my/insert-white-space)))

;;;; Section 0.5: CJK Emphasis Fix
;; org-mode emphasis markers (*bold*, =code=, ~verbatim~, /italic/, +strike+)
;; require pre/post match characters around markers. Korean (CJK) characters
;; are not in the default set, so *강조*는 fails to be recognized as bold.
;;
;; org 9.8+ uses a native C parser (org-element--set-regexps is a C subr)
;; that does NOT respect org-emphasis-regexp-components changes for export.
;; Font-lock uses org-emph-re (works), but org-element parser ignores it.
;;
;; Solution: Insert spaces between emphasis markers and CJK characters
;; in org-export-before-processing-hook. This modifies a temporary copy
;; of the buffer, NOT the original file.

;; 1. Font-lock fix (interactive editing - visual only)
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{[:multibyte:]"    ; pre-match: add multibyte
        "[:multibyte:]-[:space:].,:!?;'\")}\\[" ; post-match: add multibyte
        "[:space:]"                       ; forbidden in border
        "."                               ; body
        1))                               ; max newlines
(org-set-emph-re 'org-emphasis-regexp-components
                 org-emphasis-regexp-components)

;; 2. Export fix - insert NBSP around emphasis pairs adjacent to CJK
;;    NBSP (U+00A0) is used because:
;;    - org parser treats it as space → emphasis markers recognized
;;    - +org-export-remove-white-space removes NBSP from final output
;;    - visually invisible in rendered HTML
(defun my/org-fix-cjk-emphasis (_backend)
  "Insert NBSP around org emphasis pairs adjacent to CJK for export.
Matches emphasis pairs (*bold*, =code=, ~verb~, /italic/, +strike+)
and adds NBSP before/after the pair if Korean text is adjacent.
Emphasis content must start and end with non-whitespace (org rule).
Runs on a temporary export copy - original file is NOT modified."
  (save-excursion
    (goto-char (point-min))
    ;; Skip front matter (find first blank line)
    (when (re-search-forward "^$" nil t) (forward-line 1))
    (let ((start (point))
          (nbsp (string ?\u00A0))  ; NBSP character
          (changes 0))
      (dolist (m '("*" "=" "~" "/" "+"))
        (goto-char start)
        (let ((qm (regexp-quote m)))
          ;; Match emphasis pair: marker + non-ws content + marker
          ;; Content must start and end with non-whitespace (org emphasis rule)
          (while (re-search-forward
                  (concat qm
                          "\\([^[:space:]" qm "]"        ; first char: non-ws
                          "\\(?:[^" qm "\n]*"             ; optional middle
                          "[^[:space:]" qm "]\\)?\\)"     ; last char: non-ws
                          qm)
                  nil t)
            (let ((mb (match-beginning 0))
                  (me (match-end 0)))
              ;; After closing marker: insert NBSP if CJK follows
              (when (and (< me (point-max))
                         (let ((c (char-after me)))
                           (and c (>= c #xAC00) (<= c #xD7A3))))
                (save-excursion (goto-char me) (insert nbsp))
                (cl-incf changes))
              ;; Before opening marker: insert NBSP if CJK precedes
              (when (and (> mb (point-min))
                         (let ((c (char-before mb)))
                           (and c (>= c #xAC00) (<= c #xD7A3))))
                (save-excursion (goto-char mb) (insert nbsp))
                (cl-incf changes))))))
      (when (> changes 0)
        (message "[Export] CJK emphasis: %d NBSP insertions" changes)))

    ;; Fix markdown-style **bold** → org-style *bold* (inline only, not headings)
    ;; AI agents often write **bold** in org files out of markdown habit.
    (goto-char (point-min))
    (let ((dbl-changes 0))
      (while (re-search-forward "\\([^*]\\)\\*\\*\\([^*\n]+\\)\\*\\*" nil t)
        (replace-match "\\1*\\2*")
        (cl-incf dbl-changes))
      (when (> dbl-changes 0)
        (message "[Export] **bold** → *bold*: %d fixes" dbl-changes)))))

(add-hook 'org-export-before-processing-hook #'my/org-fix-cjk-emphasis)

(message "[Export] CJK emphasis fix applied (font-lock + export hook)")

;;;; Section 1: Org-Export Settings
;; Migrated from org-config.el

(setq org-export-headline-levels 5)               ; default 3
(setq org-export-with-toc nil)                    ; default t - Hugo handles TOC
(setq org-export-exclude-tags
      '("private" "OFFICE" "FILE" "LOG" "CREDENTIAL"
        "LOCAL" "noexport" "ignore" "crypt")) ;; "REFILED"

(setq org-publish-use-timestamps-flag t)          ; default t
(setq org-export-with-section-numbers t)          ; default t
(setq org-export-with-timestamps t)               ; default t
(setq org-export-with-todo-keywords t)            ; default t
(setq org-export-with-broken-links 'mark)         ; Mark broken links instead of error
(setq org-export-with-date t)                     ; CRITICAL: Export #+date field to frontmatter
(setq org-export-date-timestamp-format "%e %B %Y")
(setq org-export-use-babel nil)                   ; Faster export
(setq org-export-with-tags 'not-in-toc)

;;;; Section 1.5: Dblock Advice - include-date fix + folder prefix
;; Shared by both interactive Emacs (config.el → denote-export-config.el)
;; and batch/daemon export (bin/denote-export.el → denote-export-config.el)
;;
;; Fixes:
;; 1. denote-org upstream bug: include-date lambda receives nil file-type,
;;    falls back to filename instead of front-matter #+title: (denote-org#21)
;; 2. Add folder/ prefix for multi-folder dblock clarity
;;
;; Remove when upstream fixes: https://github.com/protesilaos/denote-org/issues/21

(defun my/denote-org--read-hugo-lastmod (file)
  "Read #+hugo_lastmod: from FILE front matter (first 4096 bytes).
Returns YYYY-MM-DD string or nil."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward
             "^#\\+hugo_lastmod:\\s-+\\[?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
        (match-string 1)))))

(defun my/denote-org--insert-links-override (orig-fn files &optional id-only include-date)
  "Advice around `denote-org--insert-links'.
Fixes file-type nil guard, adds folder prefix and lastmod for include-date.
Format: folder/ title 'YYYY-MM-DD #YYYY-MM-DD (citar style)"
  (if include-date
      (let ((denote-link-description-format
             (lambda (file file-type)
               (let* ((file-type (or file-type (denote-filetype-heuristics file)))
                      (title (denote-retrieve-title-or-filename file file-type))
                      (identifier (denote-retrieve-filename-identifier file))
                      (date (denote-id-to-date identifier))
                      (lastmod (my/denote-org--read-hugo-lastmod file))
                      (parent-dir (file-name-nondirectory
                                   (directory-file-name
                                    (file-name-directory file)))))
                 (if lastmod
                     (format "%s/ %s '%s #%s" parent-dir title date lastmod)
                   (format "%s/ %s '%s" parent-dir title date))))))
        (denote-link--insert-links files 'org id-only :no-other-sorting))
    (funcall orig-fn files id-only include-date)))

(with-eval-after-load 'denote-org
  (advice-add 'denote-org--insert-links :around #'my/denote-org--insert-links-override))

;;;; Section 1.6: Dblock denote-lastmod - lastmod 기준 필터링

;; #+hugo_lastmod: 날짜를 기준으로 파일을 필터링하는 dblock.
;; 새 노트(identifier)가 아닌 수정된 노트를 보여준다.
;;
;; 사용법:
;;   #+BEGIN: denote-lastmod :from "2026-03-09" :to "2026-03-15" :excluded-dirs-regexp "journal"
;;   #+END:
;;
;; :from  - 시작일 (YYYY-MM-DD). 생략 시 7일 전
;; :to    - 종료일 (YYYY-MM-DD). 생략 시 오늘
;; :excluded-dirs-regexp - 제외 디렉토리 (denote 표준)
;; :id-only - non-nil이면 id만 출력
;;
;; include-date는 항상 t (lastmod 보는 것이 목적이므로)

(defun my/denote-lastmod--read-date (file)
  "Read #+hugo_lastmod: date from FILE. Returns YYYY-MM-DD or nil.
Supports both [YYYY-MM-DD ...] and Time-stamp: <YYYY-MM-DD ...> formats."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward
             "^#\\+hugo_lastmod:\\s-+\\(?:Time-stamp:\\s-+\\)?[<\\[]?\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
             nil t)
        (match-string 1)))))

(defun my/denote-lastmod--filter-files (files from to)
  "Filter FILES by #+hugo_lastmod: date between FROM and TO (YYYY-MM-DD strings)."
  (seq-filter
   (lambda (file)
     (when-let* ((lastmod (my/denote-lastmod--read-date file)))
       (and (not (string< lastmod from))
            (not (string< to lastmod)))))
   files))

;;;###autoload
(defun org-dblock-write:denote-lastmod (params)
  "Dynamic block: list Denote files modified (by #+hugo_lastmod:) in date range.
PARAMS: :from :to :excluded-dirs-regexp :id-only"
  (let* ((from (or (plist-get params :from)
                   (format-time-string "%Y-%m-%d"
                                       (time-subtract (current-time)
                                                      (days-to-time 7)))))
         (to (or (plist-get params :to)
                 (format-time-string "%Y-%m-%d")))
         (denote-excluded-directories-regexp
          (or (plist-get params :excluded-dirs-regexp)
              denote-excluded-directories-regexp))
         (all-files (denote-directory-files))
         (filtered (my/denote-lastmod--filter-files all-files from to))
         ;; lastmod 역순 정렬 (최근 수정 먼저)
         (sorted (seq-sort
                  (lambda (a b)
                    (string> (or (my/denote-lastmod--read-date a) "")
                             (or (my/denote-lastmod--read-date b) "")))
                  filtered)))
    (if (null sorted)
        (insert (format "No files modified between %s and %s." from to))
      ;; 기존 advice가 적용된 denote-org--insert-links 사용
      (denote-org--insert-links sorted (plist-get params :id-only) :include-date))
    (join-line))) ; remove trailing empty line

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
Checks for /notes, /journal, /talks, /meta, /bib, /botlog directories."
  (let ((directories '("/test" "/notes" "/journal" "/talks" "/meta" "/bib" "/botlog"))
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
If USE-RELREF is non-nil, format it as a Hugo relref link."
  (let* (
         ;; (path (denote-get-path-by-id link)) ; only i
         ;; (pathr (file-relative-name (nth 0 path-id)))
         ;; (final-filename (concat (denote-retrieve-filename-identifier path) ".md"))
         (path-id (denote-link--ol-resolve-link-to-target link :full-data))
         (path (nth 0 path-id))
         (section (my/get-hugo-section-directory-from-path path))
         (id (nth 1 path-id))
         (query (nth 2 path-id))
         ;; (exportfilename (my/get-export-file-name-from-file path))
         (exportfilename (format "%s.md" id))
         (content-dir (concat (file-name-as-directory org-hugo-base-dir)
                              (format "content/%s/" section)))
         (exportfilepath (when (and exportfilename org-hugo-base-dir)
                           (expand-file-name exportfilename content-dir)))
         (uri (cond
               (query (format "%s/%s" exportfilename query)) ; custom header
               (t (format "%s" exportfilename)))))

    (format "[%s]" section)
    ;; (message "%s" (format "[%s]({{< relref \"%s/%s\" >}})" desc section uri))

    ;; 내보낸 파일이 없는 경우 링크 만들지 않도록
    ;; (if  (not (file-exists-p exportfilepath))
    ;;     (format "[%s]" desc) ;; 내보낸 파일이 없다면 링크 만들지 마라

    (cond
     ;; 1) For files in Digital Garden for ALL
     ;; ((and (string-match "blog" org-hugo-base-dir) (or (my/is-docs-file path) (my/is-blog-file path))) ;;
     ((string-match "notes" org-hugo-base-dir) ;; for quartz
      (if (or (my/is-draft-file path) (my/is-md-file path)) ; (my/is-llm-file path) (my/is-docs-file path) (my/is-blog-file path)
          (format "[%s]" desc)
        (if exportfilename
            ;; quartz doesn't support custom header
            (format "[%s]({{< relref \"%s/%s\" >}})" desc section exportfilename) ; uri
          (format "[%s]" desc))))

     ;; 2) For files in other directories based on org-hugo-base-dir
     ;; ((and (string-match "notes" org-hugo-base-dir))
     ;; ((string-match "notes" org-hugo-base-dir)
     ;;  (if (my/is-draft-file path)
     ;;      (format "[%s]" desc)
     ;;    (if (or (my/is-docs-file path) (my/is-blog-file path))
     ;;        (format "[%s]" desc)
     ;;      (if exportfilename
     ;;          (format "[%s]({{< relref \"%s/%s\" >}})" desc section uri)
     ;;        (format "[%s]" desc)))
     ;;    ))
     ;; Fallback
     (t (format "[%s]" desc)))
    ;; ) ; end-of if
    ))

(defun my/denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend.

Handles broken links (when target file doesn't exist) by returning
plain text, consistent with org-export-with-broken-links 'mark setting."
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :full-data))
         (path (when path-id (nth 0 path-id)))
         (id (when path-id (nth 1 path-id)))
         (query (when path-id (nth 2 path-id))))

    ;; Handle broken links - return plain text
    (if (not path)
        (let ((desc (or description (concat "denote:" link))))
          (format "[%s]" desc))  ; Just return description in brackets

      ;; Normal link processing
      (let* ((path-rel (file-relative-name path))
             (anchor (file-name-sans-extension path-rel))
             (desc (cond
                    (description)
                    (query (format "denote:%s::%s" id query))
                    (t (concat "denote:" id)))))
        (cond
         ((eq format 'html)
          (if query
              (format "<a href=\"%s.html%s\">%s</a>" anchor query desc)
            (format "<a href=\"%s.html\">%s</a>" anchor desc)))
         ((eq format 'latex) (format "\\href{%s}{%s}" (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" path-rel) desc))
         ((eq format 'texinfo) (format "@uref{%s,%s}" path-rel desc))
         ((eq format 'ascii) (format "[%s] <denote:%s>" desc path-rel))
         ((eq format 'md)  (my/denote-markdown-export link desc))
         ;; ((eq format 'md) (format "[%s](%s)" desc path))
         (t path-rel))))))

;; Register denote link export handler
(org-link-set-parameters "denote" :export #'my/denote-link-ol-export)

;;;; Section 2.3: download: → file: link rewrite (before-processing hook)
;; Doom Emacs defines "download:" link type (org-download +dragndrop contrib)
;; for images saved via org-download. The link stores a relative filename
;; resolved against `org-download-image-dir'.
;;
;; Problem: "download:" has no :export handler → ox-hugo outputs raw text.
;; Solution: rewrite [[download:file.png]] → [[file:~/screenshot/file.png]]
;; in the export temp buffer BEFORE parsing, so ox-hugo's full pipeline
;; handles it (copy to static/, path rewrite, figure shortcode).
;;
;; Same pattern as my/org-fix-cjk-emphasis — runs on temp copy, not original.
;;
;; Emacs 31 transition note:
;; - 31 builtin yank-media produces [[file:relative/path.png]]
;; - "download:" is Doom-only, will eventually retire
;; - This hook ensures existing download: links export correctly
;;   until all download: links are migrated to file: links.

(defun my/org-rewrite-download-links (_backend)
  "Rewrite [[download:file.png]] to [[file:FULL-PATH]] before export.
Resolves the filename against `org-download-image-dir', with fallback
to `org-attach-id-dir' (for files saved before the setq-default fix).
Runs on a temporary export copy — original file is NOT modified."
  (let ((image-dir (or (and (boundp 'org-download-image-dir)
                            ;; Try default-value first, then current value
                            (or (default-value 'org-download-image-dir)
                                org-download-image-dir))
                       "."))
        (attach-dir (and (boundp 'org-attach-id-dir) org-attach-id-dir))
        (changes 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\[\\[download:\\([^]]+\\)\\]\\(\\[\\([^]]*\\)\\]\\)?\\]"
              nil t)
        (let* ((filename (match-string 1))
               (desc-part (match-string 2))  ; includes brackets or nil
               (path-in-image-dir (expand-file-name filename image-dir))
               (path-in-attach-dir (and attach-dir
                                        (expand-file-name filename attach-dir)))
               ;; Try image-dir first, then attach-dir (legacy fallback)
               (full-path (cond
                           ((file-exists-p path-in-image-dir) path-in-image-dir)
                           ((and path-in-attach-dir
                                 (file-exists-p path-in-attach-dir))
                            path-in-attach-dir)
                           (t path-in-image-dir))) ; default even if missing
               (replacement
                (if desc-part
                    (format "[[file:%s]%s]" full-path desc-part)
                  (format "[[file:%s]]" full-path))))
          (replace-match replacement t t)
          (cl-incf changes))))
    (when (> changes 0)
      (message "[Export] download: → file: rewrites: %d" changes))))

(add-hook 'org-export-before-processing-hook #'my/org-rewrite-download-links)

;;;; Section 2.5: Hashtag and Mention Span Wrapping for Hugo Export
;; Wrap #hashtags and @mentions with <span> tags during ox-hugo export.
;; Counterpart of doom-themes-ext-org.el fontification (interactive editing).
;;
;; Uses org-export-filter-plain-text-functions which automatically excludes
;; content inside links, verbatim, code, and src blocks.
;;
;; See: [[denote:20260322T103300][ox-hugo 해시태그/멘션 span 래핑 작업 지침]]

(defvar my/org-hugo-hashtag-class "org-hashtag"
  "CSS class for hashtag <span> tags in Hugo export.")

(defvar my/org-hugo-mention-class "org-mention"
  "CSS class for mention <span> tags in Hugo export.")

(defvar my/org-hugo-mention-names '("junghan")
  "List of @mention names to wrap with <span>.
Word boundary applied — @junghan matches but @junghan0611 does not.
@user and @assistant are excluded (Quartz oxhugofm.ts handles them).")

(defun my/org-hugo-wrap-hashtags-and-mentions (text backend info)
  "Wrap #hashtags and @mentions with <span> tags for Hugo export.

Runs as `org-export-filter-plain-text-functions', which automatically
excludes content inside links, verbatim, code, and src blocks.

Hashtag rules:
- Pattern: #TAG where TAG starts with a letter (한글 or A-Za-z)
- Pure numbers like #1, #123 are excluded
- Preceded by whitespace, start of text, or opening punctuation ((, \", ')

Mention rules:
- Only names in `my/org-hugo-mention-names' (word boundary applied)
- @user, @assistant excluded (Quartz oxhugofm.ts handles them)

Note: org-element parser splits text at `_' boundaries into separate
plain-text nodes, so #인식의_그물망 arrives as two nodes: #인식의 and _그물망.
The re-stitching is handled by `my/org-hugo-restitch-broken-hashtag-spans'
in `org-export-filter-final-output-functions'.

Fontification counterpart: doom-themes-ext-org.el
  regex: \\(?:\\s-\\|^\\)\\(\\([#@]\\)[가-힣A-Za-z0-9_.-]+\\)"
  (when (org-export-derived-backend-p backend 'hugo)
    ;; 1. Wrap #hashtags — require first char to be a letter (not digit)
    ;;    Pre-match includes opening punctuation for (#태그) patterns
    (setq text
          (replace-regexp-in-string
           "\\(^\\|[[:space:](\"']\\)\\(#[가-힣A-Za-z][가-힣A-Za-z0-9_.-]*\\)"
           (format "\\1<span class=\"%s\">\\2</span>" my/org-hugo-hashtag-class)
           text))
    ;; 2. Wrap @mentions — word boundary prevents partial match
    (dolist (name my/org-hugo-mention-names)
      (setq text
            (replace-regexp-in-string
             (format "\\(^\\|[[:space:](\"']\\)\\(@%s\\>\\)" (regexp-quote name))
             (format "\\1<span class=\"%s\">\\2</span>" my/org-hugo-mention-class)
             text)))
    text))

(add-to-list 'org-export-filter-plain-text-functions
             #'my/org-hugo-wrap-hashtags-and-mentions)

(defun my/org-hugo-strip-hashtag-spans-in-links (data backend info)
  "Strip org-hashtag/org-mention span tags from inside link text.
The plain-text filter wraps all #hashtags and @mentions, including
those inside link descriptions.  Links already convey semantic meaning;
inline span tags inside [link text](url) break the intent.

This runs as `org-export-filter-link-functions' — after the link
element is fully transcoded (description already filtered)."
  (when (org-export-derived-backend-p backend 'hugo)
    (replace-regexp-in-string
     "<span class=\"org-\\(?:hashtag\\|mention\\)\">\\([^<]*\\)</span>"
     "\\1"
     data)))

(add-to-list 'org-export-filter-link-functions
             #'my/org-hugo-strip-hashtag-spans-in-links)

(defun my/org-hugo-restitch-broken-hashtag-spans (text backend info)
  "Re-stitch hashtag spans broken by org parser at underscore boundaries.

org-element parser splits `X_Y' into separate plain-text nodes at `_'.
The plain-text filter only sees the first fragment, producing:
  <span class=\"org-hashtag\">#인식의</span>_그물망

This final-output filter detects the pattern and extends the span:
  <span class=\"org-hashtag\">#인식의_그물망</span>

Runs in `org-export-filter-final-output-functions' (after all
transcoders and element filters have completed)."
  (when (org-export-derived-backend-p backend 'hugo)
    ;; Repeatedly extend — handles multiple underscores: #a_b_c
    (let ((re (format "<span class=\"%s\">\\([^<]+\\)</span>\\(_[가-힣A-Za-z0-9_.-]*\\)"
                      my/org-hugo-hashtag-class)))
      (while (string-match re text)
        (setq text (replace-match
                    (format "<span class=\"%s\">%s%s</span>"
                            my/org-hugo-hashtag-class
                            (match-string 1 text)
                            (match-string 2 text))
                    t nil text))))
    text))

(add-to-list 'org-export-filter-final-output-functions
             #'my/org-hugo-restitch-broken-hashtag-spans)

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

(defvar my/org-hugo-export-exclude-subdirs '("agenda")
  "List of subdirectory names to exclude from Hugo export.")

(defun my/org-hugo-export-directory (directory)
  "Export all Org files in DIRECTORY to Markdown using `org-hugo-export-to-md'.
Sequential processing - for parallel export, use external script.
Subdirectories listed in `my/org-hugo-export-exclude-subdirs' are skipped."
  (interactive "DSelect directory: ")
  (let ((org-files (directory-files-recursively directory "\\.org\\'")))
    (dolist (org-file org-files)
      (unless (cl-some (lambda (subdir)
                         (string-match-p (format "/%s/" subdir) org-file))
                       my/org-hugo-export-exclude-subdirs)
        (message "Exporting: %s" org-file)
        (with-current-buffer (find-file-noselect org-file)
          (org-hugo-export-to-md)))))

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
  '("~/org/meta" "~/org/bib" "~/org/notes" "~/org/botlog")
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

;;;; pass CLI integration
;; Future enhancement: Integrate password-store for secret validation
;; (defun my/validate-secrets-before-export () ...)

(provide 'denote-export-config)

;;; +denote-export-config.el ends here
