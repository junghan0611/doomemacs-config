;;; bin/fix-org-links.el --- Stage 1 Org link hygiene fixer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Stage 1 of garden hygiene: rewrite broken/leaky link patterns in
;; ~/org/**/*.org originals, so denotecli / semantic-memory / Emacs GUI
;; / future export pipelines all see clean targets.
;;
;; Policy source: bin/site-policy.el (SSOT).
;;
;; Rewrites:
;;   [[file:~/repos/gh/REPO]]            → [[https://github.com/USER/REPO]]
;;   [[file:~/repos/gh/REPO/path/x.el]]  → [[https://github.com/USER/REPO/blob/main/path/x.el]]
;;   host-alias targets (geworfen.junghanacs.com → agent.junghanacs.com, ...)
;;
;; Skips (never modified):
;;   - [[denote:UUID]] links — denote system invariant
;;   - [[file:~/screenshot/...]] / [[file:~/org/.attach/...]] — figure pipeline
;;   - [[file:~/org/...]] — local org cross-refs
;;   - code / verbatim / src-block contents (org-element-based, not regex)
;;
;; Usage:
;;   emacs --batch -Q --load bin/fix-org-links.el -- [~/org]            # dry-run
;;   emacs --batch -Q --load bin/fix-org-links.el -- [~/org] --apply    # write
;;
;; Or via run.sh:
;;   ./run.sh fix-org
;;   ./run.sh fix-org --apply

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

;; Load policy from same directory as this file
(let ((dir (file-name-directory (or load-file-name buffer-file-name
                                    (expand-file-name "bin/" default-directory)))))
  (load (expand-file-name "site-policy.el" dir) nil 'nomessage))

;;;; Helpers

(defun fix-org-links--should-skip-file-path (path)
  "Return non-nil if file: PATH must not be rewritten.
Protects denote/attach/screenshot/local-org link targets."
  (or (string-prefix-p "~/screenshot/" path)
      (string-prefix-p "~/org/.attach/" path)
      ;; ~/org/anything that isn't a repo path (denote files, etc.)
      (and (string-prefix-p "~/org/" path)
           (not (string-match-p "^~/org/repos/" path)))))

(defun fix-org-links--rewrite-repo-path (path &optional search-option)
  "If PATH is `~/repos/gh/REPO[/...]`, return GitHub URL. Else nil.
SEARCH-OPTION is the `::` part as parsed by org-element. Preserves
numeric line number as GitHub `#LN`. Non-numeric search options
(e.g. `%%(diary-sexp)`, `*Heading`) are dropped — the file URL still
works but anchor semantics are not portable to GitHub."
  (when (string-match "^~/repos/gh/\\([^/]+\\)\\(/.*\\)?$" path)
    (let* ((user (my/site-policy-get 'github-user))
           (branch (my/site-policy-get 'github-branch))
           (repo (match-string 1 path))
           (rest (match-string 2 path))
           (line-num (and search-option
                          (string-match-p "^[0-9]+$" search-option)
                          search-option))
           (url (if (and rest (not (string= rest "")) (not (string= rest "/")))
                    (format "https://github.com/%s/%s/blob/%s%s" user repo branch rest)
                  (format "https://github.com/%s/%s" user repo))))
      (if line-num
          (concat url "#L" line-num)
        url))))

(defun fix-org-links--rewrite-host-alias (target)
  "If TARGET contains a known old host, return alias-applied string. Else nil."
  (let ((aliases (my/site-policy-get 'host-aliases))
        (changed nil)
        (out target))
    (dolist (pair aliases)
      (let ((old (car pair))
            (new (cdr pair)))
        (when (string-match-p (regexp-quote old) out)
          (setq out (replace-regexp-in-string (regexp-quote old) new out t t))
          (setq changed t))))
    (and changed out)))

(defun fix-org-links--rewrite-link (type path &optional search-option)
  "Return (NEW-TYPE . NEW-PATH) for rewritten link, or nil.
TYPE is org-element link type (\"file\", \"http\", \"https\", ...).
PATH is the link path (no type prefix).
SEARCH-OPTION is the `::ANCHOR` part (file links only)."
  (cond
   ;; file: ~/repos/gh/REPO[...] → github URL
   ((and (string= type "file")
         (not (fix-org-links--should-skip-file-path path)))
    (when-let ((new (fix-org-links--rewrite-repo-path path search-option)))
      ;; new starts with https://, split into type + path
      (when (string-match "^\\(https?\\)://\\(.+\\)$" new)
        (cons (match-string 1 new) (concat "//" (match-string 2 new))))))
   ;; http(s): host alias
   ((or (string= type "http") (string= type "https"))
    (let* ((full (concat type ":" path))
           (new (fix-org-links--rewrite-host-alias full)))
      (when new
        (when (string-match "^\\(https?\\):\\(.+\\)$" new)
          (cons (match-string 1 new) (match-string 2 new))))))
   (t nil)))

(defun fix-org-links--format-link (new-type new-path desc)
  "Render an org link from NEW-TYPE/NEW-PATH and optional DESC."
  (let ((target (concat new-type ":" new-path)))
    (if (and desc (not (string-empty-p desc)))
        (format "[[%s][%s]]" target desc)
      (format "[[%s]]" target))))

;;;; Per-file processing

(defun fix-org-links--process-file (file apply-p)
  "Scan FILE for rewritable links. If APPLY-P, write changes back.
Returns a list of (OLD . NEW) replacement pairs."
  (let ((changes nil))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((default-directory (file-name-directory file)))
        (delay-mode-hooks (org-mode)))
      (let ((tree (org-element-parse-buffer)))
        (org-element-map tree 'link
          (lambda (link)
            (let* ((type (org-element-property :type link))
                   (path (org-element-property :path link))
                   (search-option (org-element-property :search-option link))
                   (begin (org-element-property :begin link))
                   (end (org-element-property :end link))
                   (contents-begin (org-element-property :contents-begin link))
                   (contents-end (org-element-property :contents-end link))
                   (desc (and contents-begin contents-end
                              (buffer-substring-no-properties
                               contents-begin contents-end))))
              ;; Skip denote: always — invariant
              (unless (string= type "denote")
                (when-let ((new-pair (fix-org-links--rewrite-link
                                      type path search-option)))
                  ;; Trim trailing whitespace from `end` (org-element sometimes
                  ;; extends end into trailing space)
                  (let* ((raw-end (save-excursion
                                    (goto-char end)
                                    (skip-chars-backward " \t\n")
                                    (point)))
                         (old (buffer-substring-no-properties begin raw-end))
                         (new (fix-org-links--format-link
                               (car new-pair) (cdr new-pair) desc))
                         ;; Check local existence for file: links pointing at
                         ;; ~/repos/gh — flag if absent so user can review.
                         ;; Stage 1b (verify-org-links) will validate remote.
                         (local-ok (or (not (string= type "file"))
                                       (file-exists-p (expand-file-name path)))))
                    (push (list begin raw-end old new local-ok) changes)))))))
        ;; Apply in reverse so positions remain valid
        (when (and apply-p changes)
          (dolist (ch (sort (copy-sequence changes)
                            (lambda (a b) (> (nth 0 a) (nth 0 b)))))
            (delete-region (nth 0 ch) (nth 1 ch))
            (goto-char (nth 0 ch))
            (insert (nth 3 ch)))
          (write-region (point-min) (point-max) file nil 'silent))))
    (mapcar (lambda (ch) (list (nth 2 ch) (nth 3 ch) (nth 4 ch)))
            (nreverse changes))))

;;;; Main entry

(defun fix-org-links--main ()
  "Run scan from command-line args."
  (let* ((args (cdr (member "--" command-line-args)))
         (apply-p (and (member "--apply" args) t))
         (positional (cl-remove-if (lambda (a) (string-prefix-p "--" a)) args))
         (target (or (car positional) (expand-file-name "~/org")))
         (target (expand-file-name target))
         (files (cond
                 ((file-directory-p target)
                  (directory-files-recursively target "\\.org\\'"))
                 ((file-regular-p target)
                  (list target))
                 (t (error "Target not found: %s" target))))
         (touched-files 0)
         (total-changes 0))
    (princ (format "📂 Target: %s\n" target))
    (princ (format "📄 Files:  %d\n" (length files)))
    (princ (format "🔧 Mode:   %s\n\n" (if apply-p "APPLY" "dry-run")))
    (let ((miss-total 0))
      (dolist (file files)
        (condition-case err
            (let* ((changes (fix-org-links--process-file file apply-p))
                   (miss (cl-count-if-not (lambda (ch) (nth 2 ch)) changes)))
              (when changes
                (setq touched-files (1+ touched-files))
                (setq total-changes (+ total-changes (length changes)))
                (setq miss-total (+ miss-total miss))
                (princ (format "%s %s  (%d%s)\n"
                               (if apply-p "✓" "📄")
                               (if (file-directory-p target)
                                   (file-relative-name file target)
                                 (file-name-nondirectory file))
                               (length changes)
                               (if (> miss 0) (format " %d⚠" miss) "")))
                (unless apply-p
                  (dolist (ch changes)
                    (let ((tag (if (nth 2 ch) "  " "⚠ ")))
                      (princ (format "    %s- %s\n    %s+ %s\n"
                                     tag (nth 0 ch) tag (nth 1 ch))))))))
          (error
           (princ (format "⚠ %s — %s\n" file (error-message-string err))))))
      (princ (format "\n%s %d files, %d changes"
                     (if apply-p "✅ Applied:" "🔍 Dry-run:")
                     touched-files total-changes))
      (when (> miss-total 0)
        (princ (format ", ⚠ %d 로컬 파일 부재 (검토 권장)" miss-total)))
      (princ "\n")
      (unless apply-p
        (princ "   ⚠ 표시 = 로컬에 그 파일이 없음. 변환은 가능하나 GitHub URL도 깨졌을 수 있음.\n")
        (princ "   실제 수정: ./run.sh fix-org --apply\n")
        (princ "   후처리 검증 (Stage 1b, 계획): ./run.sh verify-org-links\n")))))

(fix-org-links--main)

;;; fix-org-links.el ends here
