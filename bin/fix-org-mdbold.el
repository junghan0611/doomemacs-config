;;; bin/fix-org-mdbold.el --- Org markdown **bold** → *bold* hygiene -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Fix AI markdown habit in org sources: **bold** → *bold*.
;; Companion to interactive `my/fix-markdown-bold-to-org' (lisp/korean-input-config.el)
;; and the export-temp hook in denote-export-config.el — this one rewrites
;; ~/org originals so the corpus stays clean between exports.
;;
;; Scope (garden publish surface only — DEFAULT):
;;   notes/  bib/  meta/  botlog/
;; NEVER scanned by default: journal/ (GLG exports manually; do not touch).
;;
;; Safety:
;;   - dry-run is the default; --apply writes
;;   - org-element protected regions are skipped:
;;       src/example/export/verse/comment blocks, drawers,
;;       inline-src, code, verbatim, fixed-width
;;   - org headings (`** heading' with space after stars) are not matched
;;     (patterns require a same-line closing **)
;;
;; Usage:
;;   emacs --batch -Q --load bin/fix-org-mdbold.el -- [~/org]           # dry-run
;;   emacs --batch -Q --load bin/fix-org-mdbold.el -- [~/org] --apply   # write
;;   emacs --batch -Q --load bin/fix-org-mdbold.el -- FILE.org          # one file
;;
;; Or via run.sh:
;;   ./run.sh fix-bold
;;   ./run.sh fix-bold --apply

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

;;;; Constants

(defconst fix-org-mdbold--garden-subdirs
  '("notes" "bib" "meta" "botlog")
  "Subdirectories of ~/org that are garden publish surfaces.
journal/ is intentionally absent.")

(defconst fix-org-mdbold--protected-types
  '(src-block example-block export-block verse-block comment-block
    drawer property-drawer
    inline-src-block code verbatim fixed-width)
  "org-element types whose interior must not be rewritten.")

;; Same patterns as `my/fix-markdown-bold-to-org'.
;; 1) mid-line: non-* then **text**
;; 2) bol: **text** with closing ** on the same line
(defconst fix-org-mdbold--re-mid
  "\\([^*]\\)\\*\\*\\([^*\n]+\\)\\*\\*")

(defconst fix-org-mdbold--re-bol
  "^\\*\\*\\([^*\n]+\\)\\*\\*")

;;;; Helpers

(defun fix-org-mdbold--protected-ranges ()
  "Return list of (BEGIN . END) buffer positions that must not be touched."
  (let (ranges)
    (org-element-map (org-element-parse-buffer) fix-org-mdbold--protected-types
      (lambda (el)
        (let ((b (org-element-property :begin el))
              (e (org-element-property :end el)))
          (when (and b e)
            (push (cons b e) ranges)))))
    ranges))

(defun fix-org-mdbold--pos-protected-p (pos ranges)
  "Non-nil if POS lies inside any (BEGIN . END) in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r)))) ranges))

(defun fix-org-mdbold--overlaps-p (beg end hits)
  "Non-nil if [BEG, END) overlaps any hit in HITS."
  (cl-some (lambda (h)
             (let ((hb (nth 0 h))
                   (he (nth 1 h)))
               (and (< beg he) (< hb end))))
           hits))

(defun fix-org-mdbold--collect-matches (ranges)
  "Return list of (BEGIN END OLD NEW) for safe **bold** hits in current buffer.

Mid-line pattern can consume a leading newline before a bol `**text**`, so the
bol pass would otherwise double-hit the same span.  Prefer the tighter bol
replacement when both match: run bol first, then mid while skipping overlaps."
  (let (hits)
    ;; 1) bol first — cleaner replacement (*text*, no leading char to keep)
    (goto-char (point-min))
    (while (re-search-forward fix-org-mdbold--re-bol nil t)
      (let ((b (match-beginning 0))
            (e (match-end 0)))
        (unless (or (fix-org-mdbold--pos-protected-p b ranges)
                    (fix-org-mdbold--overlaps-p b e hits))
          (push (list b e
                      (match-string-no-properties 0)
                      (concat "*" (match-string-no-properties 1) "*"))
                hits))))
    ;; 2) mid-line — skip anything already claimed
    (goto-char (point-min))
    (while (re-search-forward fix-org-mdbold--re-mid nil t)
      (let ((b (match-beginning 0))
            (e (match-end 0)))
        (unless (or (fix-org-mdbold--pos-protected-p b ranges)
                    (fix-org-mdbold--overlaps-p b e hits))
          (push (list b e
                      (match-string-no-properties 0)
                      (concat (match-string-no-properties 1)
                              "*" (match-string-no-properties 2) "*"))
                hits))))
    ;; Sort by buffer position for stable dry-run output
    (sort hits (lambda (a b) (< (nth 0 a) (nth 0 b))))))

(defun fix-org-mdbold--process-file (file apply-p)
  "Scan FILE. If APPLY-P, write safe replacements.
Return list of (OLD . NEW) pairs."
  (let (pairs)
    (with-temp-buffer
      (insert-file-contents file)
      ;; Fast path: no ** at all
      (goto-char (point-min))
      (if (not (search-forward "**" nil t))
          nil
        (let ((default-directory (file-name-directory file)))
          (delay-mode-hooks (org-mode)))
        (let* ((ranges (fix-org-mdbold--protected-ranges))
               (hits (fix-org-mdbold--collect-matches ranges)))
          (setq pairs (mapcar (lambda (h) (cons (nth 2 h) (nth 3 h))) hits))
          (when (and apply-p hits)
            (dolist (h (sort (copy-sequence hits)
                             (lambda (a b) (> (nth 0 a) (nth 0 b)))))
              (delete-region (nth 0 h) (nth 1 h))
              (goto-char (nth 0 h))
              (insert (nth 3 h)))
            (write-region (point-min) (point-max) file nil 'silent)))))
    pairs))

(defun fix-org-mdbold--collect-files (target)
  "Return org files under TARGET, restricted to garden subdirs when a dir."
  (setq target (expand-file-name target))
  (cond
   ((file-regular-p target)
    (list target))
   ((file-directory-p target)
    (let (files)
      (dolist (sub fix-org-mdbold--garden-subdirs)
        (let ((dir (expand-file-name sub target)))
          (when (file-directory-p dir)
            (setq files
                  (nconc files
                         (directory-files-recursively dir "\\.org\\'"))))))
      files))
   (t (error "Target not found: %s" target))))

;;;; Main

(defun fix-org-mdbold--main ()
  "CLI entry: dry-run by default, --apply to write."
  (let* ((args (cdr (member "--" command-line-args)))
         (apply-p (and (member "--apply" args) t))
         (positional (cl-remove-if (lambda (a) (string-prefix-p "--" a)) args))
         (target (or (car positional) (expand-file-name "~/org")))
         (files (fix-org-mdbold--collect-files target))
         (touched 0)
         (total 0)
         (skipped-note 0))
    (princ (format "📂 Target:  %s\n" (expand-file-name target)))
    (princ (format "📁 Scope:   %s (journal 제외)\n"
                   (mapconcat #'identity fix-org-mdbold--garden-subdirs " ")))
    (princ (format "📄 Files:   %d\n" (length files)))
    (princ (format "🔧 Mode:    %s\n" (if apply-p "APPLY" "dry-run")))
    (princ "🛡  Guard:   src/example/export/verse/comment/drawer/code/verbatim\n\n")
    (dolist (file files)
      (condition-case err
          (let ((pairs (fix-org-mdbold--process-file file apply-p)))
            (when pairs
              (cl-incf touched)
              (cl-incf total (length pairs))
              (princ (format "%s %s  (%d)\n"
                             (if apply-p "✓" "📄")
                             (if (file-directory-p (expand-file-name target))
                                 (file-relative-name file (expand-file-name target))
                               (file-name-nondirectory file))
                             (length pairs)))
              (unless apply-p
                (dolist (p pairs)
                  (princ (format "    - %s\n    + %s\n"
                                 (car p) (cdr p)))))))
        (error
         (cl-incf skipped-note)
         (princ (format "⚠ %s — %s\n" file (error-message-string err))))))
    (princ (format "\n%s %d files, %d changes"
                   (if apply-p "✅ Applied:" "🔍 Dry-run:")
                   touched total))
    (when (> skipped-note 0)
      (princ (format ", ⚠ %d errors" skipped-note)))
    (princ "\n")
    (unless apply-p
      (princ "   보호 영역(src/drawer/code 등)은 건너뜀. journal/ 은 스코프 밖.\n")
      (princ "   실제 수정: ./run.sh fix-bold --apply\n"))))

(fix-org-mdbold--main)

;;; fix-org-mdbold.el ends here
