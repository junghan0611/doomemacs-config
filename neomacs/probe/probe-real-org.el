;;; neomacs/probe/probe-real-org.el --- Real corpus smoke probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; The other probes use synthetic fixtures.  This one opens the actual
;; corpus in `org-directory' — real Denote notes with real Korean prose,
;; tables, source blocks, links, and multi-thousand-line files.
;;
;; Read-only: nothing here writes to ~/org.  Buffers are visited and
;; parsed, exports go to a temp directory.
;;
;; This is the probe that answers "can I actually work in it", which the
;; fixture probes cannot.  A pass here still leaves GUI behavior — font
;; shaping, live composition, cursor motion — for a human to drive.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'probe-lib)
(require 'org)
(require 'org-element)

(defvar my/probe-corpus-dir (expand-file-name "notes/" org-directory)
  "Directory of real notes to sample.")

(defun my/probe--corpus-files (&optional n)
  "Return up to N real org files, largest first (the hard cases first)."
  (let ((files (sort (directory-files my/probe-corpus-dir t "\\.org\\'")
                     (lambda (a b) (> (file-attribute-size (file-attributes a))
                                      (file-attribute-size (file-attributes b)))))))
    (if n (seq-take files n) files)))

(my/probe-banner "corpus")

(my/probe-check "org-directory exists"
  (if (file-directory-p my/probe-corpus-dir)
      (format "%s (%d org files)" my/probe-corpus-dir
              (length (my/probe--corpus-files)))
    (cons 'skip (format "%s not found" my/probe-corpus-dir))))

(my/probe-banner "visiting real notes")

(my/probe-check "visit the 10 largest notes"
  (let ((total 0) (lines 0))
    (dolist (f (my/probe--corpus-files 10))
      (with-current-buffer (find-file-noselect f)
        (setq total (1+ total)
              lines (+ lines (count-lines (point-min) (point-max))))
        (unless (derived-mode-p 'org-mode)
          (error "%s did not enter org-mode" (file-name-nondirectory f)))
        (kill-buffer)))
    (format "%d files, %d lines total" total lines)))

(my/probe-check "org-element parse the 5 largest notes"
  ;; The full parser is the widest single exercise of the runtime:
  ;; regexps, text properties, the reader, and a lot of allocation.
  (let ((elements 0))
    (dolist (f (my/probe--corpus-files 5))
      (with-current-buffer (find-file-noselect f)
        (let ((tree (org-element-parse-buffer)))
          (org-element-map tree t (lambda (_) (setq elements (1+ elements)))))
        (kill-buffer)))
    (format "%d elements parsed" elements)))

(my/probe-check "collect headings across 30 notes"
  (let ((headings 0))
    (dolist (f (my/probe--corpus-files 30))
      (with-current-buffer (find-file-noselect f)
        (org-map-entries (lambda () (setq headings (1+ headings))))
        (kill-buffer)))
    (format "%d headings" headings)))

(my/probe-check "Denote filetags parse as org tags"
  ;; workflow-shared.el narrows org-tag-re for Denote; if the runtime's
  ;; regexp engine diverges, tags silently stop matching.
  (let ((tagged 0) (samples nil))
    (dolist (f (my/probe--corpus-files 20))
      (with-current-buffer (find-file-noselect f)
        (let ((tags (cdr (assoc "FILETAGS"
                                (org-collect-keywords '("FILETAGS"))))))
          (when tags
            (setq tagged (1+ tagged))
            (when (< (length samples) 3)
              (push (string-trim (car tags)) samples))))
        (kill-buffer)))
    (format "%d/20 notes have FILETAGS, e.g. %s"
            tagged (string-join (nreverse samples) " "))))

(my/probe-banner "editing a real note (in memory, never saved)")

(my/probe-check "insert Korean text into a real note buffer"
  (let ((f (car (my/probe--corpus-files 1))))
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-max))
      (let ((before (buffer-size)))
        (insert "\n한글 삽입 시험 문장이다.\n")
        (prog1 (format "%d -> %d chars, buffer-modified=%s"
                       before (buffer-size) (buffer-modified-p))
          ;; Discard without touching disk.
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(my/probe-check "org-table-align a real table if one exists"
  (let ((found nil))
    (catch 'done
      (dolist (f (my/probe--corpus-files 40))
        (with-current-buffer (find-file-noselect f)
          (goto-char (point-min))
          (when (re-search-forward "^[ \t]*|.*|[ \t]*$" nil t)
            (beginning-of-line)
            (when (org-at-table-p)
              (org-table-align)
              (setq found (format "%s: aligned, width=%d"
                                  (substring (file-name-nondirectory f) 0 15)
                                  (- (line-end-position) (line-beginning-position))))
              (set-buffer-modified-p nil)
              (kill-buffer)
              (throw 'done nil)))
          (set-buffer-modified-p nil)
          (kill-buffer))))
    (or found (cons 'skip "no table found in sample"))))

(my/probe-banner "exporting real notes")

(my/probe-check "export 3 real notes to HTML"
  (require 'ox-html)
  (let ((dir (make-temp-file "neomacs-real-export" t))
        (sizes nil))
    (unwind-protect
        (progn
          (dolist (f (my/probe--corpus-files 3))
            (with-current-buffer (find-file-noselect f)
              (let ((out (org-export-to-file 'html
                             (expand-file-name
                              (concat (file-name-base f) ".html") dir))))
                (push (file-attribute-size (file-attributes out)) sizes))
              (set-buffer-modified-p nil)
              (kill-buffer)))
          (format "%s bytes" (mapconcat #'number-to-string (nreverse sizes) " / ")))
      (delete-directory dir t))))

(my/probe-check "export a real note to Markdown, Hangul intact"
  (require 'ox-md)
  (let ((f (car (my/probe--corpus-files 1))))
    (with-current-buffer (find-file-noselect f)
      (let ((md (org-export-as 'md nil nil t)))
        (prog1 (if (string-match-p "[가-힣]" md)
                   (format "%d chars, Hangul present" (length md))
                 "HANGUL MISSING from markdown output")
          (set-buffer-modified-p nil)
          (kill-buffer))))))

(my/probe-summary)

;;; probe-real-org.el ends here
