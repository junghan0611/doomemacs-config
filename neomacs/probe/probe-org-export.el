;;; neomacs/probe/probe-org-export.el --- ox-html / ox-md export probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Exercises the export chain the minimal set commits to: ox-html and
;; ox-md, with Korean content throughout.  Export pulls in a wide slice of
;; the runtime — cl-lib, org-element, the regexp engine, coding systems —
;; so it doubles as a bootstrap-fidelity check.
;;
;; This is the profile's stand-in for the real garden pipeline, which runs
;; ox-hugo over ~2,000 org files across 8 daemons.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'probe-lib)
(require 'org)

(defconst my/probe-org-sample "\
#+TITLE: 한글 내보내기 시험
#+AUTHOR: 김정한
#+OPTIONS: toc:t

* 첫째 절

본문 단락이다.  *굵게*, /기울임/, =코드= 를 섞는다.

- 첫째 항목
- 둘째 항목

** 표

| 항목     | 값   |
|----------+------|
| 한글     | 2칸  |
| ascii    | 1칸  |

** 코드 블록

#+begin_src emacs-lisp
(message \"안녕하세요\")
#+end_src

** 링크

[[https://notes.junghanacs.com][디지털 가든]]
"
  "Korean org fixture covering the constructs the minimal set must export.")

(defun my/probe--export-string (backend)
  "Export the sample to BACKEND and return the result string."
  (with-temp-buffer
    (insert my/probe-org-sample)
    (org-mode)
    (org-export-as backend nil nil t)))

(my/probe-banner "export backends available")

(dolist (lib '(ox ox-html ox-md ox-ascii))
  (my/probe-check (format "require %s" lib)
    (if (require lib nil t) "loaded" (cons 'skip "unavailable"))))

(my/probe-banner "ox-html")

(my/probe-check "export sample to HTML"
  (let ((out (my/probe--export-string 'html)))
    (format "%d chars" (length out))))

(my/probe-check "HTML preserves Hangul"
  (let ((out (my/probe--export-string 'html)))
    (if (string-match-p "한글" out)
        "Hangul present in output"
      "HANGUL MISSING from output")))

(my/probe-check "HTML renders the table"
  (let ((out (my/probe--export-string 'html)))
    (if (string-match-p "<table" out)
        (format "%d rows" (cl-count-if
                           (lambda (_) t)
                           (split-string out "<tr")))
      "NO <table> in output")))

(my/probe-check "HTML renders the source block"
  (let ((out (my/probe--export-string 'html)))
    (if (string-match-p "안녕하세요" out)
        "src block content present"
      "SRC BLOCK CONTENT MISSING")))

(my/probe-banner "ox-md")

(my/probe-check "export sample to Markdown"
  (let ((out (my/probe--export-string 'md)))
    (format "%d chars" (length out))))

(my/probe-check "Markdown preserves Hangul"
  (let ((out (my/probe--export-string 'md)))
    (if (string-match-p "한글" out)
        "Hangul present in output"
      "HANGUL MISSING from output")))

(my/probe-check "Markdown keeps the link"
  (let ((out (my/probe--export-string 'md)))
    (if (string-match-p "notes\\.junghanacs\\.com" out)
        "link target preserved"
      "LINK TARGET LOST")))

(my/probe-banner "file round trip")

(my/probe-check "export to a real file and read it back"
  ;; org-export-to-file exercises the coding-system write path, which
  ;; string export never touches (upstream #104 was a coding failure).
  (let* ((dir (make-temp-file "neomacs-probe" t))
         (org-file (expand-file-name "sample.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file org-file (insert my/probe-org-sample))
          (with-current-buffer (find-file-noselect org-file)
            (let ((html (org-export-to-file 'html
                            (expand-file-name "sample.html" dir))))
              (with-temp-buffer
                (insert-file-contents html)
                (if (string-match-p "한글" (buffer-string))
                    (format "%d bytes, Hangul intact"
                            (buffer-size))
                  "ROUND TRIP LOST HANGUL")))))
      (delete-directory dir t))))

(my/probe-summary)

;;; probe-org-export.el ends here
