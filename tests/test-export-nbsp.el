;;; test-export-nbsp.el --- Tests for the two NBSPs of the export pipeline -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Two different NBSPs (U+00A0) meet in one export:
;;
;;   - source NBSP, typed by GLG between a term and its particle so
;;     org-glossary / ten can match the term — must NOT reach the md;
;;   - emphasis NBSP, inserted by `my/org-fix-cjk-emphasis' so the org
;;     parser and remark see *강조*은 as emphasis — MUST reach the md.
;;
;; They are told apart by hook order alone, so the order is the contract.
;; Regression (2026-09-27): a lint pass in a3168bc (2026-03-14) had turned
;; the NBSP literals into plain spaces and the source NBSP removal was left
;; disabled, so 2236 hangul-NBSP-hangul pairs leaked into the garden md.
;;
;; denote-export-config.el needs ox-hugo/denote, so the shipping forms are
;; read out of the source and evaluated, as test-agent-denote-heading.el does.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

(defconst test-nbsp/source-file
  (expand-file-name
   "lisp/denote-export-config.el"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name)))))
  "Path to the shipping denote-export-config.el source.")

(defconst test-nbsp/nbsp (string ? ))

(defun test-nbsp/eval-form (regexp)
  "Eval the top-level form starting with REGEXP in the shipping source."
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents test-nbsp/source-file)
      (goto-char (point-min))
      (re-search-forward regexp)
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

(test-nbsp/eval-form "^(defconst my/org-export-tag-before-nbsp-re\\_>")
(test-nbsp/eval-form "^(defconst my/org-export-particle-re\\_>")
(test-nbsp/eval-form "^(defun my/org-export-normalize-source-nbsp\\_>")
(test-nbsp/eval-form "^(defvar my/org-hugo-hashtag-class\\_>")
(test-nbsp/eval-form "^(defvar my/org-hugo-mention-class\\_>")
(test-nbsp/eval-form "^(defvar my/org-hugo-mention-names\\_>")
(test-nbsp/eval-form "^(defun my/org-hugo-wrap-hashtags-and-mentions\\_>")
(test-nbsp/eval-form "^(defun my/org-fix-cjk-emphasis\\_>")

(defun test-nbsp/n (s)
  "Return S with every \"_\" replaced by an NBSP."
  (replace-regexp-in-string "_" test-nbsp/nbsp s t t))

(defun test-nbsp/run (fns text)
  "Run export hook FNS in order on an export-like buffer holding TEXT.
A leading blank line stands in for the front matter the emphasis fix skips."
  (with-temp-buffer
    (insert "#+title: t\n\n" text)
    (dolist (fn fns) (funcall fn 'hugo))
    (goto-char (point-min))
    (forward-line 2)
    (buffer-substring-no-properties (point) (point-max))))

;;;; Source NBSP normalization

(ert-deftest test-nbsp--particle-nbsp-is-dropped ()
  "A source NBSP before a particle only split it off: drop it."
  (should (equal (test-nbsp/run '(my/org-export-normalize-source-nbsp)
                                (test-nbsp/n "조판_은 가기_를_ 기대 서버_에서는 책_으로부터"))
                 "조판은 가기를  기대 서버에서는 책으로부터")))

(ert-deftest test-nbsp--word-spacing-nbsp-becomes-space ()
  "Pasted text spaces Hangul words with NBSP; those must stay apart.
Found by cross-review 2026-09-27 in a real note: \"싹싹<NBSP>빌면\"."
  (should (equal (test-nbsp/run '(my/org-export-normalize-source-nbsp)
                                (test-nbsp/n "그래도_싹싹_빌면_집에 그_나는 개발_하는"))
                 "그래도 싹싹 빌면 집에 그 나는 개발 하는")))

(ert-deftest test-nbsp--other-nbsp-becomes-space ()
  "Elsewhere a source NBSP stood for a space; removing it would glue words."
  (should (equal (test-nbsp/run '(my/org-export-normalize-source-nbsp)
                                (test-nbsp/n "]]_=(20m)= │__└── end_"))
                 "]] =(20m)= │  └── end ")))

(ert-deftest test-nbsp--tag-keeps-its-particle-apart ()
  "After a #hashtag or @mention the NBSP becomes a space, even before Hangul."
  (should (equal (test-nbsp/run '(my/org-export-normalize-source-nbsp)
                                (test-nbsp/n "#포춘쿠키_를 _#수식_조판 @junghan_이"))
                 "#포춘쿠키 를  #수식 조판 @junghan 이")))

(ert-deftest test-nbsp--hugo-tag-stops-before-particle ()
  "End to end with the hashtag filter: the span ends at the tag."
  (cl-letf (((symbol-function 'org-export-derived-backend-p)
             (lambda (_backend &rest _) t)))
    (should (equal (my/org-hugo-wrap-hashtags-and-mentions
                    (test-nbsp/run '(my/org-export-normalize-source-nbsp)
                                   (test-nbsp/n "먼저 #포춘쿠키_를 던진다."))
                    'hugo nil)
                   "먼저 <span class=\"org-hashtag\">#포춘쿠키</span> 를 던진다."))))

;;;; Order: normalize, then emphasis

(ert-deftest test-nbsp--emphasis-nbsp-survives-normalization ()
  "Run in shipping order, the emphasis NBSP is the only one left."
  (should (equal (test-nbsp/run '(my/org-export-normalize-source-nbsp
                                  my/org-fix-cjk-emphasis)
                                (test-nbsp/n "*지평*_을 조판_은"))
                 (test-nbsp/n "*지평*_을 조판은"))))

(defvar org-export-before-parsing-functions)

(ert-deftest test-nbsp--hook-order ()
  "Glossary (depth 0) sees source NBSP; normalize runs next; emphasis last."
  (let ((org-export-before-parsing-functions nil))
    (add-hook 'org-export-before-parsing-functions #'ignore) ; org-glossary stand-in
    (test-nbsp/eval-form
     "^(add-hook 'org-export-before-parsing-functions\n *#'my/org-export-normalize-source-nbsp")
    (test-nbsp/eval-form
     "^(add-hook 'org-export-before-parsing-functions #'my/org-fix-cjk-emphasis")
    (should (equal org-export-before-parsing-functions
                   '(ignore
                     my/org-export-normalize-source-nbsp
                     my/org-fix-cjk-emphasis)))))

(ert-deftest test-nbsp--no-literal-nbsp-in-source ()
  "The source spells NBSP as ?\\u00A0; a literal is what lint pass erased."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents test-nbsp/source-file))
    (should-not (search-forward test-nbsp/nbsp nil t))))

(provide 'test-export-nbsp)
;;; test-export-nbsp.el ends here
