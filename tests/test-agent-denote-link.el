;;; test-agent-denote-link.el --- Tests for related-notes heading detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Regression tests for the related-notes heading detection used by
;; `agent-denote-add-link' in bin/agent-server.el.
;;
;; The full command needs denote, so it can't load under `emacs -Q'.  Instead
;; we read the shipping `agent-server--related-notes-heading-re' value straight
;; out of the source (no drift-prone copy) and assert which headings it does
;; and does not match.
;;
;; 2026-07 regression this locks (reported by openai-codex/gpt-5.6-sol while
;; running autholog repair): the old `관련' + whitespace pattern
;;   (a) MISSED the standard no-space '관련노트' (442 in the ~/org corpus,
;;       vs 188 spaced '관련 노트'), so links never landed in the real section;
;;   (b) FALSE-matched '관련 레퍼런스' (dropped a link into the wrong section
;;       of 20260406T140411) and would have hit '관련메타' (985 — the single
;;       most common heading, an auto-magnet section) or '관련링크' had the
;;       trailing-word not happened to break the whitespace class.

;;; Code:

(require 'test-helper)

(defconst test-link/agent-server-file
  (expand-file-name
   "bin/agent-server.el"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name)))))
  "Path to the shipping agent-server.el source.")

(defconst test-link/related-notes-re
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents test-link/agent-server-file)
      (goto-char (point-min))
      (re-search-forward "^(defconst agent-server--related-notes-heading-re")
      (goto-char (match-beginning 0))
      ;; (defconst NAME REGEXP DOCSTRING) — element 2 is the regexp value.
      (nth 2 (read (current-buffer)))))
  "The shipping related-notes heading regexp, read from source (no copy).")

(defun test-link/heading-matches-p (heading)
  "Non-nil when HEADING is detected as a related-notes section."
  (string-match-p test-link/related-notes-re heading))

;;;; Standard + accepted variants MUST match

(ert-deftest test-link-re--standard-no-space ()
  "The standard '* 관련노트' (no space) — the exact heading the regression
missed — must match."
  (should (test-link/heading-matches-p "* 관련노트")))

(ert-deftest test-link-re--accepted-variants ()
  (should (test-link/heading-matches-p "* 관련 노트"))
  (should (test-link/heading-matches-p "** 관련 노트"))
  (should (test-link/heading-matches-p "*** 관련노트"))
  (should (test-link/heading-matches-p "* 관련"))
  (should (test-link/heading-matches-p "* Related")))

(ert-deftest test-link-re--with-org-tags ()
  "Trailing org tags must not defeat the end anchor."
  (should (test-link/heading-matches-p "* 관련노트 :note:"))
  (should (test-link/heading-matches-p "* 관련 노트 :a:b:")))

;;;; Sibling sections MUST NOT match

(ert-deftest test-link-re--rejects-siblings ()
  "Distinct sibling sections must never be captured as related-notes."
  (should-not (test-link/heading-matches-p "* 관련메타"))     ; auto-magnet, top of corpus
  (should-not (test-link/heading-matches-p "* 관련링크"))
  (should-not (test-link/heading-matches-p "* 관련 레퍼런스")) ; false-match that hit 20260406T140411
  (should-not (test-link/heading-matches-p "* 관련 문서"))
  (should-not (test-link/heading-matches-p "* 관련 노트 (추가)")))

(ert-deftest test-link-re--rejects-non-headings ()
  (should-not (test-link/heading-matches-p "관련노트"))       ; no stars
  (should-not (test-link/heading-matches-p "- 관련노트"))     ; list item
  (should-not (test-link/heading-matches-p "* 관련노트입니다"))) ; longer word

(provide 'test-agent-denote-link)
;;; test-agent-denote-link.el ends here
