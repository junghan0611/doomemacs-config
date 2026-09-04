;;; test-agent-denote-heading.el --- Tests for add-heading argument parsing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Regression tests for `agent-server--parse-add-heading-args' in
;; bin/agent-server.el — the argument shape of `agent-denote-add-heading'.
;;
;; The command itself needs denote, so it can't load under `emacs -Q'.  The
;; parser is pure, so we read the two shipping forms straight out of the
;; source and eval them here (no drift-prone copy), the same way
;; test-agent-denote-link.el reads its regexp.
;;
;; Two regressions this locks, both found 2026-09-04 while a sibling repo's
;; steward reported tagged headings landing at end of file:
;;
;;   (a) The tag test ran with the default `case-fold-search' of t, so
;;       `[A-Z]' matched lowercase.  A one-word body like "body" was read as
;;       a TAG and the content silently became "".  Measured on the live
;;       "server" daemon: (string-match-p "\\`[A-Z][A-Z0-9:]*\\'" "body") => 0.
;;
;;   (b) The docstring advertised only an ignored integer index after TAGS
;;       CONTENT, so callers believed placement was unavailable whenever they
;;       passed a tag, and moved the heading by hand.  The string form did
;;       work; only the documentation was wrong.  It is now specified.

;;; Code:

(require 'test-helper)

(defconst test-heading/agent-server-file
  (expand-file-name
   "bin/agent-server.el"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name)))))
  "Path to the shipping agent-server.el source.")

(defun test-heading/eval-form (name)
  "Read the top-level form defining NAME out of the shipping source and eval it."
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents test-heading/agent-server-file)
      (goto-char (point-min))
      (re-search-forward (format "^(def[a-z]+ %s\\_>" (regexp-quote name)))
      (goto-char (match-beginning 0))
      (eval (read (current-buffer)) t))))

;; Pull in the shipping regexp + parser, in dependency order.
(test-heading/eval-form "agent-server--heading-tags-re")
(test-heading/eval-form "agent-server--parse-add-heading-args")

(defun test-heading/parse (&rest args)
  "Parse ARGS and return (TAGS CONTENT AFTER-HEADING)."
  (let ((p (agent-server--parse-add-heading-args args)))
    (list (alist-get :tags p)
          (alist-get :content p)
          (alist-get :after-heading p))))

;;;; Tag detection is case-sensitive

(ert-deftest test-heading-parse--lowercase-body-is-content ()
  "Regression (a): a one-word lowercase body must stay CONTENT, not become a tag."
  (should (equal (test-heading/parse "body") '(nil "body" nil))))

(ert-deftest test-heading-parse--lowercase-body-under-let-case-fold ()
  "The parser must not inherit a caller's `case-fold-search'."
  (let ((case-fold-search t))
    (should (equal (test-heading/parse "body") '(nil "body" nil)))))

(ert-deftest test-heading-parse--mixed-case-word-is-content ()
  "A CamelCase one-word body is content, not a tag."
  (should (equal (test-heading/parse "Body") '(nil "Body" nil))))

(ert-deftest test-heading-parse--allcaps-is-tag ()
  "An ALL-CAPS word is the TAGS argument."
  (should (equal (test-heading/parse "LLMLOG" "body") '("LLMLOG" "body" nil))))

(ert-deftest test-heading-parse--colon-separated-tags ()
  "Colon-separated ALL-CAPS words are TAGS."
  (should (equal (test-heading/parse "LLMLOG:ARCHIVE" "body")
                 '("LLMLOG:ARCHIVE" "body" nil))))

;;;; Placement combines with tags

(ert-deftest test-heading-parse--tags-plus-after-heading ()
  "Regression (b): TAGS and AFTER-HEADING must be usable together."
  (should (equal (test-heading/parse "LLMLOG" "body" "보고")
                 '("LLMLOG" "body" "보고"))))

(ert-deftest test-heading-parse--after-heading-without-tags ()
  "AFTER-HEADING works without TAGS."
  (should (equal (test-heading/parse "body" "보고") '(nil "body" "보고"))))

(ert-deftest test-heading-parse--legacy-integer-index-ignored ()
  "A legacy integer in the AFTER-HEADING slot is dropped, not an error."
  (should (equal (test-heading/parse "LLMLOG" "body" 2) '("LLMLOG" "body" nil)))
  (should (equal (test-heading/parse "body" 2) '(nil "body" nil))))

;;;; Degenerate input

(ert-deftest test-heading-parse--no-args ()
  "No content argument yields an empty body, never nil."
  (should (equal (test-heading/parse) '(nil "" nil))))

(ert-deftest test-heading-parse--multiline-body-is-content ()
  "A multi-word body is content (this always worked; lock it)."
  (should (equal (test-heading/parse "two words") '(nil "two words" nil))))

(ert-deftest test-heading-parse--allcaps-alone-is-tag-with-empty-body ()
  "TAGS with no content leaves an empty body rather than erroring."
  (should (equal (test-heading/parse "LLMLOG") '("LLMLOG" "" nil))))

(provide 'test-agent-denote-heading)
;;; test-agent-denote-heading.el ends here
