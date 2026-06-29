;;; test-denote-export-refs.el --- Tests for the #+reference: -> refs bridge -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;;; Commentary:

;; Characterization tests for `lisp/denote-export-refs.el' — the pure layer
;; that turns citar-denote `#+reference:' citekeys into resolved ref alists
;; for garden export frontmatter.
;;
;; The test set is self-contained under tests/fixtures/refs/ (org notes +
;; sample.bib), NOT the user's ~/org garden, so it is reproducible by anyone.
;; `test-refs/bib-index' mirrors sample.bib; resolution is dependency-injected
;; so these run under `emacs -Q' with neither citar nor citar-denote loaded.

;;; Code:

(require 'test-helper)
(require 'denote-export-refs)

(defconst test-refs/fixture-dir
  (expand-file-name "fixtures/refs/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory holding the self-contained refs fixtures.")

(defconst test-refs/bib-index
  '(("web-emacsageai" . (("title" . "Emacs in the age of AI")
                         ("url" . "https://vivekhaldar.com/articles/emacs-in-the-age-of-ai/")
                         ("author" . "Vivek Haldar")))
    ("ahyattekg" . (("title" . "ahyatt/ekg")
                    ("url" . "https://github.com/ahyatt/ekg")
                    ("author" . "Andrew Hyatt")))
    ("book-noturl" . (("title" . "Book Without URL")
                      ("author" . "Some Author")
                      ("year" . "2020")))
    ("notitle-key" . (("url" . "https://example.com/no-title"))))
  "In-code mirror of fixtures/refs/sample.bib (string-keyed like citar).
`notitle-key' is an extra contract case (entry lacking a title) not in
the bib file — it exercises the title-required skip rule.")

(defun test-refs/fixture (name)
  "Return the contents of fixture NAME under `test-refs/fixture-dir'."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name test-refs/fixture-dir))
    (buffer-string)))

;;;; Citekey extraction

(ert-deftest test-refs-split--basic ()
  "Semicolon-separated value splits into trimmed citekeys."
  (should (equal (my/denote-export-refs-split "web-a;book-b")
                 '("web-a" "book-b")))
  (should (equal (my/denote-export-refs-split "  web-a ;  book-b  ")
                 '("web-a" "book-b"))))

(ert-deftest test-refs-split--blank-and-nil ()
  "Blank, empty, and nil values yield nil (no empty citekeys)."
  (should (null (my/denote-export-refs-split "")))
  (should (null (my/denote-export-refs-split "   ")))
  (should (null (my/denote-export-refs-split ";;")))
  (should (null (my/denote-export-refs-split nil))))

(ert-deftest test-refs-in-string--from-fixture ()
  "The keyword regex extracts citekeys straight from a fixture note."
  (should (equal (my/denote-export-refs-in-string
                  (test-refs/fixture "20260101T100000--note-with-refs__test.org"))
                 '("web-emacsageai" "ahyattekg"))))

(ert-deftest test-refs-in-string--no-keyword ()
  "A note without a #+reference: line yields nil."
  (should (null (my/denote-export-refs-in-string
                 (test-refs/fixture "20260101T100100--note-no-refs__test.org")))))

;;;; Bib resolution

(ert-deftest test-refs-resolve--title-url-author ()
  "Resolvable citekeys carry :key/:title plus present optional fields."
  (let ((refs (my/denote-export-refs-resolve
               '("web-emacsageai" "ahyattekg") test-refs/bib-index)))
    (should (= 2 (length refs)))
    (should (equal (cdr (assq :key (car refs))) "web-emacsageai"))
    (should (equal (cdr (assq :title (car refs))) "Emacs in the age of AI"))
    (should (equal (cdr (assq :url (car refs)))
                   "https://vivekhaldar.com/articles/emacs-in-the-age-of-ai/"))
    (should (equal (cdr (assq :author (car refs))) "Vivek Haldar"))
    ;; web-emacsageai has no year in the index -> key absent, not nil-valued
    (should (null (assq :year (car refs))))))

(ert-deftest test-refs-resolve--year-when-present ()
  "A bib entry with a year carries :year; absent url is omitted."
  (let ((ref (car (my/denote-export-refs-resolve
                   '("book-noturl") test-refs/bib-index))))
    (should (equal (cdr (assq :title ref)) "Book Without URL"))
    (should (equal (cdr (assq :year ref)) "2020"))
    (should (null (assq :url ref)))))

(ert-deftest test-refs-resolve--title-required ()
  "An entry without a title is skipped even though it resolves."
  (should (null (my/denote-export-refs-resolve
                 '("notitle-key") test-refs/bib-index))))

(ert-deftest test-refs-resolve--orphan-skipped ()
  "An orphan citekey (absent from the index) is dropped, not emitted bare."
  (let* ((keys (my/denote-export-refs-in-string
                (test-refs/fixture "20260101T100200--note-orphan-ref__test.org")))
         (refs (my/denote-export-refs-resolve keys test-refs/bib-index)))
    (should (equal keys '("web-emacsageai" "nonexistent-key-xyz")))
    (should (= 1 (length refs)))
    (should (equal (cdr (assq :key (car refs))) "web-emacsageai"))))

(ert-deftest test-refs-resolve--empty-input ()
  "No citekeys resolves to no refs."
  (should (null (my/denote-export-refs-resolve nil test-refs/bib-index))))

(provide 'test-denote-export-refs)
;;; test-denote-export-refs.el ends here
