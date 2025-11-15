;;; test-denote-export.el --- Tests for denote-export system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;;; Commentary:

;; Tests for denote-export-server.el functions
;; Core functions: ID extraction, section detection, export logic

;;; Code:

(require 'test-helper)

;; Load denote-export functions
(defun extract-denote-id-from-filename (filename)
  "Extract Denote ID (YYYYMMDDTHHMMSS) from FILENAME.
Returns nil if no ID found."
  (let ((basename (file-name-nondirectory filename)))
    (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" basename)
      (match-string 1 basename))))

(defun get-org-hugo-section-from-path (filepath)
  "Determine org-hugo-section from FILEPATH directory.
Returns meta, bib, notes, or test based on parent directory name."
  (let ((parent-dir (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory filepath)))))
    (cond
     ((string= parent-dir "meta") "meta")
     ((string= parent-dir "bib") "bib")
     ((string= parent-dir "notes") "notes")
     ((string= parent-dir "test") "test")
     (t "notes")))) ; default to notes

;;;; Denote ID Extraction Tests

(ert-deftest test-extract-denote-id--valid ()
  "Test Denote ID extraction from valid filename."
  (should (equal (extract-denote-id-from-filename
                  "20231015T120000--test-file.org")
                 "20231015T120000"))

  (should (equal (extract-denote-id-from-filename
                  "/path/to/20250115T093045--another-test__tag1_tag2.org")
                 "20250115T093045")))

(ert-deftest test-extract-denote-id--with-korean ()
  "Test ID extraction from filename with Korean characters."
  (should (equal (extract-denote-id-from-filename
                  "20231015T120000--한글-제목.org")
                 "20231015T120000"))

  (should (equal (extract-denote-id-from-filename
                  "/home/user/org/20250115T093045--테스트-파일__태그.org")
                 "20250115T093045")))

(ert-deftest test-extract-denote-id--with-nbsp ()
  "Test ID extraction from filename with NBSP (U+00A0)."
  ;; NBSP는 일반 공백처럼 보이지만 U+00A0
  (let ((filename-with-nbsp "20231015T120000--title with NBSP.org"))
    (should (equal (extract-denote-id-from-filename filename-with-nbsp)
                   "20231015T120000"))))

(ert-deftest test-extract-denote-id--invalid ()
  "Test ID extraction from invalid filename."
  ;; No ID
  (should (null (extract-denote-id-from-filename "no-id-file.org")))

  ;; Wrong format
  (should (null (extract-denote-id-from-filename "20231015-120000--wrong.org")))
  (should (null (extract-denote-id-from-filename "2023101T120000--short.org"))))

;;;; Section Detection Tests

(ert-deftest test-section-detection--meta ()
  "Test section detection for meta directory."
  (should (equal (get-org-hugo-section-from-path
                  "/home/user/org/meta/20231015T120000--file.org")
                 "meta")))

(ert-deftest test-section-detection--bib ()
  "Test section detection for bib directory."
  (should (equal (get-org-hugo-section-from-path
                  "/home/user/org/bib/20231015T120000--paper.org")
                 "bib")))

(ert-deftest test-section-detection--notes ()
  "Test section detection for notes directory."
  (should (equal (get-org-hugo-section-from-path
                  "/home/user/org/notes/20231015T120000--note.org")
                 "notes")))

(ert-deftest test-section-detection--test ()
  "Test section detection for test directory."
  (should (equal (get-org-hugo-section-from-path
                  "/home/user/org/test/20231015T120000--test.org")
                 "test")))

(ert-deftest test-section-detection--default ()
  "Test section detection for unknown directory (defaults to notes)."
  (should (equal (get-org-hugo-section-from-path
                  "/home/user/org/unknown/file.org")
                 "notes"))

  (should (equal (get-org-hugo-section-from-path
                  "/tmp/file.org")
                 "notes")))

;;;; Denote ID to Date Conversion Tests

(ert-deftest test-denote-id-to-date ()
  "Test conversion of Denote ID to date format."
  (let ((denote-id "20231015T120000"))
    ;; Extract date parts
    (should (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" denote-id))

    (let ((date-str (format "%s-%s-%s"
                           (match-string 1 denote-id)
                           (match-string 2 denote-id)
                           (match-string 3 denote-id))))
      (should (equal date-str "2023-10-15")))))

;;;; File Path Handling Tests

(ert-deftest test-unicode-filename-handling ()
  "Test that Unicode filenames are handled correctly."
  (let ((test-files '("20231015T120000--한글.org"
                     "20231015T120000--日本語.org"
                     "20231015T120000--中文.org"
                     "20231015T120000--Español.org")))
    (dolist (filename test-files)
      (should (equal (extract-denote-id-from-filename filename)
                     "20231015T120000")))))

;;;; Integration Test (requires actual file system)

(ert-deftest test-export-integration--denote-id-and-section ()
  "Integration test: ID extraction + section detection."
  (test-helper/with-temp-dir
   (lambda (temp-dir)
     ;; Create directory structure
     (let ((meta-dir (expand-file-name "meta" temp-dir))
           (test-file nil))

       (make-directory meta-dir t)

       ;; Create test file
       (setq test-file (expand-file-name
                       "20231015T120000--test-note.org"
                       meta-dir))

       (with-temp-file test-file
         (insert "#+title: Test Note\n")
         (insert "#+date: [2023-10-15 Sun]\n")
         (insert "\n* Test Content\n"))

       ;; Test ID extraction
       (should (equal (extract-denote-id-from-filename test-file)
                     "20231015T120000"))

       ;; Test section detection
       (should (equal (get-org-hugo-section-from-path test-file)
                     "meta"))))))

;;; test-denote-export.el ends here
