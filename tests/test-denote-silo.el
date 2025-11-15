;;; test-denote-silo.el --- Tests for denote-silo-dynamic -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;;; Commentary:

;; Tests for denote-silo-dynamic.el functions

;;; Code:

(require 'test-helper)

;; Load denote-silo functions directly
(defun denote-silo--is-git-repo (dir)
  "DIR이 Git 저장소인지 확인합니다."
  (file-directory-p (expand-file-name ".git" dir)))

(defun denote-silo--format-display-name (path)
  "경로를 읽기 좋은 짧은 이름으로 변환합니다."
  (let* ((expanded (expand-file-name path))
         (dir-name (file-name-nondirectory (directory-file-name expanded)))
         (parent-dir (file-name-directory (directory-file-name expanded)))
         (parent-name (file-name-nondirectory (directory-file-name parent-dir))))
    (cond
     ((string= dir-name "docs")
      (concat parent-name "/docs"))
     ((string-match "/org/\\(.+\\)$" expanded)
      (concat "org/" (match-string 1 expanded)))
     (t dir-name))))

(defvar denote-silo-repos-roots nil)
(defvar denote-silo-directories nil)

(defun denote-silo-discover-repo-docs ()
  "Git 저장소의 docs 디렉토리를 자동으로 발견하여 반환."
  (let ((valid-silos '()))
    (dolist (repos-root denote-silo-repos-roots)
      (let* ((expanded-root (expand-file-name repos-root))
             (repo-dirs (when (file-directory-p expanded-root)
                          (directory-files expanded-root t "^[^.]"))))
        (dolist (repo-dir repo-dirs)
          (let* ((real-repo-dir (file-truename repo-dir))
                 (docs-path (expand-file-name "docs" real-repo-dir)))
            (when (and (denote-silo--is-git-repo real-repo-dir)
                       (file-directory-p docs-path)
                       (not (member docs-path valid-silos)))
              (push docs-path valid-silos))))))
    (nreverse valid-silos)))

(defun denote-silo-count-files ()
  "Count total org files across all silos."
  (let ((total 0))
    (dolist (silo denote-silo-directories)
      (when (file-directory-p silo)
        (setq total (+ total (length (directory-files silo nil "\\.org$"))))))
    total))

(ert-deftest test-denote-silo--is-git-repo ()
  "Test Git repository detection."
  (test-helper/with-temp-dir
   (lambda (temp-dir)
     ;; Not a git repo initially
     (should-not (denote-silo--is-git-repo temp-dir))

     ;; Create .git directory
     (let ((git-dir (expand-file-name ".git" temp-dir)))
       (make-directory git-dir)
       (should (denote-silo--is-git-repo temp-dir))))))

(ert-deftest test-denote-silo--format-display-name-docs ()
  "Test display name formatting for docs directories."
  (should (string= (denote-silo--format-display-name "/path/to/my-project/docs")
                   "my-project/docs"))
  (should (string= (denote-silo--format-display-name "/home/user/repos/gh/awesome-repo/docs")
                   "awesome-repo/docs")))

(ert-deftest test-denote-silo--format-display-name-org ()
  "Test display name formatting for org directories."
  (should (string= (denote-silo--format-display-name "/home/user/org/notes")
                   "org/notes"))
  (should (string= (denote-silo--format-display-name "/home/user/org/journal")
                   "org/journal")))

(ert-deftest test-denote-silo--format-display-name-default ()
  "Test display name formatting for other directories."
  (should (string= (denote-silo--format-display-name "/home/user/claude-memory")
                   "claude-memory"))
  (should (string= (denote-silo--format-display-name "/tmp/test-dir")
                   "test-dir")))

(ert-deftest test-denote-silo-discover-repo-docs ()
  "Test automatic discovery of docs directories in Git repos."
  (test-helper/with-temp-dir
   (lambda (temp-root)
     ;; Setup: repos-root/repo1/docs (with .git)
     (let* ((denote-silo-repos-roots (list temp-root))
            (repo1-dir (expand-file-name "repo1" temp-root))
            (repo1-git (expand-file-name ".git" repo1-dir))
            (repo1-docs (expand-file-name "docs" repo1-dir))
            (repo2-dir (expand-file-name "repo2" temp-root))
            (repo2-docs (expand-file-name "docs" repo2-dir)))

       ;; Create repo1 with .git and docs
       (make-directory repo1-git t)
       (make-directory repo1-docs t)

       ;; Create repo2 with docs but no .git (should be ignored)
       (make-directory repo2-docs t)

       ;; Test discovery
       (let ((discovered (denote-silo-discover-repo-docs)))
         (should (member repo1-docs discovered))
         (should-not (member repo2-docs discovered)))))))

(ert-deftest test-denote-silo-count-files ()
  "Test counting org files across silos."
  (test-helper/with-temp-dir
   (lambda (temp-dir)
     (let ((denote-silo-directories (list temp-dir)))
       ;; Create test org files
       (test-helper/create-test-org-file temp-dir "file1.org")
       (test-helper/create-test-org-file temp-dir "file2.org")

       ;; Should count 2 files
       (should (= (denote-silo-count-files) 2))))))

;;; test-denote-silo.el ends here
