;;; +denote-silo-dynamic.el --- Dynamic Denote Silo Management -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: 2025-11-14
;; Version: 1.0.0
;; Keywords: denote, silo, repository, docs
;; Homepage: https://notes.junghanacs.com

;;; Commentary:

;; 리포지토리 중심의 Denote Silo 자동 관리 시스템
;;
;; 핵심 기능:
;; 1. ~/repos/gh/*/docs 디렉토리 자동 발견 및 Silo 등록
;; 2. 심볼릭 링크 지원 (실제 경로로 resolve)
;; 3. Consult-notes 통합
;; 4. 동적 업데이트 (새 프로젝트 자동 인식)

;;; Code:

(require 'denote-silo)

;;;; Core Functions

(defcustom denote-silo-repos-roots
  '("~/repos/gh" "~/repos/work"
    )
  "리포지토리 루트 디렉토리 목록.
각 디렉토리 하위의 */docs 폴더를 Silo로 자동 등록합니다."
  :group 'denote-silo
  :type '(repeat directory))

(defcustom denote-silo-base-directories
  '(
    "~/claude-memory/areas"
    "~/claude-memory/projects"
    "~/claude-memory/resources/"
    "~/claude-memory/resources/solutions/"
    "~/org"
    "~/org/meta"
    "~/org/bib"
    "~/org/notes"
    "~/org/llmlog"
    "~/org/elisp"
    )
  "기본 Denote Silo 디렉토리 목록.
리포지토리 발견과 별개로 항상 포함됩니다."
  :group 'denote-silo
  :type '(repeat directory))

;;;; Helper Functions

(defun denote-silo--is-git-repo (dir)
  "DIR이 Git 저장소인지 확인합니다."
  (file-directory-p (expand-file-name ".git" dir)))

(defun denote-silo--format-display-name (path)
  "경로를 읽기 좋은 짧은 이름으로 변환합니다.
예시:
  /any/path/my-project/docs -> my-project/docs
  /home/user/org/notes -> org/notes
  /home/user/claude-memory -> claude-memory"
  (let* ((expanded (expand-file-name path))
         (dir-name (file-name-nondirectory (directory-file-name expanded)))
         (parent-dir (file-name-directory (directory-file-name expanded)))
         (parent-name (file-name-nondirectory (directory-file-name parent-dir))))
    (cond
     ;; docs 디렉토리인 경우: 부모 디렉토리 이름 포함 "repo-name/docs"
     ((string= dir-name "docs")
      (concat parent-name "/docs"))

     ;; ~/org 하위: "org/subdir"
     ((string-match "/org/\\(.+\\)$" expanded)
      (concat "org/" (match-string 1 expanded)))

     ;; 기본: 디렉토리 이름만
     (t dir-name))))

(defun denote-silo-discover-repo-docs ()
  "Git 저장소의 docs 디렉토리를 자동으로 발견하여 반환.
심볼릭 링크는 실제 경로로 resolve됩니다.
Git 저장소(.git 폴더 존재)만 포함합니다."
  (let ((valid-silos '()))
    (dolist (repos-root denote-silo-repos-roots)
      (let* ((expanded-root (expand-file-name repos-root))
             (repo-dirs (when (file-directory-p expanded-root)
                          (directory-files expanded-root t "^[^.]"))))
        (dolist (repo-dir repo-dirs)
          ;; 심볼릭 링크 resolve
          (let* ((real-repo-dir (file-truename repo-dir))
                 (docs-path (expand-file-name "docs" real-repo-dir)))
            ;; Git 저장소 + docs 폴더 존재 시에만 추가
            (when (and (denote-silo--is-git-repo real-repo-dir)
                       (file-directory-p docs-path)
                       (not (member docs-path valid-silos)))
              (push docs-path valid-silos))))))
    (nreverse valid-silos)))

(defun denote-silo-setup-all ()
  "기본 Silo + 동적 발견 Silo 통합 설정.
Emacs 시작 시 또는 수동으로 호출하여 Silo 목록을 업데이트합니다."
  (interactive)
  (setq denote-silo-directories
        (delete-dups
         (append
          ;; 기본 디렉토리
          (mapcar #'expand-file-name denote-silo-base-directories)
          ;; 동적 발견 디렉토리
          (denote-silo-discover-repo-docs))))
  (message "Denote Silo initialized: %d directories" (length denote-silo-directories)))

(defun denote-silo-list-all ()
  "모든 등록된 Silo 목록을 상대 경로로 표시합니다."
  (interactive)
  (let ((silos denote-silo-directories))
    (with-current-buffer (get-buffer-create "*Denote Silos*")
      (erase-buffer)
      (insert "=== Registered Denote Silos ===\n\n")
      (insert (format "Total: %d directories\n\n" (length silos)))
      (dolist (silo silos)
        (let* ((display-name (denote-silo--format-display-name silo))
               (files (length (directory-files silo nil "\\.org$")))
               (is-git (denote-silo--is-git-repo silo))
               (git-marker (if is-git " [Git]" "")))
          (insert (format "• %s%s\n  Path: %s\n  Files: %d\n\n" 
                          display-name git-marker silo files))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer "*Denote Silos*")))

(defun denote-silo-count-files ()
  "모든 Silo의 파일 수를 계산하여 미니버퍼에 표시합니다."
  (interactive)
  (let ((total 0))
    (dolist (silo denote-silo-directories)
      (when (file-directory-p silo)
        (setq total (+ total (length (directory-files silo nil "\\.org$"))))))
    (message "Total Denote files across all silos: %d" total)))

;;;; Consult-notes Integration

(defun denote-silo-setup-consult-notes ()
  "Consult-notes에 Denote Silo 디렉토리를 짧은 이름으로 등록합니다.
상대 경로 형식으로 표시하여 가독성을 높입니다."
  (when (featurep 'consult-notes)
    (setq consult-notes-file-dir-sources
          (mapcar (lambda (silo)
                    (let ((display-name (denote-silo--format-display-name silo)))
                      (list display-name
                            ?d  ; 좁히기 키: 'd'
                            silo)))
                  denote-silo-directories))
    (message "Consult-notes sources updated: %d silos" (length denote-silo-directories))))

;;;; Auto-update on Emacs startup

(defun denote-silo-initialize ()
  "Denote Silo 시스템 초기화 (Emacs 시작 시 자동 실행)."
  (denote-silo-setup-all)
  (when (featurep 'consult-notes)
    (denote-silo-setup-consult-notes)))

;; Emacs 시작 시 자동 실행
(add-hook 'doom-after-init-hook #'denote-silo-initialize)

;;;; Interactive Commands

(defun denote-silo-refresh ()
  "Silo 목록을 다시 스캔하고 업데이트합니다."
  (interactive)
  (denote-silo-setup-all)
  (denote-silo-setup-consult-notes)
  (message "Denote Silo refreshed!"))

(defun denote-silo-find-file-all ()
  "모든 Silo에서 파일을 검색합니다 (상대 경로 표시).
파일 선택 시 'silo-name/filename.org' 형식으로 표시됩니다."
  (interactive)
  (let* ((all-files-alist
          (mapcan (lambda (silo)
                    (when (file-directory-p silo)
                      (let ((display-prefix (denote-silo--format-display-name silo)))
                        (mapcar (lambda (f)
                                  (cons (concat display-prefix "/" f)
                                        (expand-file-name f silo)))
                                (directory-files silo nil "\\.org$")))))
                  denote-silo-directories))
         (choice (completing-read "Find denote file: " 
                                  (mapcar #'car all-files-alist) 
                                  nil t))
         (file-path (cdr (assoc choice all-files-alist))))
    (find-file file-path)))

;;;; Search Functions

(defun denote-silo-grep-all (regexp)
  "모든 Silo에서 REGEXP로 grep 검색을 수행합니다."
  (interactive "sSearch in all silos: ")
  (grep-compute-defaults)
  (let ((dirs (mapconcat #'shell-quote-argument denote-silo-directories " ")))
    (rgrep regexp "*.org" dirs)))

;;; Provide

(provide '+denote-silo-dynamic)

;;; +denote-silo-dynamic.el ends here
