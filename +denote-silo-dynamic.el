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
  '("~/repos/gh")
  "리포지토리 루트 디렉토리 목록.
각 디렉토리 하위의 */docs 폴더를 Silo로 자동 등록합니다."
  :group 'denote-silo
  :type '(repeat directory))

(defcustom denote-silo-base-directories
  '("~/claude-memory"
    "~/org"
    "~/org/llmlog")
  "기본 Denote Silo 디렉토리 목록.
리포지토리 발견과 별개로 항상 포함됩니다."
  :group 'denote-silo
  :type '(repeat directory))

(defun denote-silo-discover-repo-docs ()
  "리포지토리 루트에서 */docs 디렉토리를 자동으로 발견하여 반환.
심볼릭 링크는 실제 경로로 resolve됩니다."
  (let ((valid-silos '()))
    (dolist (repos-root denote-silo-repos-roots)
      (let* ((expanded-root (expand-file-name repos-root))
             (repo-dirs (when (file-directory-p expanded-root)
                          (directory-files expanded-root t "^[^.]"))))
        (dolist (repo-dir repo-dirs)
          ;; 심볼릭 링크 resolve
          (let* ((real-repo-dir (file-truename repo-dir))
                 (docs-path (expand-file-name "docs" real-repo-dir)))
            (when (and (file-directory-p docs-path)
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
  "모든 등록된 Silo 목록을 표시합니다."
  (interactive)
  (let ((silos denote-silo-directories))
    (with-current-buffer (get-buffer-create "*Denote Silos*")
      (erase-buffer)
      (insert "=== Registered Denote Silos ===\n\n")
      (insert (format "Total: %d directories\n\n" (length silos)))
      (dolist (silo silos)
        (let* ((name (file-name-nondirectory (directory-file-name silo)))
               (files (length (directory-files silo nil "\\.org$"))))
          (insert (format "• %s\n  Path: %s\n  Files: %d\n\n" name silo files))))
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
  "Consult-notes에 Denote Silo 디렉토리를 등록합니다."
  (when (featurep 'consult-notes)
    (setq consult-notes-file-dir-sources
          (mapcar (lambda (silo)
                    (let ((name (file-name-nondirectory (directory-file-name silo))))
                      (list name
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
  "모든 Silo에서 파일을 검색합니다 (기본 find-file 인터페이스)."
  (interactive)
  (let* ((all-files (mapcan (lambda (silo)
                              (when (file-directory-p silo)
                                (mapcar (lambda (f) (expand-file-name f silo))
                                        (directory-files silo nil "\\.org$"))))
                            denote-silo-directories))
         (file (completing-read "Find denote file: " all-files nil t)))
    (find-file file)))

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
