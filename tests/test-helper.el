;;; test-helper.el --- Test helper for doomemacs-config -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>

;;; Commentary:

;; Common test setup and helper functions for doomemacs-config tests.
;; This file should be loaded before running any tests.

;;; Code:

(require 'ert)

;; Add parent directory to load-path
(let ((parent-dir (file-name-directory
                   (directory-file-name
                    (file-name-directory load-file-name)))))
  (add-to-list 'load-path parent-dir)
  (add-to-list 'load-path (file-name-directory load-file-name)))

;;;; Helper Functions

(defun test-helper/with-temp-dir (body-fn)
  "Create temporary directory, execute BODY-FN with it, then cleanup."
  (let ((temp-dir (make-temp-file "doomemacs-test-" t)))
    (unwind-protect
        (funcall body-fn temp-dir)
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(defun test-helper/with-temp-file (content body-fn)
  "Create temporary file with CONTENT, execute BODY-FN, then cleanup."
  (let ((temp-file (make-temp-file "doomemacs-test-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (funcall body-fn temp-file))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun test-helper/create-test-org-file (dir filename)
  "Create a test org file in DIR with FILENAME."
  (let ((file-path (expand-file-name filename dir)))
    (with-temp-file file-path
      (insert "#+title: Test File\n")
      (insert "#+date: [2025-11-15 Fri]\n")
      (insert "\n* Test Content\n"))
    file-path))

(provide 'test-helper)
;;; test-helper.el ends here
