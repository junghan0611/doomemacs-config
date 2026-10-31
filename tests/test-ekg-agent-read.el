;;; test-ekg-agent-read.el --- agent-ekg-* contract without EKG installed -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Tier A: runs under `emacs -Q', where EKG is NOT on load-path.  Covers
;; argument validation, store checks that must happen before any connect,
;; and the failure classification.  The installed-package behaviour (AND
;; queries, typed reads, counts, connection handling) is the opt-in
;; tests/ekg-agent-read-integration.el.  No personal database is opened.

;;; Code:

(require 'test-helper)
(require 'cl-lib)
(require 'ekg-agent-read)
(require 'ekg-config)

(defconst test-ekg-agent/server-file
  (expand-file-name
   "bin/agent-server.el"
   (file-name-directory
    (directory-file-name
     (file-name-directory (or load-file-name buffer-file-name)))))
  "Path to the shipping agent-server.el source.")

(defmacro test-ekg-agent/with-store (var &rest body)
  "Bind VAR to a path under a fresh temporary root, then run BODY."
  (declare (indent 1))
  `(let* ((root (make-temp-file "ekg-agent-" t))
          (,var (expand-file-name "store/ekg.sqlite" root)))
     (unwind-protect
         (let ((my/ekg-agent-db-file ,var))
           ,@body)
       (delete-directory root t))))

(defun test-ekg-agent/error-kind (result)
  "The failure kind of RESULT, asserting it is a failure."
  (should (eq (plist-get result :ok) nil))
  (should (stringp (plist-get result :message)))
  (plist-get result :error))

;;;; Store checks happen before connecting

(ert-deftest test-ekg-agent/store-missing-creates-nothing ()
  (test-ekg-agent/with-store file
    (dolist (result (list (my/ekg-agent-notes "과학")
                          (my/ekg-agent-read 1)
                          (my/ekg-agent-tags)))
      (should (eq (test-ekg-agent/error-kind result) 'store-missing))
      (should (null (plist-get result :source))))
    (should-not (file-exists-p file))
    (should-not (file-exists-p (file-name-directory file)))))

(ert-deftest test-ekg-agent/store-invalid-is-not-opened ()
  (test-ekg-agent/with-store file
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "half-synced, not a database"))
    (let ((before (file-attribute-modification-time (file-attributes file)))
          (result (my/ekg-agent-notes "과학")))
      (should (eq (test-ekg-agent/error-kind result) 'store-invalid))
      (should (equal (plist-get (plist-get result :source) :db) file))
      (should (equal before (file-attribute-modification-time (file-attributes file)))))))

(ert-deftest test-ekg-agent/package-unavailable-without-ekg ()
  (skip-unless (and (sqlite-available-p) (not (featurep 'ekg))))
  (test-ekg-agent/with-store file
    (make-directory (file-name-directory file) t)
    ;; SQLite writes no header until the first write; an empty file is
    ;; correctly `store-invalid', so give the fixture one table.
    (let ((db (sqlite-open file)))
      (sqlite-execute db "CREATE TABLE fixture (x)")
      (sqlite-close db))
    (let ((load-path nil))
      (should (eq (test-ekg-agent/error-kind (my/ekg-agent-tags))
                  'package-unavailable)))))

;;;; A failure stays classified even when provenance cannot be built

(ert-deftest test-ekg-agent/bad-path-setting-still-classified ()
  (let ((my/ekg-agent-db-file 42))
    (let ((result (my/ekg-agent-tags)))
      (should (eq (test-ekg-agent/error-kind result) 'read-failed))
      (should (null (plist-get result :source))))))

;;;; Arguments are rejected, never clamped

(ert-deftest test-ekg-agent/rejects-bad-tags ()
  (test-ekg-agent/with-store _file
    (dolist (tags '(nil "" ("과학" "") (과학) 42 ("과학" . "무엇")))
      (should (eq (test-ekg-agent/error-kind (my/ekg-agent-notes tags))
                  'invalid-argument)))))

(ert-deftest test-ekg-agent/rejects-out-of-range-pages ()
  (test-ekg-agent/with-store _file
    (dolist (page '((0 0) (101 0) (20 -1) (2.5 0) ("20" 0)))
      (should (eq (test-ekg-agent/error-kind
                   (apply #'my/ekg-agent-notes "과학" page))
                  'invalid-argument)))
    (dolist (page '((0 0) (501 0) (100 -1)))
      (should (eq (test-ekg-agent/error-kind (apply #'my/ekg-agent-tags page))
                  'invalid-argument)))))

(ert-deftest test-ekg-agent/read-id-keeps-type ()
  (test-ekg-agent/with-store _file
    (dolist (id '(nil "" 4.2 (4242) 과학))
      (should (eq (test-ekg-agent/error-kind (my/ekg-agent-read id))
                  'invalid-argument)))
    ;; Valid ids of either type get past validation to the store check.
    (dolist (id '(4242 "4242" "https://example.org/page"))
      (should (eq (test-ekg-agent/error-kind (my/ekg-agent-read id))
                  'store-missing)))))

;;;; Wiring

(ert-deftest test-ekg-agent/store-path-matches-gui-config ()
  "The daemon does not load ekg-config.el; the two paths must agree."
  (should (equal my/ekg-agent-db-file my/ekg-db-file)))

(ert-deftest test-ekg-agent/agent-server-exposes-the-doors ()
  "bin/agent-server.el loads the reader and defines the three doors."
  (let* ((source test-ekg-agent/server-file)
         (forms (with-temp-buffer
                  (let ((coding-system-for-read 'utf-8))
                    (insert-file-contents source))
                  (goto-char (point-min))
                  (cl-loop for form = (condition-case nil (read (current-buffer))
                                        (end-of-file nil))
                           while form collect form)))
         (doors (cl-loop for form in forms
                         when (eq (car-safe form) 'defun)
                         collect (cons (nth 1 form) (car (last form))))))
    (should (string-match-p "lisp/ekg-agent-read\\.el" (prin1-to-string forms)))
    (pcase-dolist (`(,door . ,target) '((agent-ekg-notes . my/ekg-agent-notes)
                                        (agent-ekg-read . my/ekg-agent-read)
                                        (agent-ekg-tags . my/ekg-agent-tags)))
      (should (eq (car-safe (alist-get door doors)) target)))))

;;; test-ekg-agent-read.el ends here
