;;; test-ekg-config.el --- EKG recovery-copy safety -*- lexical-binding: t; -*-

;;; Commentary:

;; No personal database is opened.  SQLite fixtures live under temporary roots.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ekg-config)

(ert-deftest my/ekg-backup-rejects-missing-and-empty ()
  (let ((file (make-temp-file "ekg-empty-")))
    (unwind-protect
        (progn
          (should-error (my/ekg--verify-backup file))
          (delete-file file)
          (should-error (my/ekg--verify-backup file)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest my/ekg-backup-rejects-corrupt-copy ()
  (skip-unless (executable-find "sqlite3"))
  (let ((file (make-temp-file "ekg-corrupt-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "not a SQLite database"))
          (should-error (my/ekg--verify-backup file)))
      (delete-file file))))

(ert-deftest my/ekg-backup-roundtrip-and-local-routing ()
  (skip-unless (and (sqlite-available-p) (executable-find "sqlite3")))
  (let* ((root (make-temp-file "ekg-roundtrip-" t))
         (ekg-db-file (expand-file-name "shared.sqlite" root))
         (my/ekg-backup-directory (expand-file-name "local/" root))
         (backup-directory-alist nil)
         (version-control nil)
         (db (sqlite-open ekg-db-file)))
    (unwind-protect
        (progn
          (make-directory my/ekg-backup-directory)
          (sqlite-execute db "CREATE TABLE notes (text TEXT)")
          (sqlite-execute db "INSERT INTO notes VALUES (?)" '("과학 science"))
          (dotimes (_ 2)
            (my/ekg--backup-locally
             (lambda ()
               ;; Same SQLite backup mechanism as triples, not a live-file copy.
               (call-process "sqlite3" nil nil nil ekg-db-file
                             (format ".backup '%s'"
                                     (car (find-backup-file-name ekg-db-file)))))))
          (let ((copies (directory-files my/ekg-backup-directory t "~$")))
            (should (= (length copies) 2))
            (dolist (copy copies)
              (my/ekg--verify-backup copy)
              (let ((restored (sqlite-open copy)))
                (unwind-protect
                    (should (equal (sqlite-select restored "SELECT text FROM notes")
                                   '(("과학 science"))))
                  (sqlite-close restored)))))
          ;; EKG routing must not change backups for other files.
          (should-not backup-directory-alist)
          (should-not version-control))
      (sqlite-close db)
      (delete-directory root t))))

(ert-deftest my/ekg-backup-silent-failure-becomes-error ()
  (let* ((root (make-temp-file "ekg-failed-" t))
         (ekg-db-file (expand-file-name "db.sqlite" root))
         (my/ekg-backup-directory root))
    (unwind-protect
        (should-error (my/ekg--backup-locally (lambda () nil)))
      (delete-directory root t))))

(ert-deftest my/ekg-close-refuses-unsaved-note ()
  (let ((closed nil))
    (with-temp-buffer
      (setq-local ekg-capture-mode t)
      (insert "unsaved")
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'ekg-close) (lambda () (setq closed t))))
        (should-error (my/ekg-close-for-sync) :type 'user-error)
        (should-not closed)))))

(ert-deftest my/ekg-close-orders-backup-before-close ()
  (let ((ekg-db 'fixture)
        (events nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'ekg-backup)
               (lambda (force) (should force) (push 'backup events)))
              ((symbol-function 'ekg-close) (lambda () (push 'close events))))
      (my/ekg-close-for-sync)
      (should (equal (nreverse events) '(backup close))))))

(ert-deftest my/ekg-close-keeps-connection-on-backup-error ()
  (let ((ekg-db 'fixture)
        (closed nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'ekg-backup) (lambda (&rest _) (error "disk full")))
              ((symbol-function 'ekg-close) (lambda () (setq closed t))))
      (should-error (my/ekg-close-for-sync))
      (should-not closed))))

(ert-deftest my/ekg-index-acceptance-all-four-required ()
  (let ((ekg-db 'fixture))
    (cl-letf (((symbol-function 'sqlite-select)
               (lambda (&rest _) '(("subject_idx") ("subject_predicate_idx")
                                    ("predicate_object_idx")
                                    ("subject_predicate_object_properties_idx")))))
      (should (my/ekg--indexes-complete-p)))
    (cl-letf (((symbol-function 'sqlite-select)
               (lambda (&rest _) '(("subject_idx") ("subject_predicate_idx")
                                    ("predicate_object_idx")))))
      (should-not (my/ekg--indexes-complete-p)))))

(ert-deftest my/ekg-index-acceptance-reopens-once ()
  (let ((events nil)
        (attempts 0))
    (cl-letf (((symbol-function 'my/ekg--indexes-complete-p)
               (lambda () (= attempts 2)))
              ((symbol-function 'ekg-close) (lambda () (push 'close events))))
      (my/ekg--connect-checked (lambda () (cl-incf attempts) (push 'open events)))
      (should (equal (nreverse events) '(open close open))))))

(ert-deftest my/ekg-index-acceptance-no-reopen-when-complete ()
  (cl-letf (((symbol-function 'my/ekg--indexes-complete-p) (lambda () t))
            ((symbol-function 'ekg-close) (lambda () (ert-fail "Unexpected close"))))
    (should (eq (my/ekg--connect-checked (lambda () 'connected)) 'connected))))

(ert-deftest my/ekg-index-acceptance-fails-closed-after-two-retries ()
  (let ((attempts 0)
        (closed 0))
    (cl-letf (((symbol-function 'my/ekg--indexes-complete-p) (lambda () nil))
              ((symbol-function 'ekg-close) (lambda () (cl-incf closed))))
      (should-error (my/ekg--connect-checked (lambda () (cl-incf attempts)))))
    (should (= attempts 3))
    (should (= closed 3))))

(ert-deftest my/ekg-index-acceptance-converges-after-two-reopens ()
  (let ((attempts 0))
    (cl-letf (((symbol-function 'my/ekg--indexes-complete-p)
               (lambda () (= attempts 3)))
              ((symbol-function 'ekg-close) #'ignore))
      (my/ekg--connect-checked (lambda () (cl-incf attempts)))
      (should (= attempts 3)))))

(ert-deftest my/ekg-index-acceptance-skips-nested-connect ()
  (let ((my/ekg--connecting t))
    (cl-letf (((symbol-function 'my/ekg--indexes-complete-p)
               (lambda () (ert-fail "Nested schema check"))))
      (should (eq (my/ekg--connect-checked (lambda () 'nested)) 'nested)))))

(provide 'test-ekg-config)
;;; test-ekg-config.el ends here
