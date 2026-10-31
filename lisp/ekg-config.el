;;; $DOOMDIR/lisp/ekg-config.el --- Standalone EKG with verified backups -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; EKG 0.9.2 only: new database, Org capture, native triples backups.
;; No Denote export, embeddings, LLMs, agents, or agenda integration.
;; Syncthing is transport, not SQLite replication: close before switching hosts.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'xdg)

(defvar ekg-db nil)
(defvar ekg-db-file nil)

(defvar my/ekg-db-file (expand-file-name "~/org/ekg/ekg-2026.sqlite")
  "New EKG database; never implicitly reuse the previous ekg.db.")

(defvar my/ekg-backup-directory
  (expand-file-name "ekg/backups/" (xdg-state-home))
  "Local recovery copies, outside the Syncthing Org tree.")

;; Set before EKG loads, avoiding its obsolete/default database discovery.
(setq ekg-db-file my/ekg-db-file)

;;;; Fresh-store acceptance

(defvar my/ekg--connecting nil)

(defun my/ekg--indexes-complete-p ()
  "Check that the live triples table has all four expected indexes.
This reads schema metadata only; it does not repair the database."
  (let ((names (mapcar #'car
                      (sqlite-select ekg-db
                                     "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='triples'"))))
    (seq-every-p (lambda (name) (member name names))
                 '("subject_idx" "subject_predicate_idx" "predicate_object_idx"
                   "subject_predicate_object_properties_idx"))))

(defun my/ekg--connect-checked (fn &rest args)
  "Connect via FN with ARGS, accepting only a complete triples index set.
Measured 2026-10-31: EKG 0.9.2's fresh-store 0.3 upgrade with triples
abc1842 loses all four indexes through a table-rename/name collision.
At most two close/reopen cycles before the first user write restore them
via upstream setup (initial schema writes can already contain duplicates).
Never patch triples or retry beyond that bound.  Retire this advice once
installed-package probes prove BOTH upgrade and automatic-recovery rebuilds
preserve all four indexes.  Do not force-upgrade a live working database;
rehearse maintenance on a copy after taking a verified backup."
  (if my/ekg--connecting
      (apply fn args)
    (let ((my/ekg--connecting t)
          (reopens 0))
      (prog1 (apply fn args)
        (while (and (not (my/ekg--indexes-complete-p)) (< reopens 2))
          (ekg-close)
          (setq reopens (1+ reopens))
          (apply fn args))
        (unless (my/ekg--indexes-complete-p)
          (ekg-close)
          (error "EKG index acceptance failed after two reopens; preserve DB and inspect"))))))

;;;; Backup safety

(defun my/ekg--verify-backup (file)
  "Require a nonempty SQLite backup FILE that passes quick_check.
Use read-only sqlite3 access; never repair or rewrite a recovery copy."
  (unless (and (file-regular-p file)
               (> (file-attribute-size (file-attributes file)) 0))
    (error "EKG backup was not created: %s" file))
  (with-temp-buffer
    (let ((status (call-process "sqlite3" nil t nil "-readonly" file
                                "PRAGMA quick_check;")))
      (unless (and (equal status 0)
                   (equal (string-trim (buffer-string)) "ok"))
        (error "EKG backup verification failed for %s: %s"
               file (string-trim (buffer-string)))))))

(defun my/ekg--backup-locally (fn &rest args)
  "Run EKG backup FN with ARGS using numbered, locally verified copies.
Our fresh database uses every-change backups.  Verify the expected new file
because triples-backup itself does not check sqlite3's exit status.
A backup error can occur AFTER the note has been committed to the live DB."
  (let* ((backup-directory-alist `(("." . ,my/ekg-backup-directory)))
         (version-control t)
         ;; triples owns retention; do not also prune via Emacs defaults.
         (delete-old-versions 'never)
         (file (car (find-backup-file-name ekg-db-file))))
    (prog1 (apply fn args)
      (my/ekg--verify-backup file))))

(defun my/ekg-close-for-sync ()
  "Back up and close EKG before a manual Syncthing host handoff.
Refuse unsaved capture/edit buffers.  After closing on ALL Emacs instances,
wait for Syncthing to finish on BOTH hosts before opening EKG on the other.
This command neither runs Syncthing nor certifies its completion."
  (interactive)
  (require 'ekg)
  (when (seq-some
         (lambda (buffer)
           (with-current-buffer buffer
             (and (or (bound-and-true-p ekg-capture-mode)
                      (bound-and-true-p ekg-edit-mode))
                  (buffer-modified-p))))
         (buffer-list))
    (user-error "Finish or abort unsaved EKG captures/edits before switching hosts"))
  (when ekg-db
    (ekg-backup t)
    (ekg-close))
  (message "EKG closed here; wait for Syncthing on both hosts before reopening"))

;;;; Lazy package configuration

(with-eval-after-load 'ekg
  (unless (executable-find "sqlite3")
    (error "EKG recovery copies require sqlite3 on PATH"))
  (make-directory (file-name-directory my/ekg-db-file) t)
  (make-directory my/ekg-backup-directory t)
  (setq ekg-db-file my/ekg-db-file
        ekg-capture-default-mode 'org-mode
        ekg-confirm-on-buffer-kill t
        ekg-truncation-method 'character
        ;; Safety policy chosen 2026-10-31: small initial DB, every change,
        ;; 30 local recovery points.  Revisit cost after actual use, not now.
        ekg-default-num-backups 30
        ekg-default-backups-strategy 'every-change)
  (advice-add 'ekg-backup :around #'my/ekg--backup-locally)
  (advice-add 'ekg-connect :around #'my/ekg--connect-checked)
  (add-hook 'kill-emacs-hook #'ekg-close))

(provide 'ekg-config)
;;; ekg-config.el ends here
