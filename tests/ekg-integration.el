;;; ekg-integration.el --- Installed EKG backup handoff probe -*- lexical-binding: t; -*-

;;; Commentary:

;; Tier C, opt-in.  Supply installed ekg/triples/llm/vui on load-path.
;; All DBs and backups are created under a fresh temporary directory.
;; Run separately from run-tests.sh; no personal EKG store is opened.

;;; Code:

(require 'cl-lib)
(require 'seq)
(defvar my/ekg-db-file nil)
(defvar my/ekg-backup-directory nil)

(let* ((root (make-temp-file "ekg-integration-" t))
       (user-emacs-directory (expand-file-name "emacs/" root))
       (my/ekg-db-file (expand-file-name "shared/new.sqlite" root))
       (my/ekg-backup-directory (expand-file-name "local/backups/" root)))
    (require 'ekg-config)
    (require 'org)
    (require 'ekg)
    ;; The user's first capture used bare hashtags: test syntax, not language.
    (dolist (case '(("#과학 #무엇 #science #what" . nil)
                    ("#[과학] #[무엇]인가 #[science] #[what]"
                     . ("과학" "무엇" "science" "what"))))
      (let ((note (ekg-note-create :id 42 :mode 'org-mode :text (car case))))
        (ekg--convert-inline-tags-to-links note)
        (ekg--populate-inline-tags note)
        (cl-assert (equal (ekg-note-tags note) (cdr case)))))
    (cl-assert (equal ekg-version "0.9.2"))
    ;; Check every callable bound by the EKG block against this release.
    ;; A plausible name in the manual is not proof the function exists.
    (let ((block (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name "../lisp/keybindings-config.el"
                                      (file-name-directory load-file-name)))
                   (goto-char (point-min))
                   (search-forward ";;;;; EKG\n")
                   (read (current-buffer)))))
      (cl-labels ((check (form)
                    (when (consp form)
                      (if (eq (car form) 'function)
                          (cl-assert (commandp (cadr form)) nil
                                     "EKG binding targets a missing command: %S"
                                     (cadr form))
                        (mapc #'check form)))))
        (check block)))
    (unwind-protect
        (progn
          (ekg-connect)
          (cl-assert (my/ekg--indexes-complete-p))
          (let ((config (triples-backups-configuration ekg-db)))
            (cl-assert (eq (plist-get config :strategy) 'every-change))
            (cl-assert (= (plist-get config :num-to-keep) 30)))
          (dotimes (i 35)
            (ekg-save-note
             (ekg-note-create :id 4242 :text (format "과학 science revision %d" i)
                              :mode 'org-mode :tags '("과학" "science"))))
          ;; Exercise the actual list renderer and edit-target resolution.
          ;; VUI 1.4 strips keyword colons; EKG 0.9.2 requires :ekg-note-id.
          (let ((note (ekg-get-note-with-id 4242))
                (opened nil))
            (with-temp-buffer
              (ekg--notes-mount "probe" (lambda () (list note)))
              (cl-assert (equal (get-text-property (point-min) :ekg-note-id) 4242))
              (cl-letf (((symbol-function 'ekg-edit)
                         (lambda (target) (setq opened target))))
                (ekg-notes-open))
              (cl-assert (equal note opened))))
          ;; Native buffer close aborts only unsaved edits, never the DB note.
          (let ((before (ekg-get-note-with-id 4242))
                (prompted nil))
            (ekg-edit before)
            (let ((edit-buffer (current-buffer)))
              (goto-char (point-max))
              (insert " unsaved edit to discard")
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (&rest _) (setq prompted t) t)))
                (kill-current-buffer))
              (cl-assert prompted)
              (cl-assert (not (buffer-live-p edit-buffer)))
              (cl-assert (equal before (ekg-get-note-with-id 4242)))))
          (let* (;; Query under the same backup routing as the production advice.
                 (backup-directory-alist `(("." . ,my/ekg-backup-directory)))
                 (copies (file-backup-file-names ekg-db-file))
                 (before (ekg-get-note-with-id 4242)))
            (cl-assert (= (length copies) 30))
            (dolist (copy copies) (my/ekg--verify-backup copy))
            (my/ekg-close-for-sync)
            (cl-assert (null ekg-db))
            ;; Forced handoff snapshot is the newest complete DB.
            (let ((latest (car (file-backup-file-names ekg-db-file)))
                  (restored (expand-file-name "restored.sqlite" root)))
              (copy-file latest restored)
              (setq ekg-db-file restored)
              (ekg-connect)
              (cl-assert (my/ekg--indexes-complete-p))
              (cl-assert (equal before (ekg-get-note-with-id 4242)))
              (cl-assert
               (equal (ekg-note-text (ekg-get-note-with-id 4242))
                      "과학 science revision 34"))
              (ekg-close)))
          (princ (format "PASS EKG %s: inline tags, four indexes, 35 writes, list/open, 30 copies, quick_check, close, backup readback\n" ekg-version))
          (princ (format "Temporary receipt root: %s\n" root)))
      (when ekg-db (ekg-close))))

;;; ekg-integration.el ends here
