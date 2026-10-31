;;; $DOOMDIR/lisp/ekg-agent-read.el --- EKG queries behind the agent-server RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Logic for the `agent-ekg-*' doors in bin/agent-server.el (issue #12).
;; The care bots on Oracle read GLG's EKG notes through these; tags combine
;; as clues (`과학' + `무엇' = notes carrying both).
;;
;; EKG's own API does the reading — no private SQLite parser.  Each call
;; binds a fresh `ekg-db', so a connection the caller already holds is left
;; alone, and closes it before returning.
;;
;; "Read" means these functions never save, tag, trash, or delete a note.
;; It does NOT mean the file stays byte-identical.  Measured 2026-10-31
;; (EKG 0.9.2, triples abc1842): the first `ekg-connect' in each Emacs
;; process runs `ekg-upgrade-db', whose tail re-registers five core note
;; types — five write transactions (SQLite file change counter +5) with
;; identical rows.  Later connects in the same process wrote nothing.  So
;; a daemon changes the synced file's bytes once, on its first EKG call.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar ekg-db)
(defvar ekg-db-file)
(defvar ekg-hidden-tags)
(defvar ekg-version)
(declare-function ekg-connect "ekg")
(declare-function ekg-close "ekg")
(declare-function ekg-get-notes-with-tags "ekg")
(declare-function ekg-get-note-with-id "ekg")
(declare-function ekg-note-active-p "ekg")
(declare-function ekg-tags "ekg")
(declare-function ekg-note-id "ekg")
(declare-function ekg-note-text "ekg")
(declare-function ekg-note-mode "ekg")
(declare-function ekg-note-tags "ekg")
(declare-function ekg-note-creation-time "ekg")
(declare-function ekg-note-modified-time "ekg")
(declare-function ekg-note-inlines "ekg")
(declare-function triples-get-subject "triples")
(declare-function triples-get-types "triples")
(declare-function triples-get-type "triples")
(declare-function triples-subjects-of-type "triples")

;;;; Settings

(defvar my/ekg-agent-db-file (expand-file-name "~/org/ekg/ekg-2026.sqlite")
  "EKG store the agent RPC reads.
Must equal `my/ekg-db-file' in ekg-config.el; the daemon does not load
that file.  tests/test-ekg-agent-read.el guards the two against drift.")

(defconst my/ekg-agent-preview-chars 200
  "Characters of raw note text shown per note in a listing.")

(define-error 'my/ekg-agent-error "EKG agent read failed")

;;;; Failure and provenance

(defun my/ekg-agent--fail (kind format-string &rest args)
  "Signal a classified failure KIND with a message from FORMAT-STRING and ARGS."
  (signal 'my/ekg-agent-error
          (list kind (apply #'format format-string args))))

(defun my/ekg-agent--iso (time)
  "Format TIME as ISO 8601 with offset, or nil."
  (when time
    (format-time-string "%FT%T%:z" time)))

(defun my/ekg-agent--source (file)
  "Describe FILE and this reading: where, which host, how fresh."
  (let ((attrs (file-attributes file)))
    (list :db file
          :host (system-name)
          :mtime (my/ekg-agent--iso (file-attribute-modification-time attrs))
          :size (file-attribute-size attrs)
          :read-at (my/ekg-agent--iso (current-time))
          :ekg-version (bound-and-true-p ekg-version)
          :hidden-tags (bound-and-true-p ekg-hidden-tags)
          :freshness "mtime/size of this host's file at :read-at; not proof that Syncthing finished")))

(defun my/ekg-agent--sqlite-file-p (file)
  "Non-nil when FILE starts with the SQLite magic header."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 16)
    (equal (buffer-string) "SQLite format 3\0")))

(defun my/ekg-agent--run (body)
  "Call BODY and return its plist, or a classified failure plist.
Failures never look like an empty result: they carry `:ok nil' and an
`:error' kind."
  (condition-case err
      (funcall body)
    (my/ekg-agent-error
     (list :ok nil :error (nth 1 err) :message (nth 2 err)
           :source (my/ekg-agent--failure-source)))
    (error
     (list :ok nil :error 'read-failed :message (error-message-string err)
           :source (my/ekg-agent--failure-source)))))

(defun my/ekg-agent--failure-source ()
  "Best-effort provenance for a failure; nil rather than a second error.
A bad path setting must not replace the original classification."
  (ignore-errors
    (let ((file my/ekg-agent-db-file))
      (when (file-exists-p file)
        (my/ekg-agent--source file)))))

(defun my/ekg-agent--with-store (fn)
  "Call FN on a fresh EKG connection to `my/ekg-agent-db-file'.
The store is checked before connecting, so a missing file is reported
instead of being created empty.  Returns FN's plist with `:ok' and
`:source' added."
  (let ((file my/ekg-agent-db-file))
    (unless (file-regular-p file)
      (my/ekg-agent--fail 'store-missing "No EKG store at %s" file))
    (unless (my/ekg-agent--sqlite-file-p file)
      (my/ekg-agent--fail 'store-invalid "Not a SQLite database: %s" file))
    (condition-case err
        (require 'ekg)
      (error (my/ekg-agent--fail 'package-unavailable "Cannot load EKG: %s"
                                 (error-message-string err))))
    (let ((ekg-db nil)
          (ekg-db-file file))
      (unwind-protect
          (progn
            (ekg-connect)
            (append (list :ok t)
                    (funcall fn)
                    (list :source (my/ekg-agent--source file))))
        (when ekg-db (ekg-close))))))

;;;; Arguments

(defun my/ekg-agent--page (limit offset default maximum)
  "Validate LIMIT and OFFSET; LIMIT defaults to DEFAULT, capped at MAXIMUM.
Out-of-range values are rejected, never silently clamped."
  (let ((limit (or limit default))
        (offset (or offset 0)))
    (unless (and (natnump limit) (<= 1 limit maximum))
      (my/ekg-agent--fail 'invalid-argument
                          "LIMIT must be an integer in 1..%d, got %S" maximum limit))
    (unless (natnump offset)
      (my/ekg-agent--fail 'invalid-argument
                          "OFFSET must be a non-negative integer, got %S" offset))
    (cons limit offset)))

(defun my/ekg-agent--tags-arg (tags)
  "Return TAGS as a non-empty list of non-empty strings.
A single string is accepted as one tag."
  (let ((tags (if (stringp tags) (list tags) tags)))
    (unless (and (consp tags)
                 (proper-list-p tags)
                 (seq-every-p (lambda (tag) (and (stringp tag) (not (string-empty-p tag))))
                              tags))
      (my/ekg-agent--fail 'invalid-argument
                          "TAGS must be a tag string or a list of them, got %S" tags))
    tags))

(defun my/ekg-agent--slice (items page)
  "Return the PAGE (LIMIT . OFFSET) slice of ITEMS."
  (seq-take (seq-drop items (cdr page)) (car page)))

;;;; Note shapes

(defun my/ekg-agent--note< (a b)
  "Newest creation first; ties broken by printed id for a stable order."
  (let ((ta (or (ekg-note-creation-time a) 0))
        (tb (or (ekg-note-creation-time b) 0)))
    (if (/= ta tb)
        (> ta tb)
      (string< (format "%S" (ekg-note-id a)) (format "%S" (ekg-note-id b))))))

(defun my/ekg-agent--note-summary (note)
  "Listing entry for NOTE: metadata plus a raw-text preview."
  (let ((text (or (ekg-note-text note) "")))
    (list :id (ekg-note-id note)
          :tags (ekg-note-tags note)
          :created (my/ekg-agent--iso (ekg-note-creation-time note))
          :modified (my/ekg-agent--iso (ekg-note-modified-time note))
          :preview (substring text 0 (min (length text) my/ekg-agent-preview-chars))
          :length (length text))))

(defun my/ekg-agent--tagged (tag)
  "Ids of all subjects tagged TAG, hidden or not."
  (plist-get (triples-get-type ekg-db tag 'tag) :tagged))

(defun my/ekg-agent--text-note-ids ()
  "Set of ids that are text notes — the subjects `my/ekg-agent-read' accepts.
EKG's getters turn any subject into a note struct, including a tagged
subject that never had text; listings and counts keep to this set so an
id handed out by a listing is always readable."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (id (triples-subjects-of-type ekg-db 'text) table)
      (puthash id t table))))

;;;; Queries

(defun my/ekg-agent-notes (tags &optional limit offset)
  "Notes carrying every tag in TAGS, newest first, one page at a time.
See `agent-ekg-notes' for the contract."
  (my/ekg-agent--run
   (lambda ()
     (let ((tags (my/ekg-agent--tags-arg tags))
           (page (my/ekg-agent--page limit offset 20 100)))
       (my/ekg-agent--with-store
        (lambda ()
          (let* ((text-ids (my/ekg-agent--text-note-ids))
                 (notes (sort (seq-filter (lambda (note) (gethash (ekg-note-id note) text-ids))
                                          (ekg-get-notes-with-tags tags))
                              #'my/ekg-agent--note<)))
            (list :tags tags
                  :match 'all
                  :hidden-rule "notes with a :hidden-tags tag are left out unless that tag is requested"
                  :total (length notes)
                  :limit (car page)
                  :offset (cdr page)
                  :notes (mapcar #'my/ekg-agent--note-summary
                                 (my/ekg-agent--slice notes page))))))))))

(defun my/ekg-agent-read (id)
  "The complete stored note with ID, exactly as saved.
See `agent-ekg-read' for the contract."
  (my/ekg-agent--run
   (lambda ()
     (unless (or (integerp id) (and (stringp id) (not (string-empty-p id))))
       (my/ekg-agent--fail 'invalid-argument
                           "ID must be an integer or a non-empty string, got %S" id))
     (my/ekg-agent--with-store
      (lambda ()
        (unless (triples-get-subject ekg-db id)
          (my/ekg-agent--fail 'not-found "No EKG subject with id %S" id))
        (unless (memq 'text (triples-get-types ekg-db id))
          (my/ekg-agent--fail 'not-a-note "Subject %S is not a text note" id))
        (let* ((note (ekg-get-note-with-id id))
               (text (or (ekg-note-text note) "")))
          (list :note
                (list :id (ekg-note-id note)
                      :text text
                      :mode (ekg-note-mode note)
                      :tags (ekg-note-tags note)
                      :created (my/ekg-agent--iso (ekg-note-creation-time note))
                      :modified (my/ekg-agent--iso (ekg-note-modified-time note))
                      :hidden (not (ekg-note-active-p note))
                      :inline-count (length (ekg-note-inlines note))))))))))

(defun my/ekg-agent-tags (&optional limit offset)
  "Content tags with note counts, most used first.
See `agent-ekg-tags' for the contract."
  (my/ekg-agent--run
   (lambda ()
     (let ((page (my/ekg-agent--page limit offset 100 500)))
       (my/ekg-agent--with-store
        (lambda ()
          (let* ((text-ids (my/ekg-agent--text-note-ids))
                 (hidden-ids (seq-uniq (seq-mapcat #'my/ekg-agent--tagged ekg-hidden-tags)))
                 (rows (mapcar
                        (lambda (tag)
                          (let ((ids (seq-filter (lambda (id) (gethash id text-ids))
                                                 (my/ekg-agent--tagged tag))))
                            (list :tag tag
                                  :count (seq-count (lambda (id) (not (member id hidden-ids))) ids)
                                  :hidden-count (seq-count (lambda (id) (member id hidden-ids)) ids))))
                        (ekg-tags)))
                 (sorted (sort rows
                               (lambda (a b)
                                 (let ((ca (plist-get a :count))
                                       (cb (plist-get b :count)))
                                   (if (/= ca cb)
                                       (> ca cb)
                                     (string< (plist-get a :tag) (plist-get b :tag))))))))
            (list :total (length sorted)
                  :limit (car page)
                  :offset (cdr page)
                  :count-rule "text notes carrying the tag, excluding notes with any :hidden-tags tag; same rule as agent-ekg-notes with that one tag"
                  :tags (my/ekg-agent--slice sorted page)))))))))

(provide 'ekg-agent-read)
;;; ekg-agent-read.el ends here
