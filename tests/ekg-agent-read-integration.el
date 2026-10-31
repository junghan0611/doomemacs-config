;;; ekg-agent-read-integration.el --- Installed EKG behind agent-ekg-* -*- lexical-binding: t; -*-

;;; Commentary:

;; Tier C, opt-in (issue #12).  Supply installed ekg/triples/llm/vui/compat
;; and this repo's lisp/ on load-path:
;;
;;   B=~/doomemacs/.local/straight/build-31.1
;;   emacs -Q --batch -L $B/ekg -L $B/triples -L $B/llm -L $B/vui \
;;     -L $B/compat -L lisp -l tests/ekg-agent-read-integration.el
;;
;; The store is seeded by a child Emacs through ekg-config.el, as the GUI
;; writes it.  This process then plays the agent-server daemon: it never
;; loads ekg-config, takes the public `agent-ekg-*' doors from the shipping
;; bin/agent-server.el source, and loads EKG only through them.  All files
;; live under a fresh temporary root; no personal store is opened.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar ekg-db)
(defvar ekg-version)
(defvar my/ekg-agent-db-file)

(defconst ekg-agent-it/this-file (or load-file-name buffer-file-name))

(defconst ekg-agent-it/notes
  ;; id, text, tags, creation time
  '((101 "과학만" ("과학" "date/2026-10-31") 1000)
    (102 "무엇만" ("무엇" "date/2026-10-30") 1500)
    (103 "과학과 무엇" ("과학" "무엇" "date/2026-10-31") 2000)
    (104 "버린 노트" ("과학" "무엇" "trash") 3000)
    ("https://example.org/what" "문자열 id 노트" ("무엇") 1500)))

;;;; Child: seed the store the way the GUI does

(when-let* ((seed (getenv "EKG_AGENT_IT_SEED")))
  (defvar my/ekg-db-file seed)
  (defvar my/ekg-backup-directory (getenv "EKG_AGENT_IT_BACKUPS"))
  (require 'ekg-config)
  (require 'org)
  (require 'ekg)
  (ekg-connect)
  (cl-assert (my/ekg--indexes-complete-p))
  (pcase-dolist (`(,id ,text ,tags ,created) ekg-agent-it/notes)
    ;; `ekg-note-create' takes no creation time; the struct constructor does,
    ;; and `ekg-save-note' keeps a non-nil one.
    (ekg-save-note (make-ekg-note :id id :text text :mode 'org-mode
                                  :tags tags :creation-time created)))
  ;; A tagged subject with no text, and a tag no note carries.
  (triples-set-type ekg-db "https://example.org/bare" 'tagged :tag '("과학"))
  (triples-set-type ekg-db "고아" 'tag)
  (ekg-close)
  (kill-emacs 0))

;;;; Parent: the daemon side

(defun ekg-agent-it/counter (file)
  "SQLite file change counter of FILE (header bytes 24-27)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 24 28)
    (seq-reduce (lambda (acc byte) (+ (* acc 256) byte)) (buffer-string) 0)))

(defun ekg-agent-it/rows (file)
  "Every triple of FILE, read through a separate read-only connection."
  (let ((db (sqlite-open file t)))
    (unwind-protect
        (sqlite-select db "SELECT subject, predicate, object, properties FROM triples ORDER BY 1, 2, 3, 4")
      (sqlite-close db))))

(defun ekg-agent-it/ids (result)
  (cl-assert (eq (plist-get result :ok) t) nil "Expected success: %S" result)
  (mapcar (lambda (note) (plist-get note :id)) (plist-get result :notes)))

(defun ekg-agent-it/kind (result)
  (cl-assert (eq (plist-get result :ok) nil) nil "Expected failure: %S" result)
  (plist-get result :error))

(let* ((root (make-temp-file "ekg-agent-it-" t))
       (store (expand-file-name "shared/ekg.sqlite" root))
       (repo (expand-file-name ".." (file-name-directory ekg-agent-it/this-file)))
       (process-environment
        (append (list (concat "EKG_AGENT_IT_SEED=" store)
                      (concat "EKG_AGENT_IT_BACKUPS=" (expand-file-name "local/backups/" root)))
                process-environment)))
  (cl-assert (not (featurep 'ekg)))
  (let ((status (apply #'call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil (list :file (expand-file-name "seed.log" root)) nil
                       "-Q" "--batch"
                       (append (mapcan (lambda (dir) (list "-L" dir)) load-path)
                               (list "-l" ekg-agent-it/this-file)))))
    (cl-assert (eq status 0) nil "Seeding failed; see %s/seed.log" root))
  (setq process-environment (cddr process-environment))

  ;; The reader load and the public doors, run from the shipping source
  ;; rather than retyped.  Booting the whole daemon would read ~/org.
  (cl-assert (not (featurep 'ekg-agent-read)))
  (defvar doom-user-dir)
  (let ((doom-user-dir repo))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents (expand-file-name "bin/agent-server.el" repo)))
      (goto-char (point-min))
      (let (loaders doors)
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (cond
                 ((and (eq (car-safe form) 'let)
                       (string-search "lisp/ekg-agent-read.el" (prin1-to-string form)))
                  (eval form t)
                  (push form loaders))
                 ((and (eq (car-safe form) 'defun)
                       (string-prefix-p "agent-ekg-" (symbol-name (nth 1 form))))
                  (eval form t)
                  (push (nth 1 form) doors)))))
          (end-of-file nil))
        (cl-assert (= (length loaders) 1))
        (cl-assert (featurep 'ekg-agent-read) nil "The server's load form did not load the reader")
        (cl-assert (equal (sort doors #'string<)
                          '(agent-ekg-notes agent-ekg-read agent-ekg-tags))))))

  (let ((my/ekg-agent-db-file store)
        (seeded-rows (ekg-agent-it/rows store))
        (handles nil))
    (unwind-protect
        (progn
          ;; First EKG connect in this process: upgrade bookkeeping writes,
          ;; rows do not change.  Later calls write nothing.
          (let* ((c0 (ekg-agent-it/counter store))
                 (first (agent-ekg-tags))
                 (c1 (ekg-agent-it/counter store))
                 (second (agent-ekg-tags))
                 (c2 (ekg-agent-it/counter store)))
            (cl-assert (plist-get first :ok))
            (cl-assert (equal first (plist-put (copy-sequence second) :source (plist-get first :source))))
            (cl-assert (= c1 c2) nil "Second read wrote: %d -> %d" c1 c2)
            (cl-assert (equal seeded-rows (ekg-agent-it/rows store)))
            (princ (format "first-connect change counter +%d (EKG %s); second +0\n"
                           (- c1 c0) (bound-and-true-p ekg-version))))

          (advice-add 'ekg-connect :after (lambda (&rest _) (push ekg-db handles))
                      '((name . ekg-agent-it-capture)))

          ;; Combination of clues: AND, newest first, trash left out.
          (cl-assert (equal (ekg-agent-it/ids (agent-ekg-notes '("과학" "무엇"))) '(103)))
          (cl-assert (equal (ekg-agent-it/ids (agent-ekg-notes "무엇"))
                            '(103 "https://example.org/what" 102)))
          (cl-assert (equal (ekg-agent-it/ids (agent-ekg-notes "date/2026-10-31")) '(103 101)))
          (cl-assert (equal (ekg-agent-it/ids (agent-ekg-notes '("과학" "무엇" "trash"))) '(104)))
          (cl-assert (null (ekg-agent-it/ids (agent-ekg-notes '("과학" "없는태그")))))
          (let ((empty (agent-ekg-notes "없는태그")))
            (cl-assert (and (eq (plist-get empty :ok) t) (= (plist-get empty :total) 0))))
          ;; EKG's getter returns the text-less tagged subject as a note; the
          ;; listing leaves it out so every listed id is readable.
          (cl-assert (equal (ekg-agent-it/ids (agent-ekg-notes "과학")) '(103 101)))
          (let ((science (agent-ekg-notes "과학" 1 1)))
            (cl-assert (= (plist-get science :total) 2))
            (cl-assert (equal (ekg-agent-it/ids science) '(101))))
          (cl-assert (null (ekg-agent-it/ids (agent-ekg-notes "과학" 20 9))))
          (let ((listing (car (plist-get (agent-ekg-notes "과학" 1 0) :notes))))
            (cl-assert (equal (plist-get listing :preview) "과학과 무엇"))
            (cl-assert (= (plist-get listing :length) 6)))

          ;; Typed ids, raw text, explicit reads of hidden notes.
          (let ((note (plist-get (agent-ekg-read 103) :note)))
            (cl-assert (equal (plist-get note :text) "과학과 무엇"))
            (cl-assert (equal (plist-get note :tags) '("과학" "무엇" "date/2026-10-31")))
            (cl-assert (null (plist-get note :hidden))))
          (cl-assert (plist-get (plist-get (agent-ekg-read 104) :note) :hidden))
          (cl-assert (equal (plist-get (plist-get (agent-ekg-read "https://example.org/what") :note) :text)
                            "문자열 id 노트"))
          (cl-assert (eq (ekg-agent-it/kind (agent-ekg-read "103")) 'not-found))
          (cl-assert (eq (ekg-agent-it/kind (agent-ekg-read 999)) 'not-found))
          (cl-assert (eq (ekg-agent-it/kind (agent-ekg-read "과학")) 'not-a-note))
          (cl-assert (eq (ekg-agent-it/kind (agent-ekg-read "https://example.org/bare")) 'not-a-note))

          ;; Counts follow the listing rule exactly.
          (let* ((result (agent-ekg-tags))
                 (rows (plist-get result :tags)))
            (cl-assert (= (plist-get result :total) (length rows)))
            (cl-assert (not (assoc "trash" (mapcar (lambda (r) (cons (plist-get r :tag) r)) rows))))
            (dolist (row rows)
              (cl-assert (= (plist-get row :count)
                            (plist-get (agent-ekg-notes (plist-get row :tag) 100 0) :total))
                         nil "Count drifted from listing: %S" row))
            (cl-assert (member '(:tag "과학" :count 2 :hidden-count 1) rows))
            (cl-assert (member '(:tag "고아" :count 0 :hidden-count 0) rows))
            (cl-assert (equal (mapcar (lambda (r) (plist-get r :tag)) rows)
                              ;; Ties by tag: ASCII "date/…" sorts before Hangul.
                              '("무엇" "date/2026-10-31" "과학" "date/2026-10-30" "고아"))))
          ;; Every listed id is readable.
          (dolist (tag '("과학" "무엇" "date/2026-10-31" "date/2026-10-30"))
            (dolist (id (ekg-agent-it/ids (agent-ekg-notes tag 100 0)))
              (cl-assert (plist-get (agent-ekg-read id) :ok) nil "Listed id unreadable: %S" id)))

          ;; Each call closes its own connection and leaves the caller's.
          (let ((sentinel (sqlite-open (expand-file-name "caller.sqlite" root))))
            (setq ekg-db sentinel)
            (setq handles nil)
            (agent-ekg-notes "과학")
            (cl-assert (eq ekg-db sentinel))
            (cl-assert (equal (sqlite-select sentinel "SELECT 1") '((1))))
            ;; EKG calls `ekg-connect' at every query step; all reuse one handle.
            (setq handles (seq-uniq handles #'eq))
            (cl-assert (= (length handles) 1))
            (cl-assert (not (eq (car handles) sentinel)))
            (cl-assert (condition-case nil
                           (progn (sqlite-select (car handles) "SELECT 1") nil)
                         (error t))
                       nil "The per-call connection was left open")
            (sqlite-close sentinel)
            (setq ekg-db nil))

          ;; A missing store is reported, never created — even with EKG loaded.
          (let ((my/ekg-agent-db-file (expand-file-name "absent/ekg.sqlite" root)))
            (cl-assert (eq (ekg-agent-it/kind (agent-ekg-tags)) 'store-missing))
            (cl-assert (not (file-exists-p (expand-file-name "absent" root)))))

          (cl-assert (equal seeded-rows (ekg-agent-it/rows store)))
          ;; Observed here, after every call has closed; this says nothing
          ;; about a rollback journal that lived only during a write.
          (dolist (suffix '("-wal" "-shm" "-journal"))
            (cl-assert (not (file-exists-p (concat store suffix)))
                       nil "Sidecar left behind: %s%s" store suffix))
          (princ (format "PASS agent-ekg-* on EKG %s: AND, date tag, hidden, typed read, counts=listing, pages, handle isolation, store-missing, no sidecar after close, server load form\n"
                         ekg-version))
          (princ (format "Temporary receipt root: %s\n" root)))
      (advice-remove 'ekg-connect 'ekg-agent-it-capture))))

;;; ekg-agent-read-integration.el ends here
