;;; test-hugo-tag-filter.el --- Tests for the meta-defined Hugo tag filter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;;; Commentary:

;; Characterization tests for Section 1.7 of lisp/denote-export-config.el:
;; only tags declared by a meta note reach the garden front matter.
;;
;; Tier C: the module requires ox-hugo, denote and org, so every test here
;; skips unless those packages load from Doom's straight build directory.
;; Run them with tests/run-tests.sh like any other suite.
;;
;; The behaviours pinned here are the ones that fail silently in production:
;;   - a meta note body must not leak its example tags into the pool
;;   - categories ("@"-prefixed) are a separate namespace and must survive
;;   - an implausibly small pool must abort the export, never publish
;;
;; Heading tags are not covered here: `org-export-with-tags' is nil, so
;; ox-hugo never runs the tag hook on them (ox-hugo.el:2128).

;;; Code:

(require 'test-helper)

(defvar test-hugo-tag--loaded
  ;; `build-*' also matches build-NN.N-cache.el, so keep directories only.
  (let ((build (car (last (seq-filter
                           #'file-directory-p
                           (file-expand-wildcards
                            (expand-file-name "~/doomemacs/.local/straight/build-*")))))))
    (when build
      (dolist (dir (directory-files build t "\\`[^.]"))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)))
      (defvar doom-user-dir)
      (defvar config-bibfiles)
      (setq doom-user-dir
            (file-name-directory
             (directory-file-name (file-name-directory load-file-name))))
      (setq config-bibfiles nil)
      (ignore-errors (require 'denote-export-config) t)))
  "Non-nil when denote-export-config could be loaded with its dependencies.")

(defmacro test-hugo-tag--deftest (name &rest body)
  "Define ERT test NAME running BODY only when the module loaded."
  (declare (indent 1))
  `(ert-deftest ,name ()
     (skip-unless test-hugo-tag--loaded)
     ,@body))

(defmacro test-hugo-tag--with-warm-pool (tags &rest body)
  "Run BODY with a pool holding TAGS, stamped as just checked.
Without the fresh stamp the pool re-fingerprints `denote-directory', which
in a test process points at a path that does not exist."
  (declare (indent 1))
  `(let ((my/org-hugo--meta-tag-pool (make-hash-table :test 'equal))
         (my/org-hugo--meta-tag-pool-stamp (cons 0 0.0))
         (my/org-hugo--meta-tag-pool-checked (float-time)))
     (dolist (tag ,tags)
       (puthash tag t my/org-hugo--meta-tag-pool))
     ,@body))

(defmacro test-hugo-tag--with-meta-dir (root-var files &rest body)
  "Bind ROOT-VAR to a temp denote directory holding meta/ FILES, then run BODY.
FILES is an alist of (NAME . FILETAGS).  Inside BODY the pool is empty, the
size floor is 1, and staleness is re-checked on every call."
  (declare (indent 2))
  `(let ((,root-var (make-temp-file "test-hugo-tag-" t)))
     (unwind-protect
         (let ((my/org-hugo--tag-pool-minimum 1)
               (my/org-hugo--tag-pool-recheck-seconds 0.0)
               (my/org-hugo--meta-tag-pool nil)
               (my/org-hugo--meta-tag-pool-stamp nil)
               (my/org-hugo--meta-tag-pool-checked 0.0))
           (make-directory (expand-file-name "meta/" ,root-var) t)
           (pcase-dolist (`(,name . ,tags) ,files)
             (test-hugo-tag--write-meta ,root-var name tags))
           (cl-letf (((symbol-function 'denote-directory) (lambda () ,root-var)))
             ,@body))
       (delete-directory ,root-var t))))

(defun test-hugo-tag--write-meta (root name filetags)
  "Write ROOT/meta/NAME declaring FILETAGS, and push its mtime forward.
Two writes inside one test can otherwise land on the same timestamp."
  (let ((file (expand-file-name (concat "meta/" name) root)))
    (with-temp-file file
      (insert (format "#+title: %s\n#+filetags: %s\n" name filetags)))
    (set-file-times file (time-add nil 10))
    file))

;;;; Pool scanning

(test-hugo-tag--deftest test-hugo-tag--pool-reads-header
  (test-helper/with-temp-dir
   (lambda (dir)
     (with-temp-file (expand-file-name "20240101T000000--note.org" dir)
       (insert "#+title: 노트\n")
       (insert "#+filetags: :emacs:org_mode:\n"))
     (let ((pool (my/org-hugo--scan-filetags-pool dir)))
       (should (gethash "emacs" pool))
       ;; raw Org name: the hyphen/space rewrite happens later in the chain
       (should (gethash "org_mode" pool))
       (should-not (gethash "org-mode" pool))))))

(test-hugo-tag--deftest test-hugo-tag--pool-ignores-body-examples
  "Only the header is scanned: the tag-taxonomy note quotes filetags in prose."
  (test-helper/with-temp-dir
   (lambda (dir)
     (with-temp-file (expand-file-name "20231005T133900--taxonomy.org" dir)
       (insert "#+title: 태그 분류학\n")
       (insert "#+filetags: :meta:tag:\n")
       (insert "#+identifier: 20231005T133900\n")
       (insert (make-string 20 ?\n))
       (insert "#+filetags: :PKM:ADHD:\n"))
     (let ((pool (my/org-hugo--scan-filetags-pool dir)))
       (should (gethash "meta" pool))
       (should (gethash "tag" pool))
       (should-not (gethash "PKM" pool))
       (should-not (gethash "ADHD" pool))))))

(test-hugo-tag--deftest test-hugo-tag--pool-missing-dir-is-empty
  (should (zerop (hash-table-count
                  (my/org-hugo--scan-filetags-pool "/nonexistent/meta/")))))

;;;; Filter behaviour

(test-hugo-tag--deftest test-hugo-tag--drops-tags-without-a-meta-note
  (test-hugo-tag--with-warm-pool '("emacs" "bib")
    (should (equal (my/org-hugo-tag-filter-by-meta-pool
                    '("emacs" "journal" "bib" "stray") nil)
                   '("emacs" "bib")))))

(test-hugo-tag--deftest test-hugo-tag--categories-survive
  "Categories reach the hook still carrying \"@\"; ox-hugo strips it after."
  (test-hugo-tag--with-warm-pool '("emacs")
    (should (equal (my/org-hugo-tag-filter-by-meta-pool '("@book" "stray") nil)
                   '("@book")))))

;;;; Fail-closed

(test-hugo-tag--deftest test-hugo-tag--small-pool-aborts-export
  "A wrong `denote-directory' must stop the export, not wipe the garden."
  (test-helper/with-temp-dir
   (lambda (dir)
     (with-temp-file (expand-file-name "20240101T000000--lonely.org" dir)
       (insert "#+filetags: :emacs:\n"))
     (let ((my/org-hugo--meta-tag-pool nil))
       (cl-letf (((symbol-function 'denote-directory)
                  (lambda () (file-name-directory (directory-file-name dir)))))
         (should-error (my/denote-meta-tag-pool :force) :type 'user-error))))))

;;;; Staleness

;; Shipped once: `monthly' and `weekly' were added to a meta note at 15:01 and
;; the export at 15:06 dropped them from the very notes that define them --
;; while a daemon started after the edit kept them, in the same batch.  The
;; pool had been cached before the edit, and `denote-export-parallel.py' reuses
;; daemons across runs.  Nothing errored; the tags just never reached the
;; garden.  The cache therefore re-fingerprints meta/ instead of trusting
;; itself, and these tests pin that.

(test-hugo-tag--deftest test-hugo-tag--pool-rebuilds-when-a-meta-note-is-retagged
  "A reused daemon must not keep serving the pool it cached before the edit."
  (test-hugo-tag--with-meta-dir root '(("a.org" . ":alpha:"))
    (should (gethash "alpha" (my/denote-meta-tag-pool)))
    (should-not (gethash "beta" (my/denote-meta-tag-pool)))
    (test-hugo-tag--write-meta root "a.org" ":alpha:beta:")
    (should (gethash "beta" (my/denote-meta-tag-pool)))))

(test-hugo-tag--deftest test-hugo-tag--pool-notices-a-new-meta-note
  "Giving a stray tag its magnet must publish it without restarting Emacs."
  (test-hugo-tag--with-meta-dir root '(("a.org" . ":alpha:"))
    (should-not (gethash "journal" (my/denote-meta-tag-pool)))
    (test-hugo-tag--write-meta root "b.org" ":journal:")
    (should (gethash "journal" (my/denote-meta-tag-pool)))))

(test-hugo-tag--deftest test-hugo-tag--pool-notices-a-deleted-meta-note
  "Deleting a note need not move the newest mtime, so the stamp counts files."
  (test-hugo-tag--with-meta-dir root '(("a.org" . ":alpha:") ("b.org" . ":beta:"))
    (should (gethash "beta" (my/denote-meta-tag-pool)))
    (delete-file (expand-file-name "meta/b.org" root))
    (should-not (gethash "beta" (my/denote-meta-tag-pool)))))

(test-hugo-tag--deftest test-hugo-tag--warm-pool-never-touches-the-disk
  "Fingerprinting costs ~16 ms; a batch export must not pay it once per note."
  (test-hugo-tag--with-warm-pool '("emacs")
    (cl-letf (((symbol-function 'denote-directory)
               (lambda () (error "pool re-fingerprinted inside the throttle window"))))
      (should (eq (my/denote-meta-tag-pool) my/org-hugo--meta-tag-pool)))))

(provide 'test-hugo-tag-filter)
;;; test-hugo-tag-filter.el ends here
