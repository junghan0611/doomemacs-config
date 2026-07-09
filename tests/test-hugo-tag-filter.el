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
  (let ((my/org-hugo--meta-tag-pool (make-hash-table :test 'equal)))
    (puthash "emacs" t my/org-hugo--meta-tag-pool)
    (puthash "bib" t my/org-hugo--meta-tag-pool)
    (should (equal (my/org-hugo-tag-filter-by-meta-pool
                    '("emacs" "journal" "bib" "stray") nil)
                   '("emacs" "bib")))))

(test-hugo-tag--deftest test-hugo-tag--categories-survive
  "Categories reach the hook still carrying \"@\"; ox-hugo strips it after."
  (let ((my/org-hugo--meta-tag-pool (make-hash-table :test 'equal)))
    (puthash "emacs" t my/org-hugo--meta-tag-pool)
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

(test-hugo-tag--deftest test-hugo-tag--healthy-pool-caches
  (let ((my/org-hugo--meta-tag-pool (make-hash-table :test 'equal)))
    (puthash "emacs" t my/org-hugo--meta-tag-pool)
    ;; cache is warm, so no denote-directory lookup and no size check
    (should (eq (my/denote-meta-tag-pool) my/org-hugo--meta-tag-pool))))

(provide 'test-hugo-tag-filter)
;;; test-hugo-tag-filter.el ends here
