;;; $DOOMDIR/lisp/denote-export-refs.el --- citar-denote #+reference: -> export frontmatter refs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Bridge layer: turn citar-denote `#+reference:' citekeys into the data a
;; garden export needs to emit a `refs:' frontmatter block, which notes(v4)
;; then lifts into a schema.org `citation' JSON-LD array (footprints AEO).
;;
;; This file holds only the PURE transform so it runs under `emacs -Q':
;;   - `my/denote-export-refs-split'     keyword string  -> citekey list
;;   - `my/denote-export-refs-in-string' org text        -> citekey list
;;   - `my/denote-export-refs-resolve'   citekeys + index -> ref alists
;;
;; Package glue lives in the headless export daemon, NOT here:
;;   `citar-denote--retrieve-references'  supplies the citekeys,
;;   citar's in-memory bibliography       supplies the bib index.
;; Keeping resolution behind a plain alist argument (dependency injection)
;; is what lets the logic be characterized in tests/ without the garden,
;; citar, or citar-denote loaded.  The regex below mirrors citar-denote's
;; org `:reference-regex' so the pure layer can also read the keyword on
;; its own as a fallback.

;;; Code:

(require 'seq)

;;;; Citekey extraction

(defconst my/denote-export-refs-keyword-regex
  "^#\\+reference\\s-*:\\s-*\\(.*\\)$"
  "Regex matching a citar-denote `#+reference:' line; group 1 is the value.
Mirrors citar-denote's org `:reference-regex' (see `citar-denote-file-types').")

(defun my/denote-export-refs-split (value)
  "Split a `#+reference:' keyword VALUE into trimmed citekeys.
VALUE is the semicolon-separated string citar-denote writes
\(e.g. \"web-a;book-b\").  Returns nil for nil or blank input."
  (when (stringp value)
    (seq-remove #'string-empty-p
                (mapcar #'string-trim (split-string value ";" t "[ \t]+")))))

(defun my/denote-export-refs-in-string (text)
  "Return the citekey list from the first `#+reference:' line in TEXT.
Returns nil when TEXT has no `#+reference:' keyword."
  (when (and (stringp text)
             (string-match my/denote-export-refs-keyword-regex text))
    (my/denote-export-refs-split (match-string 1 text))))

;;;; Bib resolution

(defconst my/denote-export-refs-optional-fields
  '((:url . "url") (:author . "author") (:year . "year") (:doi . "doi"))
  "Optional bib fields carried into a ref, as (KEYWORD . BIB-FIELD).
`:key' and `:title' are mandatory and handled separately; these are
added only when the source entry has them.  schema.org mapping on the
notes side: url->url, author->author, year->datePublished, doi->identifier.")

(defun my/denote-export-refs-resolve (citekeys bib-index)
  "Resolve CITEKEYS into ref alists using BIB-INDEX.
BIB-INDEX is an alist (CITEKEY . FIELDS); FIELDS is a string-keyed alist
as citar returns, e.g. ((\"title\" . \"...\") (\"url\" . \"...\")).
Each result is an alist with mandatory `:key'/`:title' plus the present
members of `my/denote-export-refs-optional-fields'.  A citekey is skipped
\(no ref emitted) when it is an orphan (absent from BIB-INDEX) OR has no
title, so every returned ref is guaranteed to carry a name — bare keys
never reach public frontmatter."
  (delq nil
        (mapcar
         (lambda (key)
           (when-let* ((fields (cdr (assoc key bib-index)))
                       (title (cdr (assoc "title" fields))))
             (append
              (list (cons :key key) (cons :title title))
              (delq nil
                    (mapcar (lambda (pair)
                              (when-let* ((v (cdr (assoc (cdr pair) fields))))
                                (cons (car pair) v)))
                            my/denote-export-refs-optional-fields)))))
         citekeys)))

(provide 'denote-export-refs)
;;; denote-export-refs.el ends here
