;;; tests/test-keybinding-lint.el --- map! prefix lint -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Guards the `map!' prefix rule (AGENTS.md § map! prefix 규약).
;;
;; Doom removed general.el from `map!' (upstream de2a3364a).  A prefix carrying
;; a description now binds a FRESH `make-sparse-keymap' at that key, destroying
;; whatever prefix map already lived there — Doom's own `doom-leader-file-map',
;; the standard `help-map', or a prefix another one of our files declared.
;; `:prefix-map' binds a keymap at the key too, so it carries the same hazard.
;;
;;   (:prefix ("f" . "files") ...)      ; wipes doom-leader-file-map
;;   (:prefix-map ("f" . "files") ...)  ; also binds a map at f
;;   (:prefix "f" ...)                  ; layers onto it        <- always this
;;
;; Keys go in non-destructively; labels come from
;; `which-key-add-keymap-based-replacements' (SSOT block in keybindings-config.el),
;; which only plants a pseudo-key and never replaces a binding.
;;
;; This is a Tier A text scan: no Doom, no packages, just the sources.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

;;;; Helpers

(defconst my/keybinding-lint-dirs '("lisp" "autoload")
  "Directories, relative to the config root, scanned for `map!' misuse.")

(defconst my/keybinding-lint-re
  (rx "(:prefix" (opt "-map") (+ space) "(")
  "Matches a prefix form that binds a keymap — the destructive kind.")

;; Resolved at load time — `load-file-name' is nil once ERT runs the body.
(defconst my/keybinding-lint-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Config root, derived from this file's location.")

(defun my/keybinding-lint--files ()
  "Return every Elisp source file subject to the prefix rule."
  (mapcan (lambda (dir)
            (let ((path (expand-file-name dir my/keybinding-lint-root)))
              (when (file-directory-p path)
                (directory-files path t "\\.el\\'"))))
          my/keybinding-lint-dirs))

(defun my/keybinding-lint--offenders (file)
  "Return (LINE . TEXT) for each destructive `:prefix' in FILE.
Commented lines are skipped — the rule is documented in prose in several
Commentary blocks."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (hits)
      (while (re-search-forward my/keybinding-lint-re nil t)
        (let ((bol (line-beginning-position)))
          (goto-char bol)
          (skip-chars-forward " \t")
          (unless (eq (char-after) ?\;)
            (push (cons (line-number-at-pos)
                        (string-trim (buffer-substring-no-properties
                                      bol (line-end-position))))
                  hits))
          (forward-line 1)))
      (nreverse hits))))

;;;; Tests

(ert-deftest my/keybinding-lint-no-desc-carrying-prefix ()
  "No source file may name a prefix in `map!'.

`(:prefix (KEY . DESC))' binds a fresh empty keymap at KEY, silently
destroying the prefix map already there; `(:prefix-map (KEY . DESC))' binds a
keymap at KEY too.  Push keys in with `(:prefix KEY)' and label the group in
the which-key SSOT block in keybindings-config.el."
  (let (report)
    (dolist (file (my/keybinding-lint--files))
      (dolist (hit (my/keybinding-lint--offenders file))
        (push (format "%s:%d: %s"
                      (file-name-nondirectory file) (car hit) (cdr hit))
              report)))
    (should (equal nil (nreverse report)))))

(ert-deftest my/keybinding-lint-scans-something ()
  "The lint must actually see source files.
Guards against a path bug quietly turning the rule off."
  (should (> (length (my/keybinding-lint--files)) 20)))

;;;; Leader path extraction (command-vs-prefix collision)

(defconst my/keybinding-lint--arg-keywords
  '(:desc :map :keymap :after :mode :when :unless :if :textobj :alt)
  "Keywords in `map!' that consume the element following them.
`:prefix' is absent on purpose — it extends the key path and is handled
explicitly in `my/keybinding-lint--walk'.")

(defun my/keybinding-lint--prefix-key (form)
  "Return the key string a `(:prefix KEY ...)' FORM opens, or nil.
Handles the named `(KEY . DESC)' shape too so the walk does not stop there."
  (let ((key (cadr form)))
    (cond ((stringp key) key)
          ((and (consp key) (stringp (car key))) (car key)))))

(defun my/keybinding-lint--walk (body path leaderp acc)
  "Walk `map!' BODY, pushing (PATH . TYPE) onto ACC.
PATH is the leader key sequence reached so far, as a list of strings.
LEADERP tracks whether we are under `:leader'.  Returns the new ACC."
  (while body
    (let ((e (pop body)))
      (cond
       ;; (:prefix KEY . BODY) — descend, extending the path.
       ((and (consp e) (memq (car e) '(:prefix :prefix-map)))
        (let ((key (my/keybinding-lint--prefix-key e)))
          (setq acc (my/keybinding-lint--walk
                     (cddr e)
                     (if key (append path (list key)) path)
                     leaderp acc))))
       ;; Any other proper list is grouping — same path.  Dotted pairs are
       ;; data (alist literals live inside `map!' bodies via :when forms).
       ((proper-list-p e)
        (setq acc (my/keybinding-lint--walk e path leaderp acc)))
       ;; Bare `:prefix KEY' — `map!' takes it flat, not only as a nested list.
       ;; It extends the path for the REST of this body, so it must not be
       ;; skipped like an ordinary keyword-with-argument.
       ((memq e '(:prefix :prefix-map))
        (let* ((key (pop body))
               (str (cond ((stringp key) key)
                          ((and (consp key) (stringp (car key))) (car key)))))
          (if str
              (setq path (append path (list str)))
            ;; A computed prefix — the path is no longer knowable statically.
            (setq leaderp nil))))
       ((eq e :leader) (setq leaderp t))
       ;; localleader is a different map; stop recording under it.
       ((eq e :localleader) (setq leaderp nil))
       ;; `:map foo' leaves the leader map entirely.
       ((memq e '(:map :keymap)) (setq leaderp nil) (pop body))
       ((memq e my/keybinding-lint--arg-keywords) (pop body))
       ;; "key" DEF — a leaf binding.
       ((stringp e)
        (let ((def (pop body)))
          (when (and leaderp def)
            (push (cons (append path (list e)) 'command) acc))))
       ;; Bare state keywords (:n :i :nv …) and anything else: ignore.
       (t nil))))
  acc)

(defun my/keybinding-lint--map-forms (sexp)
  "Return every `map!' form nested anywhere inside SEXP.
Only proper lists are descended: sources carry alist literals whose dotted
pairs would otherwise be walked as code."
  (when (proper-list-p sexp)
    (if (eq (car sexp) 'map!)
        (list sexp)
      (let (found)
        (dolist (e sexp)
          (setq found (append found (my/keybinding-lint--map-forms e))))
        found))))

(defun my/keybinding-lint--leader-paths ()
  "Return an alist of (PATH-STRING . SOURCE) for every leader binding.
PATH-STRING joins the key sequence with spaces so prefix tests are plain
string operations."
  (let (paths)
    (dolist (file (my/keybinding-lint--files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((forms nil))
          (ignore-errors
            (while t (push (read (current-buffer)) forms)))
          (dolist (form forms)
            (dolist (m (my/keybinding-lint--map-forms form))
              (dolist (hit (my/keybinding-lint--walk (cdr m) nil nil nil))
                (push (cons (string-join (car hit) " ")
                            (file-name-nondirectory file))
                      paths)))))))
    paths))

;;;; Tests

(ert-deftest my/keybinding-lint-no-command-under-prefix ()
  "No leader key may be a command in one file and a prefix in another.

Doom signals `Key sequence ... starts with non-prefix key ...' at startup
and the whole config fails to boot.  The two sides usually live in different
files, so neither looks wrong on its own — this test is the only place they
meet.  (Case: 2026-08-11, `SPC j p' bound to a command while
present-config.el used it as the prefix for `SPC j p p'.)"
  (let* ((paths (my/keybinding-lint--leader-paths))
         report)
    (dolist (entry paths)
      (let ((path (car entry)))
        (dolist (other paths)
          ;; OTHER extends PATH by at least one more key → PATH must be a prefix,
          ;; but it is bound to a command here.
          (when (and (not (equal (car other) path))
                     (string-prefix-p (concat path " ") (car other)))
            (push (format "SPC %s (command, %s) blocks SPC %s (%s)"
                          path (cdr entry) (car other) (cdr other))
                  report)))))
    (should (equal nil (delete-dups (nreverse report))))))

(ert-deftest my/keybinding-lint-extracts-leader-paths ()
  "The extractor must actually find leader bindings.
Guards against a parser bug quietly turning the collision test off."
  (should (> (length (my/keybinding-lint--leader-paths)) 50)))

(provide 'test-keybinding-lint)
;;; test-keybinding-lint.el ends here
