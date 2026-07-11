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

(provide 'test-keybinding-lint)
;;; test-keybinding-lint.el ends here
