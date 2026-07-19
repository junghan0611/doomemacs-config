;;; neomacs/init.el --- Neomacs vanilla minimal profile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; The vanilla minimal set for Neomacs (doomemacs-config issue #8).
;;
;; Scope, deliberately narrow:
;;   - builtins only, no ELPA/MELPA dependency
;;   - tree-sitter syntax highlighting
;;   - org-mode document editing
;;   - org export via ox-html / ox-md
;;   - Korean (K) input and rendering, the part nobody upstream covers
;;
;; Everything here must run on a standalone Neomacs binary AND on stock GNU
;; Emacs, so a divergence can be attributed to Neomacs rather than to this
;; config.  Keep Doom macros, `use-package', and external packages out.
;;
;; Launch:  ./bin/neomacs.sh
;; Probe:   ./bin/neomacs.sh --probe

;;; Code:

;;;; Prelude

;; Required explicitly rather than relied on as preloaded: under --batch
;; these are not autoloaded, and the probes load this file that way.
(require 'subr-x)
(require 'seq)

;;;; Profile identity

(defconst my/neomacs-p (not (null (string-match-p "neomacs" (emacs-version))))
  "Non-nil when running under Neomacs rather than GNU Emacs.
Derived from `emacs-version' because Neomacs reports GNU-compatible
`emacs-major-version', which cannot distinguish the two.")

(defconst my/neomacs-profile-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this profile.  Probes and fixtures resolve against it.")

;;;; Encoding — UTF-8 everywhere

;; Neomacs issue #104 was a build failure under a Chinese locale, so coding
;; defaults are stated explicitly rather than inherited from the environment.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

;;;; Korean input — builtin only

;; No fcitx/rime here.  Neomacs #128 (external IME) is closed upstream, but
;; the builtin `korean-hangul' method is what a vanilla profile can rely on
;; without a working XIM bridge, and it exercises Neomacs' own composition
;; path rather than the toolkit's.
(setq default-input-method "korean-hangul")

;; Toggle stays on the GNU default (C-\), so a K-input bug report never has
;; to explain a custom binding.

;;;; Fonts

(defvar my/neomacs-font-size 14
  "Default font height in points for this profile.")

(defvar my/neomacs-latin-font "D2Coding"
  "Latin monospace family.  D2Coding ships Hangul at a fixed 2:1 advance,
which is what the CJK cell-width path (Neomacs #130) needs to be measured
against.")

(defvar my/neomacs-hangul-font "D2Coding"
  "Hangul family.  Kept identical to the Latin family on purpose: a single
family removes fallback as a variable when column drift is investigated.")

(defun my/neomacs-setup-fonts (&optional frame)
  "Apply profile fonts to FRAME, or the selected frame when nil.
Silently does nothing on a TTY frame, where font selection is a no-op."
  (when (display-graphic-p (or frame (selected-frame)))
    (set-face-attribute 'default frame
                        :family my/neomacs-latin-font
                        :height (* my/neomacs-font-size 10))
    (set-fontset-font t 'hangul
                      (font-spec :family my/neomacs-hangul-font)
                      frame)))

;; Run for the current frame and for every frame the daemon creates later.
(my/neomacs-setup-fonts)
(add-hook 'after-make-frame-functions #'my/neomacs-setup-fonts)

;;;; Editing defaults

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      require-final-newline t
      sentence-end-double-space nil)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)

;; Neomacs #145 reported missing/incorrect before/after-change-functions.
;; electric-pair leans on exactly that path, so it stays on as a passive
;; canary during daily editing.
(electric-pair-mode 1)

(fset 'yes-or-no-p #'y-or-n-p)

;;;; Tree-sitter

;; treesit is builtin from Emacs 29.  Grammars are NOT downloaded here —
;; that would need the TLS stack this profile refuses to depend on.  The
;; probe reports which grammars are actually available.
(defun my/neomacs-treesit-available-p ()
  "Return non-nil when treesit is compiled in and usable."
  (and (fboundp 'treesit-available-p) (treesit-available-p)))

(when (my/neomacs-treesit-available-p)
  ;; Only remap modes whose grammar is present, so a missing grammar
  ;; degrades to the classic major mode instead of erroring on visit.
  (dolist (pair '((emacs-lisp-mode . nil)      ; no ts mode upstream
                  (python-mode . python-ts-mode)
                  (bash-mode . bash-ts-mode)
                  (json-mode . json-ts-mode)
                  (rust-mode . rust-ts-mode)))
    (when-let* ((ts-mode (cdr pair))
                (lang (intern (string-remove-suffix
                               "-ts-mode" (symbol-name ts-mode))))
                (_ (treesit-language-available-p lang)))
      (add-to-list 'major-mode-remap-alist pair))))

(global-font-lock-mode 1)

;;;; Org

(setq org-directory (expand-file-name "~/org/"))

(with-eval-after-load 'org
  (setq org-startup-indented t
        org-startup-folded 'content
        org-hide-emphasis-markers nil
        org-src-fontify-natively t
        org-edit-src-content-indentation 0
        ;; Image preview is broken upstream (#40); leaving it off keeps org
        ;; buffers openable while that is unresolved.
        org-startup-with-inline-images nil
        org-export-with-toc t
        org-export-with-section-numbers nil)

  ;; ox-md is not autoloaded by default; the minimal set needs both
  ;; backends to be present without any ELPA package.
  (require 'ox-html nil t)
  (require 'ox-md nil t))

;;;; Local packages — source only, never ELPA

;; Upstream #121 blocks ELPA (see neomacs/README.md), but nothing stops a
;; pure-elisp package already checked out on disk from being put on
;; `load-path'.  That keeps the no-network rule intact while making the
;; profile usable for real note-taking, where `denote:' links must resolve.
;;
;; Source only: no byte-compiled artifacts from another Emacs are loaded,
;; and anything needing a C module or native-comp stays out.

(defvar my/neomacs-local-package-dirs
  (seq-filter #'file-directory-p
              (list (expand-file-name "~/.config/emacs/.local/straight/repos/denote")
                    (expand-file-name "~/.config/emacs/.local/straight/repos/denote-org")))
  "Local pure-elisp package checkouts to put on `load-path'.
Filtered to existing directories so the profile still boots on a machine
that has no Doom installation.")

(dolist (dir my/neomacs-local-package-dirs)
  (add-to-list 'load-path dir))

(defun my/neomacs-load-denote ()
  "Load Denote from a local checkout, if one is present.
Returns non-nil on success.  Failure is not fatal — the profile is still
usable for plain org editing without it."
  (when (require 'denote nil t)
    (setq denote-directory org-directory)
    ;; Registers the `denote:' link type so real notes export instead of
    ;; aborting on an unresolvable link.
    (require 'denote-org nil t)
    t))

(with-eval-after-load 'org
  (my/neomacs-load-denote))

;;;; Interface

(when (fboundp 'fido-vertical-mode)
  (fido-vertical-mode 1))

(setq completion-styles '(basic partial-completion flex)
      completions-detailed t
      use-short-answers t)

(savehist-mode 1)
(recentf-mode 1)

;;;; Server

;; Distinct from the Doom sockets ("user", "server", "doom-unstable") so a
;; Neomacs daemon can coexist with them.  Value may be overridden by the
;; launcher through EMACS_SERVER_NAME.
(setq server-name (or (getenv "EMACS_SERVER_NAME") "neomacs"))

;;; init.el ends here
