;;; bin/site-policy.el --- Garden hygiene SSOT -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Single source of truth for garden link/URL hygiene policy.
;; Consumed by:
;;   - bin/fix-org-links.el           (Stage 1: ~/org 원본 일괄 정정)
;;   - lisp/denote-export-config.el   (Stage 2: export-time hook)
;;   - bin/verify-content.py          (Stage 3: 가든 md 검증, 추후)
;;
;; Adding an entry to the alists below is enough for all three stages
;; to pick it up — Stage 1/2 by direct elisp load, Stage 3 via emacs
;; batch dump (planned).
;;
;; Stage 3 will read this same data via:
;;   emacs --batch -Q --load bin/site-policy.el \
;;     --eval '(prin1 my/site-policy)' > /tmp/policy.sexp
;; or a json export if needed.

;;; Code:

(defvar my/site-policy
  '(;; Host alias map. Old (sub)domain → current canonical.
    ;; Applied to both file: and http(s) link targets.
    (host-aliases
     . (("geworfen.junghanacs.com" . "agent.junghanacs.com")))

    ;; GitHub URL prefix for ~/repos/gh/REPO rewrites.
    ;; Used by Stage 1/2 to convert local repo paths to public URLs.
    (github-user . "junghan0611")
    (github-branch . "main")

    ;; Internal path patterns — leak categories for Stage 3 verifier.
    ;; Stage 1 rewrites ~/repos/gh/REPO via dedicated logic, not this list.
    (internal-paths
     . ("^~/repos/"
        "^/home/junghan/"
        "^file://"))

    ;; Private endpoints — Stage 3 detection only.
    (private-endpoints
     . ("\\<localhost\\>"
        "\\<127\\.0\\.0\\.1\\>"
        "\\<192\\.168\\."
        "\\<10\\.[0-9]"
        "\\<172\\.\\(?:1[6-9]\\|2[0-9]\\|3[01]\\)\\."))

    ;; URL with embedded credentials — Stage 3.
    (credential-in-url . "://[^/[:space:]]+:[^@/[:space:]]+@")

    ;; Dangling [desc] without target — Stage 3.
    (orphan-bracket . t)

    ;; lychee options — Stage 3.
    (lychee
     . ((skip . ("mailto:" "tel:"))
        (max-redirects . 5))))
  "Garden hygiene policy SSOT.
See file commentary for consumers.")

(defun my/site-policy-get (key)
  "Return value of KEY from `my/site-policy'."
  (cdr (assq key my/site-policy)))

(provide 'site-policy)
;;; site-policy.el ends here
