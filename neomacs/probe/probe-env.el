;;; neomacs/probe/probe-env.el --- Runtime and builtin coverage probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Establishes what the runtime actually provides, before any other probe
;; interprets its own result.  A FAIL in probe-org-korean means something
;; different depending on whether treesit and the charset tables are even
;; present, so this runs first.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'probe-lib)

(my/probe-banner "runtime")
(princ (format "%s\n" (my/probe-runtime-line)))
(princ (format "system-configuration: %s\n" system-configuration))

(my/probe-banner "builtin libraries the minimal set needs")

(dolist (lib '(org ox ox-html ox-md treesit
               subr-x seq map cl-lib
               korea-util quail ucs-normalize hangul
               server url gnutls))
  (my/probe-check (format "require %s" lib)
    (if (require lib nil t)
        (format "%s" (or (locate-library (symbol-name lib)) "builtin"))
      (cons 'skip "not available"))))

(my/probe-banner "core capabilities")

(my/probe-check "treesit compiled in"
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      "available"
    (cons 'skip "treesit-available-p is nil")))

(my/probe-check "treesit grammars present"
  (if (and (fboundp 'treesit-available-p) (treesit-available-p))
      (let ((found (seq-filter #'treesit-language-available-p
                               '(python bash json rust c elisp toml yaml))))
        (if found
            (mapconcat #'symbol-name found " ")
          (cons 'skip "no grammars installed")))
    (cons 'skip "treesit unavailable")))

(my/probe-check "gnutls-available-p"
  (if (fboundp 'gnutls-available-p)
      (format "%s" (gnutls-available-p))
    (cons 'skip "gnutls-available-p unbound")))

(my/probe-check "native-comp"
  (if (and (fboundp 'native-comp-available-p) (native-comp-available-p))
      "available"
    (cons 'skip "no native-comp (expected on Neomacs)")))

(my/probe-check "dynamic modules"
  (if module-file-suffix
      module-file-suffix
    (cons 'skip "modules not compiled in")))

(my/probe-check "make-thread"
  (if (fboundp 'make-thread)
      (let ((th (make-thread (lambda () (* 6 7)) "probe")))
        (format "thread-join => %s" (thread-join th)))
    (cons 'skip "no thread support")))

(my/probe-summary)

;;; probe-env.el ends here
