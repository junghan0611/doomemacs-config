;;; $DOOMDIR/lisp/project-config.el --- UI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;;; Projectile

;; External tools required to make projectile fly! fd, ag, rg
;; evil-dot-doom/modules/custom/projects/config.el

(after! projectile
  ;; Disable projectile cache - saves requirement to invalidate cache when moving files
  (setq projectile-enable-caching nil) ; very IMPORTANT
  (setq projectile-sort-order 'recentf)
   ;; projectile-verbose nil

  ;; create missing test files
  (setq projectile-create-missing-test-files t)

  ;; add clojure specific folders to be ignored by projectile
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '(".cpcache"
                  "tmp" "del"
                  ".local")))

  ;; Search https://discourse.doomemacs.org/ for example configuration
  (setq projectile-ignored-projects
        (list "~/" "/tmp" (expand-file-name "straight/repos" doom-local-dir)))
  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (or (mapcar
         (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

  ;; direct projectile to look for code in a specific folder.
  (setq projectile-project-search-path '("~/repos/" . 2))

  (map! :leader
        :desc "Toggle Impl & Test" "pt" #'projectile-toggle-between-implementation-and-test
        ;; :desc "List todos" "pl" #'magit-todos-list
        :desc "See project root dir" "p-" #'projectile-dired
        :desc "Ripgrep" "pG" #'projectile-ripgrep)

  ;; stop $HOME from being recognizes as a project root
  ;; (setq projectile-project-root-files-bottom-up
  ;;       (remove ".git" projectile-project-root-files-bottom-up))
  )


;;;; majutsu jj-mode

(use-package! majutsu
  :after magit
  :commands majutsu)

;;; provide

(provide 'project-config)

;;; project-config.el ends here
