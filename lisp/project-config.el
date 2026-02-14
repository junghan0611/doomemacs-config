;;; $DOOMDIR/lisp/project-config.el --- UI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;;; Projectile

;; External tools required to make projectile fly! fd, ag, rg
;; evil-dot-doom/modules/custom/projects/config.el

(progn
  (setq magit-show-long-lines-warning nil) ; default nil

  (require' projectile)
  ;; Disable projectile cache - saves requirement to invalidate cache when moving files
  (setq projectile-enable-caching nil) ; very IMPORTANT
  (setq projectile-auto-update-cache nil)
  (setq projectile-file-exists-remote-cache-expire nil)

  (setq projectile-sort-order 'recentf)

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

;;;; git-commit

(after! git-commit
  ;; doom default 50
  ;; defaults to Github's max commit message length
  (setq git-commit-summary-max-length 72))

;;;; git-link

;; 현재 git repo의 homepage link를 clipboard에 넣어준다
(use-package! git-link
  :commands (git-link-commit git-link-homepage git-link)
  :init
  ;; default is to open the generated link
  (setq git-link-open-in-browser t)
  )


;;;; magit

(use-package! magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

;;;; tramp

;; Host *
;;     ControlMaster auto
;;     ControlPath ~/.ssh/sockets/%r@%h-%p
;;     ControlPersist 600
(after! tramp
  (setq tramp-default-method "ssh")

  ;; 소켓 디렉토리 자동 생성
  (let ((socket-dir "~/.ssh/sockets"))
    (unless (file-exists-p socket-dir)
      (make-directory socket-dir t)
      (set-file-modes socket-dir #o700)))  ;; 권한 700

  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=~/.ssh/sockets/%%r@%%h-%%p -o ControlPersist=600"))

;;;; magit-gh

(use-package! magit-gh
  :after magit
  :init
  (setq magit-gh-key ";")) ; Example setting key to ";" instead of the default ","

;;;; TODO majutsu jj-mode

;; (use-package! majutsu
;;   :after magit
;;   :commands majutsu)

;;; provide

(provide 'project-config)

;;; project-config.el ends here
