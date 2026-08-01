;;; $DOOMDIR/lisp/project-config.el --- Project / VCS / GitHub surfaces -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Projectile, Magit companions, TRAMP, and the thin human GitHub search
;; surface (consult-gh). Agent-side GitHub work stays on ghcli.
;;
;; tramp-rpc: high-perf TRAMP backend (JSON-RPC → MessagePack-RPC)
;; https://github.com/ArthurHeymans/emacs-tramp-rpc
;;   /rpc:user@host:/path/to/file

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

;;;; tramp-rpc
;; upstream recursive load 이슈는 해결됨. tramp 2.8.1.4+ 전제.
;; Doom :emacs tramp 모듈이 기본 성능 설정을 담당하고,
;; 여기서는 ssh ControlMaster/ControlPath 정책만 유지.
;; straight git checkout 환경에서는 기본값(auto)이 소스 빌드를 선호하므로,
;; 이 설정은 release artifact를 우선 사용해 이종 아키텍처(aarch64 등)도
;; 로컬 Rust 빌드 없이 바로 배포되게 한다.
(after! tramp-rpc
  (setq tramp-rpc-deploy-git-build-policy 'release))

;;;; magit-gh

(use-package! magit-gh
  :after magit
  :init
  (setq magit-gh-key ";")) ; Example setting key to ";" instead of the default ","

;;;; consult-gh — global GitHub search (human surface)

;; Work-surface split:
;;   magit-gh   — PRs inside the current Magit repo
;;   git-link   — URL for the file/commit under point
;;   ghcli      — agent issue/PR/CLI work
;;   consult-gh — human global search (repo/code/issue), hand off to agents
;;
;; Keep it thin. No embark/forge/pr-review/omni, no default view-mode
;; keybindings, no dashboard/workflow keys. Search + account switch only.
(use-package! consult-gh
  :after consult
  :commands (consult-gh-search-repos
             consult-gh-search-code
             consult-gh-search-issues
             consult-gh-repo-list
             consult-gh-auth-switch
             consult-gh-favorite-repos
             consult-gh-transient)
  :custom
  (consult-gh-default-clone-directory "~/repos/3rd/")
  (consult-gh-favorite-orgs-list
   '("junghan0611" "minad" "protesilaos" "doomemacs" "earendil-works"))
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  ;; Stay inside Emacs — browser is the thing we are avoiding.
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-confirm-before-clone t)
  :init
  ;; Keys under Doom's magit prefix. Unnamed :prefix only (AGENTS.md).
  (map! :leader
        (:prefix "g"
         (:prefix "h"
          :desc "Search repos" "s" #'consult-gh-search-repos
          :desc "Search code" "c" #'consult-gh-search-code
          :desc "Search issues" "i" #'consult-gh-search-issues
          :desc "Switch account" "a" #'consult-gh-auth-switch
          :desc "consult-gh menu" "h" #'consult-gh-transient)))
  :config
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))

;;;; majutsu jj-mode

(use-package! majutsu
  :after magit
  :commands majutsu)

;;; provide

(provide 'project-config)

;;; project-config.el ends here
