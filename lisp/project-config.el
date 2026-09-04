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
;; upstream recursive load 이슈는 해결됨. tramp 2.8.1.4+ 전제 —
;; 이맥스 31.1 내장 tramp 가 2.8.2.31.1 이라 ELPA tramp 없이 충족된다.
;; Doom :emacs tramp 모듈이 기본 성능 설정을 담당하고,
;; 여기서는 ssh ControlMaster/ControlPath 정책만 유지.
;; straight git checkout 환경에서는 기본값(auto)이 소스 빌드를 선호하므로,
;; 이 설정은 release artifact를 우선 사용해 이종 아키텍처(aarch64 등)도
;; 로컬 Rust 빌드 없이 바로 배포되게 한다.
(after! tramp-rpc
  (setq tramp-rpc-deploy-git-build-policy 'release))

;; tramp-rpc registers its "rpc" method from `tramp-rpc-autoloads.el', in an
;; `eval-and-compile' block guarded by `(when (boundp 'tramp-rpc-method) ...)'.
;; That guard only holds when the neighbouring `defconst tramp-rpc-method' is a
;; *preceding top-level form*.  Doom inlines every package's autoloads into one
;; giant defun in a `no-byte-compile: t' profile init, so loading that source
;; file eagerly macro-expands the defun body: the `eval-and-compile' runs at
;; load time, before the defconst in the same body has ever executed, `boundp'
;; is nil, and the registration is silently dropped for the whole session.
;; Result: `/rpc:host:/path' still matches `tramp-file-name-regexp', so TRAMP
;; claims it and then signals "Method `rpc' is not known" — which surfaces as a
;; vertico/nerd-icons backtrace the moment a bookmark list contains an rpc path.
;; Measured 2026-09-02 on Emacs 30.2 and 31.1 daemons alike ("rpc" absent from
;; `tramp-methods', `tramp-rpc-file-name-p' unbound) — this is not a 31.1
;; regression.  Loading the autoloads file itself replays the two forms in the
;; right order and registers the method.
(after! tramp
  (unless (assoc "rpc" tramp-methods)
    (when-let* ((file (locate-library "tramp-rpc-autoloads")))
      (load file 'noerror 'nomessage))))

;;;; magit-gh

(use-package! magit-gh
  :after magit
  :init
  (setq magit-gh-key ";")) ; Example setting key to ";" instead of the default ","

;;;; consult-gh — global GitHub search (human surface)

;; Work-surface split:
;;   magit-gh         — PRs inside the current Magit repo
;;   git-link         — URL for the file/commit under point
;;   ghcli            — agent issue/PR/CLI work
;;   consult-gh       — human global search (repo/code/issue)
;;   Magit Forge      — local SQLite inbox (forge-pull / global list)
;;
;; Deliberately NOT wired to consult-gh-forge: that mode inserts every
;; selected search hit's repo into the forge DB (consult-gh-forge.el:101)
;; and overrides ghub--token/username/host globally.  Global search and the
;; local inbox are different axes; keep them apart. (2026-08-23)
;;
;; Keep it thin. No embark/forge/pr-review/omni/dashboard keys.
;; Search + account switch only.
;;
;; Transient lives in a sibling file (`consult-gh-transient.el`) with no
;; ;;;###autoload cookie.  Binding `consult-gh-transient' via :commands makes
;; Doom autoload it from feature `consult-gh', which loads consult-gh.elc and
;; then fails — the symbol is never defined there.  Route the menu key through
;; a wrapper that requires the right feature.
(defun my/consult-gh-menu ()
  "Open the consult-gh transient menu."
  (interactive)
  (require 'consult-gh-transient)
  (call-interactively #'consult-gh-transient))

(use-package! consult-gh
  :after consult
  :commands (consult-gh-search-repos
             consult-gh-search-code
             consult-gh-search-issues
             consult-gh-repo-list
             consult-gh-auth-switch
             consult-gh-favorite-repos)
  :custom
  (consult-gh-default-clone-directory "~/repos/3rd/")
  (consult-gh-favorite-orgs-list '("junghan0611" "jhkim2goqual"))
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
                  :desc "consult-gh menu" "h" #'my/consult-gh-menu)))
  :config
  (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list))

;;;; forge — inbox seeding

;; Doom ships forge via `(magit +forge)'; everything below is seeding only.
;; No advice, no overrides — `forge-add-repository' does the insert and pull
;; itself when handed a repository object (forge-commands.el:1341-1359).
;;
;; Forge's inbox is only as good as its seed list, and a hardcoded list goes
;; stale.  Ask `gh' which of my repos currently carry an open issue instead:
;; 19 repos / 65 open issues at the time of writing. (2026-08-23)
;;
;; Archiving a repo on GitHub is how it leaves the inbox.  Forge has no notion
;; of archived, so both halves are needed: `--archived=false' keeps new ones
;; out, and the prune pass drops rows for repos archived after they were
;; tracked.  Deleting is safe — the DB is a cache, and un-archiving plus a
;; re-seed brings everything back.

(defvar my/forge-seed-owner "junghan0611"
  "GitHub owner whose issue-bearing repos `my/forge-seed-repositories' tracks.")

(defun my/forge--repo-object (name)
  "Return the tracked forge repository for NAME (\"owner/name\"), or nil."
  (forge-get-repository (format "https://github.com/%s" name) nil :tracked?))

(defun my/forge-seed-repo-names (owner)
  "Return unarchived OWNER repos that currently have at least one open issue."
  (seq-uniq
   (process-lines "gh" "search" "issues"
                  "--owner" owner "--state" "open" "--archived=false"
                  "--limit" "200" "--json" "repository"
                  "--jq" ".[].repository.nameWithOwner")))

(defun my/forge-archived-repo-names (owner)
  "Return OWNER repos that are archived on GitHub."
  (process-lines "gh" "repo" "list" owner "--archived" "--limit" "500"
                 "--json" "nameWithOwner" "--jq" ".[].nameWithOwner"))

(defun my/forge-prune-archived-repositories (&optional owner)
  "Drop tracked OWNER repos that have been archived on GitHub.
Asks before deleting.  Returns the number of repos removed."
  (interactive)
  (let* ((owner (or owner my/forge-seed-owner))
         (doomed (seq-filter #'my/forge--repo-object
                             (my/forge-archived-repo-names owner))))
    (cond
     ((null doomed)
      (when (called-interactively-p 'any)
        (message "forge: no archived repository is tracked"))
      0)
     ((not (yes-or-no-p (format "Remove %d archived repo(s) from the forge db (%s)? "
                                (length doomed) (string-join doomed ", "))))
      (message "forge: prune aborted")
      0)
     (t
      (mapc (lambda (name) (forge-remove-repository (my/forge--repo-object name)))
            doomed)
      (message "forge: removed %d archived repository(-ies)" (length doomed))
      (length doomed)))))

(defun my/forge-seed-repositories (&optional owner)
  "Make the forge inbox match OWNER's unarchived repos that have open issues.
Prunes archived repos first, then tracks any that are missing.  Pulling runs
asynchronously, so the issue counts keep climbing after this returns."
  (interactive)
  (let ((owner (or owner my/forge-seed-owner))
        (added 0))
    (my/forge-prune-archived-repositories owner)
    (dolist (name (my/forge-seed-repo-names owner))
      (unless (my/forge--repo-object name)
        (when-let* ((repo (forge-get-repository
                           (format "https://github.com/%s" name) nil :valid?)))
          (forge-add-repository repo)
          (cl-incf added))))
    (message "forge: %d repository(-ies) queued for tracking" added)))

;;;; forge — staleness and refresh

;; The inbox DB is the shared face: GLG reads it through Magit, and cross-repo
;; agents read the same rows read-only.  A second scraper would give the two
;; sides different boards, so the rhythm lives here and nowhere else.
;;
;; `forge-pull' is repo-scoped (forge-commands.el:142) — it needs to be called
;; from inside a tracked worktree.  Refreshing the whole inbox therefore means
;; walking the tracked repos, which is what `my/forge-pull-all' does.  Pulls are
;; async, so it returns as soon as they are queued.
;;
;; Rate limit is not the constraint: `gh api rate_limit' reported core 5000/hr
;; and graphql 5000/hr with 0 used, against 20 tracked repos. (2026-09-04)

(defun my/forge-database-age ()
  "Return seconds since the forge database file was last written."
  (- (float-time)
     (float-time (file-attribute-modification-time
                  (file-attributes forge-database-file)))))

(defun my/forge-stale-p (&optional max-age)
  "Non-nil when the forge database is older than MAX-AGE seconds (default 6h)."
  (> (my/forge-database-age) (or max-age (* 6 60 60))))

(defun my/forge-pull-all (&optional force)
  "Pull every tracked forge repository.
Does nothing unless the database is stale (`my/forge-stale-p') or FORCE is
non-nil, so a caller may poll this without hammering the API.  Pulls are
asynchronous; the returned count is what was queued, not what has landed."
  (interactive "P")
  (if (and (not force) (not (my/forge-stale-p)))
      (progn (message "forge: db is %.1fh old, skipping"
                      (/ (my/forge-database-age) 3600.0))
             0)
    (let ((repos (forge-sql [:select [githost owner name] :from repository]))
          (queued 0))
      (pcase-dolist (`(,host ,owner ,name) repos)
        (when-let* ((repo (forge-get-repository
                           (format "https://%s/%s/%s" host owner name)
                           nil :tracked?)))
          (forge--pull repo)
          (cl-incf queued)))
      (message "forge: queued %d repository pull(s)" queued)
      queued)))

;; The inbox needs one key.  Unnamed :prefix only (AGENTS.md); seeding is a
;; rare M-x, so it gets no binding.
(map! :leader
      (:prefix "g"
       :desc "Forge inbox (issues)" "i" #'forge-list-global-issues
       :desc "Forge inbox (topics)" "I" #'forge-list-global-topics))

;;;; majutsu jj-mode

(use-package! majutsu
  :after magit
  :commands majutsu)

;;; provide

(provide 'project-config)

;;; project-config.el ends here
