;;; init.el -*- lexical-binding: t; -*-

;;; Pre-init

;; This is so that I don't accidentally start Emacs as a daemon.
;; (when (daemonp) (kill-emacs))

;;;; Termux

(setq-default root-path "/")

(defvar IS-TERMUX
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(when IS-TERMUX
  (setq root-path "/data/data/com.termux/files/"))

;;;; DONT android gui emacs

;; 2025-04-28
;; (when (string-equal system-type "android")
;;   (when (display-graphic-p) ; gui
;;     ;; Add Termux binaries to PATH environment
;;     ;; It is important that termuxpath is prepended, not appended.
;;     ;; Otherwise we will get Androids incompatible diff executable, instead of the one in Termux.
;;     (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
;;       (setenv "PATH" (format "%s:%s" termuxpath
;;                              (getenv "PATH")))
;;       (push termuxpath exec-path)
;;       (push "~/.config/emacs/bin" exec-path))
;;     )
;;   )

;;;; Custom

;; delete insert keymap
(setq evil-org-key-theme '(navigation textobjects additional calendar todo))

;;;; Modules

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row
       :completion
       (corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW/XXX/BUG
       ;; indent-guides     ; highlighted indent columns
       ;; minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults) ; tame sudden yet inevitable temporary windows
       smooth-scroll  ; So smooth you won't believe it's not butter

       ;;tabs            ; a tab bar for Emacs
       ;;treemacs        ; a project drawer, like neotree but cooler
       vc-gutter         ; +pretty - vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers) ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen            ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       (whitespace +trim)  ; a butler for your whitespace
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       eww               ; the internet is gross
       ibuffer           ; +icons - interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +flymake) ; tasing you for every semicolon you forget
       ;; (spell +flyspell) ; +hunspell - tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       biblio
       direnv
       ;; debugger ; FIXME stepping through code, to help you add bugs

       (docker +tree-sitter)
       (eval +overlay)     ; run code, run (also, repls)
       lookup
       llm                 ; when I said you needed friends, I didn't mean...
       (lsp +eglot)

       magit ; +forge a git porcelain for Emacs

       make              ; run make tasks from Emacs
       (pass +auth)              ; password manager for nerds
       (:unless IS-TERMUX (pdf)) ; pdf enhancements
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tree-sitter ;; syntax and parsing, sitting in a tree...
       upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       tty                 ; improve the terminal Emacs experience

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       ;; plantuml            ; diagrams for confusing people more
       ;; graphviz
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org                         ; organize your plain life in plain text
        +hugo                     ; use Emacs for hugo blogging
        +pandoc                     ; export-with-pandoc support
        +gnuplot                    ; who doesn't like pretty pictures
        +present                    ; using org-mode for presentations
        +contacts
        +journal
        ;; +jupyter                    ; ipython/jupyter support for babel
        ;; +pretty
        ;; +noter                      ; enhanced PDF notetaking
        ;; +pomodoro                 ; be fruitful with the tomato technique
        )                     ; wander around notes
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       (python +tree-sitter)
       (nix +tree-sitter)               ; I hereby declare "nix geht mehr!"
       (json +tree-sitter)  ; At least it ain't XML
       (janet +tree-sitter)  ; Fun fact: Janet is me!
       (javascript +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       (web +tree-sitter) ; the tubes
       (yaml +tree-sitter) ; JSON, but readable
       (zig +tree-sitter +lsp)  ; C, but simpler
       (cc +tree-sitter)

       (clojure +tree-sitter +lsp) ; +tree-sitter java with a lisp
       (scheme +mit) ;; +racket ; a fully conniving family of lisps

       :email
       (notmuch +org)
       ;; (mu4e +org +gmail +mbsync)

       :app
       calendar
       ;; emms
       ;; everywhere        ; *leave* Emacs!? You must be joking
       ;; irc               ; how neckbeards socialize
       (rss +org +youtube)        ; emacs as an RSS reader

       :config
       ;; literate ; use manually
       (default +bindings +smartparens)
       )
