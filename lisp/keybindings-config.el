;;; $DOOMDIR/lisp/keybindings-config.el --- Keybindings Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; 전역 키바인딩 설정 - Doom 스타일(map!) 통일
;; - Global keys (C-c, M-o 등)
;; - Doom leader keys (SPC)
;; - Mode-specific keymaps
;; - Evil keybindings

;;; Code:

;;;; Ignore Focus Events

;; 포커스 이벤트 무시
(define-key input-decode-map "\e[I" [ignore])
(define-key input-decode-map "\e[O" [ignore])

;;;; remap all C-c prefix keys to M-c

(define-key key-translation-map (kbd "M-c") (kbd "C-c"))

;;;; Global Keys

(map! "C-M-;" #'pp-eval-expression
      "C-M-'" #'eldoc-toggle
      ;; Embark
      "M-y" #'consult-yank-pop
      "M-o" #'embark-act
      "M-O" #'embark-dwim
      "C-h B" #'embark-bindings
      ;; Org global
      "C-c l" #'org-store-link
      "C-c L" #'my/org-store-link-id-optional
      "C-c i" #'org-insert-link
      "C-c a" #'org-agenda

      ;; Scroll (evil)
      "M-u" #'evil-scroll-up
      "M-v" #'evil-scroll-down
      ;; Window navigation (super key)
      "M-s-l" #'evil-window-right
      "M-s-h" #'evil-window-left
      "M-s-k" #'evil-window-up
      "M-s-j" #'evil-window-down

      "s-[" #'evil-window-left
      "s-]" #'evil-window-right
      "s-{" #'evil-window-up
      "s-}" #'evil-window-down

      ;; Winner
      "C-c <left>" #'winner-undo
      "C-c <right>" #'winner-redo

      ;; Minibuffer access
      "M-0" #'switch-to-minibuffer)

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-\\" #'sp-forward-barf-sexp
      :n "] p" (cmd! (evil-forward-paragraph) (recenter))
      :n "[ p" (cmd! (evil-backward-paragraph) (recenter))
      :n "DEL" #'evil-switch-to-windows-last-buffer
      :i "M-/" #'hippie-expand
      :n "g SPC" #'evil-jump-to-tag
      :i "C-v" #'evil-paste-after
      :n "[ g" #'+vc-gutter/previous-hunk
      :n "] g" #'+vc-gutter/next-hunk
      :m "8" #'evil-ex-search-word-forward
      :m "3" #'evil-ex-search-word-backward
      :m "4" #'evil-end-of-line
      :m "0" #'evil-beginning-of-line
      :n "g ]" #'evil-jump-forward
      :n "g [" #'evil-jump-backward)

;;;; F1-12: Function Keys

(map! :after imenu-list
      "<f8>" #'imenu-list-smart-toggle)

;;;; Doom Leader Keys

;;;;; Top-level

(map! :leader
      "SPC" nil
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Search symbol in cwd" "(" #'+default/search-cwd-symbol-at-point
      :desc "er/expand-region" "v" #'er/expand-region)

;;;;; Buffer (b)

(map! :leader
      (:prefix "b"
       :desc "Dashboard" "h" #'+doom-dashboard/open
       :desc "Switch to Scratch" "s" #'scratch-buffer))

;;;;; Files (f)

(map! :leader
      (:prefix ("f" . "files")
               "y" #'my/yank-buffer-absolute-path
               "RET" #'my/yank-buffer-path-with-line
               "1" #'my/yank-buffer-path-with-line
               "2" #'my/yank-buffer-path-relative-with-line
               ))

;;;;; Notes (n)

(map! :leader
      (:prefix ("n" . "notes")
               "g" #'+default/org-notes-search
               "SPC" #'org-journal-open-current-journal-file
               "L" #'my/org-store-link-id-optional
               ))

;;;;; Insert (i)

(map! :leader
      (:prefix "i"
       :desc "time-stamp" "1" #'time-stamp))

;;;;; Window (w)

(map! :leader
      :prefix "w"
      ;; Clear unused Doom defaults
      "1" nil "2" nil "3" nil "4" nil "5" nil "6" nil "7" nil "8" nil "9" nil "0" nil
      "-" nil "b" nil "d" nil "r" nil "R" nil "m" nil "<" nil ">" nil "_" nil "|" nil
      "C-=" nil "C-_" nil "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-k" nil
      "C-l" nil "C-w" nil "C-n" nil "C-o" nil "C-p" nil "C-q" nil "C-r" nil "C-s" nil
      "C-t" nil "C-u" nil "C-v" nil "C-x" nil "C-S-h" nil "C-S-j" nil "C-S-k" nil
      "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-<down>" nil "C-<left>" nil
      "C-<right>" nil "C-<up>" nil
      ;; Custom bindings
      "TAB" #'evil-window-prev
      "d" #'delete-window
      "m" #'doom/window-maximize-buffer
      "M" #'ace-swap-window
      "=" #'balance-windows-area
      :desc "vsplit" "/" #'evil-window-vsplit
      :desc "layout-toggle" "-" #'spacemacs/window-layout-toggle
      :desc "delete-other" "O" #'delete-other-windows)

;;;;; Layout (l)

;; spacemacs's style
(map! :leader
      (:prefix ("l". "layout/workspace")
       :desc "+workspace/other" "<tab>" #'+workspace/other
       :desc "+workspace/display" "d" #'+workspace/display
       :desc "+workspace/delete" "D" #'+workspace/delete
       :desc "+workspace/switch-to" "l" #'+workspace/switch-to
       :desc "open workspaces" "o" #'my/open-workspaces
       :desc "+workspace/load" "L" #'+workspace/load
       :desc "+workspace/new" "n" #'+workspace/new
       :desc "+workspace/rename " "r" #'+workspace/rename
       :desc "+workspace/restore-last-session" "R" #'+workspace/restore-last-session
       :desc "+workspace/save" "s" #'+workspace/save
       :desc "+workspace/kill-session" "x" #'+workspace/kill-session
       :desc "1st workspace" "1" #'+workspace/switch-to-0
       :desc "2nd workspace" "2" #'+workspace/switch-to-1
       :desc "3rd workspace" "3" #'+workspace/switch-to-2
       :desc "4th workspace" "4" #'+workspace/switch-to-3
       :desc "5th workspace" "5" #'+workspace/switch-to-4
       :desc "6th workspace" "6" #'+workspace/switch-to-5
       :desc "7th workspace" "7" #'+workspace/switch-to-6))

;;;;; Help (h)

(map! :leader
      (:prefix ("h" . "help")
               "t" nil
               (:prefix ("t" . "themes")
                :desc "Modus toggle"         "m" #'modus-themes-toggle
                :desc "Modus select"         "M" #'modus-themes-select
                :desc "Random EF Dark"          "d" #'modus-themes-load-random-dark
                :desc "Random EF Light"         "l" #'modus-themes-load-random-light
                :desc "Doric random"         "r" #'doric-themes-load-random
                :desc "Doric select"         "R" #'doric-themes-select)))

;;;;; TODO Toggle (T)

;;;; Key Functions

;;;;; +default/search-buffer : consult-line

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer
      :map ibuffer-mode-map
      :nv "." #'+default/search-buffer
      :map dired-mode-map
      :nv "." #'+default/search-buffer)

;;;; Mode-specific Keymaps

;;;;; Vterm

(after! vterm
  (defun my/vterm-send-alt-return ()
    "Send <alt>-<return> to vterm."
    (interactive)
    (vterm-send-key "" nil t))

  (defun my/vterm-send-meta-backspace ()
    "Send M-Backspace to vterm (backward-kill-word)."
    (interactive)
    (vterm-send-key (kbd "DEL") nil t))

  (setq vterm-always-compile-module t)
  (undefine-key! vterm-mode-map "M-," "M-e" "M-." "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9" "M-0")
  (map! :map vterm-mode-map
        :i "M-RET" #'my/vterm-send-alt-return
        :inv "M-y" #'vterm-yank-pop
        :inv "M-\\" #'other-window
        :inv "M-z" #'evil-collection-vterm-toggle-send-escape
        :inv "M-DEL" #'my/vterm-send-meta-backspace))

;;;;; Markdown

(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :nv "M-j" #'markdown-outline-next-same-level
        :nv "M-k" #'markdown-outline-previous-same-level
        :nv "M-n" #'markdown-outline-next
        :nv "M-p" #'markdown-outline-previous
        :nv "C-S-p" #'outline-up-heading
        :nv "z u" #'outline-up-heading))

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        "y" #'my/yank-as-org
        "RET" #'toc-org-markdown-follow-thing-at-point
        "-" #'markdown-insert-list-item
        ";" #'my/clear-nbsp-and-ascii-punctuations
       ":" #'my/insert-nbsp-simple-all))

;;;;; Dired

(after! dired
  (map! :map dired-mode-map
        :n "r" #'revert-buffer
        :inv "M-\\" #'other-window
        :inv "M-\\" #'other-window
        :n "C-c C-e" #'wdired-change-to-wdired-mode
        :n "C-c l" #'org-store-link
        :n "C-x /" #'dired-narrow-regexp
        :n "." #'+default/search-buffer ; 'consult-line
        :n "K" #'dired-do-kill-lines
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "S-<return>" #'dired-find-file-other-window
        :n "S-SPC" #'dired-toggle-marks)

  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode
        "SPC" #'dired-hide-details-mode
        "H" #'dired-hide-details-mode
        "p" #'dired-preview-mode
        :desc "sort-modified-date" "o" #'dired-sort-toggle-or-edit
        ;; "m" #'my/dired-attach-to-mastodon
        :desc "*denote-insert* marked-notes" "i" #'my/denote-link-dired-marked-notes
        ;; "g" #'prot-dired-grep-marked-files
        ;; "l" #'prot-dired-limit-regexp
        "y" #'+default/yank-buffer-absolute-path

        :desc "*denote-rename* files" "r" #'denote-dired-rename-files
        :desc "*denote-rename* using front-matter" "R" #'denote-dired-rename-marked-files-using-front-matter
        :desc "*denote-rename* with keywords" "w" #'denote-dired-rename-marked-files-with-keywords
        :desc "*denote-rename* add keywords" "k" #'denote-dired-rename-marked-files-add-keywords
        :desc "*denote-rename* remove keywords" "K" #'denote-dired-rename-marked-files-remove-keywords

        ;; :desc "*casual-dired* menu" ";" #'casual-dired-tmenu
        ;; "-" #'nerd-icons-dired-mode
        ;; "P" #'my/dired-hugo-export-wim-to-md
        ;; "M" #'my/diff-mark-toggle-vc-modified
        ;; "m" #'my/diff-hl-dired-mark-modified
        )
  )

;;;;; Prog-mode

(map! :map prog-mode-map
      "C-M-y" #'evil-yank
      :inv "M-\\" #'other-window)

;;;;; Org Mode

(after! org
  (map! :map org-mode-map
        :ni "C-c H" #'org-insert-heading
        :ni "C-c S" #'org-insert-subheading
        :i "C-n" #'next-line
        :i "C-p" #'previous-line
        :n "C-S-p" #'outline-up-heading
        :n "C-j" #'org-forward-heading-same-level
        :n "C-k" #'org-backward-heading-same-level
        :n "C-n" #'org-next-visible-heading
        :n "C-p" #'org-previous-visible-heading
        :n "zu" #'outline-up-heading
        )

  (map! :map org-mode-map
        :i "<tab>" #'completion-at-point
        :i "TAB" #'completion-at-point
        "M-g 1" #'bh/insert-inactive-timestamp
        "M-g 2" #'org-cite-insert
        "M-s ,"   #'denote-rename-file-using-front-matter
        "M-s <"   #'denote-rename-file-title
        ;; Link & clipboard
        "<f3>"    #'org-toggle-link-display
        "C-c M-y" #'org-download-clipboard
        "C-c o"   #'consult-org-heading
        "C-c y"   #'org-cliplink
        "C-c I"   #'org-insert-link-dwim
        :nvi "C-c M-i" #'org-cliplink
        )

  (map! :map org-mode-map
        ;; "C-c d" #'cape-dict
        "M-\\" #'other-window
        "M--" #'denote-find-backlink
        "M-s ," #'denote-rename-file-using-front-matter
        )

  (map! :map org-mode-map
        "C-x n b" #'org-cite-insert
        "C-x n -" #'bh/insert-inactive-timestamp
        "C-x n 0" #'my/org-insert-notes-drawer
        "C-x n m" #'my/split-and-indirect-orgtree
        "C-x n M" #'my/kill-and-unsplit-orgtree
        "C-x n 9" #'my/org-count-words
        "C-x n l" #'my/denote-org-store-link-to-heading
        )

  (map! :map org-mode-map
        :localleader
        "y" #'my/yank-as-markdown
        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all
        )
  )

;;;;; Org Journal

(after! org-journal
  (map! :map org-journal-mode-map
        :n "]f" #'org-journal-next-entry
        :n "[f" #'org-journal-previous-entry
        :n "C-n" #'org-next-visible-heading
        :n "C-p" #'org-previous-visible-heading)
  (map! :map org-journal-search-mode-map
        "C-n" #'org-journal-search-next
        "C-p" #'org-journal-search-previous))


;;;;; Outli and Outline Mode

(after! outli
  ;; Outline heading에서만 cycle, 그 외에는 evil-jump-item (%)
  (defun my/outline-cycle-or-evil-jump-item ()
    "On outline heading, run `outline-cycle'. Otherwise, run `evil-jump-item'."
    (interactive)
    (if (outline-on-heading-p)
        (outline-cycle)
      (evil-jump-item)))

  (map! :map outli-mode-map
        :i "C-n" #'next-line
        :i "C-p" #'previous-line
        :n "C-n" #'outline-next-heading
        :n "C-p" #'outline-previous-heading
        :nv "<tab>" #'my/outline-cycle-or-evil-jump-item
        :nv "TAB" #'my/outline-cycle-or-evil-jump-item
        :nv "M-j" #'outline-forward-same-level
        :nv "M-k" #'outline-backward-same-level
        :nv "M-n" #'outline-next-heading
        :nv "M-p" #'outline-previous-heading
        :nv "C-S-p" #'outline-up-heading
        :nv "z u" #'outline-up-heading))


;;; provide

(provide 'keybindings-config)

;;; keybindings-config.el ends here
