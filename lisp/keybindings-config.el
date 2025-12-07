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

;;;; Global Unset Keys

(map! "<f2>" nil
      "M-a" nil   ; forward-sentence - use evil motion instead
      "M-c" nil   ; capitalize-word
      "M-e" nil)  ; backward-sentence - use evil motion instead

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
      "C-c <right>" #'winner-redo)

;;;; Function Keys

(map! :after imenu-list
      "<f8>" #'imenu-list-smart-toggle
      "M-<f8>" #'spacemacs/imenu-list-smart-focus)

;;;; Denote Keymap (C-c n, M-e)

;; Bibliography submenu (requires citar-denote)
(after! citar-denote
  (map! :prefix ("C-c n b" . "bibliography")
        "b" #'org-cite-insert
        "c" #'citar-open
        "d" #'citar-denote-dwim
        "e" #'citar-denote-open-reference-entry
        "a" #'citar-denote-add-reference
        "1" #'citar-denote-find-citation
        "i" #'citar-insert-citation
        "n" #'citar-create-note
        "o" #'citar-denote-open-note
        "O" #'citar-open-links
        "f" #'citar-denote-find-reference
        "l" #'citar-denote-link-reference
        "s" #'citar-denote-create-silo-note
        "k" #'citar-denote-remove-reference))

;; Main Denote keymap (C-c n)
(after! denote
  (map! :prefix ("C-c n" . "denote")
        "B" #'denote-org-backlinks-for-heading
        "d" #'denote
        "f" #'+default/find-in-notes
        "i" #'denote-org-dblock-insert-links
        "I" #'denote-org-dblock-insert-backlinks
        "l" #'denote-link-or-create
        "L" #'denote-link-after-creating-with-command
        "n" #'consult-notes
        "G" #'consult-notes-search-in-all-notes
        "s" #'denote-silo-open-or-create
        "S" #'denote-silo-select-silo-then-command
        "M-f" #'denote-silo-find-file-all
        "M-0" #'denote-silo-list-all
        "M-9" #'denote-silo-count-files
        "M-8" #'denote-silo-refresh
        "t" #'denote-type
        "r" #'denote-region
        "," #'denote-rename-file-using-front-matter
        "-" #'denote-show-backlinks-buffer
        "SPC" #'org-journal-open-current-journal-file
        "j" #'org-journal-new-entry
        "u" #'org-transclusion-mode
        "k" #'denote-rename-file-keywords
        "z" #'denote-rename-file-signature
        "M-l" #'denote-find-link
        "M-b" #'denote-find-backlink)

  ;; M-e as alternative prefix for denote (quick access)
  (map! :prefix ("M-e" . "denote")
        "B" #'denote-org-backlinks-for-heading
        "d" #'denote
        "f" #'+default/find-in-notes
        "i" #'denote-org-dblock-insert-links
        "l" #'denote-link-or-create
        "n" #'consult-notes
        "G" #'consult-notes-search-in-all-notes
        "s" #'denote-silo-open-or-create
        "," #'denote-rename-file-using-front-matter
        "-" #'denote-show-backlinks-buffer
        "SPC" #'org-journal-open-current-journal-file
        "j" #'org-journal-new-entry))

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

;;;;; Notes (n)

(map! :leader
      (:prefix ("n" . "notes")
       "g" #'+default/org-notes-search
       "SPC" #'org-journal-open-current-journal-file
       "L" #'my/org-store-link-id-optional
       "u" #'org-transclusion-mode
       ;; Denote submenu
       (:prefix ("d" . "denote")
        "d" #'denote
        "f" #'+default/find-in-notes
        "l" #'denote-link-or-create
        "n" #'consult-notes
        "s" #'denote-silo-open-or-create
        "," #'denote-rename-file-using-front-matter
        "-" #'denote-show-backlinks-buffer)))

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

;;;; Evil Keys

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer)

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
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
        :inv "M-h" #'other-window
        :inv "M-z" #'evil-collection-vterm-toggle-send-escape
        :inv "M-DEL" #'my/vterm-send-meta-backspace))

;;;;; Outli

(after! outli
  (map! :map outli-mode-map
        :nv "M-j" #'outline-forward-same-level
        :nv "M-k" #'outline-backward-same-level
        :nv "M-n" #'outline-next-heading
        :nv "M-p" #'outline-previous-heading
        :nv "C-S-p" #'outline-up-heading
        :nv "z u" #'outline-up-heading))

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
        "RET" #'toc-org-markdown-follow-thing-at-point
        "-" #'markdown-insert-list-item
        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all))

;;;;; Dired

(after! dired
  (map! :map dired-mode-map
        :inv "M-h" #'other-window
        :n "C-c C-e" #'wdired-change-to-wdired-mode
        :n "C-c l" #'org-store-link
        :n "C-x /" #'dired-narrow-regexp
        :n "." #'consult-line
        :n "K" #'dired-do-kill-lines
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "S-<return>" #'dired-find-file-other-window
        :n "S-SPC" #'dired-toggle-marks))

;;;;; Prog Mode

(map! :map prog-mode-map
      :inv "M-h" #'other-window)

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
        "C-c d" #'cape-dict
        :i "<tab>" #'completion-at-point
        :i "TAB" #'completion-at-point
        "M--" #'denote-find-backlink
        ;; Localleader
        :localleader
        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all))

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

;;;;; Outline Mode

(after! outline
  (map! :map outline-mode-map
        :n "C-n" #'outline-next-heading
        :n "C-p" #'outline-previous-heading
        :i "C-n" #'next-line
        :i "C-p" #'previous-line
        :n "C-S-p" #'outline-up-heading
        :n "zu" #'outline-up-heading))

;;;; Emacs Lisp Mode

(add-hook! emacs-lisp-mode
  (setq-local comment-column 0))

(provide 'keybindings-config)
;;; keybindings-config.el ends here
