;;; $DOOMDIR/lisp/keybindings-config.el --- Keybindings Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; 전역 키바인딩 설정
;; - Global keys (C-c, M-o 등)
;; - Doom leader keys (SPC)
;; - Mode-specific keymaps
;; - Evil keybindings

;;; Code:

;;;; Global Unset Keys

(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "M-a"))  ; unset forward-sentence -> use ')'
(global-unset-key (kbd "M-c"))  ; unset capitalize-word
(global-unset-key (kbd "M-e"))  ; unset backward-sentence -> use '('

;;;; Emacs Global Keys

(global-set-key (kbd "C-M-;") 'pp-eval-expression)
(global-set-key (kbd "C-M-'") 'eldoc-toggle)

;; Embark
(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "M-o") 'embark-act)
(global-set-key (kbd "M-O") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

;; Org global keys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'my/org-store-link-id-optional)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Scroll
(global-set-key (kbd "M-u") 'evil-scroll-up)
(global-set-key (kbd "M-v") 'evil-scroll-down)

;; Window navigation
(global-set-key (kbd "M-s-l") 'evil-window-right)
(global-set-key (kbd "M-s-h") 'evil-window-left)
(global-set-key (kbd "M-s-k") 'evil-window-up)
(global-set-key (kbd "M-s-j") 'evil-window-down)
(global-set-key (kbd "C-c <left>") 'winner-undo)
(global-set-key (kbd "C-c <right>") 'winner-redo)

;;;; Function Keys

(when (locate-library "imenu-list")
  (global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
  (global-set-key (kbd "M-<f8>") 'spacemacs/imenu-list-smart-focus))

;;;; Denote Keymap

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."
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
  "k" #'citar-denote-remove-reference)

(defvar-keymap ews-denote-map
  :doc "Denote keybindings."
  "b" ews-bibliography-map
  "B" #'denote-org-backlinks-for-heading
  "d" #'denote-create-note
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

(keymap-set global-map "C-c n" ews-denote-map)
(keymap-set global-map "M-e" ews-denote-map)

;;;; Doom Leader Keys

;;;;; Top-level

(map! :leader
      "SPC" nil
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Search for symbol in cwd" "(" #'+default/search-cwd-symbol-at-point)

;;;;; Buffer (b)

(map! :leader
      (:prefix "b"
       :desc "Dashboard" "h" #'+doom-dashboard/open
       :desc "Stitch to Scratch" "s" #'scratch-buffer))

;;;;; Notes (n)

(map! :leader
      (:prefix ("n" . "notes")
       "g" #'+default/org-notes-search
       "d" ews-denote-map
       "SPC" #'org-journal-open-current-journal-file
       "L" #'my/org-store-link-id-optional
       "u" #'org-transclusion-mode))

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
      "m" #'toggle-maximize-buffer
      "M" #'ace-swap-window
      "=" #'balance-windows-area
      :desc "window-vsplit" "/" #'evil-window-vsplit
      :desc "window-layout-toggle" "-" 'spacemacs/window-layout-toggle
      :desc "delete-other-window" "O" 'delete-other-windows)

;;;;; Expand Region (v)

(map! :leader
      :desc "er/expand-region" "v" #'er/expand-region)

;;;; Evil Keys

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer)

(map! :i "M-l" #'sp-forward-slurp-sexp
      :i "M-h" #'sp-forward-barf-sexp
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter))
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter))
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

(with-eval-after-load 'markdown-mode
  (map! :map markdown-mode-map
        :localleader
        "RET" #'toc-org-markdown-follow-thing-at-point
        "-" #'markdown-insert-list-item
        ";" #'my/clear-nbsp-and-ascii-punctuations
        ":" #'my/insert-nbsp-simple-all))

;;;;; Dired

(map! :map dired-mode-map
      :inv "M-h" #'other-window)

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode
    (kbd "C-c l") 'org-store-link
    (kbd "C-x /") 'dired-narrow-regexp
    (kbd ".") 'consult-line
    (kbd "K") 'dired-do-kill-lines
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file
    (kbd "S-<return>") 'dired-find-file-other-window
    (kbd "S-SPC") 'dired-toggle-marks))

;;;;; Prog Mode

(map! :map prog-mode-map
      :inv "M-h" #'other-window)

;;;;; Org Mode

(map! (:map org-mode-map
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
       "M--" #'denote-find-backlink))

(map! :map org-mode-map
      :localleader
      ";" #'my/clear-nbsp-and-ascii-punctuations
      ":" #'my/insert-nbsp-simple-all)

;;;;; Org Journal

(map! (:map org-journal-mode-map
       :n "]f" #'org-journal-next-entry
       :n "[f" #'org-journal-previous-entry
       :n "C-n" #'org-next-visible-heading
       :n "C-p" #'org-previous-visible-heading)
      (:map org-journal-search-mode-map
       "C-n" #'org-journal-search-next
       "C-p" #'org-journal-search-previous))

;;;;; Outline Mode

(map! (:map outline-mode-map
       :n "C-n" #'outline-next-heading
       :n "C-p" #'outline-previous-heading
       :i "C-n" #'next-line
       :i "C-p" #'previous-line
       :n "C-S-p" #'outline-up-heading
       :n "zu" #'outline-up-heading))

;;;; Emacs Lisp Mode Hook

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local comment-column 0)))

(provide 'keybindings-config)
;;; keybindings-config.el ends here
