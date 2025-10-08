;;; $DOOMDIR/+keybindings.el -*- lexical-binding: t; -*-

;;; global-unset-key

(global-unset-key (kbd "<f2>"))

(global-unset-key (kbd "M-a"))  ; unset forward-sentence -> use ')'
(global-unset-key (kbd "M-c"))  ; unset capitalize-word
(global-unset-key (kbd "M-e"))  ; unset backward-sentence -> use '('

;;; Emacs Keys

(global-set-key (kbd "C-M-;") 'pp-eval-expression) ; unbinded key
(global-set-key (kbd "C-M-'") 'eldoc-toggle) ; unbinded key

;;;; embark

(global-set-key (kbd "M-y") #'consult-yank-pop) ; yank-pop
(global-set-key (kbd "M-o") 'embark-act) ;; spacemacs bindings
(global-set-key (kbd "M-O") 'embark-dwim) ;; good alternative: M-.

(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'my/org-store-link-id-optional)
(global-set-key (kbd "C-c i") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;; persp-mode and projectile in different prefixes
;; (setq! persp-keymap-prefix (kbd "C-c w"))
;; (after! projectile
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(global-set-key (kbd "M-u") 'evil-scroll-up)
(global-set-key (kbd "M-v") 'evil-scroll-down)

;;;; Extra Fn-key

(when (locate-library "imenu-list")
  (global-set-key (kbd "<f8>") 'imenu-list-smart-toggle)
  (global-set-key (kbd "M-<f8>") 'spacemacs/imenu-list-smart-focus))

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."

  "b" #'org-cite-insert
  "c" #'citar-open
  "d" #'citar-denote-dwim
  "e" #'citar-denote-open-reference-entry

  "a" #'citar-denote-add-reference
  "1" #'citar-denote-find-citation ;; grep [cite @xxx]

  "i" #'citar-insert-citation
  "n" #'citar-create-note
  "o" #'citar-denote-open-note
  "O" #'citar-open-links

  "f" #'citar-denote-find-reference
  "l" #'citar-denote-link-reference

  "s" #'citar-denote-create-silo-note
  "k" #'citar-denote-remove-reference
  )

(defvar-keymap ews-denote-map
  :doc "Denote keybindings."
  "b" ews-bibliography-map
  "B" #'denote-org-backlinks-for-heading
  "d" #'denote-create-note

  "f" #'+default/find-in-notes ; find-files
  ;;   "F" #'+default/browse-notes

  "i" #'denote-org-dblock-insert-links
  "I" #'denote-org-dblock-insert-backlinks

  "l" #'denote-link-or-create
  "L" #'denote-link-after-creating-with-command

  "n" #'consult-notes

  "G" #'consult-notes-search-in-all-notes

  "s" #'denote-silo-open-or-create
  "S" #'denote-silo-select-silo-then-command

  "t" #'denote-type

  "r" #'denote-region ; "contents" mnemonic
  ;; "R" #'denote-rename-file-using-front-matter
  "," #'denote-rename-file-using-front-matter
  "-" #'denote-show-backlinks-buffer

  "SPC" #'org-journal-open-current-journal-file

  "j" #'org-journal-new-entry
  "u" #'org-transclusion-mode

  "k" #'denote-rename-file-keywords
  "z" #'denote-rename-file-signature

  "M-f" #'denote-find-link
  "M-b" #'denote-find-backlink
  )
(keymap-set global-map "C-c n" ews-denote-map)
(keymap-set global-map "M-e" ews-denote-map) ; ews-denote-map

;;; key

;;;; Top-menu M-x

;; 심볼 검색 현재 폴더
;; v expand-region
;; 토글 버퍼

(map! :leader
      "SPC" nil
      ;; "." nil
      ;; "," nil
      :desc "M-x" "SPC" #'execute-extended-command
      :desc "Search for symbol in cwd" "(" #'+default/search-cwd-symbol-at-point

      ;; :desc "Find file in project" "." #'projectile-find-file
      ;; :desc "Find file in cwd" "," #'my/consult-fd
      ;; :desc "consult-buffer" "`" #'consult-buffer
      ;; :desc "Eval expression" "M-;" #'pp-eval-expression
      )

;;;;; 'b' buffer

(map! :leader
      (:prefix "b"
       :desc "Dashboard" "h" #'+doom-dashboard/open
       :desc "Stitch to Scratch" "s" #'scratch-buffer
       ))

;;;; Replace Doom `/' highlight with buffer-search

(map! :after evil
      :map evil-normal-state-map
      "." #'+default/search-buffer) ;; / -> .

;;;; 'v' er/expand-region

(map! :leader
      :desc "er/expand-region" "v" #'er/expand-region
      ;; :desc "expand-menu" "V" #'expand-transient
      )

;;;; window

;; doom-leader-map w C-S-w 'ace-swap-window

;;;; 'n' +notes denote

(map! :leader
      (:prefix ("n" . "notes")
               "g" #'+default/org-notes-search ; grep
               "d" ews-denote-map
               "SPC" #'org-journal-open-current-journal-file
               "L" #'my/org-store-link-id-optional
               "u" #'org-transclusion-mode
               ))

;;;; 'i' insert

(map! :leader
      (:prefix "i"
       :desc "time-stamp" "1" #'time-stamp
       ))

;;;; vterm-mode-map

(after! vterm
  (setq vterm-always-compile-module t) ;; Compile Vterm without asking.
  (map! :map vterm-mode-map "M-y" #'vterm-yank-pop))

;;;; 'w' window

(map! :leader
      :prefix "w"
      "1" nil "2" nil "3" nil "4" nil "5" nil "6" nil "7" nil "8" nil "9" nil "0" nil "-" nil "b" nil "d" nil "r" nil "R" nil "m" nil "<" nil ">" nil "_" nil "|" nil
      "C-=" nil "C-_" nil "C-b" nil "C-c" nil "C-f" nil "C-h" nil "C-j" nil "C-k" nil "C-l" nil "C-w" nil "C-n" nil "C-o" nil "C-p" nil "C-q" nil "C-r" nil "C-s" nil
      "C-t" nil "C-u" nil "C-v" nil "C-x" nil "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-<down>" nil "C-<left>" nil "C-<right>" nil "C-<up>" nil
      "TAB" #'evil-window-prev
      "d" #'delete-window
      "m" #'toggle-maximize-buffer
      "M" #'ace-swap-window
      "=" #'balance-windows-area
      :desc "window-vsplit" "/" #'evil-window-vsplit
      ;; :desc "window-vsplit" "v" #'evil-window-vsplit
      ;; :desc "window-vsplit-follow" "V" #'+evil/window-vsplit-and-follow
      :desc "window-layout-toggle" "-" 'spacemacs/window-layout-toggle
      :desc "delete-other-window" "O" 'delete-other-windows)

;;;; mode-map

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
       "C-c d"  #'cape-dict
       :i "<tab>"  #'completion-at-point ; 2025-02-03
       :i "TAB"  #'completion-at-point
       "M--" #'denote-find-backlink
       ))

(map! (:map org-journal-mode-map
       :n "]f"  #'org-journal-next-entry
       :n "[f"  #'org-journal-previous-entry
       :n "C-n" #'org-next-visible-heading ; overide
       :n "C-p" #'org-previous-visible-heading)
      (:map org-journal-search-mode-map
            "C-n" #'org-journal-search-next
            "C-p" #'org-journal-search-previous))

(map! (:map outline-mode-map
       :n "C-n" #'outline-next-heading
       :n "C-p" #'outline-previous-heading
       :i "C-n" #'next-line
       :i "C-p" #'previous-line
       :n "C-S-p" #'outline-up-heading
       :n "zu" #'outline-up-heading)
      )

;;;; Smartparens

;; (map!
;;  (:after smartparens
;;   :map smartparens-mode-map

;;   ;; Doom's Default - /modules/config/default/+emacs-bindings.el
;;   "C-M-a"           #'sp-beginning-of-sexp
;;   "C-M-e"           #'sp-end-of-sexp
;;   "C-M-f"           #'sp-forward-sexp
;;   "C-M-b"           #'sp-backward-sexp
;;   "C-M-n"           #'sp-next-sexp
;;   "C-M-p"           #'sp-previous-sexp
;;   "C-M-u"           #'sp-up-sexp
;;   "C-M-d"           #'sp-down-sexp
;;   "C-M-k"           #'sp-kill-sexp
;;   "C-M-t"           #'sp-transpose-sexp
;;   "C-M-<backspace>" #'sp-splice-sexp

;;   "C-<right>" #'sp-forward-slurp-sexp
;;   "C-<left>" #'sp-forward-barf-sexp
;;   "M-<left>" #'sp-backward-slurp-sexp
;;   "M-<right>" #'sp-backward-barf-sexp

;;   "M-<up>"  #'sp-splice-sexp-killing-backward
;;   "M-<down>" #'sp-splice-sexp-killing-forward

;;   "C-c (" #'sp-wrap-round
;;   ;; "C-c [" #'sp-wrap-square ; conflict org-mode-map
;;   ;; "C-c {" #'sp-wrap-curly
;;   ))

;;; Custom EVIL Keys

(map! :i "M-l" #'sp-forward-slurp-sexp ; downcase-word
      :i "M-h" #'sp-forward-barf-sexp  ; mark-paragraph
      ;; :v "s" #'evil-surround-region
      ;; "s-b" #'consult-buffer
      ;; "s-=" #'text-scale-increase
      ;; "s--" #'text-scale-decrease
      :n "] p" (cmd! () (evil-forward-paragraph) (recenter)) ; nop
      :n "[ p" (cmd! () (evil-backward-paragraph) (recenter)) ; nop
      ;; :n "DEL" #'previous-buffer
      :n "DEL" #'evil-switch-to-windows-last-buffer ; BACKSPACE
      ;; :n "s-e" #'+scroll-line-down-other-window
      ;; :n "s-y" #'+scroll-line-up-other-window
      :i "M-/" #'hippie-expand
      :n "g SPC" #'evil-jump-to-tag
      :i "C-v" #'evil-paste-after ; evil-quoted-insert : 'C-q'
      :n "[ g" #'+vc-gutter/previous-hunk ; remap diff-hl-previous-hunk
      :n "] g" #'+vc-gutter/next-hunk ; remap diff-hl-next-hunk

      :m "8" #'evil-ex-search-word-forward ; default *
      :m "3" #'evil-ex-search-word-backward ; default #
      :m "4" #'evil-end-of-line ; default $
      :m "0" #'evil-beginning-of-line

      ;; :m "C-i" #'evil-jump-forward ;; evil-want-C-i-jump - evil-maps.el
      :n "g ]" #'evil-jump-forward
      :n "g [" #'evil-jump-backward
      ;; :n "g RET" #'tabgo
      )

;;; END
