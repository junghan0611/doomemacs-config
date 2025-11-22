;;; $DOOMDIR/lisp/evil-config.el --- Evil Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (evil "1.15"))

;;; Commentary:

;; Evil 모드 설정
;; - Evil core settings
;; - Evil-escape
;; - Smartparens integration

;;; Code:

;;;; evil

(after! evil
  ;; C-h is backspace in insert state
  ;; (setq evil-want-C-h-delete t) ; default nil
  (setq evil-want-C-w-delete t) ; default t
  (setq evil-want-C-u-scroll t) ; default t

  ;; use C-i / C-o  evil-jump-backward/forward
  ;; (setq evil-want-C-i-jump t) ; default nil

  ;;  /home/junghan/sync/man/dotsamples/vanilla/mpereira-dotfiles-evil-clojure/configuration.org
  ;; FIXME: this correctly causes '*' to match on whole symbols (e.g., on a
  ;; Clojure file pressing '*' on 'foo.bar' matches the whole thing, instead of
  ;; just 'foo' or 'bar', BUT, it won't match 'foo.bar' in something like
  ;; '(foo.bar/baz)', which I don't like.
  ;; (setq-default evil-symbol-word-search t)
  ;; (setq evil-jumps-cross-buffers nil)
  (setq evil-want-Y-yank-to-eol t) ; doom t

  ;; 'Important' Prevent the cursor from moving beyond the end of line.
  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil) ; nil is better - default t
  (setq evil-move-beyond-eol nil) ; default nil

  (setq +evil-want-o/O-to-continue-comments nil) ; doom t
  (setq +default-want-RET-continue-comments nil) ; doom t

  (setq evil-want-fine-undo t) ; doom 'nil

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil) ; default t
  ;; Don't create a kill entry on every visual movement.
  ;; More details: https://emacs.stackexchange.com/a/15054:
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-motion-state-map "M" nil)

    ;; Replace Emacs Tabs key bindings with Workspace key bindings
    ;; replace "." search with consul-line in Evil normal state
    ;; use default "/" evil search

    ;; disable evil macro
    (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
    (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

    ;; o :: ace-link-info 이거면 충분하다.
    (define-key evil-insert-state-map (kbd "C-]") 'forward-char) ; very useful

    ;; =C-w= 'insert 'evil-delete-backward-word
    ;; =C-w= 'visual 'evil-window-map
    ;; use evil bindings $ ^

    ;; M-d region delete and C-d char delete
    (define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)

    ;; Don't put overwritten text in the kill ring
    ;; evil-delete-char -> delete-forward-char
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char)
    )

  ;; evil-org
  (with-eval-after-load 'evil-org
    ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-d") 'delete-forward-char)
    (evil-define-key 'normal 'evil-org-mode-map "x" 'delete-forward-char)
    ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-k") 'org-kill-line)
    ;; (evil-define-key 'insert 'org-mode-map (kbd "C-k") 'org-kill-line)
    (evil-define-key 'normal 'evil-org-mode-map "X" 'delete-backward-char))
  )

;;;; evil-escape

;; ,. as Esc key binding
;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59/7
(after! evil-escape
  (setq evil-escape-key-sequence ",.") ;; "jk"
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay 1.0) ;; 0.5, default 0.1
  (evil-escape-mode 1)

  ;; 모바일 타이핑 최적화
  (when IS-TERMUX
    (setq evil-escape-delay 0.8)))

;;;; smartparens

(after! smartparens
  ;; 2023-09-14 global 로 사용하다보니 거슬린다. 잠시만. 글로벌을 빼면 어떤가?
  ;; ("\\\\(" . "\\\\)") ;; emacs regexp parens
  ;; ("\\{"   . "\\}")   ;; latex literal braces in math mode
  ;; ("\\("   . "\\)")   ;; capture parens in regexp in various languages
  ;; ("\\\""  . "\\\"")  ;; escaped quotes in strings
  ;; ("/*"    . "*/")    ;; C-like multi-line comment
  ;; ("\""    . "\"")    ;; string double quotes
  ;; ("'"     . "'")     ;; string single quotes/character quotes
  ;; ("("     . ")")     ;; parens (yay lisp)
  ;; ("["     . "]")     ;; brackets
  ;; ("{"     . "}")     ;; braces (a.k.a. curly brackets)
  ;; ("`"     . "`")     ;; latex strings. tap twice for latex double quotes

  ;; Unbind `M-s' (set by paredit keybindings above) because it's bound
  ;; to some handy occur related functions
  ;; (define-key sp-keymap (kbd "M-s") nil)

  ;; org 모드에서 거슬린다. 제거. 굳.
  (sp-local-pair 'org-mode "(" ")" :actions '(rem)) ; for denote completion
  (sp-local-pair 'org-mode "[" "]" :actions '(rem)) ; temporarly
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "`" "`" :actions '(rem))
  (sp-local-pair 'org-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'org-mode "/" "/" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "~" "~" :actions '(rem))

  ;; markdown 에서도 삭제
  (sp-local-pair 'markdown-mode "(" ")" :actions '(rem))
  (sp-local-pair 'markdown-mode "'" "'" :actions '(rem))
  (sp-local-pair 'markdown-mode "`" "`" :actions '(rem))
  (sp-local-pair 'markdown-mode "\"" "\"" :actions '(rem))
  (sp-local-pair 'markdown-mode "/" "/" :actions '(rem))

  ;; pair management
  (sp-with-modes
      '(minibuffer-mode)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "(" nil :wrap "C-("))
  (sp-with-modes 'markdown-mode (sp-local-pair "**" "***"))
  (sp-with-modes
      'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))

  (sp-with-modes
      'org-mode
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "$$" "$$"))
  )

(provide 'evil-config)
;;; evil-config.el ends here
