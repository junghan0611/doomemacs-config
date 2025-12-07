;;; $DOOMDIR/lisp/completion-config.el --- Completion Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Completion 시스템 설정
;; - corfu: in-buffer completion
;; - vertico: minibuffer completion
;; - consult: enhanced search/navigation

;;; Code:

;;;; corfu

;; 2024-09-13 기본 설정, jump-out-of-pair 추가
;; Tab 이 자동 완성이면 괄호 점프랑 충돌 난다. C-j/k C-n/p 는 직관적인 기본 설정이므로 건들이지 않는다.

(after! corfu
  ;; (setq corfu-auto-delay 0.5) ; doom 0.24
  (setq corfu-auto-prefix 4) ; doom 2, default 3
  ;; (setq corfu-preselect 'valid) ; doom 'prompt
  ;; (setq tab-always-indent t) ; for jump-out-of-pair - doom 'complete
  (setq +corfu-want-minibuffer-completion nil) ; doom t

  (setq +corfu-want-tab-prefer-expand-snippets nil)
  (setq +corfu-want-tab-prefer-navigating-snippets nil)
  (setq +corfu-want-tab-prefer-navigating-org-tables nil)

  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (eq emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  ;; IMO, modern editors have trained a bad habit into us all: a burning need for
  ;; completion all the time -- as we type, as we breathe, as we pray to the
  ;; ancient ones -- but how often do you *really* need that information? I say
  ;; rarely. So opt for manual completion:
  ;; doom/hlissner-dot-doom/config.el
  ;; (setq corfu-auto nil)

  ;; default 'C-S-s'
  (define-key corfu-map (kbd "M-.") '+corfu-move-to-minibuffer)
  )

;;;; consult

(after! consult
  ;; (setq consult--customize-alist nil)

  (consult-customize
   +default/search-project +default/search-other-project
   +default/search-project-for-symbol-at-point
   +default/search-cwd +default/search-other-cwd
   +default/search-notes-for-symbol-at-point
   +default/search-emacsd
   :preview-key '("C-SPC" :debounce 0.3 "<up>" "<down>" "M-j" "M-k"))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '("C-SPC"
                  :debounce 0.3 "<up>" "<down>" "M-j" "M-k"))
  )

;;;; vertico

;;;;; vertico-buffer on TOP

;; vertico-buffer on-top
(progn
  (require 'vertico-buffer)
  (setq vertico-resize 'grow-only) ; doom nil

  ;; vertico on Top
  (setq vertico-buffer-display-action
        `(display-buffer-in-side-window
          (window-height . ,(+ 3 vertico-count)) (side . top)))
  (vertico-mode +1)
  (vertico-buffer-mode +1))

;;;;; vertico-map

(after! vertico
  (map! :map vertico-map
        "M-j" #'vertico-next
        "M-k" #'vertico-previous

        ;; C-d M-d: 문자 하나씩 삭제 (DEL은 기본 동작 유지: 디렉토리 단위 삭제)
        "C-d" #'delete-backward-char
        "M-d" #'delete-backward-char
        ))

;;;;; marginalia with vertico-sort

;;;;;; marginalia for file

(after! vertico
  (require 'marginalia)
  (defun gr/marginalia--annotate-local-file (cand)
    "Annotate local file CAND.
Removes modes, which I've never needed or wanted."
    (marginalia--in-minibuffer
      (when-let (attrs (ignore-errors
                         ;; may throw permission denied errors
                         (file-attributes (substitute-in-file-name
                                           (marginalia--full-candidate cand))
                                          'integer)))
        (marginalia--fields
         ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
         ((marginalia--time (file-attribute-modification-time attrs))
          :face 'marginalia-date :width -12)
         ;; File owner at the right
         ((marginalia--file-owner attrs) :face 'marginalia-file-owner)))))

  (defun gr/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
    (if-let (remote (or (marginalia--remote-file-p cand)
                        (when-let (win (active-minibuffer-window))
                          (with-current-buffer (window-buffer win)
                            (marginalia--remote-file-p (minibuffer-contents-no-properties))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (gr/marginalia--annotate-local-file cand)))

  ;; M-A 순서를 바꾸면 된다.
  (add-to-list 'marginalia-annotators
               '(file gr/marginalia-annotate-file marginalia-annotate-file builtin none))

;;;;;; vertico sort modified

  ;; (setq vertico-multiform-categories nil)
  ;; (setq vertico-multiform-categories
  ;;       '(
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         ;; (file (vertico-sort-function . gr/sort-modified))
  ;;         (file (+vertico-transform-functions . +vertico-highlight-directory)) ; doom default
  ;;         ))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun gr/sort-modified (list)
    "Sort LIST of files for latest modified."
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (x list)
        (puthash x (file-attribute-modification-time (file-attributes x)) ht))
      (sort list
            (lambda (a b)
              (let ((one
                     (gethash a ht))
                    (two
                     (gethash b ht)))
                (time-less-p two one))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'gr/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-," #'vertico-sort-modified))

;;; provide

(provide 'completion-config)

;;; completion-config.el ends here
