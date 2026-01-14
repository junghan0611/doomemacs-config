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

;;;;; Preview 최적화: org-indent-mode 건너뛰기

;; Preview 버퍼에서 org-indent-mode 비활성화 (성능 개선)
;; consult-preview-variables에 org-startup-indented 추가
;; - Doom은 org-startup-indented t 설정 (modules/lang/org/config.el:122)
;; - 이 변수가 t이면 org-mode 초기화 시 직접 org-indent-mode 호출
;; - delay-mode-hooks는 훅만 지연시키므로 직접 호출은 막지 못함
(after! consult
  (add-to-list 'consult-preview-variables '(org-startup-indented . nil))
  (add-to-list 'consult-preview-variables '(org-startup-folded . nil)))

;;;;; consult-customize

(after! consult
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

;;;;; consult-ripfd

;; consult-ripfd: 통합 파일 + 컨텐츠 검색 인터페이스
;; https://github.com/jdtsmith/consult-ripfd
;;
;; [특징]
;; - ripgrep (rg): 파일 내용 검색
;; - fd: 파일명 검색
;; - 단일 인터페이스에서 두 검색 결과 통합 표시
;; - consult 기반 → 기존 preview, embark 액션 자동 지원
;;
;; [사용법]
;; 1. s-F 또는 M-x consult-ripfd 실행
;; 2. 검색어 입력 (파일명 + 컨텐츠 동시 검색)
;; 3. C-SPC로 preview, M-o로 embark 액션
;;
;; [Embark 연계]
;; consult-ripfd는 consult-grep과 동일한 category를 사용하므로
;; embark-file-map과 embark-consult-map의 액션이 자동 적용됨:
;; - embark-file-map: 파일 열기, dired, external 등
;; - embark-consult-map: export, wgrep 편집 등
;; - 커스텀 액션: gptel-quick (`[`), project search (`/`) 등
;;
;; [gptel 연계 시나리오]
;; 1. consult-ripfd로 코드/문서 검색
;; 2. M-o → `[` (gptel-quick) 또는 커스텀 액션
;; 3. 선택한 파일/내용을 gptel에 전달하여:
;;    - 코드 설명 요청
;;    - 리팩토링 제안
;;    - 문서 요약/번역
;;    - 버그 분석
;;
;; [TODO] embark-consult-map에 다음 액션 추가 검토:
;; - 선택 파일들을 gptel context로 일괄 추가
;; - org-mode heading/src-block을 gptel에 직접 전달
;; - 검색 결과를 임시 버퍼로 collect → gptel 분석
(use-package! consult-ripfd
  :after consult
  :bind ("s-F" . consult-ripfd))

;;;; vertico

;;;;; vertico-buffer on TOP

(when (display-graphic-p) ; gui
  ;; vertico-buffer on-top
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

        ;; C-d 문자 하나씩 삭제 (DEL은 기본 동작 유지: 디렉토리 단위 삭제)
        "C-d" #'delete-backward-char
        ))

;;;;; marginalia with vertico-sort

;;;;;; marginalia for file

(after! vertico
  (require 'marginalia)
  (defun my/marginalia--annotate-local-file (cand)
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

  (defun my/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
    (if-let (remote (or (marginalia--remote-file-p cand)
                        (when-let (win (active-minibuffer-window))
                          (with-current-buffer (window-buffer win)
                            (marginalia--remote-file-p (minibuffer-contents-no-properties))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (my/marginalia--annotate-local-file cand)))

  ;; M-A 순서를 바꾸면 된다.
  (add-to-list 'marginalia-annotators
               '(file my/marginalia-annotate-file marginalia-annotate-file builtin none))

;;;;;; vertico sort modified

  ;; (setq vertico-multiform-categories nil)
  ;; (setq vertico-multiform-categories
  ;;       '(
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         ;; (file (vertico-sort-function . my/sort-modified))
  ;;         (file (+vertico-transform-functions . +vertico-highlight-directory)) ; doom default
  ;;         ))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun my/sort-modified (list)
    "Sort LIST of files for latest modified.
Handles both absolute paths and relative paths (for denote-file-prompt).
For relative paths, tries `default-directory' and `denote-directory'."
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (x list)
        (let* ((path (cond
                      ;; Already absolute
                      ((file-name-absolute-p x) x)
                      ;; Try default-directory first
                      ((file-exists-p (expand-file-name x default-directory))
                       (expand-file-name x default-directory))
                      ;; Try denote-directory if available
                      ((and (boundp 'denote-directory)
                            (file-exists-p (expand-file-name x denote-directory)))
                       (expand-file-name x denote-directory))
                      ;; Fallback: just use as-is
                      (t x)))
               (attrs (ignore-errors (file-attributes path))))
          (puthash x (if attrs
                         (file-attribute-modification-time attrs)
                       0)  ; fallback for non-existent files
                   ht)))
      (sort list
            (lambda (a b)
              (let ((one (gethash a ht))
                    (two (gethash b ht)))
                (cond
                 ;; Both are times
                 ((and (consp one) (consp two))
                  (time-less-p two one))
                 ;; Only one is a time
                 ((consp one) t)
                 ((consp two) nil)
                 ;; Neither is a time, keep original order
                 (t nil)))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'my/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-," #'vertico-sort-modified))

;;; provide

(provide 'completion-config)

;;; completion-config.el ends here
