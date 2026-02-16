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
   consult-source-recent-file consult-source-project-recent-file consult-source-bookmark
   :preview-key '("C-SPC"
                  :debounce 0.3 "<up>" "<down>" "M-j" "M-k"))
  )

;;;; vertico

;;;;; DONT vertico-buffer on TOP

;; (when (display-graphic-p) ; gui
;;   ;; vertico-buffer on-top
;;   (require 'vertico-buffer)
;;   (setq vertico-resize 'grow-only) ; doom nil

;;   ;; vertico on Top
;;   (setq vertico-buffer-display-action
;;         `(display-buffer-in-side-window
;;           (window-height . ,(+ 3 vertico-count)) (side . top)))
;;   (vertico-mode +1)
;;   (vertico-buffer-mode +1))

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

;;;; embark

(progn

;;;###autoload
  (eval-when-compile
    (defmacro embark-ace-action (fn)
      `(defun ,(intern (concat "embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

;;;###autoload
  (defmacro embark-split-action (fn split-type)
    `(defun ,(intern (concat "embark-"
                             (symbol-name fn)
                             "-"
                             (symbol-name split-type))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn)))


;;;###autoload
  (defun avy-action-embark (pt)
    ;; borrowed from
    ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

;;;###autoload
  (defun +edebug-instrument-symbol (symbol)
    (interactive "sSymbol: ")
    (edebug-instrument-function (intern symbol)))

;;;###autoload
  (defun +embark-collect-outline-cycle (&optional arg)
    (interactive "P")
    (if arg (outline-cycle-buffer)
      (outline-cycle))
    (evil-beginning-of-line))

;;;###autoload
  (defun +search-rfc-number-online (&optional rfc-num)
    "Search for RFC of RFC-NUM."
    (interactive)
    (browse-url
     (format
      "https://www.rfc-editor.org/search/rfc_search_detail.php?rfc=%s"
      rfc-num)))

;;;###autoload
  (defun +browse-rfc-number-at-point ()
    "Reads RFC number at point."
    (interactive)
    (require 'org)
    (if-let* ((rfc-pattern "\\b[rR][fF][cC][- ]?[0-9]+\\b")
              (bounds (org-in-regexp rfc-pattern 1))
              (rfc-num (string-to-number
                        (replace-regexp-in-string
                         "[^0-9]" ""
                         (buffer-substring-no-properties
                          (car bounds)
                          (cdr bounds))))))
        (if (featurep 'rfc-mode)
            (switch-to-buffer-other-window
             (rfc-mode--document-buffer rfc-num))
          (+search-rfc-number-online rfc-num))
      (if (featurep 'rfc-mode)
          (rfc-mode-browse)
        (+search-rfc-number-online))))

;;;###autoload
  (defun +embark-project-search (target)
    (+vertico-file-search :query target))

;;;###autoload
  (defun embark-open-externally+ (file)
    "Extended version of `embark-open-externally'."
    (interactive "fOpen: ")
    (pcase (file-name-extension file)
      ;; ("mp3" (dired-file-to-mplayer file))
      ;; ("m4a" (dired-file-to-mplayer file))
      (_ (embark-open-externally file))))

  ;; ;;;###autoload
  ;; (defun embark-preview+ ()
  ;;   "My custom embark preview function."
  ;;   (interactive)
  ;;   (when-let* ((target (car (embark--targets)))
  ;;               (type (plist-get target :type)))
  ;;     (cond
  ;;      ((and (member type '(url consult-omni))
  ;;            (string-match-p
  ;;             ;; only match PRs/Issues or individual files
  ;;             "https://github\\.com/\\([^/]+/[^/]+/\\)\\(pull\\|issues\\|blob\\)[^#\n]+"
  ;;             (plist-get target :target)))
  ;;       (cl-labels ((forge-visit-topic-url*
  ;;                     (url &rest _)
  ;;                     (forge-visit-topic-via-url url)))
  ;;         (embark--act #'forge-visit-topic-url* target nil)))

  ;;      ((member type '(url consult-omni))
  ;;       (cl-labels ((eww-browse-url*
  ;;                     (url &rest _)
  ;;                     (eww-browse-url url)))
  ;;         (embark--act #'eww-browse-url* target nil)))

  ;;      ((fboundp 'embark-dwim)
  ;;       (save-selected-window
  ;;         (let (embark-quit-after-action)
  ;;           (embark-dwim)))))))

;;;; embark merge file

  (progn
    ;; - Dired 모드에서 여러 파일을 마크합니다 (`m` 키 사용).
    ;; - `M-x dired-merge-files` 명령을 실행합니다.
    ;; - 출력 파일의 경로와 이름을 묻는 프롬프트가 나타나면 원하는 출력 파일명을 입력합니다.
    (defun dired-merge-files (output-file)
      "Merge marked files in dired to a single OUTPUT-FILE."
      (interactive "FOutput file: ")
      (let ((files (dired-get-marked-files)))
        (with-temp-buffer
          (dolist (file files)
            (insert-file-contents file)
            (goto-char (point-max))
            (insert "\n"))
          (write-file output-file))))

    (defun embark-dired-merge-action ()
      "Embark action to merge marked files in dired."
      (interactive)
      (let ((output-file (read-file-name "Output file: ")))
        (dired-merge-files output-file)))

    ;; (add-to-list 'embark-file-map '("Merge Files" . embark-dired-merge-action))
    )
  )

;;; provide

(provide 'completion-config)

;;; completion-config.el ends here
