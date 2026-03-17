;;; $DOOMDIR/lisp/org-config.el --- Org-mode Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (org "9.6"))

;;; Commentary:

;; Org-mode 설정
;; - Agenda, Capture, Journal
;; - Citar (bibliography)
;; - Outline 구조 (outli.el 호환)

;;; Code:

;;;; org

;; (require 'ob-tangle)

(after! org
  (message "after org - config")

  ;; (load-file (concat doom-user-dir "lisp/org-funcs.el"))
  ;; (load-file (concat doom-user-dir "lisp/org-config.el"))
  ;; (+org-init-keybinds-h) -> 2024-06-01 여기 키바인딩 관련 부분 뒤에서 다시 잡아줌
  ;; (setq org-attach-use-inheritance nil) ; selective

  (progn
    (setq org-capture-bookmark nil)
    (setq org-edit-src-content-indentation 0) ; default 2

    )

  (setq org-id-locations-file
        (file-name-concat org-directory (concat "." system-name "-orgids"))) ; ".org-id-locations"))

  ;; overide here! important
  (setq org-insert-heading-respect-content nil) ; doom t
  ;; org-indent-mode 사용하면 org-hide-leading-stars 자동 nil
  (setq org-startup-indented nil) ; doom t
  (setq org-hide-leading-stars nil) ; doom t
  )

(after! org

;;;; org-todo-keywords : whhone

  (progn
    ;; https://whhone.com/emacs-config/
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "DONT(o)")))

    (with-no-warnings
      (custom-declare-face '+org-todo-todo  '((t (:inherit (bold error org-todo)))) "")
      (custom-declare-face '+org-todo-next  '((t (:inherit (bold warning org-todo)))) "")
      (custom-declare-face '+org-todo-done  '((t (:inherit (bold success org-todo)))) "")
      (custom-declare-face '+org-todo-dont '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
      )

    (setq org-todo-keyword-faces
          '(("TODO" . +org-todo-todo) ;; red
            ("DONE" . +org-todo-done) ;; green
            ("NEXT" . +org-todo-next) ;; yellow
            ("DONT" . +org-todo-dont) ;; green
            ))

    ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
    (setq org-agenda-custom-commands
          '(("n" "Agenda / NEXT"
             ((agenda "" nil)
              (tags "INBOX+LEVEL=2|CATEGORY=\"Inbox\"+LEVEL=1")
              (todo "NEXT" nil)
              ;; (todo "TODO" nil) ;; 2024-03-18 add
              ) nil)
            (" " "Agenda and all TODOs" ; default' view
             ((agenda "")
              (alltodo "")))))
    )

;;;; custom agenda files

  ;; workflow-shared.el: config.el 끝에서 (after! denote) 로드
  ;; _aprj 태그 + botlog/agenda/ 동적 구성

  (setq org-agenda-diary-file (my/org-diary-file))
  (setq org-default-notes-file (my/org-inbox-file))

  ;; doom-emacs capture files : absolute path
  (setq +org-capture-todo-file (my/org-inbox-file))
  (setq +org-capture-notes-file (my/org-inbox-file))
  (setq +org-capture-changelog-file (my/org-inbox-file))
  (setq +org-capture-projects-file (my/org-inbox-file))
  (setq +org-capture-journal-file (my/org-diary-file))

;;;; org-agenda

  ;; NOTE 2026-03-17: org-agenda 전체가 ISO 8601 주차(%V)를 사용하는데,
  ;; 위클리저널 파일명은 %W(월요일 시작 카운트) 기준. 1주 차이 발생.
  ;; org-days-to-iso-week를 advice로 %W 기준으로 교체하여
  ;; 헤더 "Day-agenda (W##):"와 날짜 줄 모두 한번에 통일.
  (define-advice org-days-to-iso-week (:override (days) use-monday-week-number)
    "Return %W week number (Monday-start count) instead of ISO 8601.
Matches `org-journal-file-format' week%W convention."
    (let* ((date (calendar-gregorian-from-absolute days))
           (month (car date))
           (day (cadr date))
           (year (nth 2 date))
           (time (encode-time 0 0 0 day month year)))
      (string-to-number (format-time-string "%W" time))))

  ;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
  ;; (setq org-agenda-sticky t) ; default nil

  ;; Shift the agenda to show the previous 3 days and the next 7 days for
  ;; better context on your week. The past is less important than the future.
  (setq org-agenda-span 'day) ; default 'week, doom 10
  (setq org-agenda-start-day nil) ; nil = today (doom default: "-3d")

  ;; Hide all scheduled todo.
  (setq org-agenda-todo-ignore-scheduled 'all)

  ;; Ignores "far" deadline TODO items from TODO list.
  (setq org-agenda-todo-ignore-deadlines 'far)

  ;; Hide all scheduled todo, from tags search view, like tags-todo.
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; Hide all done todo in agenda
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Hide task until the scheduled date.
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  (setq org-log-into-drawer t)

  (setq org-log-done 'time)

  ;; (setcdr (assoc 'note org-log-note-headings) "%d")
  ;; Interstitial Journaling: add note to CLOCK entry after clocking out
  ;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
  (setq org-log-note-clock-out t)

  ;; 4 priorities to model Eisenhower's matrix.
  ;; - [#A] means +important +urgent
  ;; - [#B] means +important -urgent
  ;; - [#C] means -important +urgent
  ;; - [#D] means -important -urgent
  (setq org-priority-default 68
        org-priority-lowest 68)

;;;; diary-file

  (setq diary-file (concat doom-user-dir "diary"))
  (setq org-agenda-include-diary t)

;;;; org-agenda-log-mode and clock-mode

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-with-log-mode t)

  ;; Agenda log mode items to display (closed clock : default)
  ;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
  ;; closed    Show entries that have been closed on that day.
  ;; clock     Show entries that have received clocked time on that day.
  ;; state     Show all logged state changes.
  ;; (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-log-mode-add-notes nil)

  ;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

  ;; (org-clock-auto-clockout-insinuate) ; auto-clockout
  ;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
  (setq org-clock-string-limit 30) ; default 0

  ;; org-clock-persist for share with machines
  (setq org-clock-persist-query-save t)
  (setq org-clock-persist-query-resume t)

  ;; current  Only the time in the current instance of the clock
  ;; today    All time clocked into this task today
  ;; repeat   All time clocked into this task since last repeat
  ;; all      All time ever recorded for this task
  ;; auto     Automatically, either all, or repeat for repeating tasks
  (setq org-clock-mode-line-entry t)
  (setq org-clock-mode-line-line-total 'auto) ; default nil

  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties
        (quote
         (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
          ("STYLE_ALL" . "habit"))))

;;;; org-tag and category

  ;; org-tag-re: workflow-shared.el에서 설정 (agent-server와 공유)

  ;; (setq org-auto-align-tags nil) ; default t, use doom's custom
  ;; (setq org-tags-column 0) ; default -77
  ;; org-agenda-tags-column은 아래 prefix-format 근처에서 'auto로 설정
  (setq org-agenda-show-inherited-tags nil)

  (setq org-tag-alist (quote (
                              (:startgroup) ;; Action
                              ("later" . ?.)
                              ("now" . ?,)
                              (:endgroup)
                              ("important" . ?i) ; 별도 처리
                              ("waiting" . ?w)
                              ("next" . ?n)
                              ("hold" . ?h)
                              ;; ("crypt" . ?E)
                              ("note" . ?o)
                              ("noexport" . ?x)
                              ("nonum" . ?u)
                              ("ATTACH" . ?a)
                              ("latest" . ?l) ;; latest version
                              )))

  (add-to-list 'org-tags-exclude-from-inheritance "projects") ; projects 왜 구분했었지?

;;;; org-agenda-custom-commands

  ;; ol-doi ol-w3m ol-bbdb ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
  ;; ol-eww ol-bibtex
  ;; Adapted from http://stackoverflow.com/a/12751732/584121
  ;; (require 'org-protocol)
  (setq org-protocol-default-template-key "L")
  (setq org-modules `(org-habit org-protocol))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda  . " %i %-14:c%?-12t% s")
  ;;         (todo  . " %i %-14:c")
  ;;         (tags  . " %i %-14:c")
  ;;         (search . " %i %-14:c")))

  ;; https://www.pygopar.com/creating-new-columns-in-org-agenda
  ;; Originally from here: https://stackoverflow.com/a/59001859/2178312
  (defun gopar/get-schedule-or-deadline-if-available ()
    (let ((scheduled (org-get-scheduled-time (point)))
          (deadline (org-get-deadline-time (point))))
      (if (not (or scheduled deadline))
          (format " ")
        ;; (format "🗓️ ")
        "   ")))

  (defun my/org-agenda-category-short ()
    "카테고리 첫 글자 반환. marker가 없는 줄(타임라인, diary)은 공백."
    (let ((marker (org-get-at-bol 'org-marker)))
      (if marker
          (let ((cat (org-get-category)))
            (if (and cat (> (length cat) 0))
                (substring cat 0 1)
              " "))
        " ")))

  (if (display-graphic-p)
      (setq org-agenda-prefix-format
            '((agenda . " %i %-7:c%?-12t% s ")
              (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
              (tags . " %i %-12:c")
              (search . " %i %-12:c")))
    ;; TUI (Termux 등): 1글자 카테고리 + 시간 + 내용
    (setq org-agenda-prefix-format
          '((agenda  . " %i%(my/org-agenda-category-short) %?-12t% s")
            (todo  . " %i%(my/org-agenda-category-short) ")
            (tags  . " %i%(my/org-agenda-category-short) ")
            (search . " %i%(my/org-agenda-category-short) "))))

  (setq org-agenda-category-icon-alist nil)

  (setq org-agenda-tags-column 'auto) ; 창 너비에 맞춰 태그 배치

  (setq org-agenda-hide-tags-regexp
        "agenda\\|DONT\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

  (defun my/org-agenda-truncate-lines ()
    "Agenda 엔트리 제목을 잘라서 태그가 보이도록 보장.
태그 영역(마지막 N컬럼)을 침범하는 긴 제목을 '..'+태그로 교체."
    (let* ((inhibit-read-only t)
           (win-width (or (window-width) 80))
           ;; 태그 영역: 창 너비의 ~30% (최소 20자, 최대 35자)
           (tag-reserve (min 35 (max 20 (/ win-width 3)))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (org-get-at-bol 'org-marker)
            (let* ((line-end (line-end-position))
                   (tags (org-get-at-bol 'tags))
                   (tag-str (if tags (concat " :" (mapconcat #'identity tags ":") ":") ""))
                   (tag-len (length tag-str))
                   (max-content (- win-width tag-len 1)) ; 1 for space before tags
                   (line-str (buffer-substring (line-beginning-position) line-end))
                   (content-len (- (length line-str) (if (string-match "\\s-+:[^ ]+:$" line-str)
                                                         (length (match-string 0 line-str))
                                                       0))))
              (when (> content-len max-content)
                ;; 기존 태그 부분 제거 후 잘린 제목 + 태그 다시 삽입
                (let* ((content-start (line-beginning-position))
                       (pure-content (buffer-substring content-start
                                                       (+ content-start
                                                          (min content-len (length line-str)))))
                       ;; 기존 태그 제거한 순수 내용
                       (clean (if (string-match "\\(.*?\\)\\s-+:[^ \n]+:$" pure-content)
                                  (match-string 1 pure-content)
                                pure-content))
                       (trunc-len (- max-content 2)) ; 2 for ".."
                       (truncated (if (> (length clean) trunc-len)
                                      (concat (substring clean 0 trunc-len) "..")
                                    clean)))
                  (delete-region (line-beginning-position) line-end)
                  (insert truncated
                          (make-string (max 1 (- win-width (length truncated) tag-len)) ?\s)
                          tag-str)))))
          (forward-line 1)))))

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (my/org-agenda-truncate-lines)
              (define-key org-agenda-mode-map [(double-mouse-1)] 'org-agenda-goto-mouse)))

  (defun cc/org-agenda-goto-now ()
    "Redo agenda view and move point to current time '← now'.
org-agenda-sticky=t 환경에서 날짜가 캐시되는 문제 해결."
    (interactive)
    (org-agenda-redo t)      ; t = full rebuild (캐시 무시)
    (org-agenda-goto-today)
    (ignore-errors
      (if window-system
          (search-forward "← now ─")
        (search-forward "now -"))))

  (after! evil-org-agenda
    (evil-define-key* 'motion evil-org-agenda-mode-map
      "F" #'org-agenda-follow-mode))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "<backspace>") #'evil-switch-to-windows-last-buffer)
              (define-key org-agenda-mode-map (kbd "DEL") #'evil-switch-to-windows-last-buffer)
              ;; (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
              ;; (define-key org-agenda-mode-map (kbd "M-P") 'ash/org-pomodoro-til-meeting)
              (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))

  ;; (setq org-archive-location "archives/%s_archive::")
  (setq org-archive-location (file-name-concat org-directory "archives/%s::"))

  ;; nil 이면 C-c C-o 으로 접근한다.
  ;; (setq org-mouse-1-follows-link t) ; default 450

  (setq org-capture-template-dir (concat doom-user-dir "captures/"))
  (setq org-datetree-add-timestamp t)

;;;; Simple is Better

  ;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
  (setq org-capture-templates nil)
  (add-to-list
   'org-capture-templates
   `("i" "Inbox" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %?\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("I" "Inbox (Work)" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %? :work:\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("p" "Project /w template" entry (file+headline ,(my/org-inbox-file) "Projects")
     (file ,(concat org-capture-template-dir "project.capture"))))

  ;; (add-to-list
  ;;  'org-capture-templates
  ;;  `("l" "links" entry (file ,(my/org-links-file))
  ;;    "* TODO %(org-cliplink-capture)" :immediate-finish t))

  (add-to-list
   'org-capture-templates
   `("T" "Personal Todo /w clock-in" entry (file ,(my/org-inbox-file))
     "* TODO [#C] %?\n%T\n%a\n" :clock-in t :clock-resume t))
  )

;;;; org-journal

(progn
  (require 'org-journal)
  (setq org-journal-dir (concat user-org-directory "journal"))
  (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal_week%W.org")
  (setq org-journal-date-format "%Y-%m-%d %A") ; Week%W:

  ;; (setq org-journal-time-format "%R ")
  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"NEXT\"")

  (setq org-journal-enable-agenda-integration nil) ; my/org-journal-new-entry가 현재 주만 등록
  (setq org-journal-file-type 'weekly)

  (setq org-journal-tag-alist '(("meet" . ?m) ("dev" . ?d) ("idea" . ?i) ("emacs" . ?e) ("discuss" . ?c) ("1on1" . ?o))) ; default nil
  )

;;;; citar

(progn
  (require 'citar)
  (require 'bibtex)
  (setq citar-bibliography config-bibfiles)
  (setq org-cite-global-bibliography config-bibfiles)
  (setq citar-templates
        '((main
           .
           ;; [${urldate:10}]
           "'${dateadded:10} ${author editor:19} ${title:49} ${date year issued:4} ${translator:7} №${=key= id:17}")  ; 2024-09-12 김정한
          (suffix
           . "#${datemodified:10} ${=type=:10} ${shorttitle:19} ${namea:16} ${url:19} ${tags keywords:*}") ; 2024-11-17 add url
          (preview
           .
           "*** ${title}\n${shorttitle}\n${author} ${translator} ${namea} ${year issued date:4}\n\n${abstract}\n") ; citar-copy-reference
          (note . "#+title: ${author translator:10}, ${title}")))

  (add-hook 'bibtex-mode-hook 'display-line-numbers-mode)
  (add-hook 'bibtex-mode-hook 'visual-line-mode)

  (setq bibtex-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t)
  (setq bibtex-text-indentation 20)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'citar-history))
  )


;;;; org-download

(use-package! org-download
  :after org
  :hook (;; (dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :commands (org-download-enable
             org-download-yank
             org-download-screenshot)
  :config
  (setq-default org-download-heading-lvl nil)
  (setq org-download-method 'directory) ; doom 'attach
  (setq-default org-download-image-dir "~/screenshot" ) ;; share all devieces
  (setq org-download-display-inline-images nil)
  (setq org-download-timestamp"%Y%m%dT%H%M%S-") ;; denote id

  ;; #+caption: "
  ;; #+name: fig-"
  ;; #+attr_html: :width 40% :align center"
  ;; #+attr_latex: :width \\textwidth"
  (setq org-download-image-attr-list
        '("#+attr_html: :width 80% :align center"
          "#+attr_latex: :width \\textwidth"
          "#+attr_org: :width 800px"))

  ;; (defun kimim/org-download-annotate (link)
  ;;   "Annotate LINK with the time of download."
  ;;   (format "#+name: fig:%s\n#+caption: %s\n"
  ;;           (file-name-base link) (file-name-base link)))
  ;; (setq org-download-annotate-function #'kimim/org-download-annotate)
  )

;;;; org-rich-yank

(use-package! org-rich-yank
  :defer t
  :commands (org-rich-yank))

;;;; org-appear

(use-package! org-appear
  :after org
  :if window-system
  :init
  ;; (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks nil ;; default nil
        org-appear-autoemphasis t
        org-appear-autosubmarkers t)
  )

;;;; org-glossary

(use-package! org-glossary
  :after org
  :init
  (setq org-glossary-idle-update-period 1.0) ; 0.5
  ;; (setq org-glossary-autodetect-in-headings t) ; 2024-06-13 new
  ;; :hook (org-mode . org-glossary-mode)
  :config
  ;; (setq org-glossary-collection-root (concat org-directory "dict/"))
  ;; (setq org-glossary-global-terms "global")

  (define-key org-mode-map (kbd "C-}") 'org-glossary-insert-term-reference)
  (define-key org-mode-map (kbd "C-{") 'org-glossary-create-definition)
  (define-key org-mode-map (kbd "C-\"") 'org-glossary-create-definition)
  (setq org-glossary-automatic nil) ;; disable auto-export
  )

;; sample from tecosaur/org-glossary
;; (defun +org-glossary--latex-cdef (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
;;   (org-glossary--export-template
;;    (if (plist-get term-entry :uses)
;;        "*%d*\\emsp{}%v\\ensp{}@@latex:\\labelcpageref{@@%b@@latex:}@@\n"
;;      "*%d*\\emsp{}%v\n")
;;    backend info term-entry ref-index
;;    plural-p capitalized-p extra-parameters))
;; (org-glossary-set-export-spec
;;  'latex t
;;  :backref "gls-%K-use-%r"
;;  :backref-seperator ","
;;  :definition-structure #'+org-glossary--latex-cdef)

;;; provide

(provide 'org-config)

;;; org-config.el ends here
