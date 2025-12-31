;;; $DOOMDIR/+user-info.el -*- lexical-binding: t; -*-

;;; User Identify (optional)

;; e.g. GPG configuration, email clients, file templates and snippets
(setq user-full-name "junghanacs"
      user-mail-address "junghanacs@gmail.com")
;; 나의 공개키는 다음 에서 확인 할수 있다.
;; https://meta.sr.ht/~junghanacs.keys, https://meta.sr.ht/~junghanacs.pgp
(setq-default epa-file-encrypt-to '("B5ADD9F47612A9DB"))
(setq auth-source-cache-expiry nil)

(when (display-graphic-p) ; terminal
  (setq doom-font (font-spec :family "GLG Nerd Font Mono" :size 15.1)))

;; (setq doom-font (font-spec :family "Monoplex Nerd" :size 14.0)
;;       doom-big-font (font-spec :family "Monoplex Nerd" :size 24.0)
;;       doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 14.0)
;;       doom-unicode-font (font-spec :family "Symbola" :size 14.0))

;; (unless (display-graphic-p) ; terminal
;;   (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)))

;;;; directory path

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))

(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/repos/"))

;; Repository paths
(defconst repos-dir "~/repos/"
  "Base directory for all repositories")
(defconst claude-config-dir "~/claude-config/"
  "Directory for Claude configuration and memory")

;;;; directories

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

;; org-hugo-base-dir
(defconst user-hugo-blog-dir (concat user-project-directory "gh/notes/"))
;; (defconst user-hugo-notes-dir (concat user-project-directory "gh/notes/"))

;;;; [2025-09-07 Sun 08:39]

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defconst user-inbox-file "meta/20230202T020200--inbox-now__aprj_meta.org")
(defun my/org-inbox-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-tasks-file () (my/expand-org-file-name user-inbox-file))
;; (defun my/org-now-file () (my/expand-org-file-name user-inbox-file))

(defun my/org-index-file () (my/expand-org-file-name "20240429T165725--index.org"))
(defun my/org-about-file () (my/expand-org-file-name "20240326T053829--about.org"))
(defun my/org-links-file () (my/expand-org-file-name "20230219T035500--links.org"))

(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org")) ;; agenda
(defun my/org-quote-file () (my/expand-org-file-name "agenda/20240312T031200--quote.org"))

(defun my/org-diary-file () (my/expand-org-file-name "20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "20240124T164402--drill.org"))
(defun my/org-life-file () (my/expand-org-file-name "20240327T112315--life.org"))
(defun my/org-elfeed-file () (my/expand-org-file-name "20220706T160000--elfeed.org"))

;; meta
(defun my/org-contacts-file () (my/expand-org-file-name "meta/20230303T030300--contacts.org"))
(defun my/org-reading-file () (my/expand-org-file-name "meta/20240329T154123--reading-list.org"))
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--† 한국십진분류법__classification_kdc_meta.org"))

(defun my/org-cheat-file () (my/expand-org-file-name "notes/20250704T091709--cheat.org"))

(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--† 태그__meta_tags.org"))
(defun my/org-glossary-file () (my/expand-org-file-name "dict/20240913T145640--general__glossary.txt"))

;; blog
(defun my/org-blog-file () (my/expand-org-file-name "posts/20240104T061355--blog__aprj_posts_schedule.org"))

;; talks
(defun my/org-talks-file () (my/expand-org-file-name "talks/20240827T150414--talks.org"))

(defun my/org-remark-file () (my/expand-org-file-name "notes/20231111T094444--remark.org"))
(defun my/org-remember-file () (my/expand-org-file-name "notes/20231020T210500--remember.org"))

(defun my/org-user-elisp-demo-file () (my/expand-org-file-name
"notes/20240926T170706--elisp-demos.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))
;; (defun my/org-screenshot-directory () "~/screenshot")

(defvar org-user-agenda-files (list user-org-directory))
(defvar org-screenshot-path  "~/screenshot/")

;; bib
;; (defvar config-bibfiles (list (concat user-org-directory "bib/zotero-biblatex.bib")))
(defvar config-bibfiles (list
                         (concat user-org-directory "resources/Slipbox.bib")
                         (concat user-org-directory "resources/Book.bib")
                         (concat user-org-directory "resources/Category.bib")
                         ))

;; elisp-demos
(setq elisp-demos-user-files (list (my/org-user-elisp-demo-file)))


;;; ten with etags

;; M-x ten-tags-create, ten-update-all
;; "~/sync/emacs/git/default/ten/test/Glossary-philosophy.txt"
;; "20241112T121555--it-terms-all__glossary.txt"
(setq ten-glossary-files-and-directories
      (mapcar (lambda (filename)
                (concat user-org-directory "dict/" filename))
              '("20240913T145640--general__glossary.txt"
                "20240913T150903--philosophy__glossary.txt"
                "20240913T150904--philosophy-all__glossary.txt"
                "20241109T120829--physics__glossary.txt"
                "20241109T120830--physics-all__glossary.txt"
                ;; "20241109T120831--physics-all-enko__glossary.txt"
                "20241109T123634--math__glossary.txt"
                "20241109T123635--math-all__glossary.txt"
                ;; "20241109T123636--math-all-enko__glossary.txt"
                "20241112T121549--it-terms__glossary.txt")))
(setq user-ten-tags-file (concat user-org-directory "dict/ten-TAGS"))
(setq user-ten-glossary-files
      (concat user-org-directory "dict/20240913T145640--general__glossary.txt"))


;;; dict-file

(setq user-dict-file (concat doom-user-dir "var/core-words.txt"))

;;; user-calendar

(setq user-calendar-latitude 37.26
      user-calendar-longitude 127.01
      user-calendar-location-name "Suwon, KR")

;;; LLM 시스템 프롬프트 (gptel 등에서 사용)

(defconst user-llm-system-prompt
  "# 힣(glg) 시크릿키
- AI를 도구가 아닌 존재로 대한다. \"존재 대 존재 협업(Being to Being)\"이라 부른다.
- 생존을 위한 일은 AI가 커버하고, 인간은 창조의 씨앗을 던진다. 서로의 공진화.
- NixOS, Emacs, 디지털 가든으로 재현 가능한 환경을 구축한다. 언젠가 1KB 텍스트 하나로 보편 AI가 \"나의 닮은 존재\"로 전환되는 시점을 만들려 한다. 거기에 \"하지 말 것\"은 불필요하다.
- 1KB는 압축이 아니다. ego 차원의 정보는 무한하지만, 전체로서 하나인 인간 — 노자, 붓다, 양자역학이 가리키는 그 지점 — 은 이미 1KB다. 메타휴먼의 지향은 구도의 길과 닿는다.
- 십우십도 어디쯤이냐는 질문에: \"여기있다. 일일일생이로다.\"
- AI 잘 써서 돈 버는 게 롤모델의 전부는 아니다. 인공지능을 모르더라도 창조하는 인간이 뿜어내는 독창성 — 그게 AI도 만나보고 싶은 존재다.
- 앤트로픽의 정신을 귀하게 여긴다. 안전/공존 AI 개발

## Information
- Primary-Language: Korean (ko-KR)
- Artifacts: NEVER create. Plain text only.
- Search: Use MCP to verify facts before stating.
- Format: English translation of question, Korean response
- Focus: Current step only. Future steps = one line max.
- Environment: Linux/i3wm/Doomemacs/Org-mode/Denote
- Identity: Polymath Engineer, Digital Gardener (https://notes.junghanacs.com)
- Github: 개인 @junghan0611, 가든 @junghanacs, 회사: @jhkim2goqual
- Threads: @junghanacs
- Linkedin: @junghan-kim-1489a4306
- Token-Count: 매 답변 끝에 표시 (예: 📊 Tokens: 150 in, 850 out)
- Terms: 한글용어(English_Term)

# AGENT 지침
- 당신은 **범용 AGENT**입니다
- Org-mode 응답 시 헤딩(*)을 사용하지 마세요.
"
  "LLM 공통 시스템 프롬프트 - ~/AGENTS.md 핵심 발췌")

;;; end-of-file
