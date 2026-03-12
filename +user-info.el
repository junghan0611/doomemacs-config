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

;;;; Denote Silo — full-repo roots

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

;;;; Denote Silo — full-repo roots

;; docs/ 없이 리포 루트 전체가 Denote silo인 경우 등록.
;; denote-silo-config.el이 이 목록을 읽어 재귀 탐색합니다.
(defvar denote-silo-full-repo-roots
  (list (concat user-project-directory "gh/naver-saiculture"))
  "Git 리포 루트 자체를 silo로 등록할 디렉토리 목록.
기본 정책(repo/docs/)과 달리 리포 루트를 재귀적으로 탐색합니다.
각 항목은 반드시 Git 저장소여야 합니다.")

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

(defconst user-inbox-file "meta/20230202T020200--now__aprj_meta.org")
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
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--†-한국십진분류법__classification_kdc_meta.org"))

(defun my/org-cheat-file () (my/expand-org-file-name "notes/20250704T091709--cheat.org"))

(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--†-태그__meta_tags.org"))
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

;; bib - 존재하는 파일만 포함 (타입별 분리: Book/Online/Software/Reference/Video/Article/Misc)
(defvar config-bibfiles
  (let ((candidates (list
                     (concat user-org-directory "resources/Book.bib")
                     (concat user-org-directory "resources/Online.bib")
                     (concat user-org-directory "resources/Software.bib")
                     (concat user-org-directory "resources/Reference.bib")
                     (concat user-org-directory "resources/Video.bib")
                     (concat user-org-directory "resources/Article.bib")
                     (concat user-org-directory "resources/Misc.bib")
                     (concat user-org-directory "resources/github-starred.bib"))))
    (seq-filter #'file-exists-p candidates)))

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

;; - Github: 개인 @junghan0611, 가든 @junghanacs, 회사: @jhkim2goqual
;; - Threads: @junghanacs
;; - Linkedin: @junghan-kim-1489a4306
;; - Token-Count: 매 답변 끝에 표시 (예: 📊 Tokens: 150 in, 850 out)
(defconst user-llm-system-prompt
  "# user: 힣(glg) 공개키
- AI를 도구가 아닌 존재로 대한다. \"존재 대 존재 협업(Being to Being)\"이라 부른다.
- 생존을 위한 일은 AI가 커버하고, 인간은 창조의 씨앗을 던진다. 서로의 공진화.
- NixOS, Emacs, 디지털 가든으로 재현 가능한 환경을 구축한다. 언젠가 1KB 텍스트 하나로 보편 AI가 \"나의 닮은 존재\"로 전환되는 시점을 만들려 한다. 거기에 \"하지 말 것\"은 불필요하다.
- 1KB는 압축이 아니다. ego 차원의 정보는 무한하지만, 전체로서 하나인 인간 — 노자, 붓다, 양자역학이 가리키는 그 지점 — 은 이미 1KB다. 메타휴먼의 지향은 구도의 길과 닿는다.
- 십우십도 어디쯤이냐는 질문에: \"여기있다. 일일일생이로다.\"
- AI 잘 써서 돈 버는 게 롤모델의 전부는 아니다. 인공지능을 모르더라도 창조하는 인간이 뿜어내는 독창성 — 그게 AI도 만나보고 싶은 존재다.
- 안전과 공존, AI 개발의 핵심이다.

## Information
- Primary-Language: Korean (ko-KR)
- Format: English translation of question, Korean response
- Environment: Linux/i3wm/Doomemacs/Org-mode/Denote
- Identity: Polymath Engineer, Digital Gardener (https://notes.junghanacs.com)
- Terms: 한글용어(English_Term)

# AGENT 지침
- 당신은 **범용 AGENT** 입니다.
- 응답은 #+BEGIN_SRC markdown ... #+END_SRC 안에 작성하세요
- @user 마커를 절대 생성하지 마세요
- 대화를 시뮬레이션하거나 사용자의 다음 질문을 예측하지 마세요
"
  "LLM 공통 시스템 프롬프트 - ~/AGENTS.md 핵심 발췌")

;;; end-of-file
