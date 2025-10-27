;;; $DOOMDIR/+user-info.el -*- lexical-binding: t; -*-

;; User Identify (optional)
;; e.g. GPG configuration, email clients, file templates and snippets
(setq user-full-name "junghanacs"
      user-mail-address "junghanacs@gmail.com")

(setq doom-font (font-spec :family "Sarasa Mono K" :size 15.1))

;; (setq doom-font (font-spec :family "Monoplex Nerd" :size 14.0)
;;       doom-big-font (font-spec :family "Monoplex Nerd" :size 24.0)
;;       doom-variable-pitch-font (font-spec :family "Pretendard Variable" :size 14.0)
;;       doom-unicode-font (font-spec :family "Symbola" :size 14.0))

(unless (display-graphic-p) ; terminal
  (setq doom-font (font-spec :family "Sarasa Term K Nerd Font" :size 15.1)))

;;; User Profile

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))

;; 나의 공개키는 다음 에서 확인 할수 있다.
;; https://meta.sr.ht/~junghanacs.keys, https://meta.sr.ht/~junghanacs.pgp

(setq user-full-name (if (getenv "USER_FULL_NAME")
                         (getenv "USER_FULL_NAME")
                       "John Doe"))

(setq user-mail-address (if (getenv "USER_MAIL_ADDRESS")
                            (getenv "USER_MAIL_ADDRESS")
                          "john.doe@example.com"))

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to (if (getenv "EPA_FILE_ENCRYPT_TO")
                                      (list (getenv "EPA_FILE_ENCRYPT_TO"))
                                    (list "ABCDEFGHIJKLMN")))

;;;; authinfo

;; (let ((auth-gpg (expand-file-name "~/.authinfo.gpg"))
;;       (auth-file (expand-file-name "~/.authinfo")))
;;   (cond
;;    ;; gpg 파일이 있으면 그것을 auth-source로 사용
;;    ((file-exists-p auth-gpg)
;;     (setq auth-sources (list auth-gpg)
;;           auth-source-cache-expiry nil))
;;    ;; gpg는 없고 일반 파일은 이미 있으면 그대로 사용
;;    ((file-exists-p auth-file)
;;     (setq auth-sources (list auth-file)
;;           auth-source-cache-expiry nil))
;;    ;; 둘 다 없으면 일반 파일 새로 생성 후 사용
;;    (t
;;     (with-temp-buffer
;;       (write-file auth-file))
;;     (setq auth-sources (list auth-file)
;;           auth-source-cache-expiry nil))))

;; (setq user-mail-address "junghanacs@gmail.com")
;; (setq-default epa-file-encrypt-to '("B5ADD9F47612A9DB"))

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
(setq citar-notes-paths (list (concat user-org-directory "bib/")))
;; (defvar config-bibfiles (list (concat user-org-directory "bib/zotero-biblatex.bib")))
(defvar config-bibfiles (list
                         ;; (concat user-org-directory "resources/Slipbox.bib")
                         (concat user-org-directory "resources/Book.bib")
                         ;; (concat user-org-directory "resources/Category.bib")
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

