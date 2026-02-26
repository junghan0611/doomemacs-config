;;; $DOOMDIR/lisp/elfeed-config.el --- Elfeed configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Elfeed 커스텀 설정
;; - elfeed-tube 연동 (YouTube 자막)
;; - 컨텐츠 검색
;; - gptel 인라인 요약/번역 (elfeed-summarize 참고, gptel 백엔드)
;; - remember 연동 (annotation)

;;; Code:

;;;; Elfeed

;; gc copy-link
(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  (setq elfeed-search-filter "") ; "@1-year-ago" "@6-months-ago" "@1-month-ago +unread"
  (setq elfeed-search-title-max-width 90) ; default 70
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)
  )

(after! elfeed-tube
  (require 'elfeed-tube)
  ;; (setq elfeed-tube-invidious-url "https://vid.puffyan.us")
  (setq elfeed-tube-captions-languages '("en" "ko" "englsh (auto generated)")))

;;;;; Elfeed-web (웹 인터페이스)

(let ((elfeed-web-dir (expand-file-name "lisp/elfeed-web" doom-user-dir)))
  (when (file-directory-p elfeed-web-dir)
    (when (locate-library "simple-httpd")
      (require 'simple-httpd)
      (load! "elfeed-web/elfeed-web"))))

;; 외부 기기에서 접속 허용
;; (setq httpd-host "0.0.0.0")

;;;; 본문 검색 (elfeed-deref 기반 - archive.gz 호환)

;; Doom Emacs는 elfeed 컨텐츠를 archive.gz로 압축 저장
;; cuckoo-search(ripgrep 직접 검색)는 호환 안됨
;; → elfeed-deref로 컨텐츠 읽어서 검색

(defun +elfeed-search-content (pattern)
  "Search elfeed entries by content PATTERN.
elfeed-deref를 사용하여 archive.gz에서도 컨텐츠를 읽음."
  (interactive "sSearch content: ")
  (unless (derived-mode-p 'elfeed-search-mode)
    (user-error "Not in elfeed-search buffer"))
  (let ((matches nil)
        (case-fold-search t))
    (dolist (entry elfeed-search-entries)
      (let ((content (elfeed-deref (elfeed-entry-content entry)))
            (title (elfeed-entry-title entry)))
        (when (or (and content (string-match-p pattern content))
                  (and title (string-match-p pattern title)))
          (push entry matches))))
    (if matches
        (progn
          (setq elfeed-search-entries (nreverse matches))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (entry elfeed-search-entries)
              (funcall elfeed-search-print-entry-function entry)
              (insert "\n")))
          (setq header-line-format
                (concat (elfeed-search--header) " [" pattern "]"))
          (goto-char (point-min))
          (message "Found %d entries matching '%s'" (length matches) pattern))
      (message "No matches for: %s" pattern))))

(defun +elfeed-search-content-clear ()
  "Clear content search and restore original filter."
  (interactive)
  (elfeed-search-update :force)
  (message "Filter cleared"))

;;;; org-web-tools

(use-package! org-web-tools)

;;;; gptel 인라인 요약/번역

;; elfeed-summarize (fritzgrabo) 패턴 참고:
;; - elfeed-meta에 캐싱 (:summary, :translation)
;; - show 버퍼: 헤더 영역에 삽입
;; - search 버퍼: overlay로 표시
;; 차이점: llm 대신 gptel 사용, 요약+번역 2단계 지원

;;;;; 설정

(defvar +elfeed-gptel-backend nil
  "elfeed 요약/번역용 gptel 백엔드.
nil이면 CLIProxyAPI(있으면) 또는 OpenRouter 사용.")

(defvar +elfeed-gptel-model nil
  "elfeed 요약/번역용 gptel 모델.
nil이면 현재 설정된 모델 사용.")

(defvar +elfeed-max-entry-length 3000
  "LLM에 보낼 엔트리 텍스트 최대 길이.")

(defvar +elfeed-summary-system-prompt
  "You are a reading assistant for an RSS feed reader.
Summarize the article concisely in English:
- 1-2 sentence TLDR
- 3-5 bullet points for key takeaways
- Keep it brief — this helps the reader decide whether to read the full article."
  "영어 요약 프롬프트. 영어로 먼저 요약 → 번역 시 품질 향상.")

(defvar +elfeed-translate-system-prompt
  "You are a professional translator.
Translate the following English summary to natural Korean.
- Maintain bullet point structure
- Use natural Korean expressions
- Keep technical terms in English where appropriate
- Do NOT add any preamble or explanation"
  "요약 결과를 한국어로 번역하는 프롬프트.")

(defvar +elfeed-translate-full-system-prompt
  "You are a professional translator.
Translate the following article to natural Korean.
- Maintain paragraph structure
- Use natural Korean expressions
- Keep technical terms in English where appropriate
- Do NOT add any preamble or explanation"
  "전문 번역 프롬프트.")

(defface +elfeed-summary-face
  '((t :inherit font-lock-comment-face))
  "인라인 요약/번역 텍스트 표시 face.")

(defface +elfeed-translation-face
  '((t :inherit font-lock-doc-face))
  "인라인 번역 텍스트 표시 face.")

;;;;; 엔트리 텍스트 추출

(defun +elfeed--entry-text (entry)
  "ENTRY에서 plain text 추출 (HTML 렌더링 포함, 길이 제한)."
  (when-let* ((raw (elfeed-entry-content entry))
              (content (elfeed-deref raw)))
    (let ((text (if (eq (elfeed-entry-content-type entry) 'html)
                    (with-temp-buffer
                      (insert content)
                      (shr-render-region (point-min) (point-max))
                      (buffer-string))
                  content)))
      (if (and +elfeed-max-entry-length
               (> (length text) +elfeed-max-entry-length))
          (substring text 0 +elfeed-max-entry-length)
        text))))

;;;;; gptel 비동기 요청 (핵심)

(defun +elfeed--gptel-request (prompt system-msg callback)
  "PROMPT를 gptel로 비동기 전송, 결과를 CALLBACK에 전달.
SYSTEM-MSG는 시스템 프롬프트.
기존 gptel 백엔드/모델 설정을 재활용."
  (let ((gptel-backend (or +elfeed-gptel-backend
                           (and (boundp 'gptel-cliproxy-backend) gptel-cliproxy-backend)
                           gptel-openrouter-backend
                           gptel-backend))
        (gptel-model (or +elfeed-gptel-model
                         (and (boundp 'gptel-cliproxy-backend)
                              'claude-sonnet-4-6)
                         gptel-openrouter-flash-model
                         gptel-model)))
    (gptel-request prompt
      :system system-msg
      :callback (lambda (response info)
                  (if response
                      (funcall callback (string-trim response))
                    (message "elfeed-gptel: 요청 실패 — %s"
                             (plist-get info :status)))))))

;;;;; 요약 (영어 → 한국어 2단계)

(defun +elfeed-summarize (entry callback)
  "ENTRY를 영어로 요약 후 한국어로 번역. CALLBACK에 결과 전달.
캐시가 있으면 즉시 반환."
  (if-let ((cached (elfeed-meta entry :summary-ko)))
      (funcall callback cached)
    ;; Step 1: 영어 요약
    (let ((text (+elfeed--entry-text entry)))
      (unless text (user-error "Entry has no content"))
      (+elfeed--gptel-request
       text +elfeed-summary-system-prompt
       (lambda (en-summary)
         ;; 영어 요약 캐시
         (setf (elfeed-meta entry :summary-en) en-summary)
         ;; Step 2: 한국어 번역
         (+elfeed--gptel-request
          en-summary +elfeed-translate-system-prompt
          (lambda (ko-summary)
            (setf (elfeed-meta entry :summary-ko) ko-summary)
            (funcall callback ko-summary))))))))

;;;;; 전문 번역

(defun +elfeed-translate-full (entry callback)
  "ENTRY 전문을 한국어로 번역. CALLBACK에 결과 전달.
캐시가 있으면 즉시 반환."
  (if-let ((cached (elfeed-meta entry :translation)))
      (funcall callback cached)
    (let ((text (+elfeed--entry-text entry)))
      (unless text (user-error "Entry has no content"))
      (+elfeed--gptel-request
       text +elfeed-translate-full-system-prompt
       (lambda (translation)
         (setf (elfeed-meta entry :translation) translation)
         (funcall callback translation))))))

;;;;; show 버퍼 표시

(defun +elfeed--show-insert-text (label text face)
  "elfeed-show 버퍼에 LABEL: TEXT를 삽입 (FACE 적용)."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      ;; Link: 헤더 다음에 삽입
      (when (re-search-forward "^Link:" nil t)
        (forward-line 1)
        ;; 기존 삽입된 요약/번역이 있으면 제거
        (while (looking-at (format "^%s:" label))
          (let ((start (point)))
            (forward-paragraph)
            (delete-region start (point))))
        (insert (propertize (format "%s: " label) 'face 'message-header-name)
                (propertize (string-trim text) 'face face)
                "\n\n")))))

(defun +elfeed-show-summarize ()
  "현재 elfeed-show 엔트리를 요약+번역하여 인라인 표시."
  (interactive)
  (unless (derived-mode-p 'elfeed-show-mode)
    (user-error "Not in elfeed-show buffer"))
  (let ((entry elfeed-show-entry)
        (buf (current-buffer)))
    ;; 캐시가 있으면 즉시 표시
    (if-let ((cached (elfeed-meta entry :summary-ko)))
        (+elfeed--show-insert-text "요약" cached '+elfeed-summary-face)
      ;; 진행 표시
      (+elfeed--show-insert-text "요약" "생성 중..." '+elfeed-summary-face)
      (+elfeed-summarize entry
       (lambda (summary)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (+elfeed--show-insert-text "요약" summary '+elfeed-summary-face))))))))

(defun +elfeed-show-translate ()
  "현재 elfeed-show 엔트리를 전문 번역하여 인라인 표시."
  (interactive)
  (unless (derived-mode-p 'elfeed-show-mode)
    (user-error "Not in elfeed-show buffer"))
  (let ((entry elfeed-show-entry)
        (buf (current-buffer)))
    (if-let ((cached (elfeed-meta entry :translation)))
        (+elfeed--show-insert-text "번역" cached '+elfeed-translation-face)
      (+elfeed--show-insert-text "번역" "번역 중..." '+elfeed-translation-face)
      (+elfeed-translate-full entry
       (lambda (translation)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (+elfeed--show-insert-text "번역" translation '+elfeed-translation-face))))))))

;;;;; search 버퍼 overlay

(defun +elfeed--search-overlay (entry)
  "Search 버퍼에서 ENTRY의 요약 overlay 반환."
  (seq-find (lambda (ov) (eq (overlay-get ov '+elfeed-entry) entry))
            (overlays-in (point-min) (point-max))))

(defun +elfeed--search-show-overlay (entry text face)
  "Search 버퍼에서 ENTRY 아래에 TEXT overlay 표시."
  ;; 기존 overlay 제거
  (when-let ((ov (+elfeed--search-overlay entry)))
    (delete-overlay ov))
  (let ((pos (seq-position elfeed-search-entries entry)))
    (when pos
      (save-excursion
        (elfeed-goto-line (+ pos elfeed-search--offset))
        (let ((ov (make-overlay (line-end-position) (1+ (line-end-position)))))
          (overlay-put ov '+elfeed-entry entry)
          (overlay-put ov 'after-string
                       (concat "\n"
                               (propertize (string-trim text) 'face face)
                               "\n")))))))

(defun +elfeed-search-summarize ()
  "Search 버퍼에서 선택된 엔트리를 요약+번역하여 overlay 표시."
  (interactive)
  (unless (derived-mode-p 'elfeed-search-mode)
    (user-error "Not in elfeed-search buffer"))
  (let ((entry (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (if-let ((cached (elfeed-meta entry :summary-ko)))
          ;; 토글: 이미 overlay가 있으면 제거
          (if (+elfeed--search-overlay entry)
              (delete-overlay (+elfeed--search-overlay entry))
            (+elfeed--search-show-overlay entry cached '+elfeed-summary-face))
        ;; 생성
        (+elfeed--search-show-overlay entry "요약 생성 중..." '+elfeed-summary-face)
        (+elfeed-summarize entry
         (lambda (summary)
           (when (+elfeed--search-overlay entry)
             (+elfeed--search-show-overlay entry summary '+elfeed-summary-face))))))))

;;;;; 캐시 관리

(defun +elfeed-clear-cache (entry)
  "ENTRY의 요약/번역 캐시 삭제."
  (interactive (list (cond
                      ((derived-mode-p 'elfeed-search-mode)
                       (elfeed-search-selected :ignore-region))
                      ((derived-mode-p 'elfeed-show-mode)
                       elfeed-show-entry))))
  (when (elfeed-entry-p entry)
    (setf (elfeed-meta entry :summary-en) nil
          (elfeed-meta entry :summary-ko) nil
          (elfeed-meta entry :translation) nil)
    (message "캐시 삭제: %s" (elfeed-entry-title entry))))

;;;;; remember 연동 (annotation)

(defun +elfeed-remember ()
  "현재 elfeed 엔트리에 대한 메모를 remember로 작성.
elfeed:URL#ID 링크를 자동 삽입. ~/org/remember.org에 저장 (Syncthing 동기화)."
  (interactive)
  (unless (derived-mode-p 'elfeed-show-mode)
    (user-error "Not in elfeed-show buffer"))
  (require 'elfeed-link)
  (let* ((entry elfeed-show-entry)
         (title (elfeed-entry-title entry))
         (feed (elfeed-feed-title (elfeed-entry-feed entry)))
         (date (format-time-string "%Y-%m-%d"
                 (seconds-to-time (elfeed-entry-date entry))))
         (link (format "[[elfeed:%s#%s][%s]]"
                       (car (elfeed-entry-id entry))
                       (cdr (elfeed-entry-id entry))
                       title))
         ;; 요약이 있으면 포함
         (summary (elfeed-meta entry :summary-ko))
         (initial (concat (format "** %s\n%s | %s\n%s\n" title feed date link)
                          (when summary (format "\n요약:\n%s\n" summary))
                          "\n")))
    (remember initial)))

;;;; Keybindings

(after! elfeed
  (map! :map elfeed-search-mode-map
        :niv "/" #'+elfeed-search-content
        :niv "q" #'+elfeed-search-content-clear
        :n "z" #'+elfeed-search-summarize        ; 인라인 요약 토글
        :localleader
        "/" #'+elfeed-search-content
        "c" #'+elfeed-search-content-clear
        "t" #'org-web-tools-read-url-as-org
        "T" #'org-web-tools-convert-links-to-page-entries)

  (map! :map elfeed-show-mode-map
        :n "z" #'+elfeed-show-summarize           ; 요약 (영어→한국어)
        :n "Z" #'+elfeed-show-translate            ; 전문 번역
        :n "a" #'+elfeed-remember                  ; remember 메모
        :localleader
        "z" #'+elfeed-show-summarize
        "Z" #'+elfeed-show-translate
        "a" #'+elfeed-remember
        "x" #'+elfeed-clear-cache))

;;; Provide

(provide 'elfeed-config)

;;; elfeed-config.el ends here
