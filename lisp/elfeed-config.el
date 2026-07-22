;;; $DOOMDIR/lisp/elfeed-config.el --- Elfeed configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Elfeed 커스텀 설정
;;
;; 기능:
;; - elfeed-tube 연동 (YouTube 자막)
;; - 컨텐츠 검색 (elfeed-deref 기반, archive.gz 호환)
;; - gptel 인라인 요약/번역 (elfeed-summarize 패턴 참고, gptel 백엔드)
;; - remember 연동 (annotation — elfeed: org 링크, Syncthing 동기화)
;;
;; 요약/번역 아키텍처:
;;   elfeed entry → z → 영어 요약 (Step1) → 한국어 번역 (Step2) → 인라인 표시
;;   elfeed entry → Z → 전문 한국어 번역 → 인라인 표시
;;   elfeed entry → a → remember 메모 (elfeed 링크 자동 삽입)
;;
;; 모델은 ai-gptel.el의 OpenAI-sub 하나로 통일 — 빠른 모델
;; (`my/gptel-model-fast')이 기본. 다중 백엔드 비교 벤치마크는 제거했다.

;;; Code:

(eval-when-compile (require 'elfeed-db))

;;;; Elfeed

;; gc copy-link
(after! elfeed
  ;; +rss-enable-sliced-images ;  default t
  (setq rmh-elfeed-org-files (list (my/org-elfeed-file))) ; default ~/org/elfeed.org
  (setq elfeed-search-filter "") ; "@1-year-ago" "@6-months-ago" "@1-month-ago +unread"
  ;; (add-hook 'elfeed-search-mode-hook #'elfeed-update)

  (if (display-graphic-p)
      (setq elfeed-search-title-max-width 90  ; GUI: 넓게
            elfeed-search-trailing-width 30)
    ;; TUI (Termux 등): date + title만, feed/tag 생략
    (setq elfeed-search-title-max-width 200    ; 창 너비까지 최대한 사용
          elfeed-search-trailing-width 0)       ; trailing 영역 제거
    (defun elfeed-search-print-entry--default (entry)
      "TUI용 elfeed 엔트리 표시: date + title만."
      (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
             (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (title-width (- (window-width) 12)) ; date(10) + space(2)
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   title-width)
                            :left)))
        (insert (propertize date 'face 'elfeed-search-date-face) " ")
        (insert (propertize title-column 'face title-faces 'kbd-help title)))))
  )

(after! elfeed-tube
  (require 'elfeed-tube)
  ;; (setq elfeed-tube-invidious-url "https://vid.puffyan.us")
  (setq elfeed-tube-captions-languages '("en" "ko" "englsh (auto generated)")))

;;;;; Elfeed-web (웹 인터페이스) — 비활성

;; 2026-05-06: 비활성. upstream에서 elfeed-web이 별도 repo로 분리됨
;; (emacs-elfeed/elfeed-web). 로컬 카피 lisp/elfeed-web/ 는 보존하되,
;; with-elfeed-web macro 제거 등 upstream 변경과 동기화가 필요해 일단 끔.
;; 재활성 시 emacs-elfeed/elfeed-web 의 최신 파일로 lisp/elfeed-web/ 갱신.
;;
;; (let ((elfeed-web-dir (expand-file-name "lisp/elfeed-web" doom-user-dir)))
;;   (when (file-directory-p elfeed-web-dir)
;;     (when (locate-library "simple-httpd")
;;       (require 'simple-httpd)
;;       (load! "elfeed-web/elfeed-web"))))
;;
;; ;; 외부 기기에서 접속 허용
;; ;; (setq httpd-host "0.0.0.0")

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
nil이면 OpenAI-sub (ChatGPT 구독) 사용. 없으면 현재 gptel-backend.")

(defvar +elfeed-gptel-model nil
  "elfeed 요약/번역용 gptel 모델.
nil이면 `my/gptel-model-fast' 사용.")

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
기본: OpenAI-sub + `my/gptel-model-fast' (구독 활용, 빠른 응답)."
  (let ((gptel-backend (or +elfeed-gptel-backend
                           gptel-openai-sub-backend
                           gptel-backend))
        (gptel-model (or +elfeed-gptel-model
                         my/gptel-model-fast)))
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

(defun +elfeed--fill-text (text)
  "TEXT를 현재 윈도우 너비에 맞게 fill하여 반환."
  (with-temp-buffer
    (insert (string-trim text))
    (let ((fill-column (max 60 (min 80 (- (window-width) 4)))))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun +elfeed--show-insert-text (label text face)
  "elfeed-show 버퍼에 LABEL: TEXT를 삽입 (FACE 적용, fill 처리)."
  (let ((inhibit-read-only t)
        (filled (+elfeed--fill-text text)))
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
        (insert (propertize (format "%s:\n" label) 'face 'message-header-name)
                (propertize filled 'face face)
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

;;;;; search 버퍼 overlay — 보류
;; search 버퍼에서 overlay 삽입 시 엔트리 목록이 망가지는 문제.
;; elfeed-summarize는 elfeed-goto-line + elfeed-search--offset으로 처리하지만
;; Doom의 elfeed search 커스텀과 충돌 가능. 추후 재검토.

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

;;;; elfeed-tree

;; evil-collection은 readonly 기본 바인딩(j/k/q 등)만 잡고, RET을
;; 존재하지 않는 함수 `elfeed-tree-show'에 묶음 — evil-collection 측
;; stale reference (`elfeed-tree-search'로 바뀐 것을 미반영).
;; evil-collection은 after-load-functions 후크로 binding을 지연 적용해서
;; eval-after-load(=after!)보다 뒤에 실행, RET이 항상 덮어써짐.
;; 우회: mode-hook에서 fix (load 후크보다 더 뒤).
(after! elfeed-tree
  (map! :map elfeed-tree-mode-map
        :n "/"   #'elfeed-search-new-live          ; 필터 시작
        :n "T"   #'elfeed-tree-set-title           ; feed 제목 변경
        :n "TAB" #'outline-cycle                   ; 노드 토글
        :n "<backtab>" #'outline-cycle-buffer      ; 전체 토글
        :n "gr"  #'elfeed-tree-update              ; 트리 새로고침
        :n "gR"  #'elfeed-update                   ; 전체 fetch (새 항목)
        :n "q"   #'quit-window)

  (defun +elfeed-tree-fix-ret-h ()
    "evil-collection의 stale `elfeed-tree-show' 바인딩을 정정."
    (evil-define-key 'normal elfeed-tree-mode-map
      (kbd "RET") #'elfeed-tree-search))
  (add-hook 'elfeed-tree-mode-hook #'+elfeed-tree-fix-ret-h))

;;; Provide

(provide 'elfeed-config)

;;; elfeed-config.el ends here
