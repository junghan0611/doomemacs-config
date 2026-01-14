;;; +gptel.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: September 07, 2025
;; Modified: September 07, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/junghan0611/+gptel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;

;;; Code:

;;;; evil-collection-gptel

(setq evil-collection-gptel-want-ret-to-send nil)
(setq evil-collection-gptel-want-shift-ret-menu t)
(setq evil-collection-gptel-want-shift-ret-to-send nil)

;;;; macher

(use-package! macher
  :custom
  ;; The org UI has structured navigation and nice content folding.
  (macher-action-buffer-ui 'org)

  :config
  ;; Recommended - register macher tools and presets with gptel.
  (macher-install)

  ;; Recommended - enable macher infrastructure for tools/prompts in
  ;; any buffer.  (Actions and presets will still work without this.)
  (macher-enable)

  ;; Adjust buffer positioning to taste.
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . bottom)))
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("\\*macher-patch:.*\\*"
  ;;    (display-buffer-in-side-window)
  ;;    (side . right)))
  )

;;;; gptel

;;;;; load

(use-package! gptel
  :init
  (require 'password-store)  ; API 키 접근을 위해 미리 로드
  :config

  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)

  (setq gptel-include-reasoning nil)
  (setq gptel-default-mode 'org-mode)
  ;; (setq gptel-temperature 0.3) ; gptel 1.0
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t)
  (set-popup-rule! "^\\*gptel-buffer\\*$" :side 'right :size 0.4 :vslot 99 :quit nil :select t)

;;;;; gptel-org

  (after! gptel-org
    (defun my/gptel-org-toggle-branching-context ()
      "Toggle gptel context between doc and subheading."
      (interactive)
      (if gptel-org-branching-context
          (progn
            (setq-local gptel-org-branching-context nil)
            (message "Context: whole doc"))
        (setq-local gptel-org-branching-context t)
        (message "Context: subheading")))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user "
          (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "@user "
          (alist-get 'markdown-mode gptel-response-prefix-alist) "@assistant\n"
          )
    (setq-default gptel-org-branching-context t))

;;;;; gptel backend

;;;;;; gptel deepseek

  (setq gptel-deepseek-backend
        (gptel-make-deepseek "DeepSeek"
          :stream t
          :key (lambda () (password-store-get "work/api/deepseek/goqual-from-che"))))

;;;;;; gptel openrouter models

  (defconst gptel--openrouter-models
    '(
      (google/gemini-2.5-flash
       :description "Best in terms of price-performance, with well-rounded capabilities"
       :capabilities (tool-use json media audio video)
       :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                    "application/pdf" "text/plain" "text/csv" "text/html"
                    "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                    "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
       :context-window 1048               ; 65536 output token limit
       :input-cost 0.3
       :output-cost 2.50
       :cutoff-date "2025-01")

      (google/gemini-3-flash-preview
       :description "Most intelligent Gemini model built for speed"
       :capabilities (tool-use json media audio video)
       :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                    "application/pdf" "text/plain" "text/csv" "text/html"
                    "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                    "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
       :context-window 1048               ; 65536 output token limit
       :input-cost 0.50
       :output-cost 3.00
       :cutoff-date "2025-01")

      ;; https://openrouter.ai/google/gemini-2.5-pro
      (google/gemini-2.5-pro
       :description "Most powerful Gemini thinking model with state-of-the-art performance"
       :capabilities (tool-use json media audio video)
       :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                    "application/pdf" "text/plain" "text/csv" "text/html"
                    "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                    "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
       :context-window 1048               ; 65536 output token limit
       :input-cost 1.25                   ; 2.50 for >200k tokens
       :output-cost 10.00                 ; 15 for >200k tokens
       :cutoff-date "2025-01")

      (google/gemini-3-pro-preview
       :description "Most intelligent Gemini model with SOTA reasoning and multimodal understanding"
       :capabilities (tool-use json media audio video)
       :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                    "application/pdf" "text/plain" "text/csv" "text/html"
                    "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                    "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
       :context-window 1048               ; 65536 output token limit
       :input-cost 2.0                    ; 4.0 for >200k tokens
       :output-cost 12.00                 ; 18.0 for >200k tokens
       :cutoff-date "2025-01")

      ;; openai/gpt-5.1, 400k, "The best model for coding and agentic tasks"
      (openai/gpt-5.1-chat
       :description
       "GPT-5.1 Chat (AKA Instant is the fast, lightweight member of the 5.1 family, optimized for low-latency chat while retaining strong general intelligence. "
       :capabilities (media tool-use json url)
       :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
       :context-window 128 ; 400
       :input-cost 1.25
       :output-cost 10
       :cutoff-date "2024-09")
      )
    )

  (setq gptel-openrouter-backend
        (gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          ;; :key #'gptel-api-key
          :key (lambda () (password-store-get "work/api/openrouter/devteam-backup"))
          :models gptel--openrouter-models))
  (setq gptel-openrouter-chat-model 'openai/gpt-5.1-chat) ; < 1.0s latency
  (setq gptel-openrouter-flash-model 'google/gemini-2.5-flash) ; < 0.5s latency
  (setq gptel-openrouter-pro-model 'google/gemini-2.5-pro) ; < 2.5s latency

;;;;;; gptel Claude-Code (via wrapper)

  ;; [서비스 관리] - run-claude-wrapper 스크립트 사용
  ;; run-claude-wrapper              # 서비스 시작
  ;; run-claude-wrapper --update     # 최신 코드로 재빌드
  ;; run-claude-wrapper --stop       # 서비스 중지
  ;; run-claude-wrapper --status     # 상태 확인
  ;;
  ;; [지원 기능]
  ;; - 파일 접근: Read, Write, Edit, Glob, Grep
  ;; - 명령 실행: Bash
  ;; - 웹검색: WebSearch, WebFetch
  ;; - 시스템 프롬프트: gptel 메시지 전달
  ;;
  ;; [gptel에서 사용]
  ;; M-x gptel → 백엔드 메뉴에서 "Claude-Code" 선택
  ;; 또는: (setq gptel-backend gptel-claude-code-backend)
  (setq gptel-claude-code-backend
        (gptel-make-openai "Claude-Code"
          :host "localhost:8000"
          :endpoint "/v1/chat/completions"
          :protocol "http"
          :stream t
          :key "not-needed"
          :models '((claude-sonnet-4-5-20250929
                     :capabilities (media tool-use)
                     :context-window 200
                     :input-cost 3
                     :output-cost 15)
                    (claude-opus-4-5-20251101
                     :description "Latest Opus 4.5 - Most capable"
                     :capabilities (media tool-use)
                     :context-window 200
                     :input-cost 5
                     :output-cost 25)
                    (claude-haiku-4-5-20251001
                     :capabilities (media tool-use)
                     :context-window 200
                     :input-cost 1
                     :output-cost 5))))

  ;; Claude-Code 백엔드용 enable_tools 자동 추가
  (defvar gptel-claude-code-enable-tools t
    "When non-nil, enable Claude Code tools (Read, Write, Bash, etc.).")

  (defun gptel--claude-code-add-enable-tools (orig-fun &rest args)
    "Advice to add enable_tools to Claude-Code requests."
    (let ((result (apply orig-fun args)))
      (when (and gptel-claude-code-enable-tools
                 (eq gptel-backend gptel-claude-code-backend))
        (setq result (append result `(:enable_tools t))))
      result))

  (advice-add 'gptel--request-data :around #'gptel--claude-code-add-enable-tools)

;;;;; Default Backend and Prompt

  ;; 시스템 프롬프트 설정 (+user-info.el에서 정의)
  (setq gptel--system-message user-llm-system-prompt)

;;;;; 프롬프트 선택 함수

  (defun my/gptel-select-prompt ()
    "현재 gptel 버퍼에서 시스템 프롬프트를 선택하여 변경."
    (interactive)
    (let* ((prompts `(("기본 (General)" . ,user-llm-system-prompt)
                      ("요약 (Summarize)" . ,+gptel-summarize-system-message)
                      ("번역 (Translate)" . ,+gptel-translate-system-message)))
           (choice (completing-read "프롬프트: " (mapcar #'car prompts) nil t))
           (prompt (cdr (assoc choice prompts))))
      (setq-local gptel--system-message prompt)
      (message "프롬프트 변경: %s" choice)))

  ;; Magit 백엔드 (항상 OpenRouter - 웹검색 불필요)
  (setq gptel-magit-backend gptel-openrouter-backend)
  (setq gptel-magit-model gptel-openrouter-flash-model)

  ;; Claude-Code 서버 상태 확인 함수
  (defun gptel--claude-code-server-available-p ()
    "Check if Claude-Code wrapper server is running on localhost:8000."
    (condition-case nil
        (let ((url-request-method "GET")
              (url-show-status nil))
          (with-current-buffer
              (url-retrieve-synchronously "http://localhost:8000/health" t nil 2)
            (goto-char (point-min))
            (and (search-forward "healthy" nil t) t)))
      (error nil)))

  ;; 기본 백엔드 설정: Claude-Code 서버가 떠있으면 사용, 아니면 OpenRouter
  (if (gptel--claude-code-server-available-p)
      (progn
        (setq gptel-backend gptel-claude-code-backend)
        (setq gptel-model 'claude-sonnet-4-5-20250929)
        (message "gptel: Claude-Code 서버 감지 → 기본 백엔드로 설정"))
    (setq gptel-backend gptel-openrouter-backend)
    (setq gptel-model gptel-openrouter-chat-model)
    (message "gptel: Claude-Code 서버 없음 → OpenRouter 사용"))

  ;; 수동 백엔드 전환 함수
  (defun my/gptel-switch-to-claude-code ()
    "Switch gptel backend to Claude-Code (requires server running)."
    (interactive)
    (if (gptel--claude-code-server-available-p)
        (progn
          (setq gptel-backend gptel-claude-code-backend)
          (setq gptel-model 'claude-sonnet-4-5-20250929)
          (message "Switched to Claude-Code backend"))
      (message "Claude-Code server not available! Run: run-claude-wrapper")))

  (defun my/gptel-switch-to-openrouter ()
    "Switch gptel backend to OpenRouter."
    (interactive)
    (setq gptel-backend gptel-openrouter-backend)
    (setq gptel-model gptel-openrouter-chat-model)
    (message "Switched to OpenRouter backend"))

;;;;; gptel-save-as-org-with-denote-metadata

  (defun gptel-save-as-org-with-denote-metadata ()
    "Save buffer to disk when starting gptel with metadata."
    (interactive)
    (unless (buffer-file-name (current-buffer))
      (let* ((suffix (format-time-string "%Y%m%dT%H%M%S"))
             (chat-dir (concat org-directory "/llmlog"))
             (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode)))
             (filename (concat suffix "__llmlog" "." ext))
             (full-path (expand-file-name filename chat-dir)))
        (unless (file-directory-p chat-dir)
          (make-directory chat-dir :parents))
        (write-file full-path)

        ;; Add metadata to the file
        (goto-char 0) (search-forward ":END:") (end-of-line)
        (insert (format "\n#+title: #LLM: %s\n" suffix))
        (insert "#+filetags: :llmlog:\n")
        (insert (format "#+hugo_lastmod: %s\n" (format-time-string "[%Y-%m-%d]")))
        (insert (format "#+date: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "#+identifier: %s\n" suffix))
        (insert (format "#+export_file_name: %s.md\n" suffix))
        (insert (format "#+description: %s\n" suffix))
        (insert (format "#+hugo_categories: Noname\n#+OPTIONS: toc:1\n"))

        (insert (format "\n* 히스토리\n- %s Created!" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "\n* 관련메타\n- \n#+print_bibliography:\n\n"))

        ;; heading-1 add backlink to today
        (insert (format "* 로그 :LLMLOG:\n** [[denote:%s::#%s][%s]]\n"
                        ;; (format-time-string "%Y%m%dT000000")
                        (format-time-string "%Y%m%dT000000"
                                            (org-journal--convert-time-to-file-type-time
                                             (time-subtract (current-time)
                                                            (* 3600 org-extend-today-until))))
                        (downcase (format-time-string "%Y-%m-%d-%a"))
                        (format-time-string "|%Y-%m-%d %a %H:%M|")))
        ;; heading-2 [SUM]:
        ;; (insert (format "** TODO [SUM]: \n"))
        (insert "\n"))))

;;;;; gptel-mode-hook

  (map! :map gptel-mode-map
        "M-<return>" #'gptel-send
        "M-RET" #'gptel-send
        (:localleader
         "RET" #'gptel-mode
         "1" #'gptel-menu
         "TAB" #'gptel-menu
         "M-s" #'gptel-save-as-org-with-denote-metadata
         "M-l" #'gptel-clear-buffer+
         (:prefix ("s" . "session")
          :desc "clear" "l" #'gptel-clear-buffer+
          "p" #'gptel-save-as-org-with-denote-metadata
          )))

  (add-hook! 'gptel-mode-hook
    (defun cae-gptel-mode-setup-h ()
      ;; (setq-local nobreak-char-display nil) ; 2025-07-26 보는게 좋아
      (auto-fill-mode -1)
      (doom-mark-buffer-as-real-h)))

  ;; Optional - set up macher as soon as gptel is loaded.
  (require 'macher)

  ) ; end of use-package! gptel

;;;; gptel-buffer: 범용 버퍼 요약/번역

;;;;; after gptel

(after! gptel
  (defvar +gptel-buffer-name "*gptel-buffer*"
    "gptel 버퍼 요약/번역용 사이드 버퍼 이름.")

  (defvar +gptel-summarize-system-message
    "You are a helpful reading assistant. Generate a concise TLDR summary.
- Cover the main points clearly
- Use bullet points for key takeaways
- Respond in Korean"
    "버퍼 요약용 시스템 프롬프트.")

  (defvar +gptel-translate-system-message
    "You are a professional translator. Translate the following text naturally to Korean.
- Maintain the original meaning and tone
- Use natural Korean expressions
- Preserve technical terms when appropriate"
    "버퍼 번역용 시스템 프롬프트.")

  ;; Temperature 가이드:
  ;; | 작업 | 권장 온도 | 이유                      |
  ;; |------|----------|---------------------------|
  ;; | 번역 | 0.1-0.2  | 원문 충실, 일관성          |
  ;; | 요약 | 0.3-0.5  | 핵심 추출 + 약간 재구성    |
  ;; | 창작 | 0.7-1.0  | 다양성, 창의성             |

  (defvar +gptel-translate-temperature 0.1
    "번역용 낮은 temperature - 정확성과 일관성 우선.")

  (defvar +gptel-summarize-temperature 0.4
    "요약용 중간 temperature - 핵심 추출 + 약간의 재구성 허용.")

  ;; Content Extractors - 각 모드별 컨텐츠 추출

  (defun +gptel--extract-eww-content ()
    "eww 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'eww-mode)
      (list :title (plist-get eww-data :title)
            :url (plist-get eww-data :url)
            :text (buffer-string))))

  (defun +gptel--extract-elfeed-content ()
    "elfeed-show 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'elfeed-show-mode)
      (let* ((entry elfeed-show-entry)
             (feed (elfeed-entry-feed entry))
             (authors (elfeed-meta entry :authors)))
        (list :title (elfeed-entry-title entry)
              :url (elfeed-entry-link entry)
              :feed (elfeed-feed-title feed)
              :date (format-time-string "%Y-%m-%d %H:%M"
                                        (seconds-to-time (elfeed-entry-date entry)))
              :authors (when authors
                         (mapconcat (lambda (a) (plist-get a :name)) authors ", "))
              :text (buffer-string)))))

  (defun +gptel--extract-pdf-content ()
    "pdf-view 버퍼에서 선택 영역 또는 현재 페이지 텍스트 추출."
    (when (derived-mode-p 'pdf-view-mode)
      (list :title (file-name-nondirectory (buffer-file-name))
            :url (buffer-file-name)
            :text (if (pdf-view-active-region-p)
                      (mapconcat #'identity (pdf-view-active-region-text) "\n\n")
                    (pdf-info-gettext (pdf-view-current-page))))))

  (defun +gptel--extract-nov-content ()
    "nov.el (epub) 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'nov-mode)
      (list :title (or (alist-get 'title nov-metadata) "Unknown")
            :url (nov-content-unique-identifier)
            :text (buffer-string))))

  (defun +gptel--extract-buffer-content ()
    "현재 버퍼에서 LLM용 컨텐츠 추출. plist 반환."
    (or (+gptel--extract-eww-content)
        (+gptel--extract-elfeed-content)
        (+gptel--extract-pdf-content)
        (+gptel--extract-nov-content)
        ;; 기본: 일반 버퍼
        (list :title (buffer-name)
              :url (or (buffer-file-name) default-directory)
              :text (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (buffer-string)))))

  (defun +gptel--format-content-for-llm (content)
    "CONTENT plist를 LLM 프롬프트 형식으로 변환."
    (let ((title (plist-get content :title))
          (url (plist-get content :url))
          (feed (plist-get content :feed))
          (date (plist-get content :date))
          (authors (plist-get content :authors))
          (text (plist-get content :text)))
      (concat
       "Article Metadata:\n"
       (when title (format "- Title: %s\n" title))
       (when url (format "- URL: %s\n" url))
       (when feed (format "- Feed: %s\n" feed))
       (when date (format "- Date: %s\n" date))
       (when authors (format "- Authors: %s\n" authors))
       "\nContent:\n"
       "```\n"
       (string-trim text)
       "\n```")))

  ;; 요약/번역 전용 백엔드 및 모델 (긴 컨텍스트용)
  (defvar +gptel-buffer-backend nil
    "요약/번역 전용 백엔드. nil이면 gptel-openrouter-backend 사용.")

  (defvar +gptel-buffer-model gptel-openrouter-flash-model
    "요약/번역 전용 모델. 긴 컨텍스트 지원 필요.")

  (defun my/gptel-buffer-model-toggle ()
    "Toggle +gptel-buffer-model between Flash and Pro."
    (interactive)
    (setq +gptel-buffer-model
          (if (eq +gptel-buffer-model gptel-openrouter-flash-model)
              gptel-openrouter-pro-model
            gptel-openrouter-flash-model
            ))
    (message "gptel-buffer 모델: %s" +gptel-buffer-model))

  ;; 핵심 함수

  (defun +gptel--send-to-buffer (content system-message action-name &optional temperature)
    "CONTENT를 gptel 사이드 버퍼로 보내고 SYSTEM-MESSAGE로 요청.
ACTION-NAME은 표시용 (예: \"요약\", \"번역\").
TEMPERATURE는 선택적 온도 설정 (nil이면 전역값 사용).
항상 OpenRouter/Gemini 모델 사용 (긴 컨텍스트 지원)."
    (let* ((formatted (+gptel--format-content-for-llm content))
           (buf (get-buffer-create +gptel-buffer-name))
           ;; 요약/번역 전용 백엔드 및 모델
           (target-backend (or +gptel-buffer-backend gptel-openrouter-backend))
           (target-model +gptel-buffer-model))
      ;; 사이드 버퍼 설정
      (with-current-buffer buf
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (unless gptel-mode
          (gptel-mode 1))

        (setq-local gptel-org-branching-context nil)
        ;; 백엔드 및 모델 명시적 설정 (긴 컨텍스트용)
        (setq-local gptel-backend target-backend)
        (setq-local gptel-model target-model)
        ;; 시스템 메시지 설정
        (setq-local gptel--system-message system-message)
        ;; 로컬 온도 설정 (작업별 최적화)
        (when temperature
          (setq-local gptel-temperature temperature))
        ;; 이전 내용 아래에 새 요청 추가
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (format "@user: [%s 요청]\n\n%s" action-name formatted))
        (insert "\n\n@assistant:\n"))
      ;; 사이드 윈도우로 표시
      (display-buffer buf '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.4)))
      ;; gptel-send 호출
      (with-current-buffer buf
        (goto-char (point-max))
        (gptel-send))
      (message "%s 요청 [%s] (temp=%.1f) → %s"
               action-name target-model
               (or temperature gptel-temperature) +gptel-buffer-name)))

;;;###autoload
  (defun +gptel-summarize-buffer ()
    "현재 버퍼 내용을 요약하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
    (interactive)
    (let ((content (+gptel--extract-buffer-content)))
      (+gptel--send-to-buffer content +gptel-summarize-system-message
                              "요약" +gptel-summarize-temperature)))

;;;###autoload
  (defun +gptel-translate-buffer ()
    "현재 버퍼 내용을 번역하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
    (interactive)
    (let ((content (+gptel--extract-buffer-content)))
      (+gptel--send-to-buffer content +gptel-translate-system-message
                              "번역" +gptel-translate-temperature)))

;;;###autoload
  (defun +gptel-buffer-dwim ()
    "현재 버퍼에 대해 gptel 액션 선택 (요약/번역)."
    (interactive)
    (let ((action (completing-read "Action: " '("요약 (Summarize)" "번역 (Translate)"))))
      (pcase action
        ("요약 (Summarize)" (+gptel-summarize-buffer))
        ("번역 (Translate)" (+gptel-translate-buffer)))))

;;;;; gptel functions

  ;; TODO 뭐하는 함수인가?! 다시 검토
  (defun gptel-clear-buffer+ ()
    (interactive)
    (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
           (keep-line (save-excursion
                        (goto-char (point-max))
                        (when (re-search-backward beg-marker nil t)
                          (unless (save-excursion
                                    (forward-line)
                                    (re-search-forward beg-marker nil t))
                            (point))))))
      (delete-region (point-min) keep-line)
      (evil-insert-state)))

;;;;; gptel-buffer 키바인딩

  ;; 전역 키바인딩 (SPC a G 접두어) - SPC a 충돌로 비활성화
  (map! :leader
        (:prefix ("=" . "AI")
                 (:prefix ("g" . "gptel-buffer")
                  :desc "Summarize buffer" "s" #'+gptel-summarize-buffer
                  :desc "Translate buffer" "t" #'+gptel-translate-buffer
                  :desc "DWIM (choose action)" "g" #'+gptel-buffer-dwim)))

  ;; eww 모드 키바인딩
  (after! eww
    (map! :map eww-mode-map
          :localleader
          (:prefix ("G" . "gptel")
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; elfeed 모드 키바인딩
  (after! elfeed
    (map! :map elfeed-show-mode-map
          :localleader
          (:prefix ("G" . "gptel")
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; pdf-view 모드 키바인딩
  (after! pdf-tools
    (map! :map pdf-view-mode-map
          :localleader
          (:prefix ("G" . "gptel")
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; nov 모드 키바인딩
  (after! nov
    (map! :map nov-mode-map
          :localleader
          (:prefix ("G" . "gptel")
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

;;;;; gptel org-mode-map

  (after! org
    (map! :map org-mode-map
          :localleader
          "RET" #'gptel-mode
          "1" #'gptel-menu
          "5" #'my/gptel-org-toggle-branching-context
          "TAB" #'gptel-menu
          "M-s" #'gptel-save-as-org-with-denote-metadata
          "M-l" #'gptel-clear-buffer+
          ))

  ) ; end of after! gptel

;;;; embark-region + gptel 통합

;; 코드 파일에서 영역 선택 → M-o (embark) → prompt 선택 → gptel 전달
;; Claude Code 호출 없이 gptel로 빠른 수정/주석화
;; 관련 beads: doomemacs-config-cx6
;;
;; [embark-region-map 키바인딩] (keybindings-config.el)
;; | Key | 함수                          | 설명           |
;; |-----|-------------------------------|----------------|
;; | p   | my/gptel-apply-prompt-to-region | prompt 파일 선택 |
;; | [   | my/gptel-quick-region         | 빠른 질의      |
;; | t   | my/gptel-translate-region     | 번역 (한↔영)   |
;; | s   | my/gptel-summarize-region     | 요약           |
;; | e   | my/gptel-explain-region       | 코드 설명      |
;; | r   | my/gptel-rewrite-region       | 재작성         |
;;
;; [embark-file-map 키바인딩] (keybindings-config.el)
;; | Key | 함수                          | 설명                    |
;; |-----|-------------------------------|-------------------------|
;; | p   | my/gptel-apply-prompt-to-file | prompt 파일 선택        |
;; | t   | my/gptel-translate-file       | immersive-translate 번역|
;; | s   | my/gptel-summarize-file       | 파일 요약               |
;;
;; [프롬프트 디렉토리]
;; ~/sync/org/resources/prompts/
;; - immersive-translate.poet : 문단별 번역 (원문 형식 유지)
;; - instant-korean.poet      : 즉시 한국어 번역
;; - instant-english.poet     : 즉시 영어 번역
;; - code-review-ko.poet      : 코드 리뷰 (한국어)
;; - summarize.md             : 요약
;;
;; [TODO] 테스트 후 keybindings-config.el 바인딩 활성화

(after! gptel

;;;;; 프롬프트 선택 함수

  (defun my/gptel--list-prompts ()
    "gptel-prompts-directory에서 프롬프트 파일 목록 반환."
    (when (and (boundp 'gptel-prompts-directory)
               (file-directory-p gptel-prompts-directory))
      (directory-files gptel-prompts-directory nil
                       "\\.\\(org\\|md\\|txt\\|poet\\)$")))

  (defun my/gptel--read-prompt-file (filename)
    "FILENAME에서 프롬프트 내용 읽기."
    (let ((filepath (expand-file-name filename gptel-prompts-directory)))
      (when (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (buffer-string)))))

;;;;; embark-region용 gptel 함수들

;;;###autoload
  (defun my/gptel-apply-prompt-to-region (beg end)
    "선택 영역에 gptel prompt 적용.
프롬프트 파일 선택 후 선택된 텍스트와 함께 gptel에 전달."
    (interactive "r")
    (let* ((text (buffer-substring-no-properties beg end))
           (prompts (my/gptel--list-prompts))
           (prompt-file (completing-read "Prompt: " prompts nil t))
           (prompt-content (my/gptel--read-prompt-file prompt-file))
           (full-prompt (format "%s\n\n---\n\n%s" prompt-content text)))
      (if prompt-content
          (progn
            (gptel full-prompt)
            (message "gptel에 전달: %s" prompt-file))
        (user-error "프롬프트 파일을 읽을 수 없습니다: %s" prompt-file))))

;;;###autoload
  (defun my/gptel-quick-region (beg end)
    "선택 영역으로 gptel-quick 호출."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-quick text)))

;;;###autoload
  (defun my/gptel-translate-region (beg end)
    "선택 영역 번역 (한↔영 자동 감지)."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
       (format "다음 텍스트를 번역해주세요. 한국어면 영어로, 영어면 한국어로:\n\n%s" text)
       :system +gptel-translate-system-message
       :callback (lambda (response info)
                   (if response
                       (with-current-buffer (get-buffer-create "*gptel-translate*")
                         (erase-buffer)
                         (insert response)
                         (display-buffer (current-buffer)))
                     (message "번역 실패: %s" (plist-get info :status)))))))

;;;###autoload
  (defun my/gptel-summarize-region (beg end)
    "선택 영역 요약."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
       (format "다음 텍스트를 간결하게 요약해주세요:\n\n%s" text)
       :system +gptel-summarize-system-message
       :callback (lambda (response info)
                   (if response
                       (with-current-buffer (get-buffer-create "*gptel-summary*")
                         (erase-buffer)
                         (insert response)
                         (display-buffer (current-buffer)))
                     (message "요약 실패: %s" (plist-get info :status)))))))

;;;###autoload
  (defun my/gptel-explain-region (beg end)
    "선택 영역 (코드) 설명."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
       (format "다음 코드/텍스트를 설명해주세요:\n\n```\n%s\n```" text)
       :callback (lambda (response info)
                   (if response
                       (with-current-buffer (get-buffer-create "*gptel-explain*")
                         (erase-buffer)
                         (insert response)
                         (display-buffer (current-buffer)))
                     (message "설명 실패: %s" (plist-get info :status)))))))

;;;###autoload
  (defun my/gptel-rewrite-region (beg end)
    "선택 영역 재작성/개선."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
       (format "다음 텍스트를 더 명확하고 간결하게 재작성해주세요:\n\n%s" text)
       :callback (lambda (response info)
                   (if response
                       (with-current-buffer (get-buffer-create "*gptel-rewrite*")
                         (erase-buffer)
                         (insert response)
                         (display-buffer (current-buffer)))
                     (message "재작성 실패: %s" (plist-get info :status)))))))

;;;;; embark-file-map용 gptel 함수들

;;;###autoload
  (defun my/gptel-apply-prompt-to-file (file)
    "FILE에 gptel prompt 적용.
프롬프트 파일 선택 후 파일 내용과 함께 gptel에 전달."
    (interactive "fFile: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (prompts (my/gptel--list-prompts))
           (prompt-file (completing-read "Prompt: " prompts nil t))
           (prompt-content (my/gptel--read-prompt-file prompt-file))
           (full-prompt (format "%s\n\n---\n\n%s" prompt-content content)))
      (if prompt-content
          (progn
            (gptel full-prompt)
            (message "gptel에 전달: %s → %s" (file-name-nondirectory file) prompt-file))
        (user-error "프롬프트 파일을 읽을 수 없습니다: %s" prompt-file))))

;;;###autoload
  (defun my/gptel-translate-file (file)
    "FILE을 immersive-translate 프롬프트로 번역.
~/sync/org/resources/prompts/immersive-translate.poet 사용."
    (interactive "fFile to translate: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (prompt-file (expand-file-name "immersive-translate.poet"
                                          gptel-prompts-directory))
           (output-buffer (get-buffer-create
                           (format "*gptel-translate: %s*"
                                   (file-name-nondirectory file)))))
      (if (file-exists-p prompt-file)
          (progn
            (message "번역 중: %s..." (file-name-nondirectory file))
            (gptel-request
             content
             :system (with-temp-buffer
                       (insert-file-contents prompt-file)
                       ;; .poet 파일에서 system content 추출
                       (goto-char (point-min))
                       (if (re-search-forward "role: system\\s-*\n\\s-*content:\\s-*>-?\\s-*\n" nil t)
                           (let ((start (point)))
                             (if (re-search-forward "^- name:" nil t)
                                 (buffer-substring-no-properties start (match-beginning 0))
                               (buffer-substring-no-properties start (point-max))))
                         ;; fallback: 전체 내용
                         (buffer-string)))
             :callback (lambda (response info)
                         (if response
                             (progn
                               (with-current-buffer output-buffer
                                 (erase-buffer)
                                 (insert response)
                                 (goto-char (point-min)))
                               (display-buffer output-buffer)
                               (message "번역 완료: %s" (file-name-nondirectory file)))
                           (message "번역 실패: %s" (plist-get info :status))))))
        (user-error "immersive-translate.poet 파일이 없습니다: %s" prompt-file))))

;;;###autoload
  (defun my/gptel-summarize-file (file)
    "FILE 내용 요약."
    (interactive "fFile to summarize: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (output-buffer (get-buffer-create
                           (format "*gptel-summary: %s*"
                                   (file-name-nondirectory file)))))
      (message "요약 중: %s..." (file-name-nondirectory file))
      (gptel-request
       (format "다음 파일 내용을 간결하게 요약해주세요:\n\n%s" content)
       :system +gptel-summarize-system-message
       :callback (lambda (response info)
                   (if response
                       (progn
                         (with-current-buffer output-buffer
                           (erase-buffer)
                           (insert response)
                           (goto-char (point-min)))
                         (display-buffer output-buffer)
                         (message "요약 완료: %s" (file-name-nondirectory file)))
                     (message "요약 실패: %s" (plist-get info :status)))))))

  ) ; end of embark-region + gptel

;;;; gptel-prompt

(after! gptel
  ;; git@github.com:character-ai/prompt-poet.git
  (require 'gptel-prompts)
  (setq gptel-prompts-directory (concat org-directory "resources/prompts/"))

  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers)

  (use-package! uuidgen)

  ;; (require 'gptel-litellm)
  ;; (gptel-litellm-install-sessions)
  )

;;; Provide

(provide 'ai-gptel)

;;; ai-gptel.el ends here
