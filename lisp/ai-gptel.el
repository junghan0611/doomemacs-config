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

;;;; gptel

(use-package! gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-temperature 0.3) ; gptel 1.0, Perplexity 0.2
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t)
  (set-popup-rule! "^\\*gptel-buffer\\*$" :side 'right :size 0.4 :vslot 99 :quit nil :select t)

  (with-eval-after-load 'gptel-org
    (defun gptel-org-toggle-branching-context ()
      "Toggle gptel context between doc and subheading."
      (interactive)
      (if gptel-org-branching-context
          (progn
            (setq-local gptel-org-branching-context nil)
            (message "Context: whole doc"))
        (setq-local gptel-org-branching-context t)
        (message "Context: subheading")))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user: "
          (alist-get 'org-mode gptel-response-prefix-alist) "@assistant:\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "#### ")
    (setq-default gptel-org-branching-context t))

;;;;; gptel openrouter models

(defconst gptel--openrouter-models
  '(
    ;; https://openrouter.ai/provider/deepseek
    (deepseek/deepseek-v3.2-speciale
     :capabilities (tool reasoning)
     :context-window 131
     :input-cost 0.28
     :output-cost 0.42)

    (deepseek/deepseek-v3.2
     :capabilities (tool reasoning)
     :context-window 131
     :input-cost 0.25
     :output-cost 0.38)

    ;; https://openrouter.ai/google/gemini-2.5-pro
    (google/gemini-2.5-pro
     :capabilities (media tool-use cache reasoning)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1048
     :input-cost 1.25
     :output-cost 10)

    ;; https://openrouter.ai/google/gemini-2.5-flash
    (google/gemini-2.5-flash
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 1048
     :input-cost 0.30
     :output-cost 2.5)

    (openai/gpt-5-chat
     :description "Flagship model for coding, reasoning, and agentic tasks across domains"
     :capabilities (media json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 1.25
     :output-cost 10
     :cutoff-date "2024-09")

    ;; https://openrouter.ai/anthropic/claude-sonnet-4
    ;; (anthropic/claude-sonnet-4.5
    ;;  :description "Hybrid model capable of standard thinking and extended thinking modes"
    ;;  :capabilities (media tool-use cache)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
    ;;  :context-window 200
    ;;  :input-cost 3
    ;;  :output-cost 15
    ;;  :cutoff-date "2025-05")

    ;; (anthropic/claude-opus-4.5
    ;;  :description "Hybrid model capable of standard thinking and extended thinking modes"
    ;;  :capabilities (media tool-use cache)
    ;;  :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
    ;;  :context-window 200
    ;;  :input-cost 15
    ;;  :output-cost 25
    ;;  :cutoff-date "2025-12")
    )
  )

(setq gptel-openrouter-backend
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key #'gptel-api-key
        :models gptel--openrouter-models))

(setq gptel-backend gptel-openrouter-backend)
(setq gptel-model 'deepseek/deepseek-v3.2)
;; (setq gptel-model 'google/gemini-2.5-flash)

(gptel-make-deepseek "DeepSeek"       ;Any name you want
  :stream t                           ;for streaming responses
  :key #'gptel-api-key)               ;can be a function that returns the key

;;;;; gptel-mode-hook

(add-hook! 'gptel-mode-hook
  (defun gptel-mode-set-local-keys ()
    (map! :map gptel-mode-map
          :iv "M-<return>" #'gptel-send
          :iv "M-RET" #'gptel-send
          (:localleader
           :desc "gptel/default" "5" #'gptel-menu ;; TODO fixme
           ;; "M-s" #'gptel-save-as-org-with-denote-metadata
           "0" #'cashpw/gptel-send
           (:prefix ("s" . "session")
            :desc "clear" "l" #'gptel-clear-buffer+
            ;; "p" #'gptel-save-as-org-with-denote-metadata
            )))))

(add-hook! 'gptel-mode-hook
  (defun cae-gptel-mode-setup-h ()
    ;; (setq-local nobreak-char-display nil) ; 2025-07-26 보는게 좋아
    (auto-fill-mode -1)
    (doom-mark-buffer-as-real-h)))

;;;;; gptel-buffer: 범용 버퍼 요약/번역

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

;; 핵심 함수

(defun +gptel--send-to-buffer (content system-message action-name)
  "CONTENT를 gptel 사이드 버퍼로 보내고 SYSTEM-MESSAGE로 요청.
ACTION-NAME은 표시용 (예: \"요약\", \"번역\")."
  (let* ((formatted (+gptel--format-content-for-llm content))
         (buf (get-buffer-create +gptel-buffer-name)))
    ;; 사이드 버퍼 설정
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless gptel-mode
        (gptel-mode 1))
      ;; 시스템 메시지 설정
      (setq-local gptel--system-message system-message)
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
    (message "%s 요청을 보냈습니다. 결과는 %s 버퍼에서 확인하세요."
             action-name +gptel-buffer-name)))

;;;###autoload
(defun +gptel-summarize-buffer ()
  "현재 버퍼 내용을 요약하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
  (interactive)
  (let ((content (+gptel--extract-buffer-content)))
    (+gptel--send-to-buffer content +gptel-summarize-system-message "요약")))

;;;###autoload
(defun +gptel-translate-buffer ()
  "현재 버퍼 내용을 번역하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
  (interactive)
  (let ((content (+gptel--extract-buffer-content)))
    (+gptel--send-to-buffer content +gptel-translate-system-message "번역")))

;;;###autoload
(defun +gptel-buffer-dwim ()
  "현재 버퍼에 대해 gptel 액션 선택 (요약/번역)."
  (interactive)
  (let ((action (completing-read "Action: " '("요약 (Summarize)" "번역 (Translate)"))))
    (pcase action
      ("요약 (Summarize)" (+gptel-summarize-buffer))
      ("번역 (Translate)" (+gptel-translate-buffer)))))

;;;;; gptel-buffer 키바인딩

;; 전역 키바인딩 (SPC a G 접두어) - SPC a 충돌로 비활성화
;; (map! :leader
;;       (:prefix ("a" . "AI")
;;        (:prefix ("G" . "gptel-buffer")
;;         :desc "Summarize buffer" "s" #'+gptel-summarize-buffer
;;         :desc "Translate buffer" "t" #'+gptel-translate-buffer
;;         :desc "DWIM (choose action)" "g" #'+gptel-buffer-dwim)))

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

  ) ; end of use-package! gptel

(provide 'ai-gptel)

;;; ai-gptel.el ends here
