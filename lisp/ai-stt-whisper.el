;;; $DOOMDIR/lisp/ai-stt-whisper.el --- Whisper 음성 입력 (Groq API) -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; whisper.el + Groq API 기반 음성 입력.
;; - Groq의 whisper-large-v3 모델 사용 (로컬 whisper.cpp 불필요)
;; - 한국어, 영어 등 다국어 지원
;; - 녹음/변환 후 커서 위치에 텍스트 삽입
;;
;; 필수 외부 도구:
;;   - ffmpeg (오디오 녹음)
;;   - curl (API 호출)
;;
;; 설정:
;;   - Groq API 키: pass api/groq/junghanacs

;;; Code:

;;;; whisper.el (Groq API)

(use-package! whisper
  :defer 5
  :commands (whisper-run whisper-file whisper-select-language)
  :config
  (setq whisper-server-mode 'openai
        whisper-openai-api-baseurl "https://api.groq.com/openai/"
        whisper-openai-api-key (password-store-get "api/groq/junghanacs")
        whisper-openai-model "whisper-large-v3"
        whisper-language "ko"
        whisper-insert-text-at-point t
        whisper-return-cursor-to-start nil
        whisper-show-progress-in-mode-line t)
  (setq whisper-recording-timeout 120))

;;;; Keybindings

(map! :leader
      (:prefix "-"
       :desc "녹음/중지" "w" #'whisper-run
       :desc "파일 변환" "f" #'whisper-file
       :desc "언어 선택" "l" #'whisper-select-language))

(global-unset-key (kbd "M-a"))  ; unset forward-sentence
(global-set-key (kbd "M-a") #'whisper-run)

(provide 'ai-stt-whisper)
;;; ai-stt-whisper.el ends here
