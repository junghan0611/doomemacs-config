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

(use-package! gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-temperature 0.3) ; gptel 1.0, Perplexity 0.2
  (set-popup-rule! "^\\*ChatGPT\\*$" :side 'right :size 84 :vslot 100 :quit t) ; size 0.4

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

;;;; gptel-mode-hook

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

  ) ; end of use-package! gptel

(provide 'ai-gptel)

;;; ai-gptel.el ends here
