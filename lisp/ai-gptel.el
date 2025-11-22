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
    ;; Created Jan 20, 2025 163,840 context $0.40/M input tokens $2/M output tokens
    (deepseek/deepseek-r1-0528
     :capabilities (tool reasoning)
     :context-window 164
     :input-cost 0.55
     :output-cost 2.19)

    (deepseek/deepseek-chat-v3-0324
     :capabilities (tool)
     :context-window 131
     :input-cost 0.27
     :output-cost 1.1)

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

    (openai/gpt-oss-120b
     :description "gpt-oss-120b is an open-weight, 117B-parameter Mixture-of-Experts (MoE) language model from OpenAI designed for high-reasoning, agentic, and general-purpose production use cases."
     :capabilities (media tool-use json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 131
     :input-cost 0.072
     :output-cost 0.28
     :cutoff-date "2025-08")

    (openai/gpt-5-mini
     :description "Faster, more cost-efficient version of GPT-5"
     :capabilities (media json url)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 400
     :input-cost 0.25
     :output-cost 2.0
     :cutoff-date "2024-09")

    ;; https://openrouter.ai/anthropic/claude-sonnet-4
    (anthropic/claude-sonnet-4
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 3
     :output-cost 15
     :cutoff-date "2025-05")

    (anthropic/claude-opus-4.1
     :description "Hybrid model capable of standard thinking and extended thinking modes"
     :capabilities (media tool-use cache)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
     :context-window 200
     :input-cost 15
     :output-cost 75
     :cutoff-date "2025-07")))

(setq gptel-openrouter-backend
      (gptel-make-openai "OpenRouter"
        :host "openrouter.ai"
        :endpoint "/api/v1/chat/completions"
        :stream t
        :key #'gptel-api-key
        :models gptel--openrouter-models))

(setq gptel-backend gptel-openrouter-backend)
(setq gptel-model 'google/gemini-2.5-flash)

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
