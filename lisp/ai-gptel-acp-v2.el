;;; ai-gptel-acp-v2.el --- gptel ACP backend (redesigned) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (gptel "0.9") (acp "0.8"))

;;; Commentary:

;; gptel을 ACP(Agent Client Protocol)를 통해 claude-code와 연결하는 백엔드
;; minuet-acp.el의 성공적인 패턴을 참고하여 재설계
;;
;; 핵심 설계:
;; 1. 영속 세션 - 한 번 생성 후 재사용 (cold start 제거)
;; 2. notification 기반 스트리밍 - 청크 즉시 gptel callback 호출
;; 3. on-success는 완료 신호만 처리
;; 4. gptel FSM 상태 전환 패턴 준수
;;
;; 의존성:
;; - gptel
;; - acp.el (xenodium)
;; - claude-code-acp CLI
;; ;; 2. 백엔드 확인
;; gptel--known-backends  ; "Claude-ACP" 있어야 함
;; ;; 3. 백엔드 선택
;; (setq gptel-backend (gptel-get-backend "Claude-ACP"))
;; (setq gptel-model 'claude-sonnet-4-6)
;; ;; 4. 테스트
;; M-x gptel-send
;; ;; 5. 문제 시 리셋
;; M-x gptel-acp-reset-session

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'gptel)
(require 'acp)

(defvar gptel--request-alist)
(defvar gptel--system-message)
(defvar gptel-stream)
(defvar gptel-mode)
(defvar gptel-track-response)
(defvar gptel-model)

(declare-function gptel--model-name "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--process-models "gptel-openai")
(declare-function gptel-curl--stream-insert-response "gptel")
(declare-function gptel--insert-response "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel-fsm-info "gptel")
(declare-function gptel--fsm-transition "gptel")

;;; ============================================================================
;;; Backend Structure
;;; ============================================================================

(cl-defstruct (gptel-acp (:constructor gptel-acp--make)
                         (:copier nil)
                         (:include gptel-backend))
  "ACP backend for gptel.
Uses Agent Client Protocol to communicate with Claude Code."
  (command "claude-code-acp" :documentation "ACP agent command.")
  (command-args nil :documentation "Command arguments."))

;;; ============================================================================
;;; Session Management
;;; ============================================================================

(defvar gptel-acp--client nil
  "Global ACP client instance.")

(defvar gptel-acp--session-id nil
  "Current ACP session ID.")

(defvar gptel-acp--initialized nil
  "Non-nil if ACP client is initialized.")

(defvar gptel-acp--notification-handler-installed nil
  "Non-nil if notification handler is already installed.")

;; 스트리밍 상태 (요청별)
(defvar gptel-acp--current-fsm nil
  "Current FSM for streaming response.")

(defvar gptel-acp--accumulated-text nil
  "Accumulated text from streaming chunks.")

(defvar gptel-acp--first-chunk nil
  "Non-nil if waiting for first chunk.")

(defun gptel-acp--available-p ()
  "Check if ACP is available."
  (executable-find "claude-code-acp"))

(defun gptel-acp--ensure-session (callback &optional error-callback)
  "Ensure ACP session exists, then call CALLBACK with session-id.
ERROR-CALLBACK is called on failure."
  (if (and gptel-acp--client gptel-acp--session-id gptel-acp--initialized)
      ;; 세션 이미 있음 - 바로 콜백
      (funcall callback gptel-acp--session-id)
    ;; 세션 생성 필요
    (message "[gptel-acp] Initializing ACP session...")
    (setq gptel-acp--client
          (acp-make-client :command "claude-code-acp"
                           :context-buffer (current-buffer)))

    ;; notification handler 등록 (한 번만)
    (unless gptel-acp--notification-handler-installed
      (acp-subscribe-to-notifications
       :client gptel-acp--client
       :on-notification #'gptel-acp--handle-notification)
      (setq gptel-acp--notification-handler-installed t))

    ;; Step 1: Initialize protocol
    (acp-send-request
     :client gptel-acp--client
     :request (acp-make-initialize-request :protocol-version 1)
     :on-success
     (lambda (_response)
       (message "[gptel-acp] Protocol initialized")
       ;; Step 2: Create session
       (acp-send-request
        :client gptel-acp--client
        :request (acp-make-session-new-request :cwd default-directory)
        :on-success
        (lambda (response)
          (setq gptel-acp--session-id (alist-get 'sessionId response))
          (setq gptel-acp--initialized t)
          (message "[gptel-acp] Session created: %s" gptel-acp--session-id)
          (funcall callback gptel-acp--session-id))
        :on-failure
        (lambda (err)
          (message "[gptel-acp] Session creation failed: %S" err)
          (when error-callback (funcall error-callback err)))))
     :on-failure
     (lambda (err)
       (message "[gptel-acp] Protocol init failed: %S" err)
       (when error-callback (funcall error-callback err))))))

;;; ============================================================================
;;; Notification Handler (스트리밍)
;;; ============================================================================

(defun gptel-acp--handle-notification (notification)
  "Handle ACP NOTIFICATION for streaming responses."
  (when-let* ((method (alist-get 'method notification))
              ((equal method "session/update"))
              (params (alist-get 'params notification))
              (update (alist-get 'update params))
              (update-type (alist-get 'sessionUpdate update))
              ((equal update-type "agent_message_chunk"))
              (content (alist-get 'content update))
              (text (alist-get 'text content))
              ((> (length text) 0))
              ;; 현재 활성 요청이 있어야 함
              (fsm gptel-acp--current-fsm))
    (let* ((info (gptel-fsm-info fsm))
           (buffer (plist-get info :buffer))
           (stream (plist-get info :stream))
           (callback (plist-get info :callback)))

      ;; 텍스트 누적
      (setq gptel-acp--accumulated-text
            (concat gptel-acp--accumulated-text text))

      ;; 스트리밍 모드: 즉시 gptel callback 호출
      (when (and stream callback (buffer-live-p buffer))
        (when gptel-acp--first-chunk
          ;; 첫 청크: HTTP 상태 설정 및 FSM 전환
          (plist-put info :http-status "200")
          (plist-put info :status "200")
          (gptel--fsm-transition fsm)
          (setq gptel-acp--first-chunk nil))
        ;; 청크 전달
        (with-current-buffer buffer
          (funcall callback text info))))))

;;; ============================================================================
;;; Core: ACP Request Handler
;;; ============================================================================

(defun gptel-acp-get-response (fsm)
  "Fetch response using ACP protocol.
FSM is gptel's state machine.

This replaces `gptel-curl-get-response' for ACP backends."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (stream (plist-get info :stream))
         (callback (or (plist-get info :callback)
                       (if stream
                           #'gptel-curl--stream-insert-response
                         #'gptel--insert-response))))

    ;; callback 저장
    (unless (plist-get info :callback)
      (plist-put info :callback callback))

    ;; 스트리밍 상태 초기화
    (setq gptel-acp--current-fsm fsm
          gptel-acp--accumulated-text ""
          gptel-acp--first-chunk t)

    ;; 세션 확보 후 요청
    (gptel-acp--ensure-session
     (lambda (session-id)
       ;; 프롬프트 구성
       (let* ((data (plist-get info :data))
              (prompts (plist-get data :prompts))
              (prompt-text (gptel-acp--extract-last-user-message prompts))
              (prompt-blocks (vector `((type . "text")
                                       (text . ,prompt-text)))))

         (message "[gptel-acp] Sending prompt: %s..."
                  (truncate-string-to-width prompt-text 50))

         ;; gptel 요청 추적 등록 (취소 지원)
         (setf (alist-get 'gptel-acp-request gptel--request-alist)
               (cons fsm (lambda ()
                           (gptel-acp--cancel-request)
                           (message "[gptel-acp] Request cancelled"))))

         ;; ACP 요청 전송
         (acp-send-request
          :client gptel-acp--client
          :buffer buffer
          :request (acp-make-session-prompt-request
                    :session-id session-id
                    :prompt prompt-blocks)
          :on-success
          (lambda (_response)
            (message "[gptel-acp] Request completed (text: %d chars)"
                     (length gptel-acp--accumulated-text))

            ;; 요청 추적에서 제거
            (setf (alist-get 'gptel-acp-request gptel--request-alist nil 'remove) nil)

            (let ((final-text gptel-acp--accumulated-text))
              ;; 스트리밍이 아닌 경우, 여기서 전체 응답 처리
              (if (not stream)
                  (progn
                    (plist-put info :http-status "200")
                    (plist-put info :status "200")
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (funcall callback final-text info)))
                    (gptel--fsm-transition fsm))
                ;; 스트리밍인 경우, 완료 신호
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (funcall callback t info)))
                ;; FSM 전환 (첫 청크에서 이미 전환 안 됐으면)
                (when gptel-acp--first-chunk
                  (gptel--fsm-transition fsm))))

            ;; 상태 초기화
            (setq gptel-acp--current-fsm nil))

          :on-failure
          (lambda (err)
            (message "[gptel-acp] Request failed: %S" err)
            ;; 요청 추적에서 제거
            (setf (alist-get 'gptel-acp-request gptel--request-alist nil 'remove) nil)

            (plist-put info :status "error")
            (plist-put info :error err)
            (when (buffer-live-p buffer)
              (funcall callback nil info))
            (gptel--fsm-transition fsm)
            (setq gptel-acp--current-fsm nil)))))

     ;; 세션 생성 실패
     (lambda (err)
       (plist-put info :status "error")
       (plist-put info :error err)
       (funcall callback nil info)
       (setq gptel-acp--current-fsm nil)))))

(defun gptel-acp--cancel-request ()
  "Cancel current ACP request."
  (when gptel-acp--session-id
    (acp-send-notification
     :client gptel-acp--client
     :notification (acp-make-session-cancel-notification
                    :session-id gptel-acp--session-id
                    :reason "User cancelled")))
  (setq gptel-acp--current-fsm nil))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun gptel-acp--extract-last-user-message (prompts)
  "Extract the last user message from PROMPTS.
ACP sessions maintain their own history, so we only send the latest."
  (let ((last-user nil))
    (dolist (msg (reverse prompts))
      (when (and (null last-user)
                 (equal (plist-get msg :role) "user"))
        (setq last-user (plist-get msg :content))))
    (or last-user
        (error "[gptel-acp] No user message found"))))

;;; ============================================================================
;;; gptel Integration
;;; ============================================================================

(defun gptel-acp--handle-wait-advice (orig-fun fsm)
  "Advice for `gptel--handle-wait'.
Use ACP protocol for ACP backends instead of curl.
ORIG-FUN is the original function, FSM is the state machine."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (if (gptel-acp-p backend)
        (progn
          ;; 플래그 리셋 (원본과 동일)
          (dolist (key '(:tool-success :tool-use :error :http-status :reasoning))
            (when (plist-get info key)
              (plist-put info key nil)))
          ;; ACP로 요청
          (gptel-acp-get-response fsm)
          (run-hooks 'gptel-post-request-hook))
      ;; ACP가 아니면 원본 함수
      (funcall orig-fun fsm))))

;;; ============================================================================
;;; gptel Protocol Methods
;;; ============================================================================

(cl-defmethod gptel--parse-buffer ((_backend gptel-acp) &optional max-entries)
  "Parse buffer for conversation history.
MAX-ENTRIES limits message count."
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min)))
                    (not (= (point) prev-pt)))
          (unless (save-excursion (skip-syntax-forward " ") (>= (point) prev-pt))
            (pcase (get-char-property (point) 'gptel)
              ('response
               (when-let* ((content (gptel--trim-prefixes
                                     (buffer-substring-no-properties (point) prev-pt))))
                 (when (not (string-blank-p content))
                   (push (list :role "assistant" :content content) prompts))))
              ('ignore)
              ('nil
               (and max-entries (cl-decf max-entries))
               (when-let* ((content (gptel--trim-prefixes
                                     (buffer-substring-no-properties (point) prev-pt))))
                 (when (not (string-blank-p content))
                   (push (list :role "user" :content content) prompts))))))
          (setq prev-pt (point)))
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (when (not (string-blank-p content))
          (push (list :role "user" :content content) prompts))))
    prompts))

(cl-defmethod gptel--request-data ((_backend gptel-acp) prompts)
  "Prepare request data from PROMPTS."
  (when gptel--system-message
    (push (list :role "system" :content gptel--system-message) prompts))
  (list :prompts prompts
        :model (gptel--model-name gptel-model)
        :stream gptel-stream))

;;; ============================================================================
;;; Public API
;;; ============================================================================

;;;###autoload
(cl-defun gptel-make-acp (name &key command models host (stream t))
  "Create an ACP backend for gptel.

NAME is the backend identifier.
COMMAND is the ACP agent path (default: claude-code-acp).
MODELS is a list of model names.
HOST is a display name.
STREAM enables streaming (default: t)."
  (declare (indent 1))
  (let ((backend (gptel-acp--make
                  :name name
                  :host (or host "Claude via ACP")
                  :protocol "acp"
                  :stream stream
                  :models (gptel--process-models models)
                  :url (or command "claude-code-acp")
                  :command (or command "claude-code-acp"))))
    ;; 백엔드 등록
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    ;; advice 설치 (최초 1회)
    (unless (advice-member-p #'gptel-acp--handle-wait-advice 'gptel--handle-wait)
      (advice-add 'gptel--handle-wait :around #'gptel-acp--handle-wait-advice))
    backend))

;;;###autoload
(defun gptel-acp-reset-session ()
  "Reset ACP session.
Call this if session becomes stale or on errors."
  (interactive)
  (when gptel-acp--client
    (ignore-errors (acp-shutdown :client gptel-acp--client)))
  (setq gptel-acp--client nil
        gptel-acp--session-id nil
        gptel-acp--initialized nil
        gptel-acp--notification-handler-installed nil
        gptel-acp--current-fsm nil
        gptel-acp--accumulated-text nil
        gptel-acp--first-chunk nil)
  (message "[gptel-acp] Session reset"))

;;; ============================================================================
;;; Auto-registration
;;; ============================================================================

(when (gptel-acp--available-p)
  (gptel-make-acp "Claude-ACP"
    :command "claude-code-acp"
    :models '(claude-sonnet-4-6 claude-opus-4-6 claude-haiku-4-5)
    :host "Claude Code (ACP)"
    :stream t)
  (message "[gptel-acp] Backend registered"))

(provide 'ai-gptel-acp-v2)
;;; ai-gptel-acp-v2.el ends here
