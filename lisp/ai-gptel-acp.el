;;; $DOOMDIR/lisp/ai-gptel-acp.el --- gptel ACP backend integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1") (gptel "0.9") (acp "0.1"))

;; Based on yqrashawn's acp-integration.el
;; https://github.com/yqrashawn/.doom.d

;;; Commentary:

;; gptel을 ACP(Agent Client Protocol)를 통해 claude-code와 연결하는 백엔드
;; 정액제 Claude Code 계정으로 gptel 무제한 사용 가능
;;
;; 의존성:
;; - gptel (ai-gptel.el에서 로드)
;; - acp (ai-agent-shell.el에서 로드)
;; - claude-code-acp (npm install -g @zed-industries/claude-code-acp)
;;
;; 로딩 순서:
;; 1. ai-gptel.el
;; 2. ai-agent-shell.el (require 'acp)
;; 3. ai-gptel-acp.el (this file)

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'gptel)
(require 'acp)
(require 'cl-generic)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel--system-message)
(defvar gptel-stream)

(declare-function gptel--model-name "gptel")
(declare-function gptel-backend-name "gptel")
(declare-function gptel--trim-prefixes "gptel")
(declare-function gptel--process-models "gptel-openai")

;;;; Backend structure

(cl-defstruct (gptel-acp (:constructor gptel--make-acp)
                         (:copier nil)
                         (:include gptel-backend))
  "ACP backend structure for gptel."
  (command nil :documentation "Path to ACP agent executable.")
  (command-params nil :documentation "Command-line arguments for ACP agent.")
  (mcp-servers nil :documentation "List of MCP server configurations."))

;;;; Client and session management

(defvar gptel-acp--clients (make-hash-table :test 'equal)
  "Hash table of ACP clients keyed by backend name.")

(defvar gptel-acp--sessions (make-hash-table :test 'equal)
  "Hash table of ACP session IDs keyed by backend name.")

(defvar gptel-acp--stream-buffers (make-hash-table :test 'equal)
  "Hash table mapping session IDs to accumulated stream text.")

(defun gptel-acp--get-or-create-client (backend)
  "Get or create ACP client for BACKEND."
  (let ((key (gptel-backend-name backend)))
    (or (gethash key gptel-acp--clients)
        (let* ((command (gptel-acp-command backend))
               (command-params (gptel-acp-command-params backend))
               (client (acp-make-client
                        :context-buffer (current-buffer)
                        :command command
                        :command-params command-params)))
          ;; Setup notification handler for streaming
          (acp-subscribe-to-notifications
           :client client
           :on-notification
           (lambda (notification)
             (let* ((method (alist-get 'method notification))
                    (params (alist-get 'params notification)))
               (when (equal method "session/update")
                 (let* ((sid (alist-get 'sessionId params))
                        (update (alist-get 'update params))
                        (update-type (alist-get 'sessionUpdate update)))
                   (when (equal update-type "agent_message_chunk")
                     (let* ((content (alist-get 'content update))
                            (text (alist-get 'text content)))
                       (when (and text (> (length text) 0))
                         ;; Accumulate text for this session
                         (let ((current (gethash sid gptel-acp--stream-buffers "")))
                           (puthash sid (concat current text) gptel-acp--stream-buffers)
                           (message "[gptel-acp] Chunk received: '%s' (total: %d chars)"
                                    text (length (gethash sid gptel-acp--stream-buffers ""))))))))))))
          (puthash key client gptel-acp--clients)
          client))))

(defun gptel-acp--get-session-id (backend)
  "Get current session ID for BACKEND."
  (gethash (gptel-backend-name backend) gptel-acp--sessions))

(defun gptel-acp--set-session-id (backend session-id)
  "Set session ID for BACKEND to SESSION-ID."
  (puthash (gptel-backend-name backend) session-id gptel-acp--sessions))

(defun gptel-acp--clear-session (backend)
  "Clear session for BACKEND."
  (remhash (gptel-backend-name backend) gptel-acp--sessions))

;;;; Format MCP servers

(defun gptel-acp--format-mcp-servers (servers)
  "Format MCP SERVERS for ACP protocol."
  (if (null servers)
      []
    (vconcat
     (mapcar
      (lambda (server)
        (let ((name (alist-get 'name server))
              (command (alist-get 'command server))
              (args (alist-get 'args server))
              (env (alist-get 'env server)))
          (unless name
            (error "MCP server missing `name' field: %S" server))
          (unless command
            (error "MCP server missing `command' field: %S" server))
          `((name . ,name)
            (command . ,(expand-file-name command))
            (args . ,(or args []))
            (env . ,(or env [])))))
      servers))))

;;;; Backend protocol implementation

(cl-defmethod gptel--parse-buffer ((_backend gptel-acp) &optional max-entries)
  "Parse gptel buffer for conversation history.

MAX-ENTRIES limits how many messages to parse.
Uses same format as OpenAI since both use role-based messages."
  (let ((prompts) (prev-pt (point)))
    (if (or gptel-mode gptel-track-response)
        (while (and (or (not max-entries) (>= max-entries 0))
                    (/= prev-pt (point-min))
                    (goto-char (previous-single-property-change
                                (point) 'gptel nil (point-min))))
          (pcase (get-char-property (point) 'gptel)
            ('response
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "assistant" :content content) prompts)))
            ('ignore)
            ('nil
             (and max-entries (cl-decf max-entries))
             (when-let* ((content (gptel--trim-prefixes
                                   (buffer-substring-no-properties (point) prev-pt))))
               (push (list :role "user" :content content) prompts))))
          (setq prev-pt (point)))
      ;; Not in gptel-mode, just grab all content
      (let ((content (string-trim (buffer-substring-no-properties
                                   (point-min) (point-max)))))
        (push (list :role "user" :content content) prompts)))
    prompts))

(cl-defmethod gptel--request-data ((_backend gptel-acp) prompts)
  "Convert PROMPTS to ACP request format.

PROMPTS is a list of message plists.
This prepares data for the backend but doesn't send it yet."
  ;; Add system message if present
  (when gptel--system-message
    (push (list :role "system"
                :content gptel--system-message)
          prompts))
  ;; Return data that will be used by ACP send
  (list :prompts prompts
        :model (gptel--model-name gptel-model)
        :stream gptel-stream))

(cl-defmethod gptel--parse-response ((_backend gptel-acp) response info)
  "Parse ACP non-streaming RESPONSE.
RESPONSE is the result from a session/prompt request.
INFO is a plist with request metadata."
  (when response
    ;; Store stop reason if present
    (when-let ((stop-reason (alist-get 'stopReason response)))
      (plist-put info :stop-reason stop-reason))
    ;; Extract and return text content
    (gptel-acp--extract-text response)))

(defun gptel-acp--extract-text (response)
  "Extract text content from ACP RESPONSE."
  (let ((messages (alist-get 'messages response))
        (texts '()))
    (when (and messages (> (length messages) 0))
      (dotimes (i (length messages))
        (let* ((msg (aref messages i))
               (role (alist-get 'role msg))
               (content (alist-get 'content msg)))
          ;; Only extract from assistant messages
          (when (equal role "assistant")
            (when (vectorp content)
              (dotimes (j (length content))
                (let* ((item (aref content j))
                       (type (alist-get 'type item))
                       (text (alist-get 'text item)))
                  (when (and (equal type "text") text)
                    (push text texts)))))))))
    (mapconcat #'identity (nreverse texts) "")))

;;;; Helper functions

(defun gptel-acp--format-prompt (prompts)
  "Convert gptel PROMPTS to ACP ContentBlock format.

PROMPTS is a list of message plists.
ACP sessions manage their own conversation history, so we only send
the LAST user message as the new prompt."
  ;; Find the last user message (prompts are in chronological order)
  (let ((last-user-content nil))
    (dolist (msg (reverse prompts))  ; Iterate from end to find last user msg
      (when (and (not last-user-content)
                 (equal (plist-get msg :role) "user"))
        (setq last-user-content (plist-get msg :content))))
    ;; Return as ContentBlock list (acp-make-session-prompt-request will vconcat)
    (if last-user-content
        (list `((type . "text")
                (text . ,last-user-content)))
      (error "No user message found in prompts"))))

(defun gptel-acp--ensure-session (backend callback error-callback)
  "Ensure ACP session exists for BACKEND, then call CALLBACK.

ERROR-CALLBACK is called if session creation fails.
If session doesn't exist, initialize and create one first."
  (let* ((client (gptel-acp--get-or-create-client backend))
         (session-id (gptel-acp--get-session-id backend)))
    (if session-id
        ;; Session exists, proceed
        (funcall callback session-id)
      ;; Need to initialize and create session
      (acp-send-request
       :client client
       :request (acp-make-initialize-request :protocol-version 1)
       :on-success
       (lambda (_response)
         ;; Now create session
         (let ((mcp-servers (or (gptel-acp-mcp-servers backend) [])))
           (acp-send-request
            :client client
            :request (acp-make-session-new-request
                      :cwd default-directory
                      :mcp-servers mcp-servers)
            :on-success
            (lambda (response)
              (let ((new-session-id (alist-get 'sessionId response)))
                (gptel-acp--set-session-id backend new-session-id)
                (funcall callback new-session-id)))
            :on-failure error-callback)))
       :on-failure error-callback))))

;;;; Streaming parser

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-acp) info)
  "Parse ACP streaming responses.

INFO is a plist with request metadata.
For ACP, streaming is handled via notifications in acp.el, not curl output.
This method is called but we don't parse from curl's buffer - instead we
return accumulated text from our notification handler."
  (let* ((session-id (plist-get info :acp-session-id)))
    (when session-id
      (let ((accumulated (gethash session-id gptel-acp--stream-buffers "")))
        ;; Clear after reading
        (puthash session-id "" gptel-acp--stream-buffers)
        accumulated))))

;;;; Request handling - replace curl entirely

(defvar gptel-acp--pending-requests (make-hash-table :test 'equal)
  "Hash table tracking pending ACP requests for cleanup.")

(defun gptel-acp--cleanup-request (request-id)
  "Clean up resources for REQUEST-ID.
NOTE: Does NOT kill the acp process - acp.el manages its own process lifecycle."
  (when-let ((request-data (gethash request-id gptel-acp--pending-requests)))
    (let ((proc (plist-get request-data :process)))
      ;; Only remove from gptel's tracking, don't kill acp's process
      (when proc
        (setf (alist-get proc gptel--request-alist nil 'remove) nil)))
    (remhash request-id gptel-acp--pending-requests)))

(defun gptel-acp-get-response (fsm)
  "Fetch response to prompt in state FSM from ACP agent.

This is the ACP equivalent of `gptel-curl-get-response'.
Instead of using curl, we use acp.el's client/server protocol."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend))
         (data (plist-get info :data))
         (prompts (plist-get data :prompts))
         (stream (plist-get info :stream))
         (callback (or (plist-get info :callback)
                       (if stream
                           #'gptel-curl--stream-insert-response
                         #'gptel--insert-response)))
         ;; Generate unique request ID
         (request-id (format "gptel-acp-%s" (float-time)))
         ;; Get or create client first to access its process
         (client (gptel-acp--get-or-create-client backend))
         ;; Use acp client's actual process (will be started by ensure-session if needed)
         (acp-proc (map-elt client :process)))

    ;; Store callback in info if not already there
    (unless (plist-get info :callback)
      (plist-put info :callback callback))

    ;; Actually send the ACP request
    (message "[gptel-acp] Ensuring session exists...")
    (gptel-acp--ensure-session
     backend
     ;; On session ready
     (lambda (session-id)
       (message "[gptel-acp] Session ready: %s" session-id)
       (plist-put info :acp-session-id session-id)
       (puthash session-id "" gptel-acp--stream-buffers)

       ;; Now client process is definitely started, store for tracking
       (let ((proc (map-elt client :process)))
         (puthash request-id (list :process proc :fsm fsm) gptel-acp--pending-requests)
         ;; Register with gptel's tracking (but cleanup won't kill process)
         (setf (alist-get proc gptel--request-alist)
               (cons fsm (lambda () (gptel-acp--cleanup-request request-id)))))

       (let ((acp-messages (gptel-acp--format-prompt prompts)))

         (message "[gptel-acp] Sending prompt...")
         (acp-send-request
          :client client
          :buffer (plist-get info :buffer)  ; Pass target buffer for context
          :request (acp-make-session-prompt-request
                    :session-id session-id
                    :prompt acp-messages)
          :on-success
          (lambda (response)
            (message "[gptel-acp] on-success called! response=%S" response)
            (plist-put info :http-status "200")
            (plist-put info :status "200")

            ;; Get accumulated streaming text first
            (let* ((accumulated (gethash session-id gptel-acp--stream-buffers ""))
                   (text (if (and stream (> (length accumulated) 0))
                             accumulated
                           ;; Fallback: try to extract from response
                           (or (gptel-acp--extract-text response) ""))))

              (message "[gptel-acp] Accumulated text: '%s'" accumulated)
              (message "[gptel-acp] Final text length: %d" (length text))

              ;; Only proceed if we have text
              (when (> (length text) 0)
                ;; Transition FSM and call callback
                (gptel--fsm-transition fsm)
                (funcall callback text info)

                ;; Signal completion for streaming
                (when stream
                  (funcall callback t info))))

            ;; Clean up
            (remhash session-id gptel-acp--stream-buffers)
            (gptel-acp--cleanup-request request-id))
          :on-failure
          (lambda (error)
            (message "[gptel-acp] Request failed: %S" error)
            (plist-put info :status "error")
            (plist-put info :error error)
            (funcall callback nil info)
            (gptel-acp--cleanup-request request-id)))))
     ;; On session creation error
     (lambda (error)
       (message "[gptel-acp] Session creation failed: %S" error)
       (plist-put info :status "error")
       (plist-put info :error error)
       (funcall callback nil info)
       (gptel-acp--cleanup-request request-id)))))

;;;; Integration with gptel request system

;; Override gptel's --handle-wait to use ACP for ACP backends
(defun gptel-acp--handle-wait-advice (orig-fun fsm)
  "Advice for `gptel--handle-wait' to use ACP for ACP backends.

ORIG-FUN is the original function.
FSM is the request state machine."
  (let* ((info (gptel-fsm-info fsm))
         (backend (plist-get info :backend)))
    (if (gptel-acp-p backend)
        ;; Use ACP protocol
        (progn
          ;; Reset flags like the original does
          (dolist (key '(:tool-success :tool-use :error :http-status :reasoning))
            (when (plist-get info key)
              (plist-put info key nil)))
          (gptel-acp-get-response fsm)
          (run-hooks 'gptel-post-request-hook))
      ;; Not ACP, use default behavior
      (funcall orig-fun fsm))))

;;;; Public API

;;;###autoload
(cl-defun gptel-make-acp
    (name &key command command-params models host mcp-servers
          (stream t)
          (protocol "acp")
          (endpoint "/session/prompt"))
  "Register an ACP agent backend for gptel.

NAME is a string identifying this backend.

COMMAND is the path to the ACP agent executable.

COMMAND-PARAMS is a list of strings passed as command-line arguments.

MODELS is a list of available model symbols or strings.

HOST is a display name for the agent (defaults to \"ACP Agent\").

MCP-SERVERS is a list of MCP server configurations.

STREAM enables streaming responses (defaults to t).

PROTOCOL and ENDPOINT are included for compatibility.

Example:

  (gptel-make-acp \"Claude-ACP\"
    :command \"/usr/local/bin/claude-code-acp\"
    :models \\='(claude-sonnet-4-5)
    :host \"Claude via ACP\")"
  (declare (indent 1))
  (unless command
    (user-error "`:command' is required for ACP backend"))
  (let ((backend (gptel--make-acp
                  :name name
                  :host (or host "ACP Agent")
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :models (gptel--process-models models)
                  :url command  ; Store command in url slot for compatibility
                  :command command
                  :command-params command-params
                  :mcp-servers (gptel-acp--format-mcp-servers mcp-servers))))
    (setf (alist-get name gptel--known-backends nil nil #'equal) backend)
    ;; Install advice when first ACP backend is registered
    (unless (advice-member-p #'gptel-acp--handle-wait-advice 'gptel--handle-wait)
      (advice-add 'gptel--handle-wait :around #'gptel-acp--handle-wait-advice))
    backend))

;;;; Cleanup

(defun gptel-acp-shutdown (backend)
  "Shutdown the ACP client for BACKEND."
  (interactive (list gptel-backend))
  (when (gptel-acp-p backend)
    (let ((client (gethash (gptel-backend-name backend) gptel-acp--clients)))
      (when client
        (acp-shutdown :client client)
        (remhash (gptel-backend-name backend) gptel-acp--clients)
        (gptel-acp--clear-session backend)
        (message "ACP backend %s shutdown" (gptel-backend-name backend))))))

(defun gptel-acp-shutdown-all ()
  "Shutdown all ACP clients."
  (interactive)
  (maphash (lambda (name client)
             (acp-shutdown :client client)
             (message "Shutdown ACP client: %s" name))
           gptel-acp--clients)
  (clrhash gptel-acp--clients)
  (clrhash gptel-acp--sessions)
  (clrhash gptel-acp--stream-buffers))

;;;; Backend Registration

;; claude-code-acp 백엔드 등록
(when (executable-find "claude-code-acp")
  (setq gptel-claude-code-acp
        (gptel-make-acp "Claude-Code-ACP"
          :command (executable-find "claude-code-acp")
          :models '(claude-sonnet-4-5 claude-opus-4-5)
          :host "Claude Code via ACP"
          :stream t))
  (message "[gptel-acp] Claude Code ACP backend registered"))

(provide 'ai-gptel-acp)
;;; ai-gptel-acp.el ends here
