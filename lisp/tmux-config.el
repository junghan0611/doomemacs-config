;;; $DOOMDIR/lisp/tmux-config.el --- Tmux + Claude Code Orchestration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Claude Code 멀티에이전트 오케스트레이션 (별도 세션 방식)
;;
;; ┌─────────────────────────────────────────────────────────────────┐
;; │ 아키텍처: 완전 격리된 별도 세션                                 │
;; │                                                                  │
;; │   tmux attach -t claude-pm    ← PM 에이전트                     │
;; │   tmux attach -t claude-code  ← 코딩 에이전트                   │
;; │   tmux attach -t claude-test  ← 테스트 에이전트                 │
;; │                                                                  │
;; │ 장점:                                                           │
;; │   - Claude Code 하나 멈춰도 다른 세션에 영향 없음               │
;; │   - 각 터미널 창에서 독립적으로 attach 가능                     │
;; │   - 간단한 세션 이름으로 관리                                   │
;; └─────────────────────────────────────────────────────────────────┘
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 사용법
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 1. Emacs에서 세션 생성:
;;    SPC \ t c  →  에이전트 선택 (pm, code, test, debug)
;;
;; 2. 터미널에서 attach 후 프로젝트로 이동:
;;    $ tmux attach -t claude-pm
;;    $ cd ~/repos/work/sks-hub-zig
;;    $ claude
;;
;; 3. Emacs에서 에이전트에 메시지 전송:
;;    SPC \ t s  →  에이전트 선택 → 메시지 입력
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 키바인딩 (SPC \ t ...)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 세션 관리:
;;   SPC \ t c   Create session     세션 생성 (detached)
;;   SPC \ t l   List sessions      세션 목록
;;   SPC \ t a   Attach (copy)      attach 명령 복사
;;   SPC \ t +   Add agent          새 에이전트 추가
;;
;; Claude 에이전트:
;;   SPC \ t s   Send to agent      에이전트에 텍스트 전송
;;   SPC \ t r   Send region        선택 영역 전송
;;   SPC \ t b   Send buffer        버퍼 전체 전송
;;   SPC \ t d   Send defun         현재 함수 전송
;;   SPC \ t i   Assign issue       bd 이슈 할당
;;   SPC \ t n   Assign next        다음 ready 이슈 할당
;;   SPC \ t o   Capture output     세션 출력 캡처
;;   SPC \ t v   Show conversation  대화 내용 정리 표시
;;   SPC \ t ?   Agent status       에이전트 상태 확인
;;
;; 추출:
;;   SPC \ t e   Extract response   마지막 Claude 응답 추출
;;   SPC \ t E   Extract input      마지막 사용자 입력 추출
;;
;; 권한 프롬프트 처리:
;;   SPC \ t p   Pending prompts    대기 중인 프롬프트 표시
;;   SPC \ t y   Approve            에이전트 승인 (y 전송)
;;   SPC \ t N   Reject             에이전트 거부 (n 전송)
;;   SPC \ t Y   Approve all        모든 에이전트 일괄 승인
;;
;; emamux:
;;   SPC \ t m s   emamux:send-command
;;   SPC \ t m r   emamux:run-command
;;   SPC \ t m l   emamux:run-last-command

;;; Code:

;;;; Variables

(defvar +claude-agents
  '(("pm"    . "claude-pm")    ; PM 에이전트 (별도 세션)
    ("code"  . "claude-code")  ; 코딩 에이전트 (별도 세션)
    ("test"  . "claude-test")  ; 테스트 에이전트 (별도 세션)
    ("debug" . "claude-debug"));; 디버그 에이전트 (별도 세션)
  "Claude Code 에이전트 레지스트리.
각 에이전트는 (NAME . SESSION-NAME) 형식.
완전 격리된 별도 세션으로 운영.

사용법:
  tmux attach -t claude-pm    ; PM 세션 연결
  tmux attach -t claude-code  ; Code 세션 연결

동적 추가:
  (add-to-list '+claude-agents '(\"review\" . \"claude-review\"))")

(defvar +claude-default-agent "pm"
  "기본 에이전트 이름.")

(defvar +claude-session-prefix "claude-"
  "Claude Code 에이전트 세션 이름 접두사.")

;;;; Core Functions

(defun +claude--session-exists-p (session)
  "SESSION이 존재하는지 확인."
  (= 0 (call-process "tmux" nil nil nil
                     "has-session" "-t" session)))

(defun +claude--ensure-session (agent-name)
  "AGENT-NAME의 세션이 없으면 생성 (detached).
Claude 실행은 하지 않음 - 사용자가 직접 cd 후 실행.
반환값: 세션 이름"
  (let ((session (cdr (assoc agent-name +claude-agents))))
    (unless session
      (error "Unknown agent: %s" agent-name))

    ;; 세션 확인/생성
    (unless (+claude--session-exists-p session)
      (call-process "tmux" nil nil nil "new-session" "-d" "-s" session)
      (message "Created tmux session: %s (attach with: tmux attach -t %s)"
               session session))
    session))

(defun +claude--get-session (agent-name)
  "AGENT-NAME에 해당하는 세션 이름 반환."
  (or (cdr (assoc agent-name +claude-agents))
      (error "Unknown agent: %s" agent-name)))



(defun +claude--send-keys (session text &optional no-enter)
  "SESSION에 TEXT 전송. NO-ENTER가 nil이면 Enter도 전송."
  (let ((args (list "send-keys" "-t" session text)))
    (unless no-enter
      (setq args (append args '("Enter"))))
    (apply #'call-process "tmux" nil nil nil args)))

;;;; Interactive Commands - Send

;;;###autoload
(defun +claude-send-to-agent (agent-name text)
  "AGENT-NAME에게 TEXT 전송.
세션이 없으면 자동 생성 (detached)."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)
         (read-string "Message: ")))
  (let ((session (+claude--ensure-session agent-name)))
    (+claude--send-keys session text)
    (message "Sent to %s: %s" agent-name (truncate-string-to-width text 50))))

;;;###autoload
(defun +claude-send-region (beg end &optional agent-name)
  "선택 영역을 AGENT-NAME에게 전송.
prefix arg로 에이전트 선택."
  (interactive
   (list (region-beginning)
         (region-end)
         (when current-prefix-arg
           (completing-read "Agent: " (mapcar #'car +claude-agents) nil t))))
  (let* ((agent (or agent-name +claude-default-agent))
         (text (buffer-substring-no-properties beg end))
         (session (+claude--get-session agent)))
    (+claude--send-keys session text)
    (message "Region sent to %s (%d chars)" agent (length text))))

;;;###autoload
(defun +claude-send-buffer (&optional agent-name)
  "현재 버퍼 전체를 AGENT-NAME에게 전송."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Agent: " (mapcar #'car +claude-agents) nil t))))
  (+claude-send-region (point-min) (point-max) agent-name))

;;;###autoload
(defun +claude-send-defun (&optional agent-name)
  "현재 함수/defun을 AGENT-NAME에게 전송."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Agent: " (mapcar #'car +claude-agents) nil t))))
  (save-excursion
    (mark-defun)
    (+claude-send-region (region-beginning) (region-end) agent-name)))

;;;; Interactive Commands - bd Integration

;;;###autoload
(defun +claude-assign-issue (issue-id &optional agent-name)
  "BD ISSUE-ID를 AGENT-NAME에게 할당."
  (interactive
   (list (read-string "Issue ID: ")
         (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (issue-json (shell-command-to-string
                      (format "bd show %s --json 2>/dev/null" issue-id)))
         (issue (condition-case nil
                    (json-parse-string issue-json :object-type 'plist)
                  (error nil))))
    (if issue
        (let* ((title (plist-get issue :title))
               (desc (or (plist-get issue :description) ""))
               (prompt (format "Work on issue %s: %s\n\n%s" issue-id title desc)))
          (+claude-send-to-agent agent prompt)
          (shell-command (format "bd update %s --status in_progress" issue-id))
          (message "Assigned %s to %s" issue-id agent))
      (message "Issue %s not found" issue-id))))

;;;###autoload
(defun +claude-assign-ready-issue (&optional agent-name)
  "다음 ready 이슈를 AGENT-NAME에게 할당."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((ready-json (shell-command-to-string "bd ready --json 2>/dev/null"))
         (issues (condition-case nil
                     (json-parse-string ready-json :array-type 'list :object-type 'plist)
                   (error nil))))
    (if (and issues (> (length issues) 0))
        (let* ((first-issue (car issues))
               (issue-id (plist-get first-issue :id)))
          (+claude-assign-issue issue-id agent-name))
      (message "No ready issues found"))))

;;;; Interactive Commands - Session Management

;;;###autoload
(defun +claude-list-sessions ()
  "Claude 에이전트 세션 목록 표시."
  (interactive)
  (let ((output (shell-command-to-string
                 "tmux list-sessions -F '#{session_name} - #{session_created_string}' 2>/dev/null")))
    (if (string-empty-p output)
        (message "No tmux sessions")
      (message "Sessions:\n%s" output))))

;;;###autoload
(defun +claude-create-session (agent-name)
  "AGENT-NAME 세션 생성 (detached)."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t)))
  (+claude--ensure-session agent-name))

;;;###autoload
(defun +claude-attach-session (agent-name)
  "AGENT-NAME 세션에 attach하는 명령 표시."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t)))
  (let ((session (+claude--get-session agent-name)))
    (if (+claude--session-exists-p session)
        (progn
          (kill-new (format "tmux attach -t %s" session))
          (message "Copied: tmux attach -t %s (paste in terminal)" session))
      (message "Session %s does not exist. Create with SPC \\ t c" session))))

;;;###autoload
(defun +claude-capture-pane (&optional agent-name)
  "AGENT-NAME 세션의 출력을 캡처하여 새 버퍼에 표시."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (output (shell-command-to-string
                  (format "tmux capture-pane -t %s -p 2>/dev/null" session)))
         (buf (get-buffer-create (format "*tmux-capture: %s*" agent))))
    (if (string-empty-p output)
        (message "Session %s not found or empty" session)
      (with-current-buffer buf
        (erase-buffer)
        (insert output)
        (goto-char (point-min)))
      (pop-to-buffer buf)
      (message "Captured %s session output" agent))))

;;;###autoload
(defun +claude-add-session ()
  "새 에이전트 세션을 동적으로 추가."
  (interactive)
  (let* ((name (read-string "Agent name: "))
         (session (read-string "Session name: " (concat +claude-session-prefix name))))
    (add-to-list '+claude-agents (cons name session))
    (message "Added agent '%s' -> %s" name session)))

;;;; Claude Code Output Parsing
;;
;; Claude Code 출력 패턴:
;;   > 사용자 입력          (사용자 프롬프트)
;;   ∴ Thinking…           (사고 중)
;;   ● Claude 응답         (응답 시작)
;;   ───────────           (구분선)
;;   -- INSERT --          (입력 대기)

(defun +claude--capture-raw (session &optional scrollback)
  "SESSION의 원시 출력 캡처. SCROLLBACK은 히스토리 줄 수."
  (let ((scroll-opt (if scrollback (format "-S -%d" scrollback) "-S -200")))
    (shell-command-to-string
     (format "tmux capture-pane -t %s %s -p 2>/dev/null" session scroll-opt))))

;;;###autoload
(defun +claude-extract-last-response (&optional agent-name)
  "AGENT-NAME의 마지막 Claude 응답(● 로 시작) 추출."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (output (+claude--capture-raw session 200))
         (response nil))
    ;; ● 로 시작하는 마지막 응답 블록 찾기
    (when (string-match "● \\([^─]+\\)" output)
      (setq response (match-string 1 output))
      ;; 여러 응답이 있으면 마지막 것
      (while (string-match "● \\([^─]+\\)" output (match-end 0))
        (setq response (match-string 1 output))))
    (if response
        (let ((cleaned (string-trim response)))
          (if (called-interactively-p 'any)
              (progn
                (kill-new cleaned)
                (message "Response copied: %s" (truncate-string-to-width cleaned 60)))
            cleaned))
      (message "No response found")
      nil)))

;;;###autoload
(defun +claude-extract-last-input (&optional agent-name)
  "AGENT-NAME의 마지막 사용자 입력(> 로 시작) 추출."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (output (+claude--capture-raw session 200))
         (input nil))
    ;; > 로 시작하는 마지막 입력 찾기
    (when (string-match "^> \\(.+\\)$" output)
      (setq input (match-string 1 output))
      (while (string-match "^> \\(.+\\)$" output (match-end 0))
        (setq input (match-string 1 output))))
    (if input
        (let ((cleaned (string-trim input)))
          (if (called-interactively-p 'any)
              (progn
                (kill-new cleaned)
                (message "Input copied: %s" cleaned))
            cleaned))
      (message "No input found")
      nil)))

;;;###autoload
(defun +claude-agent-status (&optional agent-name)
  "AGENT-NAME의 현재 상태 확인.
반환값:
  'pending-approval - 권한 승인 대기 중 (y/n 프롬프트)
  'waiting          - 일반 입력 대기 중 (INSERT 모드)
  'thinking         - 사고 중
  'working          - 작업 중
  'not-found        - 세션 없음"
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (output (+claude--capture-raw session 50))
         (status (cond
                  ((string-empty-p output) 'not-found)
                  ;; 권한 프롬프트 패턴 (가장 먼저 체크)
                  ((+claude--has-permission-prompt-p output) 'pending-approval)
                  ((string-match "-- INSERT --" output) 'waiting)
                  ((string-match "∴ Thinking" output) 'thinking)
                  (t 'working))))
    (when (called-interactively-p 'any)
      (message "Agent %s: %s" agent
               (pcase status
                 ('not-found "❌ 세션 없음")
                 ('pending-approval "🔐 권한 승인 대기 중!")
                 ('waiting "⏳ 입력 대기 중")
                 ('thinking "🤔 사고 중")
                 ('working "⚙️ 작업 중"))))
    status))

;;;; Permission Prompt Handling
;;
;; Claude Code 권한 프롬프트 패턴:
;;   "Allow ..."          도구 사용 허가
;;   "Proceed? [y/n]"     진행 확인
;;   "Do you want to ..." 작업 확인
;;   "[y/n]"              일반 확인
;;
;; 사용법:
;;   SPC \ t a   모든 에이전트의 대기 중인 프롬프트 표시
;;   SPC \ t y   에이전트 승인 (y 전송)
;;   SPC \ t N   에이전트 거부 (n 전송)

(defvar +claude-permission-patterns
  '("Allow"                           ; 도구 사용 허가
    "Proceed\\?"                      ; 진행 확인
    "Do you want to"                  ; 작업 확인
    "\\[y/n\\]"                       ; 일반 y/n
    "\\[Y/n\\]"                       ; 대문자 Y (기본 yes)
    "\\[yes/no\\]"                    ; 명시적 yes/no
    "approve"                         ; 승인 요청
    "continue\\?")                    ; 계속 확인
  "Claude Code 권한 프롬프트 감지 패턴 목록.")

(defun +claude--has-permission-prompt-p (output)
  "OUTPUT에 권한 프롬프트가 있는지 확인."
  (cl-some (lambda (pattern)
             (string-match-p pattern output))
           +claude-permission-patterns))

(defun +claude--extract-permission-context (output)
  "OUTPUT에서 권한 프롬프트 컨텍스트 추출."
  (let ((lines (split-string output "\n"))
        (result nil))
    (dolist (line lines)
      (when (+claude--has-permission-prompt-p line)
        (push (string-trim line) result)))
    (nreverse result)))

;;;###autoload
(defun +claude-pending-prompts (&optional agent-name)
  "AGENT-NAME의 대기 중인 권한 프롬프트 반환.
AGENT-NAME이 nil이면 모든 에이전트 확인."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Agent: " (mapcar #'car +claude-agents) nil t))))
  (let ((agents (if agent-name
                    (list (cons agent-name (+claude--get-session agent-name)))
                  +claude-agents))
        (pending '()))
    (dolist (agent agents)
      (let* ((name (car agent))
             (session (cdr agent))
             (output (+claude--capture-raw session 30))
             (prompts (+claude--extract-permission-context output)))
        (when prompts
          (push (cons name prompts) pending))))
    (if (called-interactively-p 'any)
        (if pending
            (let ((buf (get-buffer-create "*claude-pending-prompts*")))
              (with-current-buffer buf
                (erase-buffer)
                (insert "=== 대기 중인 권한 프롬프트 ===\n\n")
                (dolist (item pending)
                  (insert (format "🔐 [%s]\n" (car item)))
                  (dolist (prompt (cdr item))
                    (insert (format "   %s\n" prompt)))
                  (insert "\n"))
                (insert "───────────────────────────────\n")
                (insert "SPC \ t y  승인 (y)\n")
                (insert "SPC \ t N  거부 (n)\n")
                (goto-char (point-min)))
              (pop-to-buffer buf))
          (message "대기 중인 프롬프트 없음"))
      pending)))

;;;###autoload
(defun +claude-approve (&optional agent-name)
  "AGENT-NAME의 권한 프롬프트 승인 (y 전송)."
  (interactive
   (list (completing-read "Approve agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (status (+claude-agent-status agent)))
    (if (eq status 'pending-approval)
        (progn
          (+claude--send-keys session "y")
          (message "✅ Approved: %s" agent))
      (message "⚠️ %s: 대기 중인 프롬프트 없음 (status: %s)" agent status))))

;;;###autoload
(defun +claude-reject (&optional agent-name)
  "AGENT-NAME의 권한 프롬프트 거부 (n 전송)."
  (interactive
   (list (completing-read "Reject agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (status (+claude-agent-status agent)))
    (if (eq status 'pending-approval)
        (progn
          (+claude--send-keys session "n")
          (message "❌ Rejected: %s" agent))
      (message "⚠️ %s: 대기 중인 프롬프트 없음 (status: %s)" agent status))))

;;;###autoload
(defun +claude-approve-all ()
  "모든 에이전트의 대기 중인 프롬프트 일괄 승인."
  (interactive)
  (let ((approved 0))
    (dolist (agent +claude-agents)
      (let* ((name (car agent))
             (status (+claude-agent-status name)))
        (when (eq status 'pending-approval)
          (+claude--send-keys (cdr agent) "y")
          (cl-incf approved))))
    (message "✅ Approved %d agent(s)" approved)))

;;;###autoload
(defun +claude-show-conversation (&optional agent-name)
  "AGENT-NAME의 대화 내용을 정리하여 버퍼에 표시.
사용자 입력(>) 과 Claude 응답(●)만 추출."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (session (+claude--get-session agent))
         (output (+claude--capture-raw session 500))
         (buf (get-buffer-create (format "*claude-conversation: %s*" agent)))
         (lines (split-string output "\n"))
         (in-response nil)
         (result '()))
    ;; 대화 추출
    (dolist (line lines)
      (cond
       ;; 사용자 입력
       ((string-match "^> \\(.+\\)" line)
        (setq in-response nil)
        (push (format "👤 USER: %s\n" (match-string 1 line)) result))
       ;; Claude 응답 시작
       ((string-match "^● \\(.+\\)" line)
        (setq in-response t)
        (push (format "\n🤖 CLAUDE: %s" (match-string 1 line)) result))
       ;; 응답 계속 (들여쓰기된 줄)
       ((and in-response (string-match "^  \\(.+\\)" line))
        (push (format "%s" (match-string 1 line)) result))
       ;; 구분선 - 응답 끝
       ((string-match "^───" line)
        (when in-response
          (push "\n" result))
        (setq in-response nil))))
    ;; 버퍼에 표시
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== Claude Agent: %s ===\n\n" agent))
      (insert (mapconcat #'identity (nreverse result) "\n"))
      (goto-char (point-min))
      (org-mode))
    (pop-to-buffer buf)))

;;;; Setup emamux

(use-package! emamux
  :commands (emamux:send-command
             emamux:run-command
             emamux:run-last-command
             emamux:copy-kill-ring
             emamux:yank-from-list-buffers
             emamux:zoom-runner
             emamux:inspect-runner
             emamux:interrupt-runner
             emamux:close-runner-pane
             emamux:close-panes)
  :config
  (setq emamux:completing-read-type 'normal)
  (setq emamux:default-orientation 'vertical)
  (setq emamux:runner-pane-height 30))

;;;; Keybindings

;; SPC \ t ... (tmux-claude agents)
;; SPC \은 efrit/beads 그룹 (ai-orchestration.el)
(map! :leader
      (:prefix "\\"
               (:prefix ("t" . "tmux-agents")
                ;; 세션 관리
                :desc "Create session"      "c" #'+claude-create-session
                :desc "List sessions"       "l" #'+claude-list-sessions
                :desc "Attach (copy cmd)"   "a" #'+claude-attach-session
                :desc "Add new agent"       "+" #'+claude-add-session
                ;; Claude 에이전트
                :desc "Send to agent"       "s" #'+claude-send-to-agent
                :desc "Send region"         "r" #'+claude-send-region
                :desc "Send buffer"         "b" #'+claude-send-buffer
                :desc "Send defun"          "d" #'+claude-send-defun
                :desc "Assign issue"        "i" #'+claude-assign-issue
                :desc "Assign ready issue"  "n" #'+claude-assign-ready-issue
                :desc "Capture output"      "o" #'+claude-capture-pane
                :desc "Show conversation"   "v" #'+claude-show-conversation
                :desc "Agent status"        "?" #'+claude-agent-status
                ;; Extract
                :desc "Extract response"    "e" #'+claude-extract-last-response
                :desc "Extract input"       "E" #'+claude-extract-last-input
                ;; Permission handling
                :desc "Pending prompts"     "p" #'+claude-pending-prompts
                :desc "Approve (y)"         "y" #'+claude-approve
                :desc "Reject (n)"          "N" #'+claude-reject
                :desc "Approve all"         "Y" #'+claude-approve-all
                ;; emamux
                (:prefix ("m" . "emamux")
                 :desc "Send command"       "s" #'emamux:send-command
                 :desc "Run command"        "r" #'emamux:run-command
                 :desc "Run last command"   "l" #'emamux:run-last-command
                 :desc "Yank from tmux"     "y" #'emamux:yank-from-list-buffers
                 :desc "Copy kill-ring"     "c" #'emamux:copy-kill-ring
                 :desc "Zoom runner"        "z" #'emamux:zoom-runner
                 :desc "Inspect runner"     "i" #'emamux:inspect-runner
                 :desc "Interrupt runner"   "x" #'emamux:interrupt-runner))))

(provide 'tmux-config)
;;; tmux-config.el ends here
