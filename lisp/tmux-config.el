;;; $DOOMDIR/lisp/tmux-config.el --- Tmux + Claude Code Orchestration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Claude Code 멀티에이전트 오케스트레이션
;;
;; ┌─────────────────────────────────────────────────────────────────┐
;; │ Phase 1: tmux 기반 (현재)                                       │
;; │   - Doom +tmux 함수 + emamux 패키지                             │
;; │   - 에이전트 레지스트리로 타겟팅                                │
;; │   - 단방향: Emacs → tmux pane (send-keys)                       │
;; │                                                                  │
;; │ Phase 2: Zellij 마이그레이션 (예정)                             │
;; │   - zellij pipe + Plugin API (양방향)                           │
;; │   - Orchestrator 서버                                           │
;; └─────────────────────────────────────────────────────────────────┘
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 사용법
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 1. tmux 세션 생성:
;;    $ tmux new-session -s agents
;;
;; 2. pane 분할 (예: 4개):
;;    Ctrl-b %  (수직) 또는 Ctrl-b " (수평)
;;
;; 3. 각 pane에서 claude 실행:
;;    $ claude
;;
;; 4. Emacs에서 에이전트에 메시지 전송:
;;    SPC 3 t s  →  에이전트 선택 → 메시지 입력
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; tmux target 형식
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 형식: session:window.pane
;;
;; 예시:
;;   agents:0.0  → 세션 "agents", 윈도우 0, pane 0
;;   0:1.1       → 세션 "0", 윈도우 1, pane 1
;;
;; 확인 명령:
;;   $ tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}'
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Doom +tmux 함수 (tools/tmux 모듈)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 핵심 함수:
;;   (+tmux COMMAND &rest ARGS)     tmux 명령 실행
;;   (+tmux/run COMMAND)            focused pane에 명령 전송
;;   (+tmux/send-region BEG END)    선택 영역 전송
;;   (+tmux/rerun)                  마지막 명령 재실행
;;   (+tmux/cd DIRECTORY)           pane의 pwd 변경
;;   (+tmux/cd-to-here)             현재 버퍼 디렉토리로 cd
;;   (+tmux/cd-to-project)          프로젝트 루트로 cd
;;
;; 조회 함수:
;;   (+tmux-list-sessions)          세션 목록
;;   (+tmux-list-windows SESSION)   윈도우 목록
;;   (+tmux-list-panes WINDOW)      pane 목록
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; emamux 패키지 함수
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 명령 전송:
;;   (emamux:send-command)          특정 target에 명령 전송 (C-u로 target 변경)
;;   (emamux:run-command)           runner pane에서 명령 실행
;;   (emamux:run-last-command)      마지막 명령 재실행
;;
;; 클립보드:
;;   (emamux:copy-kill-ring)        Emacs kill-ring → tmux 버퍼
;;   (emamux:yank-from-list-buffers) tmux 버퍼 → Emacs
;;
;; Runner pane 관리:
;;   (emamux:zoom-runner)           runner pane 확대
;;   (emamux:inspect-runner)        runner pane 진입 (copy mode)
;;   (emamux:interrupt-runner)      실행 중인 명령 중단 (C-c)
;;   (emamux:close-runner-pane)     runner pane 닫기
;;   (emamux:close-panes)           모든 다른 pane 닫기
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 키바인딩 (SPC 3 t ...)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; Claude 에이전트:
;;   SPC 3 t s   Send to agent      에이전트에 텍스트 전송
;;   SPC 3 t r   Send region        선택 영역 전송
;;   SPC 3 t b   Send buffer        버퍼 전체 전송
;;   SPC 3 t d   Send defun         현재 함수 전송
;;   SPC 3 t i   Assign issue       bd 이슈 할당
;;   SPC 3 t n   Assign next        다음 ready 이슈 할당
;;   SPC 3 t l   List panes         pane 목록
;;   SPC 3 t f   Focus agent        에이전트로 포커스
;;   SPC 3 t c   Capture pane       pane 출력 캡처 (raw)
;;   SPC 3 t p   Select pane        pane 선택 (completing-read)
;;   SPC 3 t v   Show conversation  대화 내용 정리 표시
;;   SPC 3 t ?   Agent status       에이전트 상태 확인
;;   SPC 3 t e   Extract response   마지막 Claude 응답 추출
;;   SPC 3 t E   Extract input      마지막 사용자 입력 추출
;;
;; 권한 프롬프트 처리:
;;   SPC 3 t a   Pending prompts    대기 중인 프롬프트 표시
;;   SPC 3 t y   Approve            에이전트 승인 (y 전송)
;;   SPC 3 t N   Reject             에이전트 거부 (n 전송)
;;   SPC 3 t Y   Approve all        모든 에이전트 일괄 승인
;;
;; emamux:
;;   SPC 3 t m s   emamux:send-command
;;   SPC 3 t m r   emamux:run-command
;;   SPC 3 t m l   emamux:run-last-command
;;   SPC 3 t m y   emamux:yank-from-list-buffers
;;   SPC 3 t m c   emamux:copy-kill-ring
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 한계 (Phase 2에서 해결 예정)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; - 단방향 통신만 가능 (Emacs → tmux)
;; - pane 출력 실시간 수신 불가 (capture-pane은 스냅샷)
;; - 에이전트 상태 모니터링 수동
;;
;; Zellij로 마이그레이션 시:
;; - zellij pipe로 양방향 통신
;; - Plugin API로 pane 출력 스트리밍
;; - Orchestrator 서버로 에이전트 상태 관리

;;; Code:

;;;; Variables

(defvar +claude-agents
  '(("pm"    . "agents:0.0")   ; PM 에이전트
    ("code"  . "agents:0.1")   ; 코딩 에이전트
    ("test"  . "agents:0.2")   ; 테스트 에이전트
    ("debug" . "agents:0.3"))  ; 디버그 에이전트
  "Claude Code 에이전트 레지스트리.
각 에이전트는 (NAME . TMUX-TARGET) 형식.
TMUX-TARGET은 'session:window.pane' 형식.

현재 세션에 맞게 수정:
  (setq +claude-agents
        \\='((\"pm\" . \"0:1.0\")
          (\"code\" . \"0:1.1\")))")

(defvar +claude-default-agent "pm"
  "기본 에이전트 이름.")

(defvar +claude-session-name "agents"
  "Claude Code 에이전트용 tmux 세션 이름.")

;;;; Core Functions

(defun +claude--get-target (agent-name)
  "AGENT-NAME에 해당하는 tmux target 반환."
  (or (cdr (assoc agent-name +claude-agents))
      (error "Unknown agent: %s" agent-name)))

(defun +claude--send-keys (target text &optional no-enter)
  "TARGET pane에 TEXT 전송. NO-ENTER가 nil이면 Enter도 전송."
  (let ((cmd (format "send-keys -t %s %s %s"
                     (shell-quote-argument target)
                     (shell-quote-argument text)
                     (if no-enter "" "Enter"))))
    (+tmux cmd)))

;;;; Interactive Commands - Send

;;;###autoload
(defun +claude-send-to-agent (agent-name text)
  "AGENT-NAME에게 TEXT 전송."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)
         (read-string "Message: ")))
  (let ((target (+claude--get-target agent-name)))
    (+claude--send-keys target text)
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
         (target (+claude--get-target agent)))
    (+claude--send-keys target text)
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
(defun +claude-list-panes ()
  "모든 tmux pane 목록을 미니버퍼에 표시."
  (interactive)
  (let ((output (shell-command-to-string
                 "tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index} - #{pane_current_command}'")))
    (message "Panes:\n%s" output)))

;;;###autoload
(defun +claude-focus-agent (agent-name)
  "AGENT-NAME의 pane으로 포커스 이동."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t)))
  (let ((target (+claude--get-target agent-name)))
    (+tmux (format "select-pane -t %s" target))
    (+tmux (format "select-window -t %s" target))
    (message "Focused on %s" agent-name)))

;;;###autoload
(defun +claude-capture-pane (&optional agent-name)
  "AGENT-NAME pane의 출력을 캡처하여 새 버퍼에 표시.
tmux capture-pane 사용 (스냅샷, 실시간 아님)."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (target (+claude--get-target agent))
         (output (shell-command-to-string
                  (format "tmux capture-pane -t %s -p" target)))
         (buf (get-buffer-create (format "*tmux-capture: %s*" agent))))
    (with-current-buffer buf
      (erase-buffer)
      (insert output)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Captured %s pane output" agent)))

;;;###autoload
(defun +claude-select-pane ()
  "completing-read로 pane 선택 후 에이전트로 등록."
  (interactive)
  (let* ((panes (split-string
                 (shell-command-to-string
                  "tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}'")
                 "\n" t))
         (selected (completing-read "Select pane: " panes nil t))
         (name (read-string "Agent name: ")))
    (add-to-list '+claude-agents (cons name selected))
    (message "Added agent '%s' -> %s" name selected)))

;;;; Claude Code Output Parsing
;;
;; Claude Code 출력 패턴:
;;   > 사용자 입력          (사용자 프롬프트)
;;   ∴ Thinking…           (사고 중)
;;   ● Claude 응답         (응답 시작)
;;   ───────────           (구분선)
;;   -- INSERT --          (입력 대기)

(defun +claude--capture-raw (target &optional scrollback)
  "TARGET pane의 원시 출력 캡처. SCROLLBACK은 히스토리 줄 수."
  (let ((scroll-opt (if scrollback (format "-S -%d" scrollback) "-S -200")))
    (shell-command-to-string
     (format "tmux capture-pane -t %s %s -p" target scroll-opt))))

;;;###autoload
(defun +claude-extract-last-response (&optional agent-name)
  "AGENT-NAME의 마지막 Claude 응답(● 로 시작) 추출."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (target (+claude--get-target agent))
         (output (+claude--capture-raw target 200))
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
         (target (+claude--get-target agent))
         (output (+claude--capture-raw target 200))
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
  'working          - 작업 중"
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (target (+claude--get-target agent))
         (output (+claude--capture-raw target 50))
         (status (cond
                  ;; 권한 프롬프트 패턴 (가장 먼저 체크)
                  ((+claude--has-permission-prompt-p output) 'pending-approval)
                  ((string-match "-- INSERT --" output) 'waiting)
                  ((string-match "∴ Thinking" output) 'thinking)
                  (t 'working))))
    (when (called-interactively-p 'any)
      (message "Agent %s: %s" agent
               (pcase status
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
;;   SPC 3 t a   모든 에이전트의 대기 중인 프롬프트 표시
;;   SPC 3 t y   에이전트 승인 (y 전송)
;;   SPC 3 t N   에이전트 거부 (n 전송)

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
                    (list (cons agent-name (+claude--get-target agent-name)))
                  +claude-agents))
        (pending '()))
    (dolist (agent agents)
      (let* ((name (car agent))
             (target (cdr agent))
             (output (+claude--capture-raw target 30))
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
                (insert "SPC 3 t y  승인 (y)\n")
                (insert "SPC 3 t N  거부 (n)\n")
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
         (target (+claude--get-target agent))
         (status (+claude-agent-status agent)))
    (if (eq status 'pending-approval)
        (progn
          (+claude--send-keys target "y")
          (message "✅ Approved: %s" agent))
      (message "⚠️ %s: 대기 중인 프롬프트 없음 (status: %s)" agent status))))

;;;###autoload
(defun +claude-reject (&optional agent-name)
  "AGENT-NAME의 권한 프롬프트 거부 (n 전송)."
  (interactive
   (list (completing-read "Reject agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         (target (+claude--get-target agent))
         (status (+claude-agent-status agent)))
    (if (eq status 'pending-approval)
        (progn
          (+claude--send-keys target "n")
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
         (target (+claude--get-target agent))
         (output (+claude--capture-raw target 500))
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

;; SPC 3 t ... (tmux-claude agents)
;; SPC 3은 efrit/beads 그룹 (ai-orchestration.el)
(map! :leader
      (:prefix "3"
       (:prefix ("t" . "tmux-agents")
        ;; Claude 에이전트
        :desc "Send to agent"       "s" #'+claude-send-to-agent
        :desc "Send region"         "r" #'+claude-send-region
        :desc "Send buffer"         "b" #'+claude-send-buffer
        :desc "Send defun"          "d" #'+claude-send-defun
        :desc "Assign issue"        "i" #'+claude-assign-issue
        :desc "Assign ready issue"  "n" #'+claude-assign-ready-issue
        :desc "List panes"          "l" #'+claude-list-panes
        :desc "Focus agent"         "f" #'+claude-focus-agent
        :desc "Capture pane"        "c" #'+claude-capture-pane
        :desc "Select pane"         "p" #'+claude-select-pane
        :desc "Show conversation"   "v" #'+claude-show-conversation
        :desc "Agent status"        "?" #'+claude-agent-status
        ;; Extract
        :desc "Extract response"    "e" #'+claude-extract-last-response
        :desc "Extract input"       "E" #'+claude-extract-last-input
        ;; Permission handling
        :desc "Pending prompts"     "a" #'+claude-pending-prompts
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
