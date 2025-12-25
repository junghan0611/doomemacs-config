;;; $DOOMDIR/lisp/tmux-config.el --- Tmux + Claude Code Orchestration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Claude Code 멀티에이전트 오케스트레이션
;;
;; Phase 1: tmux 기반 (현재)
;; - Doom +tmux 함수 + emamux 패키지
;; - 에이전트 레지스트리로 타겟팅
;;
;; Phase 2: Zellij 마이그레이션 (예정)
;; - zellij pipe + Plugin API
;; - Orchestrator 서버
;;
;; 사용법:
;; 1. tmux 세션 생성: tmux new-session -s agents
;; 2. pane 분할하여 각각 claude 실행
;; 3. SPC 3 t s 로 에이전트에 텍스트 전송

;;; Code:

;;;; Variables

(defvar +claude-agents
  '(("pm"    . "agents:0.0")   ; PM 에이전트
    ("code"  . "agents:0.1")   ; 코딩 에이전트
    ("test"  . "agents:0.2")   ; 테스트 에이전트
    ("debug" . "agents:0.3"))  ; 디버그 에이전트
  "Claude Code 에이전트 레지스트리.
각 에이전트는 (NAME . TMUX-TARGET) 형식.
TMUX-TARGET은 'session:window.pane' 형식.")

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

;;;; Interactive Commands

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
    (message "Region sent to %s" agent)))

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

;;;; bd Integration

;;;###autoload
(defun +claude-assign-issue (issue-id &optional agent-name)
  "BD ISSUE-ID를 AGENT-NAME에게 할당."
  (interactive
   (list (read-string "Issue ID: ")
         (completing-read "Agent: " (mapcar #'car +claude-agents) nil t nil nil +claude-default-agent)))
  (let* ((agent (or agent-name +claude-default-agent))
         ;; bd show로 이슈 정보 가져오기
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
          ;; 이슈 상태 업데이트
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

;;;; Session Management

;;;###autoload
(defun +claude-list-panes ()
  "현재 agents 세션의 pane 목록 표시."
  (interactive)
  (let ((output (+tmux (format "list-panes -t %s -F '#{pane_index}: #{pane_current_command}'"
                               +claude-session-name))))
    (message "Panes in %s:\n%s" +claude-session-name output)))

;;;###autoload
(defun +claude-focus-agent (agent-name)
  "AGENT-NAME의 pane으로 포커스 이동."
  (interactive
   (list (completing-read "Agent: " (mapcar #'car +claude-agents) nil t)))
  (let ((target (+claude--get-target agent-name)))
    (+tmux (format "select-pane -t %s" target))
    (+tmux (format "select-window -t %s" target))
    (message "Focused on %s" agent-name)))

;;;; Setup emamux

(use-package! emamux
  :commands (emamux:send-command
             emamux:run-command
             emamux:run-last-command
             emamux:copy-kill-ring
             emamux:yank-from-list-buffers)
  :config
  (setq emamux:completing-read-type 'normal)
  (setq emamux:default-orientation 'vertical)
  (setq emamux:runner-pane-height 30))

;;;; Keybindings

;; SPC 3 t ... (tmux-claude agents)
;; SPC 3은 efrit/beads 그룹 (ai-orchestration.el)
(map! :leader
      (:prefix "3"  ; efrit/beads 그룹에 추가
       (:prefix ("t" . "tmux-agents")
        :desc "Send to agent"       "s" #'+claude-send-to-agent
        :desc "Send region"         "r" #'+claude-send-region
        :desc "Send buffer"         "b" #'+claude-send-buffer
        :desc "Send defun"          "d" #'+claude-send-defun
        :desc "Assign issue"        "i" #'+claude-assign-issue
        :desc "Assign ready issue"  "n" #'+claude-assign-ready-issue
        :desc "List panes"          "l" #'+claude-list-panes
        :desc "Focus agent"         "f" #'+claude-focus-agent)))

(provide 'tmux-config)
;;; tmux-config.el ends here
