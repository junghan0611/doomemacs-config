;;; $DOOMDIR/lisp/ai-pi-agent.el --- Pi Coding Agent Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Pi Coding Agent (https://pi.dev) Emacs 통합 설정
;;
;; 특징:
;; - 터미널(vterm/comint/eat) 의존성 없음 — 순수 RPC over stdio
;; - 2-윈도우 네이티브 UI (chat + input)
;; - persp-mode workspace 기반 세션 격리
;; - tabulated-list 기반 세션 매니저 (agent-shell-manager 스타일)
;; - 멀티 프로바이더 (Claude, GPT, Gemini, 자체호스팅)
;;
;; 패키지:
;; - packages.el에서 upstream pi-coding-agent 설치 (MELPA)
;; - pi-mono lockSync retry 수정이 머지되면 fork → upstream 전환 예정
;;
;; 키바인딩 (SPC j):
;;   j — Pi 시작 (현재 프로젝트)
;;   f — Pi 세션 찾기 (completing-read, workspace 전환)
;;   l — 세션 매니저 토글
;;   q — 세션 종료

;;; Code:

;;;; pi-coding-agent 기본 설정

(use-package! pi-coding-agent
  :init (defalias 'pi 'pi-coding-agent)
  :custom
  (pi-coding-agent-input-window-height 10)
  (pi-coding-agent-tool-preview-lines 10)
  (pi-coding-agent-bash-preview-lines 5)
  (pi-coding-agent-context-warning-threshold 70)
  (pi-coding-agent-context-error-threshold 90)
  (pi-coding-agent-visit-file-other-window t)
  :config
  (add-hook 'pi-coding-agent-chat-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'pi-coding-agent-input-mode-hook #'doom-mark-buffer-as-real-h))

;;;; 유틸리티

(defun my/pi--project-name ()
  "현재 프로젝트명 반환. 프로젝트 없으면 디렉터리명."
  (file-name-nondirectory
   (directory-file-name
    (or (when-let ((proj (project-current)))
          (project-root proj))
        default-directory))))

(defun my/pi--all-chat-buffers ()
  "모든 live pi chat 버퍼 목록 반환."
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-live-p buf)
          (with-current-buffer buf
            (derived-mode-p 'pi-coding-agent-chat-mode))))
   (buffer-list)))

(defun my/pi--restore-layout (chat-buf)
  "CHAT-BUF의 Pi 2-윈도우 레이아웃 복원 (chat + input)."
  (delete-other-windows)
  (switch-to-buffer chat-buf)
  (with-current-buffer chat-buf
    (goto-char (point-max)))
  (when-let ((input-buf (and (boundp 'pi-coding-agent--input-buffer)
                             (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))))
    (when (buffer-live-p input-buf)
      (let ((input-win (split-window nil (- pi-coding-agent-input-window-height) 'below)))
        (set-window-buffer input-win input-buf)
        (select-window input-win)))))

;;;; persp-mode workspace 통합

(defun my/pi--find-workspace-for-buffer (buffer)
  "BUFFER가 속한 workspace 이름 반환. 없으면 nil.
버퍼의 default-directory 기준으로 같은 디렉터리의 버퍼를 가진 workspace 탐색."
  (when (featurep 'persp-mode)
    (let ((buffer-dir (with-current-buffer buffer default-directory)))
      (when buffer-dir
        (cl-loop for persp-name in (persp-names)
                 for persp = (persp-get-by-name persp-name)
                 when (and persp
                           (seq-find (lambda (b)
                                       (and (buffer-live-p b)
                                            (buffer-local-value 'default-directory b)
                                            (string-prefix-p buffer-dir
                                                             (buffer-local-value 'default-directory b))))
                                     (persp-buffers persp)))
                 return persp-name)))))

(defun my/pi--switch-to-session (chat-buf &optional no-workspace-switch)
  "CHAT-BUF 세션으로 전환. workspace 전환 포함.
NO-WORKSPACE-SWITCH가 non-nil이면 workspace 전환 생략."
  (unless no-workspace-switch
    (when-let ((ws (my/pi--find-workspace-for-buffer chat-buf)))
      (when (fboundp 'persp-frame-switch)
        (persp-frame-switch ws))))
  (my/pi--restore-layout chat-buf))

;;;; 세션 시작/종료/전환

(defun my/pi-start ()
  "현재 프로젝트에서 Pi 세션 시작 또는 기존 세션 표시.
pi-coding-agent의 기본 동작을 그대로 사용하며,
workspace 내에서 세션이 자연스럽게 관리됨."
  (interactive)
  (pi-coding-agent))

(defun my/pi-find ()
  "completing-read로 Pi 세션 선택 후 해당 workspace로 전환.
vertico/ivy 등 completing-read 프레임워크와 호환."
  (interactive)
  (let ((chat-bufs (my/pi--all-chat-buffers)))
    (if (null chat-bufs)
        (user-error "활성 Pi 세션 없음")
      (let* ((candidates
              (mapcar (lambda (buf)
                        (with-current-buffer buf
                          (let* ((project (file-name-nondirectory
                                           (directory-file-name default-directory)))
                                 (status (symbol-name (or pi-coding-agent--status 'unknown)))
                                 (proc pi-coding-agent--process)
                                 (alive (if (and proc (process-live-p proc)) "live" "dead")))
                            (cons (format "%-25s [%s] %s" project status alive) buf))))
                      chat-bufs))
             (selected (completing-read "Pi session: " candidates nil t))
             (chat-buf (cdr (assoc selected candidates))))
        (when chat-buf
          (my/pi--switch-to-session chat-buf))))))

(defun my/pi-quit ()
  "현재 Pi 세션 종료."
  (interactive)
  (when (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
    (pi-coding-agent-quit)))

;;;; Pi 세션 매니저

(defvar my/pi-manager-buffer-name "*Pi Sessions*"
  "Pi 세션 매니저 버퍼 이름.")

(defvar my/pi-manager-side 'bottom
  "매니저 윈도우 위치.")

(defvar my/pi-manager--refresh-timer nil
  "자동 갱신 타이머.")

(defvar my/pi-manager--global-buffer nil
  "매니저 버퍼 참조.")

(defun my/pi--status-face (status)
  "STATUS 문자열에 대한 face 반환."
  (pcase status
    ("ready" 'success)
    ("working" 'warning)
    ("error" 'error)
    (_ 'font-lock-comment-face)))

(defun my/pi--collect-sessions ()
  "모든 Pi chat 버퍼에서 세션 정보를 수집."
  (let (sessions)
    (dolist (buf (my/pi--all-chat-buffers))
      (with-current-buffer buf
        (let* ((dir (or default-directory "?"))
               (project (file-name-nondirectory (directory-file-name dir)))
               (status (symbol-name (or pi-coding-agent--status 'unknown)))
               (state pi-coding-agent--state)
               (model-obj (plist-get state :model))
               (model-name (cond
                            ((stringp model-obj) model-obj)
                            ((plist-get model-obj :name))
                            (t "—")))
               (model-short (if (string-empty-p model-name) "—"
                              (pi-coding-agent--shorten-model-name model-name)))
               (last-usage pi-coding-agent--last-usage)
               (context-tokens (when last-usage
                                 (+ (or (plist-get last-usage :input) 0)
                                    (or (plist-get last-usage :output) 0)
                                    (or (plist-get last-usage :cacheRead) 0)
                                    (or (plist-get last-usage :cacheWrite) 0))))
               (context-window (or (plist-get model-obj :contextWindow) 0))
               (context-pct (if (and context-tokens (> context-window 0))
                                (format "%.0f%%" (* (/ (float context-tokens) context-window) 100))
                              "—"))
               (proc pi-coding-agent--process)
               (alive (if (and proc (process-live-p proc)) "live" "dead")))
          (push (list buf (vector project model-short
                                 (propertize status 'face (my/pi--status-face status))
                                 context-pct alive))
                sessions))))
    (nreverse sessions)))

(defvar my/pi-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'my/pi-manager-goto-session)
    (define-key map (kbd "S-<return>") #'my/pi-manager-goto-no-switch)
    (define-key map (kbd "k") #'my/pi-manager-kill-session)
    (define-key map (kbd "c") #'my/pi-manager-create-session)
    (define-key map (kbd "g") #'my/pi-manager-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Pi 세션 매니저 키맵.")

(define-derived-mode my/pi-manager-mode tabulated-list-mode "Pi-Manager"
  "Pi 세션 관리 모드.
\\[my/pi-manager-goto-session] - 세션으로 전환 (workspace 전환 포함)
\\[my/pi-manager-goto-no-switch] - 세션으로 전환 (workspace 전환 없이)
\\[my/pi-manager-kill-session] - 세션 종료
\\[my/pi-manager-create-session] - 새 세션
\\[my/pi-manager-refresh] - 새로고침"
  (setq tabulated-list-format
        [("Project" 25 t)
         ("Model" 20 t)
         ("Status" 12 t)
         ("Context" 10 t)
         ("Process" 8 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'my/pi--collect-sessions)
  (tabulated-list-init-header)
  ;; evil 키바인딩
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal my/pi-manager-mode-map
      (kbd "RET") #'my/pi-manager-goto-session
      (kbd "S-<return>") #'my/pi-manager-goto-no-switch
      (kbd "k") #'my/pi-manager-kill-session
      (kbd "c") #'my/pi-manager-create-session
      (kbd "g") #'my/pi-manager-refresh
      (kbd "q") #'quit-window))
  ;; 자동 갱신 타이머
  (when my/pi-manager--refresh-timer
    (cancel-timer my/pi-manager--refresh-timer))
  (setq my/pi-manager--refresh-timer
        (run-with-timer 2 2
                        (lambda ()
                          (when (and my/pi-manager--global-buffer
                                     (buffer-live-p my/pi-manager--global-buffer)
                                     (get-buffer-window my/pi-manager--global-buffer))
                            (with-current-buffer my/pi-manager--global-buffer
                              (my/pi-manager-refresh))))))
  (add-hook 'kill-buffer-hook
            (lambda ()
              (when my/pi-manager--refresh-timer
                (cancel-timer my/pi-manager--refresh-timer)
                (setq my/pi-manager--refresh-timer nil)))
            nil t))

(defun my/pi-manager-refresh ()
  "매니저 버퍼 데이터 갱신."
  (interactive)
  (when (and my/pi-manager--global-buffer
             (buffer-live-p my/pi-manager--global-buffer))
    (with-current-buffer my/pi-manager--global-buffer
      (when (derived-mode-p 'my/pi-manager-mode)
        (setq tabulated-list-entries (my/pi--collect-sessions))
        (tabulated-list-print t)))))

(defun my/pi-manager-goto-session (&optional no-workspace-switch)
  "선택한 세션의 workspace로 전환하고 레이아웃 복원.
NO-WORKSPACE-SWITCH가 non-nil이면 workspace 전환 생략."
  (interactive)
  (when-let ((chat-buf (tabulated-list-get-id)))
    (when (buffer-live-p chat-buf)
      (delete-window (get-buffer-window my/pi-manager-buffer-name))
      (my/pi--switch-to-session chat-buf no-workspace-switch))))

(defun my/pi-manager-goto-no-switch ()
  "선택한 세션으로 전환 (workspace 전환 없이)."
  (interactive)
  (my/pi-manager-goto-session t))

(defun my/pi-manager-kill-session ()
  "선택한 세션 종료."
  (interactive)
  (when-let ((chat-buf (tabulated-list-get-id)))
    (when (and (buffer-live-p chat-buf)
               (yes-or-no-p (format "Kill Pi session in %s?" (buffer-name chat-buf))))
      (with-current-buffer chat-buf
        (when-let ((input (and (boundp 'pi-coding-agent--input-buffer)
                               pi-coding-agent--input-buffer)))
          (when (buffer-live-p input) (kill-buffer input)))
        (kill-buffer chat-buf))
      (my/pi-manager-refresh))))

(defun my/pi-manager-create-session ()
  "새 Pi 세션 생성."
  (interactive)
  (quit-window)
  (call-interactively #'my/pi-start))

(defun my/pi-manager-toggle ()
  "Pi 세션 매니저 토글."
  (interactive)
  (let* ((buffer (get-buffer-create my/pi-manager-buffer-name))
         (window (get-buffer-window buffer)))
    (if (and window (window-live-p window))
        (delete-window window)
      (setq my/pi-manager--global-buffer buffer)
      (with-current-buffer buffer
        (unless (derived-mode-p 'my/pi-manager-mode)
          (my/pi-manager-mode))
        (my/pi-manager-refresh))
      (let ((win (display-buffer-in-side-window buffer
                   `((side . ,my/pi-manager-side)
                     (window-height . 10)
                     (window-parameters . ((no-delete-other-windows . t)))))))
        (set-window-dedicated-p win t)
        (select-window win)))))

;;;; 키바인딩

(map! :leader
      (:prefix ("j" . "pi-agent")
       :desc "Pi start"           "j" #'my/pi-start
       :desc "Pi find session"    "f" #'my/pi-find
       :desc "Pi session manager" "l" #'my/pi-manager-toggle
       :desc "Pi quit session"    "q" #'my/pi-quit))

;;; Provide

(provide 'ai-pi-agent)

;;; ai-pi-agent.el ends here
