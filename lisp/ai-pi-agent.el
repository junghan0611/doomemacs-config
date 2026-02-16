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
;; - tab-bar 기반 세션 격리 (탭 = 세션)
;; - tabulated-list 기반 세션 매니저
;; - 멀티 프로바이더 (Claude, GPT, Gemini, 자체호스팅)
;;
;; 키바인딩 (SPC j):
;;   j — Pi 시작/전환
;;   l — 세션 매니저 토글
;;   q — 세션 종료
;;
;; 고려사항:
;; - Doom Emacs persp-mode 워크스페이스와 공존
;; - doom-mark-buffer-as-real-h로 버퍼 목록에 표시
;; - Phase 2: org-mode → worktree → pi 자동화 (ahyatt 스타일)

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
  ;; Pi 버퍼를 Doom 실제 버퍼로 표시 (버퍼 목록에서 보이게)
  (add-hook 'pi-coding-agent-chat-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'pi-coding-agent-input-mode-hook #'doom-mark-buffer-as-real-h))

;;;; 탭 기반 세션 관리

(defun my/pi--project-name ()
  "현재 프로젝트명 반환. 프로젝트 없으면 디렉터리명."
  (file-name-nondirectory
   (directory-file-name
    (or (when-let ((proj (project-current)))
          (project-root proj))
        default-directory))))

(defun my/pi--tab-name (&optional project)
  "Pi 탭 이름 생성. PROJECT 미지정 시 현재 프로젝트 사용."
  (format "Pi:%s" (or project (my/pi--project-name))))

(defun my/pi--find-tab (tab-name)
  "TAB-NAME과 일치하는 탭 반환. 없으면 nil."
  (seq-find (lambda (tab)
              (string= (alist-get 'name tab) tab-name))
            (funcall tab-bar-tabs-function)))

(defun my/pi-start ()
  "Pi 세션 시작 또는 기존 세션으로 전환.
현재 프로젝트 기반으로 tab-bar 탭을 생성하고 pi-coding-agent를 실행.
이미 세션이 존재하면 해당 탭으로 전환."
  (interactive)
  (let* ((project (my/pi--project-name))
         (tab-name (my/pi--tab-name project))
         (existing-tab (my/pi--find-tab tab-name)))
    (if existing-tab
        (tab-bar-select-tab-by-name tab-name)
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (pi-coding-agent))))

(defun my/pi-quit ()
  "현재 Pi 세션 종료 및 탭 정리."
  (interactive)
  (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
    (when (string-prefix-p "Pi:" tab-name)
      (when (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
        (pi-coding-agent-quit))
      (tab-bar-close-tab))))

;;;; Pi 세션 매니저

(defvar my/pi-manager-buffer-name "*Pi Sessions*"
  "Pi 세션 매니저 버퍼 이름.")

(defvar my/pi-manager-side 'bottom
  "매니저 윈도우 위치.")


(defun my/pi--collect-sessions ()
  "모든 Pi chat 버퍼에서 세션 정보를 수집."
  (let (sessions)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (with-current-buffer buf
                   (derived-mode-p 'pi-coding-agent-chat-mode)))
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
            (push (list buf (vector project model-short status context-pct alive))
                  sessions)))))
    (nreverse sessions)))

(defvar my/pi-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/pi-manager-goto-session)
    (define-key map (kbd "k") #'my/pi-manager-kill-session)
    (define-key map (kbd "c") #'my/pi-manager-create-session)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Pi 세션 매니저 키맵.")

(define-derived-mode my/pi-manager-mode tabulated-list-mode "Pi-Manager"
  "Pi 세션 관리 모드."
  (setq tabulated-list-format
        [("Project" 25 t)
         ("Model" 20 t)
         ("Status" 12 t)
         ("Context" 10 t)
         ("Process" 8 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'my/pi--collect-sessions)
  (tabulated-list-init-header))

(defun my/pi-manager-refresh ()
  "매니저 버퍼 데이터 갱신."
  (when-let ((buf (get-buffer my/pi-manager-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (revert-buffer)))))

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

(defun my/pi-manager-goto-session ()
  "선택한 세션의 탭으로 전환하고 레이아웃 복원."
  (interactive)
  (when-let ((entry (tabulated-list-get-entry))
             (chat-buf (tabulated-list-get-id)))
    (let* ((project (aref entry 0))
           (tab-name (my/pi--tab-name project)))
      (delete-window (get-buffer-window my/pi-manager-buffer-name))
      (if (my/pi--find-tab tab-name)
          (progn
            (tab-bar-select-tab-by-name tab-name)
            (my/pi--restore-layout chat-buf))
        (my/pi--restore-layout chat-buf)))))

(defun my/pi-manager-kill-session ()
  "선택한 세션 종료."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (when (and (buffer-live-p id)
               (yes-or-no-p (format "Kill Pi session in %s?" (buffer-name id))))
      (let* ((project (with-current-buffer id
                        (file-name-nondirectory (directory-file-name default-directory))))
             (tab-name (my/pi--tab-name project)))
        (with-current-buffer id
          (when-let ((input (and (boundp 'pi-coding-agent--input-buffer)
                                 pi-coding-agent--input-buffer)))
            (when (buffer-live-p input) (kill-buffer input)))
          (kill-buffer id))
        (when (my/pi--find-tab tab-name)
          (tab-bar-close-tab-by-name tab-name)))
      (my/pi-manager-refresh))))

(defun my/pi-manager-create-session ()
  "새 Pi 세션 생성."
  (interactive)
  (quit-window)
  (call-interactively #'my/pi-start))

(defun my/pi-manager-toggle ()
  "Pi 세션 매니저 토글."
  (interactive)
  (if-let ((win (get-buffer-window my/pi-manager-buffer-name)))
      (delete-window win)
    (let ((buf (get-buffer-create my/pi-manager-buffer-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'my/pi-manager-mode)
          (my/pi-manager-mode))
        (my/pi-manager-refresh))
      (display-buffer-in-side-window buf
        `((side . ,my/pi-manager-side)
          (window-height . 10))))))

;;;; 키바인딩

(map! :leader
      (:prefix ("j" . "pi-agent")
       :desc "Pi start/switch"    "j" #'my/pi-start
       :desc "Pi session manager" "l" #'my/pi-manager-toggle
       :desc "Pi quit session"    "q" #'my/pi-quit))

;;; Provide

(provide 'ai-pi-agent)

;;; ai-pi-agent.el ends here
