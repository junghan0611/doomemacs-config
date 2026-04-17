;;; $DOOMDIR/lisp/ai-bot-config.el --- AI Bot Communication (Telegram) -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Telegram 봇과의 대화를 위한 telega.el 최소 설정.
;; 사람과의 채팅이 아닌, AI 에이전트(봇)와의 소통 매체.
;;
;; 봇:
;;   @junghan_openclaw_bot (아이온스클럽B) - 개발 리포 전체 인지
;;   @glg_junghanacs_bot (힣봋) - 디지털 분신
;;
;; 키바인딩 (SPC j):
;;   t — telega 시작
;;   T — 봇 선택 후 바로 채팅

;;; Code:

;;;; telega 기본 설정

(use-package! telega
  :commands (telega)
  :init
  ;; NixOS: tdlib 경로 자동 탐지 — 버전순 정렬 후 최신 선택
  (setq telega-server-libs-prefix
        (car (last (sort (seq-filter #'file-directory-p
                                     (file-expand-wildcards "/nix/store/*-tdlib-*"))
                         #'string<))))

  ;; Emoji OFF — defcustom 평가 전 pre-bind.
  ;; Why: telega-customize.el의 telega-emoji-font-family defcustom이 초기화 시
  ;; (font-family-list)로 "Noto Color Emoji"를 적극 선택한다. 이미지는 꺼져있지만
  ;; 이 조회 과정에서 시스템 fontconfig가 컬러 이모지 폰트를 Emacs fontset에 캐싱하여,
  ;; GUI Emacs 전체 이모지 렌더링이 컬러↔흑백으로 들쭉날쭉해진다.
  ;; 변수를 미리 bind해두면 defcustom이 기본값 람다를 평가하지 않는다.
  (setq telega-emoji-font-family nil
        telega-emoji-use-images nil
        telega-emoji-large-height nil)

  :config
  ;; 봇 전용 최소 설정
  (setq telega-completing-read-function #'completing-read
        telega-use-tracking-for nil
        telega-emoji-use-images nil)

  ;; chat 버퍼
  (setq telega-chat-fill-column 80
        telega-chat-show-deleted-messages-for nil)

  ;; transient 메뉴 활성화 (magit 스타일)
  (telega-transient-keymaps-mode 1)

  ;; D-Bus 알림 → dunst 연동 (봇 채팅 포함)
  ;; 기본 telega-notifications-msg-notify-p는 (type private secret)만 허용하여
  ;; bot 타입 채팅의 알림이 억제됨. bot 타입을 추가한 커스텀 predicate 사용.
  (defun my/telega-notifications-msg-notify-p (msg)
    "봇 채팅 알림을 포함하는 알림 predicate."
    (let ((chat (telega-msg-chat msg)))
      (unless (or (not (telega-chat-match-p chat
                         '(or (type private secret bot) me-is-member)))
                  (and (telega-chat-muted-p chat)
                       (or (telega-chat-notification-setting
                            chat :disable_mention_notifications)
                           (not (plist-get msg :contains_unread_mention))))
                  (telega-msg-seen-p msg chat)
                  (with-telega-chatbuf chat
                    (and (telega-chatbuf--msg-observable-p msg)
                         (not (telega-chatbuf--history-state-get :newer-freezed)))))
        t)))
  (setq telega-notifications-msg-temex
        '(call my/telega-notifications-msg-notify-p))
  (telega-notifications-mode 1)

  ;; Doom workspace(persp-mode) + consult-buffer에서 보이도록 real buffer 등록
  (add-hook 'telega-root-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'telega-chat-mode-hook #'doom-mark-buffer-as-real-h)

  ;; WORKAROUND: telega 이벤트 핸들러에서 setTdlibParameters 전송 실패 시
  ;; WaitTdlibParameters 상태에서 벗어나지 못하는 버그 우회
  ;; TODO: telega upstream에서 수정되면 제거
  (defun my/telega-fix-auth ()
    "WaitTdlibParameters 상태에서 멈춤면 수동으로 setTdlibParameters 재전송."
    (interactive)
    (if (and (fboundp 'telega-server-live-p)
             (telega-server-live-p)
             (string= telega--auth-state "WaitTdlibParameters"))
        (progn
          (telega--setTdlibParameters)
          (message "telega: setTdlibParameters 재전송 완료"))
      (message "telega: 필요 없음 (상태: %s)" (or telega--auth-state "nil")))))

;;;; 봇 바로가기

(defvar my/telega-bots
  '(("아이온스클럽B (OpenClaw)" . "junghan_openclaw_bot")
    ("힣봋 (GLG)" . "glg_junghanacs_bot"))
  "자주 사용하는 Telegram 봇 목록. (표시이름 . username)")

(defun my/telega-chat-bot ()
  "봇 선택 후 바로 채팅 버퍼 열기.
telega가 실행 중이 아니면 먼저 시작한다."
  (interactive)
  (unless (telega-server-live-p)
    (telega)
    (while (not (telega-server-live-p))
      (sit-for 0.5)))
  (let* ((choices (mapcar #'car my/telega-bots))
         (selected (completing-read "Bot: " choices nil t))
         (username (cdr (assoc selected my/telega-bots))))
    (telega-chat-with username)))

;;;; Evil 키바인딩 — telega transient (normal state)

(map! :after telega
      ;; Root 버퍼 (채팅 목록)
      :map telega-root-mode-map
      :n "\\" #'telega-transient--prefix-telega-sort-map
      :n "/"  #'telega-transient--prefix-telega-filter-map
      :n "?"  #'telega-transient--prefix-telega-describe-map
      :n "F"  #'telega-transient--prefix-telega-folder-map
      :n "v"  #'telega-transient--prefix-telega-root-view-map
      :n "M-g f" #'telega-transient--prefix-telega-root-fastnav-map

      ;; Chat 버퍼 (대화창)
      :map telega-chat-mode-map
      :n "M-g f" #'telega-transient--prefix-telega-chatbuf-fastnav-map
      ;; M-j/M-k: 메시지 단위 이동 (j/k는 라인, M-j/M-k는 대화 단위)
      :n "M-j" #'telega-button-forward
      :n "M-k" #'telega-button-backward
      :n "M-n" #'telega-button-forward
      :n "M-p" #'telega-button-backward
      )

;;;; 키바인딩 (SPC j 확장)

(map! :leader
      (:prefix ("j" . "pi-agent")
       :desc "telega fix auth" "M-t" #'my/telega-fix-auth
       :desc "Telega start"    "t" #'telega
       :desc "Telega chat bot" "T" #'my/telega-chat-bot))

(provide 'ai-bot-config)
;;; ai-bot-config.el ends here
