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
  ;; NixOS: tdlib 경로 자동 탐지 (.drv 제외, 실제 디렉토리만)
  (setq telega-server-libs-prefix
        (car (seq-filter #'file-directory-p
                         (file-expand-wildcards "/nix/store/*-tdlib-*"))))

  :config
  ;; 봇 전용 최소 설정
  (setq telega-completing-read-function #'completing-read
        telega-use-tracking-for nil
        telega-emoji-use-images nil)

  ;; chat 버퍼
  (setq telega-chat-fill-column 80
        telega-chat-show-deleted-messages-for nil)

  ;; transient 메뉴 활성화 (magit 스타일)
  (telega-transient-keymaps-mode 1))

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
       :desc "Telega start"    "t" #'telega
       :desc "Telega chat bot" "T" #'my/telega-chat-bot))

(provide 'ai-bot-config)
;;; ai-bot-config.el ends here
