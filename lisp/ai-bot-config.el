;;; $DOOMDIR/lisp/ai-bot-config.el --- AI Bot Communication (Telegram) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

;; Pure TDLib richMessage → markdown serializer (vanilla, ERT-gated).  The
;; messageRichMessage insert/advice glue below feeds its :blocks here.
(require 'telega-rich-md)

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

  ;; 메시지 본문 속 smart punctuation/bullet을 ASCII로 표시 치환.
  ;; Why: "• " " ' ' — – …" 같은 문자가 CJK 폰트에서 2-cell로 잡혀
  ;; 메시지 정렬이 깨진다. 원문은 손대지 않고 display-table로 보여주기만 바꾼다.
  (defun my/telega-chat-display-table-setup ()
    "Telega chat 버퍼에서 cell drift 유발 문자를 ASCII로 치환 표시."
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (dolist (pair '((?\u2022 . "+")    ; • bullet
                    (?\u201C . "\"")   ; " left double quote
                    (?\u201D . "\"")   ; " right double quote
                    (?\u2018 . "'")    ; ' left single quote
                    (?\u2019 . "'")    ; ' right single quote
                    (?\u2014 . "--")   ; — em dash
                    (?\u2013 . "-")    ; – en dash
                    (?\u2026 . "...")  ; … horizontal ellipsis
                    (?\u00B7 . ".")))  ; · middle dot
      (aset buffer-display-table (car pair)
            (vconcat (mapcar (lambda (c) (make-glyph-code c)) (cdr pair))))))
  (add-hook 'telega-chat-mode-hook #'my/telega-chat-display-table-setup)
  (add-hook 'telega-root-mode-hook #'my/telega-chat-display-table-setup)

  ;; Unicode Cell Drift 회피 — telega 심볼을 ASCII 로 대체.
  ;;
  ;; Why: telega 기본 심볼(📎📷🎶📹⌛⛔⭐📌🔔 등)은 폰트별 advance width 가 제각각이라
  ;; 터미널·GUI 에서 chat-list/메시지 정렬이 쉽게 깨진다. cell-widths.lua 와 Emacs
  ;; char-width-table 로 대부분 2-cell 잡히고 FE0F/composition 도 TTY 에서 끊었지만,
  ;; 폰트 단계(특히 GUI fontconfig, non-WezTerm 터미널)에서 여전히 드리프트 가능.
  ;; telega 는 가장 자주 깨지는 지점이므로 defensive remap 유지.
  ;;
  ;; 변수 출처: telega-customize.el (telega.el upstream).
  ;; 공식 이름 기준으로 매핑 — dice/timer 같은 오타 변수명은 upstream 과 동기화.
  (setq telega-symbol-attachment      "@"
        telega-symbol-photo           "#"
        telega-symbol-audio           "~"
        telega-symbol-video           "v"
        telega-symbol-game            "G"
        telega-symbol-pending         "."
        telega-symbol-checkmark       "v"
        telega-symbol-heavy-checkmark "V"
        telega-symbol-ton             "T"
        telega-symbol-verified        "V"
        ;; telega-symbol-verified-by-bot — 기본값이 이미 (propertize "A+" ...) 라 redundant
        telega-symbol-failed          "!"
        telega-symbol-star            "*"
        telega-symbol-lightning       "~"
        telega-symbol-location        "@"
        telega-symbol-phone           "T"
        telega-symbol-member          "u"
        telega-symbol-contact         "C"
        telega-symbol-play            ">"
        telega-symbol-pause           "="
        telega-symbol-invoice         "$"
        telega-symbol-credit-card     "$"
        telega-symbol-poll            "P"
        telega-symbol-alarm           "a"
        ;; telega-symbol-dice-list: 7-요소 리스트 (generic + face-1..6)
        telega-symbol-dice-list       (list "D" "1" "2" "3" "4" "5" "6")
        telega-symbol-folder          "/"
        telega-symbol-multiple-folders "//"
        telega-symbol-direct-messages ">>"
        telega-symbol-pin             "*"
        telega-symbol-lock            "L"
        telega-symbol-flames          "~"
        telega-symbol-eye             "o"
        telega-symbol-keyboard        "K"
        telega-symbol-bulp            "i"
        telega-symbol-chat-list       "="
        telega-symbol-bell            "!"
        telega-symbol-favorite        "*"
        telega-symbol-leave-comment   "c"
        telega-symbol-timer-clock     "t"   ; 구: telega-symbol-timer (오타)
        telega-symbol-distance        "d"
        telega-symbol-reaction        "+"
        telega-symbol-premium         "*"
        telega-symbol-forum           "F"
        telega-symbol-my-notes        "N"
        telega-symbol-bot-menu        "="
        telega-symbol-checklist       "[x]")

  ;; messageRichMessage 렌더링 (TDLib 1.8.64+ 신규 content 타입)
  ;;
  ;; Why: TDLib 1.8.64 부터 리치 포맷 텍스트 메시지를 messageText 가 아닌 신규
  ;; content 타입 messageRichMessage 로 전달한다 (td_api.tl):
  ;;   messageRichMessage message:richMessage = MessageContent;
  ;;   richMessage blocks:vector<PageBlock> is_rtl:Bool is_full:Bool = RichMessage;
  ;; telega.el 은 아직 이 타입 렌더러가 없어 telega-ins--content 의 pcase fallback 이
  ;; "<TODO: messageRichMessage>" 만 찍는다 — 봇 메시지가 전부 깨진다.
  ;;
  ;; 렌더 방식: WYSIWYG 가 아니라 **markdown 소스 텍스트**로 직렬화한다. 헤딩은
  ;; #/##/### (sectionHeading :size 1-6 그대로), 문단 사이는 빈 줄, 인라인은
  ;; **bold** *italic* `code` ~~strike~~ [text](url) 마커로. 텍스트 크기를 키우지
  ;; 않아 (markdown-mode 처럼) 보기·복사·에이전트 프롬프트 전달이 쉽다.
  ;; 직렬화기는 cl-case (t) 로 total — 1.8.64 신규 블록/리치텍스트도 inner text 로
  ;; degrade, 절대 에러를 던지지 않아 메시지·root 목록 PP(telega-root--chat-known-pp)
  ;; 가 안 깨진다 (telega 의 telega-webpage--ins-pb/--ins-rt 는 cl-ecase 라 신규
  ;; 타입에서 throw — 그래서 재사용하지 않고 자체 직렬화기를 쓴다).
  ;; one-line 경로(reply preview / root 목록)는 telega-ins--content-one-line 의
  ;; (t (telega-ins--content msg)) fallback 을 타고 자동으로 거친다(한 줄로 잘림).
  ;; TODO: telega upstream 이 messageRichMessage 를 지원하면 advice 와 함께 제거.
  (defun my/telega-ins--rich-message-content (msg)
    "Insert messageRichMessage MSG as markdown source text (not WYSIWYG)."
    (condition-case _err
        (let* ((blocks (telega--tl-get msg :content :message :blocks))
               (md (and blocks (> (length blocks) 0)
                        (my/telega--rich-blocks->md blocks))))
          (if (and md (not (string-empty-p md)))
              (telega-ins md)
            (telega-ins--with-face 'telega-shadow (telega-ins "[rich message]"))))
      (error
       (telega-ins--with-face 'telega-shadow (telega-ins "[rich message]")))))

  (defun my/telega-ins--content-rich-a (orig-fn msg)
    "Around advice: intercept messageRichMessage, else delegate to ORIG-FN."
    (if (eq (telega--tl-type (plist-get msg :content)) 'messageRichMessage)
        (my/telega-ins--rich-message-content msg)
      (funcall orig-fn msg)))

  (advice-add 'telega-ins--content :around #'my/telega-ins--content-rich-a)

  ;; draftMessage 스키마 호환 shim (TDLib 신규 draftMessage.content)
  ;;
  ;; Why: TDLib 가 draftMessage 의 텍스트 필드를 추상화했다 (td_api.tl):
  ;;   OLD: draftMessage.input_message_text : InputMessageContent (inputMessageText)
  ;;   NEW: draftMessage.content : DraftMessageContent (draftMessageContentText)
  ;; telega.el 은 아직 옛 :input_message_text 만 읽어 root 버퍼 chat-status 의
  ;; draft 분기(telega-ins--chat-status)에서 (telega--tl-type nil) → (intern nil)
  ;; → (wrong-type-argument stringp nil) 로 터진다. 공식 클라이언트에서 작성한
  ;; draft 가 있는 채팅이 목록 PP(telega-root--chat-known-pp)를 깨뜨린다.
  ;;
  ;; 신규 :content 에서 옛 :input_message_text 를 합성해 옛 read 경로(목록 표시,
  ;; chatbuf 입력란 복원)가 그대로 동작하게 한다. 텍스트가 아닌 draft content 는
  ;; 빈 formattedText 로 degrade — intern 에 nil 을 절대 넘기지 않아 안전하다.
  ;; TODO: telega upstream 이 DraftMessageContent 를 지원하면 함께 제거.
  (defun my/telega--draft-compat (draft-msg)
    "Backfill legacy :input_message_text on DRAFT-MSG for the new draft schema.
Mutates DRAFT-MSG in place (and returns it) so all old read paths see an
`inputMessageText'.  No-op when the legacy field is already present."
    (when (and draft-msg (not (plist-get draft-msg :input_message_text)))
      (let* ((content (plist-get draft-msg :content))
             (ctype (and content (plist-get content :@type)))
             (fmt-text (if (equal ctype "draftMessageContentText")
                           (plist-get content :text)
                         (list :@type "formattedText" :text ""))))
        (plist-put draft-msg :input_message_text
                   (list :@type "inputMessageText" :text fmt-text))))
    draft-msg)

  (defun my/telega-ins--chat-status-draft-a (chat &optional topic)
    "Normalize the legacy draft schema before CHAT (or TOPIC) status renders."
    (my/telega--draft-compat (plist-get (or topic chat) :draft_message)))
  (advice-add 'telega-ins--chat-status :before #'my/telega-ins--chat-status-draft-a)

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
