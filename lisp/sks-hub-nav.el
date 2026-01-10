;;; sks-hub-nav.el --- SKS Hub Zig State Machine Navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, zig, navigation, state-machine
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; ============================================================================
;; SKS Hub Zig 상태머신 네비게이션 도구
;; ============================================================================
;;
;; ## 왜 이 도구가 필요한가?
;;
;; Zig 상태머신 코드는 다음과 같은 가독성 문제가 있습니다:
;;
;;   1. 중첩 switch 지옥: 상태 x 이벤트 조합이 기하급수적으로 증가
;;   2. ZLS 한계: 아직 0.x 버전, jump-to-definition이 종종 실패
;;   3. comptime 추적 불가: IDE가 컴파일타임 평가를 따라가지 못함
;;   4. 타입 추론 체인: anytype, @TypeOf 등이 얽히면 흐름 파악 불가
;;
;; 이 도구는 sks-hub-zig 프로젝트에 특화된 패턴 기반 네비게이션을 제공합니다.
;; ZLS가 실패해도 상태머신의 State → Event → Transition 흐름을 추적할 수 있습니다.
;;
;; ============================================================================
;; 프로젝트 구조
;; ============================================================================
;;
;; sks-hub-zig/
;; ├── src/
;; │   ├── config_as_ssot.zig   # SSOT: 상수, 타이밍, JSON 템플릿
;; │   ├── types/
;; │   │   ├── state.zig        # HubState, BootPhase, PairingPhase...
;; │   │   └── event.zig        # Event union, Output union
;; │   ├── core/
;; │   │   ├── transition.zig   # transition(state, event) → (state, []Output)
;; │   │   ├── timeout.zig      # 타이머 체크 (순수 함수)
;; │   │   └── led_view.zig     # LED 상태 도출 (순수 함수)
;; │   └── io/
;; │       └── real/            # 실제 I/O (GPIO, MQTT, Zigbee)
;; └── docs/
;;     └── ARCHITECTURE.org     # 상태머신 철학
;;
;; ============================================================================
;; 상태머신 아키텍처
;; ============================================================================
;;
;;   Event → transition(state, event) → (new_state, []Output)
;;                                              ↓
;;                                        IO layer executes
;;
;; 핵심 원칙:
;;   - State: 허브의 모든 상태는 HubState 구조체에
;;   - Event: 외부 입력은 모두 Event tagged union으로 변환
;;   - Transition: 순수 함수, 부작용 없음
;;   - IO: 실제 I/O는 io/real/ 계층에서만
;;
;; 주요 상태 (Phase):
;;   - BootPhase: init → wifi_provisioning → mqtt_connecting → ready
;;   - PairingPhase: idle → searching → device_found → registering → completed
;;   - ConnectionPhase: disconnected → connecting → connected
;;   - OtaPhase: idle → downloading → installing → pending_reboot
;;
;; ============================================================================
;; 명령어
;; ============================================================================
;;
;; 기본 네비게이션:
;;   M-x sks-jump-to-state      상태 enum 정의로 점프 (BootPhase, PairingPhase...)
;;   M-x sks-jump-to-event      Event union 타입으로 점프
;;   M-x sks-jump-to-output     Output union 타입으로 점프
;;   M-x sks-jump-to-transition 특정 상태/이벤트의 전이 로직으로 점프
;;   M-x sks-jump-to-ssot       config_as_ssot.zig 섹션으로 점프
;;
;; 개요 보기:
;;   M-x sks-show-architecture  아키텍처 개요 버퍼 표시
;;   M-x sks-nav-menu           빠른 선택 메뉴 (C-c C-s)
;;
;; ============================================================================
;; 키바인딩
;; ============================================================================
;;
;;   C-c C-s   zig-mode/zig-ts-mode에서 네비게이션 메뉴 열기
;;
;;   메뉴 단축키:
;;     s - State 정의로 점프
;;     e - Event 타입으로 점프
;;     o - Output 타입으로 점프
;;     t - Transition 로직으로 점프
;;     c - SSOT Config로 점프
;;     a - Architecture 개요 표시
;;     q - 취소
;;
;; ============================================================================
;; 사용 예시
;; ============================================================================
;;
;; 예시 1: "페어링 중 디바이스 발견" 로직을 찾고 싶을 때
;;
;;   1. M-x sks-nav-menu (또는 C-c C-s)
;;   2. 't' 입력 (Transition)
;;   3. "PairingPhase" 선택
;;   4. "device_found" 선택
;;   → transition.zig에서 .device_found => 매칭 위치로 점프
;;
;; 예시 2: "zigbee_join 이벤트 정의"를 보고 싶을 때
;;
;;   1. M-x sks-jump-to-event
;;   2. "zigbee_join" 선택
;;   → event.zig에서 해당 union variant로 점프
;;
;; 예시 3: MQTT 타임아웃 상수를 확인하고 싶을 때
;;
;;   1. M-x sks-jump-to-ssot
;;   2. "timeouts" 선택
;;   → config_as_ssot.zig의 timeouts 섹션으로 점프
;;
;; ============================================================================
;; 커스터마이징
;; ============================================================================
;;
;; 프로젝트 루트 수동 설정:
;;   (setq sks-hub-root "~/my/custom/path/sks-hub-zig/")
;;
;; 상태 목록 확장:
;;   (add-to-list 'sks-state-enums "MyCustomPhase")
;;
;; 이벤트 목록 확장:
;;   (add-to-list 'sks-event-types "my_custom_event")
;;
;; 키바인딩 변경:
;;   (with-eval-after-load 'sks-hub-nav
;;     (define-key zig-mode-map (kbd "C-c s") #'sks-nav-menu))
;;
;; ============================================================================
;; 알려진 문제 / TODO
;; ============================================================================
;;
;; - [ ] transition.zig가 너무 큼 (47K tokens) → 섹션별 분할 점프 필요
;; - [ ] 패턴 매칭 실패 시 더 나은 피드백 필요
;; - [ ] imenu 통합으로 사이드바에 상태 목록 표시
;; - [ ] consult 통합으로 실시간 미리보기
;; - [ ] xref 백엔드로 등록하여 M-. 에서 동작
;; - [ ] Mermaid 다이어그램 렌더링 통합
;;
;; 문제 발견 시 이 파일을 직접 수정하세요.
;; 패턴이 맞지 않으면 sks--jump-to-pattern의 regex를 조정하면 됩니다.

;;; Code:

(require 'project)

;;;; Variables

(defvar sks-hub-root nil
  "SKS Hub Zig 프로젝트 루트 디렉토리. 자동 감지됨.")

(defvar sks-state-file "src/types/state.zig"
  "상태 정의 파일 (HubState, Phase enums).")

(defvar sks-event-file "src/types/event.zig"
  "이벤트 정의 파일 (Event union, Output union).")

(defvar sks-transition-file "src/core/transition.zig"
  "전이 로직 파일.")

(defvar sks-ssot-file "src/config_as_ssot.zig"
  "SSOT 설정 파일.")

;;;; Utility Functions

(defun sks--find-project-root ()
  "SKS Hub 프로젝트 루트를 찾습니다."
  (or sks-hub-root
      (when-let* ((proj (project-current)))
        (project-root proj))
      (locate-dominating-file default-directory "src/types/state.zig")
      (expand-file-name "~/repos/work/sks-hub-zig/")))

(defun sks--file-path (relative)
  "프로젝트 루트 기준 상대 경로를 절대 경로로 변환."
  (expand-file-name relative (sks--find-project-root)))

(defun sks--jump-to-pattern (file pattern &optional after-pattern)
  "FILE에서 PATTERN을 찾아 점프합니다. AFTER-PATTERN이 있으면 추가 검색."
  (let ((path (sks--file-path file)))
    (if (file-exists-p path)
        (progn
          (find-file path)
          (goto-char (point-min))
          (if (re-search-forward pattern nil t)
              (progn
                (when after-pattern
                  (re-search-forward after-pattern nil t))
                (recenter)
                (pulse-momentary-highlight-one-line (point)))
            (message "Pattern not found: %s" pattern)))
      (message "File not found: %s" path))))

;;;; State Navigation

(defvar sks-state-enums
  '("BootPhase" "PairingPhase" "ConnectionPhase" "OtaPhase"
    "OperationStatus" "DeviceType" "LedState" "HubState" "HubPersistentState")
  "상태 관련 enum/struct 목록.")

(defun sks-jump-to-state ()
  "상태 정의로 점프합니다."
  (interactive)
  (let* ((states (append sks-state-enums
                         ;; 현재 버퍼에서 Phase/State로 끝나는 심볼 수집
                         (sks--collect-symbols-matching "\\(Phase\\|State\\)$")))
         (state (completing-read "Jump to state: " (delete-dups states) nil t)))
    (sks--jump-to-pattern sks-state-file (format "pub const %s = " state))))

(defun sks--collect-symbols-matching (regex)
  "현재 버퍼에서 REGEX에 매치되는 심볼 수집."
  (let (symbols)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "\\.\\([a-z_]+\\)%s" regex) nil t)
        (push (match-string 1) symbols)))
    symbols))

;;;; Event Navigation

(defvar sks-event-types
  '("shadow_raw" "shadow_delta" "jobs_raw"
    "zigbee_join" "zigbee_leave" "zigbee_data"
    "permit_join_status" "command_ack"
    "network_manage" "connect_status" "sync_time"
    "manufacturer_name_report"
    "button" "timer" "system" "tester")
  "Event union 타입 목록.")

(defun sks-jump-to-event ()
  "이벤트 정의로 점프합니다."
  (interactive)
  (let ((event (completing-read "Jump to event: " sks-event-types nil t)))
    (sks--jump-to-pattern sks-event-file (format "^\\s-+%s:" event))))

;;;; Output Navigation

(defvar sks-output-types
  '("check_wifi_config" "enter_ap_mode" "factory_reset"
    "init_peripherals" "start_wifi_connection"
    "connect_mqtt" "disconnect_mqtt" "subscribe_all_shadows"
    "publish_initial_shadows" "send_shadow"
    "zigbee_permit_join" "zigbee_send_command"
    "save_hub_state" "set_led" "reboot" "log"
    "publish_keepalive" "device_control")
  "Output union 타입 목록.")

(defun sks-jump-to-output ()
  "Output 정의로 점프합니다."
  (interactive)
  (let ((output (completing-read "Jump to output: " sks-output-types nil t)))
    (sks--jump-to-pattern sks-event-file
                          (format "pub const Output = union" )
                          (format "^\\s-+%s:" output))))

;;;; Transition Navigation

(defun sks-jump-to-transition ()
  "transition.zig에서 특정 상태/이벤트 처리 로직으로 점프."
  (interactive)
  (let* ((type (completing-read "Search in transition for: "
                                '("BootPhase" "PairingPhase" "ConnectionPhase"
                                  "Event type" "Output type" "Custom pattern")
                                nil t))
         (pattern (pcase type
                    ("BootPhase"
                     ;; 실제 패턴: next.boot = .wifi_provisioning, next.boot == .ready
                     (format "\\.boot.*\\.%s" (completing-read "Boot phase: "
                                                          '("init" "wifi_provisioning" "wifi_connecting"
                                                            "fleet_provisioning" "mqtt_connecting"
                                                            "shadow_subscribing" "shadow_syncing"
                                                            "ready" "failed"))))
                    ("PairingPhase"
                     ;; 실제 패턴: next.pairing = .idle, next.pairing == .searching
                     (format "\\.pairing.*\\.%s" (completing-read "Pairing phase: "
                                                          '("idle" "requested" "searching"
                                                            "device_found" "registering"
                                                            "completed" "failed"))))
                    ("ConnectionPhase"
                     ;; 실제 패턴: next.connection = .connected
                     (format "\\.connection.*\\.%s" (completing-read "Connection phase: "
                                                          '("disconnected" "connecting"
                                                            "connected" "reconnecting"))))
                    ("Event type"
                     (format "\\.%s =>" (completing-read "Event: " sks-event-types)))
                    ("Output type"
                     (format "\\.%s" (completing-read "Output: " sks-output-types)))
                    ("Custom pattern"
                     (read-string "Pattern: ")))))
    (sks--jump-to-pattern sks-transition-file pattern)))

;;;; SSOT Navigation

(defun sks-jump-to-ssot ()
  "config_as_ssot.zig로 점프합니다."
  (interactive)
  (let ((section (completing-read "SSOT section: "
                                  '("network" "shadow" "timeouts" "led" "zigbee")
                                  nil t)))
    (sks--jump-to-pattern sks-ssot-file
                          (format "pub const %s = " section))))

;;;; Project Overview

(defun sks-show-architecture ()
  "SKS Hub 아키텍처 개요를 표시합니다."
  (interactive)
  (let ((buf (get-buffer-create "*SKS Hub Architecture*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# SKS Hub Zig State Machine Architecture\n\n")
      (insert "## Core Files\n\n")
      (insert "| File | Role |\n")
      (insert "|------|------|\n")
      (insert "| types/state.zig | State definitions (HubState, Phases) |\n")
      (insert "| types/event.zig | Event/Output unions |\n")
      (insert "| core/transition.zig | Pure transition function |\n")
      (insert "| core/timeout.zig | Timer checking (pure) |\n")
      (insert "| core/led_view.zig | LED state derivation (pure) |\n")
      (insert "| config_as_ssot.zig | SSOT constants/templates |\n\n")
      (insert "## State Flow\n\n")
      (insert "```\n")
      (insert "Event → transition(state, event) → (new_state, []Output)\n")
      (insert "                                          ↓\n")
      (insert "                                    IO layer executes\n")
      (insert "```\n\n")
      (insert "## Key States\n\n")
      (insert "- BootPhase: init → wifi_provisioning → mqtt_connecting → ready\n")
      (insert "- PairingPhase: idle → searching → device_found → registering → completed\n")
      (insert "- ConnectionPhase: disconnected → connecting → connected\n")
      (insert "- OtaPhase: idle → downloading → installing → pending_reboot\n\n")
      (insert "## Navigation Commands\n\n")
      (insert "- M-x sks-jump-to-state : Jump to state definition\n")
      (insert "- M-x sks-jump-to-event : Jump to event type\n")
      (insert "- M-x sks-jump-to-output : Jump to output type\n")
      (insert "- M-x sks-jump-to-transition : Jump to transition logic\n")
      (insert "- M-x sks-jump-to-ssot : Jump to SSOT config\n")
      (markdown-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;; Hydra/Transient Menu (optional)

(defun sks-nav-menu ()
  "SKS Hub 네비게이션 메뉴를 표시합니다."
  (interactive)
  (let ((choice (read-char-choice
                 "[s]tate [e]vent [o]utput [t]ransition [c]onfig [a]rch [q]uit: "
                 '(?s ?e ?o ?t ?c ?a ?q))))
    (pcase choice
      (?s (call-interactively #'sks-jump-to-state))
      (?e (call-interactively #'sks-jump-to-event))
      (?o (call-interactively #'sks-jump-to-output))
      (?t (call-interactively #'sks-jump-to-transition))
      (?c (call-interactively #'sks-jump-to-ssot))
      (?a (call-interactively #'sks-show-architecture))
      (?q (message "Cancelled")))))

;;;; Auto-detect project

(defun sks-hub-mode-maybe ()
  "sks-hub-zig 프로젝트에서 자동으로 네비게이션 활성화."
  (when (and buffer-file-name
             (string-match-p "sks-hub-zig" buffer-file-name))
    (setq-local sks-hub-root (sks--find-project-root))))

(add-hook 'zig-mode-hook #'sks-hub-mode-maybe)
(add-hook 'zig-ts-mode-hook #'sks-hub-mode-maybe)

;;;; Keybindings

(with-eval-after-load 'zig-mode
  (define-key zig-mode-map (kbd "C-c C-s") #'sks-nav-menu))

(with-eval-after-load 'zig-ts-mode
  (define-key zig-ts-mode-map (kbd "C-c C-s") #'sks-nav-menu))

;;;; Provide

(provide 'sks-hub-nav)

;;; sks-hub-nav.el ends here
