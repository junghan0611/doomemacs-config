;;; $DOOMDIR/lisp/zellij-config.el --- Zellij Terminal Multiplexer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Zellij 터미널 멀티플렉서 통합 및 Claude Code 오케스트레이션
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Zellij vs tmux 비교
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; ┌──────────────────┬──────────────────┬──────────────────┐
;; │ 기능             │ tmux             │ Zellij           │
;; ├──────────────────┼──────────────────┼──────────────────┤
;; │ 레이아웃 정의    │ 스크립트 (복잡)  │ KDL (선언적) ✅  │
;; │ 출력 캡처        │ capture-pane ✅  │ dump-screen △   │
;; │ 플러그인         │ 없음             │ WASM 플러그인 ✅ │
;; │ 양방향 통신      │ 없음             │ zellij pipe ✅   │
;; │ 패인 이름        │ 수동 설정        │ 레이아웃서 선언  │
;; │ 성숙도           │ 20+ 년           │ ~4 년            │
;; └──────────────────┴──────────────────┴──────────────────┘
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Zellij 핵심 개념
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 계층 구조:
;;   Session (하나의 Zellij 인스턴스)
;;     ├── Tab 1
;;     │     ├── Pane 1 (tiled or floating)
;;     │     ├── Pane 2
;;     │     └── Pane 3
;;     └── Tab 2
;;           └── Pane 1
;;
;; 패인 타입:
;;   - Tiled Panes: 그리드 레이아웃, 겹치지 않음 (기본)
;;   - Floating Panes: 자유 배치, 겹칠 수 있음 (팝업)
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; KDL 레이아웃 문법
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 기본 구조:
;;   layout {
;;       pane                                    // 기본 셸
;;       pane command="htop"                     // 명령 실행
;;       pane split_direction="vertical" {      // 세로 분할
;;           pane
;;           pane
;;       }
;;   }
;;
;; 4-패인 Claude 에이전트 예시:
;;   layout {
;;       pane split_direction="horizontal" {
;;           pane split_direction="vertical" {
;;               pane name="agent-1" { command "claude" }
;;               pane name="agent-2" { command "claude" }
;;           }
;;           pane split_direction="vertical" {
;;               pane name="agent-3" { command "claude" }
;;               pane name="agent-4" { command "claude" }
;;           }
;;       }
;;   }
;;
;; 레이아웃 파일 위치:
;;   ~/.config/zellij/layouts/
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Zellij CLI 명령어 (핵심)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 세션 관리:
;;   zellij                              세션 시작 (기본)
;;   zellij --layout LAYOUT              레이아웃으로 시작
;;   zellij attach SESSION               세션 연결
;;   zellij list-sessions                세션 목록
;;   zellij kill-session SESSION         세션 종료
;;
;; 패인 제어 (action):
;;   zellij action new-pane              새 패인
;;   zellij action new-pane -f           플로팅 패인
;;   zellij action close-pane            패인 닫기
;;   zellij action focus-next-pane       다음 패인 포커스
;;   zellij action move-focus right      방향으로 포커스 이동
;;
;; 텍스트 전송:
;;   zellij action write-chars "TEXT"    현재 패인에 텍스트 전송
;;   zellij action write 72 105          바이트로 전송 ("Hi")
;;
;; 출력 캡처 (제한적!):
;;   zellij action dump-screen FILE      현재 패인 출력을 파일로 덤프
;;   zellij action edit-scrollback       에디터로 스크롤백 열기
;;
;; 실행:
;;   zellij run -- COMMAND               새 패인에서 명령 실행
;;   zellij run -f -- COMMAND            플로팅 패인에서 실행
;;   zellij run --name NAME -- CMD       이름 있는 패인에서 실행
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Zellij Pipe 시스템
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 개념:
;;   - 플러그인과 CLI 간 메시지 전달
;;   - 플러그인이 실행 중이 아니면 자동 시작
;;   - 양방향 통신 가능
;;
;; CLI에서 사용:
;;   zellij pipe [OPTIONS] -- PAYLOAD
;;
;;   -n, --name NAME        Pipe 이름
;;   -a, --args KEY=VALUE   키-값 인자
;;   -p, --plugin PLUGIN    대상 플러그인 (file:/path/to/plugin.wasm)
;;
;; 예시:
;;   zellij pipe -n "my-cmd" -- '{"action": "run", "prompt": "..."}'
;;   echo "data" | zellij pipe --name logs
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; Plugin API: get_pane_scrollback (문서화 안 됨, 하지만 존재!)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; Zellij 소스코드에서 발견:
;;   zellij-tile/src/shim.rs:1105:  pub fn get_pane_scrollback(...)
;;   zellij-server/src/plugins/zellij_exports.rs:2333:  fn get_pane_scrollback(...)
;;
;; 의미:
;;   - CLI에서는 불가능하지만, Plugin에서는 패인 출력 캡처 가능!
;;   - Rust WASM 플러그인 개발 시 활용 가능
;;   - 향후 Emacs 통합의 핵심 기능
;;
;; 향후 로드맵:
;;   1. Rust 플러그인 개발 (zellij-emacs-bridge.wasm)
;;   2. get_pane_scrollback으로 에이전트 응답 수집
;;   3. zellij pipe로 Emacs와 양방향 통신
;;   4. tmux-config.el의 모든 기능 Zellij로 이식
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 키바인딩 (SPC 3 z ...)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 세션/레이아웃:
;;   SPC 3 z o   Open layout       레이아웃으로 세션 시작
;;   SPC 3 z a   Attach session    세션 연결
;;   SPC 3 z l   List sessions     세션 목록
;;   SPC 3 z k   Kill session      세션 종료
;;
;; 패인 제어:
;;   SPC 3 z n   New pane          새 패인
;;   SPC 3 z f   Floating pane     플로팅 패인
;;   SPC 3 z r   Run command       명령 실행
;;   SPC 3 z s   Send text         텍스트 전송
;;   SPC 3 z d   Dump screen       출력 캡처
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 현재 제약 및 tmux와의 차이
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 1. 패인 출력 캡처:
;;    - tmux: capture-pane -p (stdout으로 출력, 파이프 가능)
;;    - Zellij: dump-screen FILE (파일로만, stdout 불가)
;;
;; 2. 특정 패인 타게팅:
;;    - tmux: session:window.pane 형식 (어디서든 접근)
;;    - Zellij: focus-pane-by-name (먼저 포커스 필요)
;;
;; 3. 결론:
;;    - 현재는 tmux가 Claude Code 오케스트레이션에 더 적합
;;    - Zellij는 레이아웃 관리 및 향후 플러그인 통합용으로 탐색
;;
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 빠른 시작 가이드
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 1. 설치:
;;    $ nix-env -iA nixos.zellij
;;
;; 2. 기본 레이아웃 생성:
;;    $ mkdir -p ~/.config/zellij/layouts
;;    $ cat > ~/.config/zellij/layouts/four-agents.kdl << 'EOF'
;;    layout {
;;        pane split_direction="horizontal" {
;;            pane split_direction="vertical" { pane; pane; }
;;            pane split_direction="vertical" { pane; pane; }
;;        }
;;    }
;;    EOF
;;
;; 3. 실행:
;;    $ zellij --layout four-agents
;;
;; 4. Emacs에서 제어:
;;    M-x +zellij-send-text RET "hello" RET
;;
;; Sources:
;;   - https://zellij.dev/documentation/
;;   - https://zellij.dev/documentation/plugin-pipes.html
;;   - https://zellij.dev/documentation/creating-a-layout.html

;;; Code:

;;;; Variables

(defvar +zellij-layout-dir (expand-file-name "~/.config/zellij/layouts/")
  "Zellij 레이아웃 파일 디렉터리.")

(defvar +zellij-default-layout "four-agents"
  "기본 레이아웃 이름.")

(defvar +zellij-agent-layout-template
  "layout {
    pane split_direction=\"horizontal\" {
        pane split_direction=\"vertical\" {
            pane name=\"%s\" %s
            pane name=\"%s\" %s
        }
        pane split_direction=\"vertical\" {
            pane name=\"%s\" %s
            pane name=\"%s\" %s
        }
    }
}"
  "4-에이전트 레이아웃 템플릿.
%s: agent-1, cmd, agent-2, cmd, agent-3, cmd, agent-4, cmd")

;;;; Core Functions

(defun +zellij--run (command &rest args)
  "Zellij COMMAND를 ARGS와 함께 실행."
  (let ((cmd (string-join (cons "zellij" (cons command args)) " ")))
    (shell-command-to-string cmd)))

(defun +zellij-action (action &rest args)
  "Zellij ACTION을 ARGS와 함께 실행."
  (apply #'+zellij--run "action" action args))

;;;; Session Management

;;;###autoload
(defun +zellij-open-layout (layout-name)
  "LAYOUT-NAME 레이아웃으로 Zellij 세션 시작."
  (interactive
   (list (completing-read "Layout: "
                          (when (file-directory-p +zellij-layout-dir)
                            (directory-files +zellij-layout-dir nil "\\.kdl$"))
                          nil nil nil nil +zellij-default-layout)))
  (let ((layout (file-name-sans-extension layout-name)))
    (start-process "zellij" nil "zellij" "--layout" layout)
    (message "Zellij started with layout: %s" layout)))

;;;###autoload
(defun +zellij-list-sessions ()
  "Zellij 세션 목록 표시."
  (interactive)
  (let ((output (+zellij--run "list-sessions")))
    (if (string-empty-p (string-trim output))
        (message "No active Zellij sessions")
      (message "Sessions:\n%s" output))))

;;;###autoload
(defun +zellij-attach-session (session)
  "Zellij SESSION에 연결."
  (interactive
   (list (completing-read "Session: "
                          (split-string (+zellij--run "list-sessions") "\n" t))))
  (start-process "zellij" nil "zellij" "attach" session)
  (message "Attached to session: %s" session))

;;;###autoload
(defun +zellij-kill-session (session)
  "Zellij SESSION 종료."
  (interactive
   (list (completing-read "Kill session: "
                          (split-string (+zellij--run "list-sessions") "\n" t))))
  (+zellij--run "kill-session" session)
  (message "Killed session: %s" session))

;;;; Pane Control

;;;###autoload
(defun +zellij-new-pane (&optional floating)
  "새 패인 생성. FLOATING이 non-nil이면 플로팅 패인."
  (interactive "P")
  (if floating
      (+zellij-action "new-pane" "-f")
    (+zellij-action "new-pane"))
  (message "New %spane created" (if floating "floating " "")))

;;;###autoload
(defun +zellij-run-command (command &optional pane-name)
  "새 패인에서 COMMAND 실행. PANE-NAME으로 이름 지정."
  (interactive "sCommand: \nsPane name (optional): ")
  (let ((args (if (and pane-name (not (string-empty-p pane-name)))
                  (list "run" "--name" pane-name "--" command)
                (list "run" "--" command))))
    (apply #'+zellij--run args)
    (message "Running: %s" command)))

;;;###autoload
(defun +zellij-send-text (text)
  "현재 포커스된 패인에 TEXT 전송."
  (interactive "sText: ")
  (+zellij-action "write-chars" (shell-quote-argument text))
  (message "Sent: %s" (truncate-string-to-width text 40)))

;;;###autoload
(defun +zellij-dump-screen ()
  "현재 패인 출력을 임시 파일로 캡처하고 버퍼에 표시."
  (interactive)
  (let ((tmpfile (make-temp-file "zellij-screen")))
    (+zellij-action "dump-screen" tmpfile)
    (find-file tmpfile)
    (message "Screen dumped to: %s" tmpfile)))

;;;###autoload
(defun +zellij-focus-pane (direction)
  "DIRECTION(up/down/left/right)으로 패인 포커스 이동."
  (interactive
   (list (completing-read "Direction: " '("up" "down" "left" "right"))))
  (+zellij-action "move-focus" direction))

;;;; Layout Generation
;;
;; 사용자 선택 가능 부분:
;; 레이아웃을 어떻게 생성할지 결정하는 핵심 함수입니다.
;;

;; TODO: 사용자가 구현할 함수
;; 이 함수는 에이전트 정보를 받아 KDL 패인 정의를 반환합니다.
;;
;; 예시 구현:
;;   (defun +zellij-agent-to-kdl (agent)
;;     "AGENT를 KDL pane 정의로 변환."
;;     (let ((name (car agent))
;;           (cmd (cdr agent)))
;;       (if cmd
;;           (format "{ command \"%s\" }" cmd)
;;         "")))
;;
;; 고려할 점:
;;   - 에이전트가 명령을 가지면 command 속성 추가
;;   - 빈 에이전트는 기본 셸
;;   - 추가 옵션: cwd, args 등

(defun +zellij-agent-to-kdl (agent)
  "AGENT를 KDL pane 정의로 변환.
AGENT는 (NAME . COMMAND) 형식의 cons cell.
COMMAND가 nil이면 기본 셸 패인."
  (let ((name (car agent))
        (cmd (cdr agent)))
    (if cmd
        (format "{ command \"%s\" }" cmd)
      "")))

;;;###autoload
(defun +zellij-generate-4agent-layout (agents)
  "AGENTS 리스트로 4-에이전트 KDL 레이아웃 생성.
AGENTS는 ((name1 . cmd1) (name2 . cmd2) ...) 형식."
  (let* ((a1 (or (nth 0 agents) '("agent-1" . nil)))
         (a2 (or (nth 1 agents) '("agent-2" . nil)))
         (a3 (or (nth 2 agents) '("agent-3" . nil)))
         (a4 (or (nth 3 agents) '("agent-4" . nil))))
    (format +zellij-agent-layout-template
            (car a1) (+zellij-agent-to-kdl a1)
            (car a2) (+zellij-agent-to-kdl a2)
            (car a3) (+zellij-agent-to-kdl a3)
            (car a4) (+zellij-agent-to-kdl a4))))

;;;###autoload
(defun +zellij-create-agent-layout (layout-name)
  "LAYOUT-NAME으로 새 에이전트 레이아웃 생성하고 파일로 저장."
  (interactive "sLayout name: ")
  (let* ((agents '(("pm" . nil)
                   ("code" . nil)
                   ("test" . nil)
                   ("debug" . nil)))
         (kdl (+zellij-generate-4agent-layout agents))
         (file (expand-file-name (concat layout-name ".kdl") +zellij-layout-dir)))
    (unless (file-directory-p +zellij-layout-dir)
      (make-directory +zellij-layout-dir t))
    (with-temp-file file
      (insert kdl))
    (message "Layout saved: %s" file)
    (find-file file)))

;;;; Pipe Communication (Future)
;;
;; TODO: Rust 플러그인 개발 후 활성화
;;
;; (defun +zellij-pipe (name payload &optional plugin)
;;   "Zellij pipe로 NAME 메시지 전송."
;;   (let ((args (list "pipe" "--name" name)))
;;     (when plugin
;;       (setq args (append args (list "--plugin" plugin))))
;;     (setq args (append args (list "--" payload)))
;;     (apply #'+zellij--run args)))

;;;; Keybindings

(map! :leader
      (:prefix "3"
       (:prefix ("z" . "zellij")
        ;; Session
        :desc "Open layout"       "o" #'+zellij-open-layout
        :desc "Attach session"    "a" #'+zellij-attach-session
        :desc "List sessions"     "l" #'+zellij-list-sessions
        :desc "Kill session"      "k" #'+zellij-kill-session
        ;; Pane
        :desc "New pane"          "n" #'+zellij-new-pane
        :desc "Floating pane"     "f" (cmd! (+zellij-new-pane t))
        :desc "Run command"       "r" #'+zellij-run-command
        :desc "Send text"         "s" #'+zellij-send-text
        :desc "Dump screen"       "d" #'+zellij-dump-screen
        :desc "Focus direction"   "g" #'+zellij-focus-pane
        ;; Layout
        :desc "Create layout"     "c" #'+zellij-create-agent-layout)))

(provide 'zellij-config)
;;; zellij-config.el ends here
