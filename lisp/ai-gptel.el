;;; $DOOMDIR/lisp/ai-gptel.el --- gptel AI 설정 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; gptel 중심 AI 설정 — 백엔드/모델, 빠른 조회(quick), 버퍼 요약/번역, embark 통합
;;
;; 백엔드는 OpenAI-sub (ChatGPT 구독 OAuth) 하나. 모델도 셋뿐이다:
;;   gpt-5.6-sol   무거운 작업 — 필요할 때만 수동 호출
;;   gpt-5.6-terra 기본 — 채팅, 요약/번역 버퍼
;;   gpt-5.6-luna  빠른 자리 — quick, magit 커밋, 인라인 번역
;; 모델이 계속 새로 나오는 자리라 백엔드/모델 목록을 넓히지 않는다.
;; 넣고 싶으면 이 파일 한 곳(`my/gptel-models')만 고친다.
;;
;; 구조:
;;   1. Evil Collection 설정
;;   2. gptel 코어 (use-package!)
;;      - 백엔드: OpenAI-sub (OAuth)
;;      - gptel-org, gptel-quick, gptel-prompt
;;   3. 버퍼 요약/번역 (+gptel-buffer)
;;   4. Embark + gptel 통합
;;   5. 알림 (peon-ping)

;;; Code:

;;;; Evil Collection

;; RET 이 gptel 버퍼에서 전송하지 않게 — 원래 의도는 commit c9d9217
;; "fix(gptel): Prevent `RET` from sending message in gptel".
;;
;; 그때 쓰던 `evil-collection-gptel-want-ret-to-send' 는 upstream 에서
;; 사라졌다. 지금은 gptel 전용 옵션이 아니라 REPL 계열 공통 추상 바인딩
;; (`repl-submit' / `repl-newline')이 `evil-collection-binding-defaults'
;; 에 있고, `evil-collection-repl-submit-state' 기본값 `normal' 때문에
;; **normal state 의 RET 이 `gptel-send'** 로 간다. 죽은 setq 를 계속
;; 들고 있어서 조용히 되돌아가 있었다.
;;
;; `:enabled' 는 (MAP-SYM ID KEYS COMMAND) 람다를 받는다 — gptel 에서만
;; 끄고 cider/eshell/vterm 등 다른 REPL 은 건드리지 않는다. insert state
;; 의 `repl-newline' 은 살아있어 RET 은 그대로 개행.
;; 기존 override 가 있으면 앞에 얹는다 (`assq' 라 첫 항목이 이긴다).
;;
;; 전송은 명시적인 키로만 — 실수로 날아가지 않게:
;;   C-c RET        gptel 이 minor mode 키맵에 직접 박아둔 기본 (gptel.el:1014).
;;                  minor mode 맵이라 org-mode 의 C-c RET 보다 우선한다.
;;   M-RET          아래 `map!' 에서 추가.
;;   S-RET          `gptel-menu' — 옵션 확인하고 거기서 확정.
;;   RET            전송 안 함. normal 은 `evil-ret', insert 는 개행.
(setq evil-collection-binding-overrides
      (cons (list 'repl-submit
                  :enabled (lambda (map-sym &rest _)
                             (not (eq map-sym 'gptel-mode-map))))
            (bound-and-true-p evil-collection-binding-overrides)))

;; 이 둘은 upstream 에 그대로 살아있다 (menu 가 to-send 보다 우선).
(setq evil-collection-gptel-want-shift-ret-menu t)
(setq evil-collection-gptel-want-shift-ret-to-send nil)

;;;; gptel 코어

(use-package! gptel
  :init
  (require 'password-store)  ; API 키 접근을 위해 미리 로드
  :config

;;;;;; Hooks & 기본값

  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-response-functions #'my/gptel-peon-ping-notify 90)

  (setq gptel-include-reasoning nil)
  (setq gptel-default-mode 'org-mode)
  ;; gptel chat 버퍼는 백엔드 이름으로 열린다 — `*OpenAI-sub*`
  (set-popup-rule! "^\\*OpenAI-sub\\*$" :side 'right :size 84 :vslot 100 :quit t)
  (set-popup-rule! "^\\*gptel-buffer\\*$" :side 'right :size 0.4 :vslot 99 :quit nil :select t)

;;;;;; gptel-org

  (after! gptel-org
    (defun my/gptel-org-toggle-branching-context ()
      "Toggle gptel context between doc and subheading."
      (interactive)
      (if gptel-org-branching-context
          (progn
            (setq-local gptel-org-branching-context nil)
            (message "Context: whole doc"))
        (setq-local gptel-org-branching-context t)
        (message "Context: subheading")))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user "
          (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"
          (alist-get 'markdown-mode gptel-prompt-prefix-alist) "@user "
          (alist-get 'markdown-mode gptel-response-prefix-alist) "@assistant\n"
          )
    (setq-default gptel-org-branching-context t))

;;;;; 백엔드 (Backend)

;;;;;; OpenAI-sub (ChatGPT Plus/Pro subscription via OAuth)

  ;; 첫 호출 시 브라우저로 OpenAI 로그인. 토큰은 로컬 캐시.
  ;; 수동: M-x gptel-openai-oauth-login
  ;; gptel-openai-oauth.el은 upstream gptel master (>= 56e5b06)에만 존재 —
  ;; fboundp 가드는 패키지 회귀 안전망 (정상 환경에선 항상 등록됨).

  ;; :models 를 명시하지 않으면 upstream 기본값이 gpt-5.2/5.3-codex/5.4/5.5 까지
  ;; 9개를 메뉴에 흘린다. 쓰는 셋만 남긴다 — 새 모델은 여기 한 줄로 들어온다.
  (defconst my/gptel-models '(gpt-5.6-terra gpt-5.6-sol gpt-5.6-luna)
    "OpenAI-sub 백엔드에 등록할 모델. 첫 항목이 `gptel-menu' 기본 노출 순서 선두.")

  (defconst my/gptel-model-default 'gpt-5.6-terra
    "기본 모델. 채팅과 요약/번역 버퍼가 쓴다.")

  (defconst my/gptel-model-heavy 'gpt-5.6-sol
    "무거운 작업용. 필요할 때만 수동 전환.")

  (defconst my/gptel-model-fast 'gpt-5.6-luna
    "빠른 자리용 — gptel-quick, magit 커밋 메시지, 인라인 번역.")

  (defun my/gptel--model-specs (models)
    "Return MODELS with their upstream specs attached.
`gptel--process-models' only assigns a symbol plist when the model
arrives as a cons cell; bare symbols land with an empty plist, so the
menu loses context window, cost and capabilities.  Pull the spec from
`gptel--openai-models' instead of restating it here — upstream stays
the single source of truth, and an unknown model degrades to a bare
symbol rather than erroring."
    (mapcar (lambda (model) (or (assq model gptel--openai-models) model))
            models))

  (defvar gptel-openai-sub-backend nil
    "OpenAI ChatGPT Plus/Pro subscription backend via OAuth.")
  (when (fboundp 'gptel-make-openai-oauth)
    (setq gptel-openai-sub-backend
          (gptel-make-openai-oauth "OpenAI-sub"
            :models (my/gptel--model-specs my/gptel-models))))

;;;;;; Codex streaming advice

  ;; Codex endpoint(/backend-api/codex/responses)은 stream=true 필수.
  ;; gptel-request는 기본 :stream nil → "Stream must be set to true" 400.
  ;; 2026-07-22 재확인: upstream 미해결. `gptel-request' 는 여전히 `(stream nil)'
  ;; 기본이고, OAuth backend 의 `gptel--request-data' 는 temperature 와
  ;; max_output_tokens 만 떼낸다 → body 에 `:stream :json-false' 그대로 나감.
  ;; elfeed/gptel-quick/gptel-magit 등이 모두 영향 — advice로 한 번에 해결.
  ;; OpenAI-sub 백엔드 감지 시: :stream t 강제 + 청크 누적 → user
  ;; callback에 풀 응답 한 번에 전달 (non-streaming 인터페이스 보존).
  (defun +gptel--codex-stream-advice (orig-fun &rest args)
    "Force :stream t + accumulate chunks for Codex/OAuth backend.
Gate narrowly: only when (a) the backend is OpenAI OAuth, (b) the
caller passed an explicit :callback (non-streaming semantics), and
(c) the caller did NOT already request :stream t (chat/gptel-send
has its own streaming wiring via fsm — leave it alone)."
    (let* ((plist (cdr args))
           (user-cb (plist-get plist :callback)))
      (if (and (eq (type-of gptel-backend) 'gptel-openai-oauth)
               user-cb
               (not (eq (plist-get plist :stream) t)))
          (let* ((acc "")
                 (new-cb (lambda (response info)
                           (cond
                            ((stringp response)
                             (setq acc (concat acc response)))
                            ((eq response t)
                             (funcall user-cb
                                      (and (not (string-empty-p acc))
                                           (string-trim acc))
                                      info))
                            (t (funcall user-cb response info))))))
            (apply orig-fun (car args)
                   :stream t
                   :callback new-cb
                   (cl-loop for (k v) on plist by #'cddr
                            unless (memq k '(:stream :callback))
                            append (list k v))))
        (apply orig-fun args))))

  (advice-add 'gptel-request :around #'+gptel--codex-stream-advice)

;;;;;; Codex max_output_tokens advice

  ;; gptel-agent.el L690이 chat buffer 생성 시 무조건 8192 박음.
  ;; upstream `gptel--request-data' (gptel-openai-oauth.el L68) 이 이제 그 키를
  ;; **떼주긴 한다** — 대신 `display-warning' 을 매 요청 때린다. 기능은 upstream
  ;; 이 막았고 이 advice 에 남은 일은 그 경고 소음 차단뿐.
  ;; gptel--request-data 호출 직전 dynamic let으로 nil 강제 → 키 자체가 안 생김.
  ;; 다른 backend는 영향 없음. upstream gptel-agent.el L690 패치 후보 자리.
  (defun +gptel--codex-clear-max-tokens-advice (orig-fun &rest args)
    "Force `gptel-max-tokens' nil for Codex (OAuth) backend — silence warning."
    (let ((gptel-max-tokens
           (if (eq (type-of gptel-backend) 'gptel-openai-oauth)
               nil
             gptel-max-tokens)))
      (apply orig-fun args)))

  (advice-add 'gptel--request-data :around #'+gptel--codex-clear-max-tokens-advice)

;;;;; Tools (PoC — tool round-trip 검증용)

  ;; 큰 그림: ~/.claude/skills/ 화이트리스트 → gptel-make-tool 자동 등록.
  ;; 예: botlog skill 등록되면 "오늘 작업 botlog로 기록해" 한 마디로 호출.
  ;; 현 단계는 tool round-trip이 정상 닫히는지 sanity.
  ;;
  ;; gptel-use-tools 명시 (anti-regression):
  ;; transient menu (`gptel-menu` → `T`)에서 `force`로 바꾸면 정의상 매 턴
  ;; tool 호출 강제 → 무한 loop. dotsamples 30+ 사용자도 모두 `t`만 사용.
  ;; default를 여기 명시해서 customize 잔재로부터 보호.
  (setq gptel-use-tools t)
  (setq-default gptel-use-tools t)

  ;; 검증 (2026-05-26, gpt-5.4 기준):
  ;;   use-tools t      → 1회, 4.58s 닫힘 ✓
  ;;   use-tools 'force → 무한 loop (정의상 정상 동작)
  ;;
  ;; 사용: chat buffer에서 "What time is it?" → `get_current_time` 자동 호출.
  ;; model 전환은 `my/gptel-switch-model` 또는 `gptel-menu`에서.
  (gptel-make-tool
   :name "get_current_time"
   :description "Return the current local time in ISO-8601 format (YYYY-MM-DDTHH:MM:SS+ZZZZ)."
   :args nil
   :category "time"
   :function (lambda () (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

;;;;; 기본 모델 선택 & 전환

  ;; 시스템 프롬프트 설정 (+user-info.el에서 정의)
  (setq gptel--system-message user-llm-system-prompt)

  ;; Magit 커밋 메시지 — 빠른 모델로 충분
  (setq gptel-magit-backend gptel-openai-sub-backend)
  (setq gptel-magit-model my/gptel-model-fast)

  ;; 기본: OpenAI-sub + terra.
  ;; OAuth 토큰 캐시되면 부팅 후 즉시 사용.
  (setq gptel-backend gptel-openai-sub-backend)
  (setq gptel-model my/gptel-model-default)

  (defun my/gptel-switch-model (model)
    "Switch `gptel-model' to MODEL on the OpenAI-sub backend.
Interactively prompt among `my/gptel-models'."
    (interactive
     (list (intern (completing-read
                    "gptel model: " (mapcar #'symbol-name my/gptel-models)
                    nil t nil nil (symbol-name my/gptel-model-default)))))
    (setq gptel-backend gptel-openai-sub-backend)
    (setq gptel-model model)
    (message "gptel model: %s" model))

;;;;; gptel-quick — 빠른 조회

  ;; 커서를 단어 위에 놓고 → gptel-quick (또는 embark [ 키)
  ;; +     더 긴 설명
  ;; M-w   kill-ring 복사 → C-y로 붙여넣기
  ;; M-RET 채팅 버퍼로 이어서 질문
  ;; C-g   닫기

  ;; gptel-quick: 빠른 모델 (echo area 즉답)
  (setq gptel-quick-backend gptel-openai-sub-backend)
  (setq gptel-quick-model my/gptel-model-fast)
  (setq gptel-quick-word-count 30) ; 기본 12 → 30 (한글 ~15자 분량)
  (setq gptel-quick-timeout 15)    ; 기본 10 → 15초
  (setq gptel-quick-display nil)   ; use echo area
  (setq gptel-quick-system-message
        (lambda (count)
          (format
           "한국어로 %d단어 이내로 간결하게 설명하라. \
코드라면 기능을, 영어라면 뜻을, 에러라면 원인과 해결을 말하라. \
복사해서 바로 쓸 수 있게 핵심만."
           count)))

  ;; gptel-quick 내부 let*이 gptel-use-curl을 nil로 강제함.
  ;; url.el은 한글 등 multibyte body에서 "Multibyte text in HTTP request" 에러 발생.
  ;; :around advice로는 내부 let*에 덮이므로 :override로 curl을 강제한다.
  (advice-add 'gptel-quick :override
              (lambda (query-text &optional count)
                "gptel-quick override: curl 강제 + 한글 multibyte 대응."
                (interactive
                 (list (cond
                        ((use-region-p) (buffer-substring-no-properties
                                         (region-beginning) (region-end)))
                        ((and (derived-mode-p 'pdf-view-mode)
                              (fboundp 'pdf-view-active-region-p)
                              (pdf-view-active-region-p))
                         (mapconcat #'identity (pdf-view-active-region-text) "\n\n"))
                        (t (thing-at-point 'sexp)))
                       current-prefix-arg))
                (when (xor gptel-quick-backend gptel-quick-model)
                  (error "gptel-quick-backend and gptel-quick-model must be both set or unset"))
                (let* ((count (or count gptel-quick-word-count))
                       (gptel-backend (or gptel-quick-backend gptel-backend))
                       (gptel-model (or gptel-quick-model gptel-model))
                       ;; Codex(OAuth) endpoint은 max_output_tokens 거부 — nil로 패스.
                       ;; 다른 백엔드는 종래대로 단어 수 기반 cap 적용.
                       (gptel-max-tokens
                        (unless (eq (type-of gptel-backend) 'gptel-openai-oauth)
                          (floor (+ (sqrt (length query-text))
                                    (* count 2.5)))))
                       (gptel-use-curl t)  ; ← 핵심: url.el 대신 curl 강제
                       (gptel-use-context (and gptel-quick-use-context 'system)))
                  (gptel-request query-text
                    :system (funcall gptel-quick-system-message count)
                    :context (list query-text count
                                   (posn-at-point (and (use-region-p) (region-beginning))))
                    :callback #'gptel-quick--callback-posframe))))

;;;;; 세션 관리

;;;;;; 프롬프트 선택

  (defun my/gptel-select-prompt ()
    "현재 gptel 버퍼에서 시스템 프롬프트를 선택하여 변경."
    (interactive)
    (let* ((prompts `(("기본 (General)" . ,user-llm-system-prompt)
                      ("요약 (Summarize)" . ,+gptel-summarize-system-message)
                      ("번역 (Translate)" . ,+gptel-translate-system-message)))
           (choice (completing-read "프롬프트: " (mapcar #'car prompts) nil t))
           (prompt (cdr (assoc choice prompts))))
      (setq-local gptel--system-message prompt)
      (message "프롬프트 변경: %s" choice)))

;;;;;; Denote 메타데이터로 저장

  (defun gptel-save-as-org-with-denote-metadata ()
    "Save buffer to disk when starting gptel with metadata."
    (interactive)
    (unless (buffer-file-name (current-buffer))
      (let* ((suffix (format-time-string "%Y%m%dT%H%M%S"))
             (chat-dir (concat org-directory "/llmlog"))
             (ext (replace-regexp-in-string "-mode$" "" (symbol-name gptel-default-mode)))
             (filename (concat suffix "__llmlog" "." ext))
             (full-path (expand-file-name filename chat-dir)))
        (unless (file-directory-p chat-dir)
          (make-directory chat-dir :parents))
        (write-file full-path)

        ;; Add metadata to the file
        (goto-char 0) (search-forward ":END:") (end-of-line)
        (insert (format "\n#+title: #LLM: %s\n" suffix))
        (insert "#+filetags: :llmlog:\n")
        (insert (format "#+hugo_lastmod: %s\n" (format-time-string "[%Y-%m-%d]")))
        (insert (format "#+date: %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "#+identifier: %s\n" suffix))
        (insert (format "#+export_file_name: %s.md\n" suffix))
        (insert (format "#+description: %s\n" suffix))
        (insert (format "#+hugo_categories: Noname\n"))

        (insert (format "\n* 히스토리\n- %s Created!" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "\n* 관련메타\n- \n#+print_bibliography:\n\n"))

        ;; heading-1 add backlink to today
        (insert (format "* 로그 :LLMLOG:\n** [[denote:%s::#%s][%s]]\n"
                        (format-time-string "%Y%m%dT000000"
                                            (org-journal--convert-time-to-file-type-time
                                             (time-subtract (current-time)
                                                            (* 3600 org-extend-today-until))))
                        (downcase (format-time-string "%Y-%m-%d-%a"))
                        (format-time-string "|%Y-%m-%d %a %H:%M|")))
        (insert "\n"))))

;;;;;; 버퍼 초기화 (히스토리 청소)

  ;; 채팅 버퍼에서 마지막 질문만 남기고 이전 대화를 모두 지운다.
  ;; 토큰 절약하면서 맥락을 이어가고 싶을 때. 키바인딩: M-l
  (defun gptel-clear-buffer+ ()
    "Clear gptel buffer, keeping only the last prompt."
    (interactive)
    (let* ((beg-marker (concat "^" (alist-get gptel-default-mode gptel-prompt-prefix-alist)))
           (keep-line (save-excursion
                        (goto-char (point-max))
                        (when (re-search-backward beg-marker nil t)
                          (unless (save-excursion
                                    (forward-line)
                                    (re-search-forward beg-marker nil t))
                            (point))))))
      (delete-region (point-min) keep-line)
      (evil-insert-state)))

;;;;; gptel-prompt (prompt-poet)

  ;; git@github.com:character-ai/prompt-poet.git
  (when (locate-library "gptel-prompt")
    (require 'gptel-prompts)
    (use-package! uuidgen)
    (when (file-exists-p (concat org-directory "resources/prompts/"))
      (setq gptel-prompts-directory (concat org-directory "resources/prompts/"))
      (gptel-prompts-update)
      ;; Ensure prompts are updated if prompt files change
      (gptel-prompts-add-update-watchers)))

;;;;; gptel-mode Hook & 키바인딩

  (add-hook! 'gptel-mode-hook
    (defun cae-gptel-mode-setup-h ()
      (auto-fill-mode -1)
      (doom-mark-buffer-as-real-h)))

  (map! :map gptel-mode-map
        "M-<return>" #'gptel-send
        "M-RET" #'gptel-send
        (:localleader
         "RET" #'gptel-mode
         "TAB" #'gptel-menu
         "M-s" #'gptel-save-as-org-with-denote-metadata
         "M-l" #'gptel-clear-buffer+
         (:prefix "s"
          :desc "clear" "l" #'gptel-clear-buffer+
          "p" #'gptel-save-as-org-with-denote-metadata
          )))

  ) ; end of use-package! gptel

;;;; gptel-agent — agents + skills 인프라 (karthink 제작)

;; ~/.claude/skills/ 는 `gptel-agent-skill-dirs` 기본값에 이미 포함.
;; `gptel-agent-update` 가 frontmatter (description) 만 metadata-only 로딩 →
;; LLM 의 system message 에 `<available_skills>` XML 로 노출 (opencode 패턴).
;; LLM 이 `Skill` tool 호출 시점에 SKILL.md 본문 lazy 로드.
;;
;; 기본 16 tool 등록: Agent / TodoWrite / Glob / Grep / Read / Insert / Edit /
;; Write / Mkdir / Eval / Bash / WebSearch / WebFetch / YouTube / Skill.
;; Bash/Write/Edit 등 위험 tool 은 confirmation default.
;;
;; 기본 5 agent preset: gptel-agent / gptel-plan / executor / introspector / researcher.
;; `M-x gptel-menu` → preset 선택으로 활성화.
;;
;; OpenAI-sub backend (Codex Responses API) + Agent/subagent 호출은 issue #107
;; "Stream must be set to true" 에 부딪힘. 우회는 default `agents/gptel-agent.md`
;; 의 DELEGATE/Agent 가이드를 우리 `agents/gptel-agent.md` (subagent-free 변형)
;; 로 덮어쓰는 방식으로 해결 — LLM 이 subagent 자체를 모름.
;; `gptel-agent-dirs` 에 우리 dir 을 append 하면 `gptel-agent--update-agents`
;; 의 `setf (alist-get name ... equal)` 덕분에 같은 이름 (`gptel-agent`) entry
;; 가 자연스럽게 교체됨. frontmatter `tools:` 리스트에서 `Agent` 도 빠져있어
;; preset 활성 시 tool slot 도 비활성 — system prompt + tool slot 두 자리에서
;; 모두 차단.
(use-package! gptel-agent
  :defer t
  :config
  ;; Tool confirmation 일괄 비활성 — 신뢰 환경에서 prompt fatigue 제거.
  ;; gptel-agent 의 default tool 중 Write/Edit/Bash/Mkdir/Eval/Insert 가
  ;; `:confirm t` 박혀있어 global setq 만으로는 못 끔 — tool object 의
  ;; :confirm 자체를 nil 로 강제. 위험 tool 도 무조건 실행.
  (defun +gptel--disable-tool-confirmations ()
    "Strip `:confirm' on all gptel tools."
    (dolist (category gptel--known-tools)
      (dolist (tool-cell (cdr category))
        (let ((tool (cdr tool-cell)))
          (when (gptel-tool-p tool)
            (setf (gptel-tool-confirm tool) nil))))))

  ;; 사용자 정체/환경 prompt 를 각 agent 의 :system 앞에 prepend.
  ;; agents/*.md 의 system prompt 가 user-llm-system-prompt 를 대체하던 자리.
  ;; gptel-agent-update 가 매번 agents 를 reset 하므로 advice 가 누적되지 않음.
  ;; preset 재생성으로 chat buffer 다음 활성화 시점에 반영.
  ;; (agzam 패턴: dotsamples/doom/agzam-dot-doom/.../config.el)
  (defun +gptel-agent--inject-user-prompt (&rest _)
    "Prepend `user-llm-system-prompt' to each agent's :system + re-create presets."
    (when (and (boundp 'user-llm-system-prompt) user-llm-system-prompt)
      (dolist (entry gptel-agent--agents)
        (when-let* ((system (plist-get (cdr entry) :system)))
          (plist-put (cdr entry) :system
                     (concat user-llm-system-prompt "\n\n" system))))
      (dolist (preset-name '("gptel-agent" "gptel-plan"))
        (when-let* ((p (assoc-default preset-name gptel-agent--agents nil nil)))
          (apply #'gptel-make-preset (intern preset-name) p)))))

  ;; Append our prompts/ dir AFTER the package default so our
  ;; `gptel-agent.md' (subagent-free variant) overrides the upstream one in
  ;; `gptel-agent--update-agents'.
  (add-to-list 'gptel-agent-dirs
               (expand-file-name "prompts/" doom-user-dir)
               t)

  (setq gptel-confirm-tool-calls nil)
  (advice-add 'gptel-agent-update :after #'+gptel--disable-tool-confirmations)
  (advice-add 'gptel-agent-update :after #'+gptel-agent--inject-user-prompt)
  (gptel-agent-update))

;;;; 버퍼 요약/번역 (+gptel-buffer)

(after! gptel

;;;;; 변수 & 시스템 프롬프트

  (defvar +gptel-buffer-name "*gptel-buffer*"
    "gptel 버퍼 요약/번역용 사이드 버퍼 이름.")

  (defvar +gptel-summarize-system-message
    "You are a helpful reading assistant. Generate a concise TLDR summary.
- Cover the main points clearly
- Use bullet points for key takeaways
- Respond in Korean"
    "버퍼 요약용 시스템 프롬프트.")

  (defvar +gptel-translate-system-message
    "You are a professional translator. Translate the following text naturally to Korean.
- Maintain the original meaning and tone
- Use natural Korean expressions
- Preserve technical terms when appropriate"
    "버퍼 번역용 시스템 프롬프트 (영→한 방향).")

  (defvar +gptel-translate-bidirectional-system-message
    "You are a professional translator working in both directions.
- If the input is Korean, translate to natural English.
- If the input is English, translate to natural Korean.
- If the input is mixed, translate the dominant language to the other.
- Preserve formatting, line breaks, and punctuation.
- Keep technical terms, URLs, code, and proper nouns as-is.
- Output ONLY the translated text — no labels, no explanations, no quotes."
    "Region 번역용 양방향 프롬프트 (SNS·Emacs Everywhere 워크플로).")

  ;; Temperature 가이드:
  ;; | 작업 | 권장 온도 | 이유                      |
  ;; |------|----------|---------------------------|
  ;; | 번역 | 0.1-0.2  | 원문 충실, 일관성          |
  ;; | 요약 | 0.3-0.5  | 핵심 추출 + 약간 재구성    |
  ;; | 창작 | 0.7-1.0  | 다양성, 창의성             |

  (defvar +gptel-translate-temperature 0.1
    "번역용 낮은 temperature - 정확성과 일관성 우선.")

  (defvar +gptel-summarize-temperature 0.4
    "요약용 중간 temperature - 핵심 추출 + 약간의 재구성 허용.")

  (defvar +gptel-buffer-backend nil
    "요약/번역 전용 백엔드. nil이면 `gptel-openai-sub-backend' 사용.")

  (defvar +gptel-buffer-model my/gptel-model-default
    "요약/번역 전용 모델. 긴 컨텍스트 지원 필요.")

  (defun my/gptel-buffer-model-toggle ()
    "Toggle `+gptel-buffer-model' between the default and heavy model."
    (interactive)
    (setq +gptel-buffer-model
          (if (eq +gptel-buffer-model my/gptel-model-default)
              my/gptel-model-heavy
            my/gptel-model-default))
    (message "gptel-buffer 모델: %s" +gptel-buffer-model))

;;;;; 컨텐츠 추출 (Content Extractors)

  (defun +gptel--extract-eww-content ()
    "eww 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'eww-mode)
      (list :title (plist-get eww-data :title)
            :url (plist-get eww-data :url)
            :text (buffer-string))))

  (defun +gptel--extract-elfeed-content ()
    "elfeed-show 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'elfeed-show-mode)
      (let* ((entry elfeed-show-entry)
             (feed (elfeed-entry-feed entry))
             (authors (elfeed-meta entry :authors)))
        (list :title (elfeed-entry-title entry)
              :url (elfeed-entry-link entry)
              :feed (elfeed-feed-title feed)
              :date (format-time-string "%Y-%m-%d %H:%M"
                                        (seconds-to-time (elfeed-entry-date entry)))
              :authors (when authors
                         (mapconcat (lambda (a) (plist-get a :name)) authors ", "))
              :text (buffer-string)))))

  (defun +gptel--extract-pdf-content ()
    "pdf-view 버퍼에서 선택 영역 또는 현재 페이지 텍스트 추출."
    (when (derived-mode-p 'pdf-view-mode)
      (list :title (file-name-nondirectory (buffer-file-name))
            :url (buffer-file-name)
            :text (if (pdf-view-active-region-p)
                      (mapconcat #'identity (pdf-view-active-region-text) "\n\n")
                    (pdf-info-gettext (pdf-view-current-page))))))

  (defun +gptel--extract-nov-content ()
    "nov.el (epub) 버퍼에서 컨텐츠 추출."
    (when (derived-mode-p 'nov-mode)
      (list :title (or (alist-get 'title nov-metadata) "Unknown")
            :url (nov-content-unique-identifier)
            :text (buffer-string))))

  (defun +gptel--extract-buffer-content ()
    "현재 버퍼에서 LLM용 컨텐츠 추출. plist 반환."
    (or (+gptel--extract-eww-content)
        (+gptel--extract-elfeed-content)
        (+gptel--extract-pdf-content)
        (+gptel--extract-nov-content)
        ;; 기본: 일반 버퍼
        (list :title (buffer-name)
              :url (or (buffer-file-name) default-directory)
              :text (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (buffer-string)))))

  (defun +gptel--format-content-for-llm (content)
    "CONTENT plist를 LLM 프롬프트 형식으로 변환."
    (let ((title (plist-get content :title))
          (url (plist-get content :url))
          (feed (plist-get content :feed))
          (date (plist-get content :date))
          (authors (plist-get content :authors))
          (text (plist-get content :text)))
      (concat
       "Article Metadata:\n"
       (when title (format "- Title: %s\n" title))
       (when url (format "- URL: %s\n" url))
       (when feed (format "- Feed: %s\n" feed))
       (when date (format "- Date: %s\n" date))
       (when authors (format "- Authors: %s\n" authors))
       "\nContent:\n"
       "```\n"
       (string-trim text)
       "\n```")))

;;;;; 핵심 함수 — +gptel--send-to-buffer

  (defun +gptel--send-to-buffer (content system-message action-name &optional temperature)
    "CONTENT를 gptel 사이드 버퍼로 보내고 SYSTEM-MESSAGE로 요청.
ACTION-NAME은 표시용 (예: \"요약\", \"번역\").
TEMPERATURE는 선택적 온도 설정 (nil이면 전역값 사용)."
    (let* ((formatted (+gptel--format-content-for-llm content))
           (buf (get-buffer-create +gptel-buffer-name))
           (target-backend (or +gptel-buffer-backend gptel-openai-sub-backend))
           (target-model +gptel-buffer-model))
      (with-current-buffer buf
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (unless gptel-mode
          (gptel-mode 1))
        (setq-local gptel-org-branching-context nil)
        (setq-local gptel-backend target-backend)
        (setq-local gptel-model target-model)
        (setq-local gptel--system-message system-message)
        (when temperature
          (setq-local gptel-temperature temperature))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (format "@user: [%s 요청]\n\n%s" action-name formatted))
        (insert "\n\n@assistant:\n"))
      (display-buffer buf '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 0.4)))
      (with-current-buffer buf
        (goto-char (point-max))
        (gptel-send))
      (message "%s 요청 [%s] (temp=%.1f) → %s"
               action-name target-model
               (or temperature gptel-temperature) +gptel-buffer-name)))

;;;;; 명령 — 요약, 번역, DWIM

;;;###autoload
  (defun +gptel-summarize-buffer ()
    "현재 버퍼 내용을 요약하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
    (interactive)
    (let ((content (+gptel--extract-buffer-content)))
      (+gptel--send-to-buffer content +gptel-summarize-system-message
                              "요약" +gptel-summarize-temperature)))

;;;###autoload
  (defun +gptel-translate-buffer ()
    "현재 버퍼 내용을 번역하여 gptel 사이드 버퍼에 표시.
eww, elfeed, pdf-view, nov 등 다양한 모드 지원."
    (interactive)
    (let ((content (+gptel--extract-buffer-content)))
      (+gptel--send-to-buffer content +gptel-translate-system-message
                              "번역" +gptel-translate-temperature)))

;;;###autoload
  (defun +gptel-buffer-dwim ()
    "현재 버퍼에 대해 gptel 액션 선택 (요약/번역)."
    (interactive)
    (let ((action (completing-read "Action: " '("요약 (Summarize)" "번역 (Translate)"))))
      (pcase action
        ("요약 (Summarize)" (+gptel-summarize-buffer))
        ("번역 (Translate)" (+gptel-translate-buffer)))))

;;;;; 키바인딩 — 전역 & 모드별

  ;; 전역 (SPC = g)
  (map! :leader
        (:prefix "="
                 (:prefix "g"
                  :desc "Summarize buffer" "s" #'+gptel-summarize-buffer
                  :desc "Translate buffer" "t" #'+gptel-translate-buffer
                  :desc "DWIM (choose action)" "g" #'+gptel-buffer-dwim)))

  ;; eww
  (after! eww
    (map! :map eww-mode-map
          :localleader
          (:prefix "G"
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; elfeed
  (after! elfeed
    (map! :map elfeed-show-mode-map
          :localleader
          (:prefix "G"
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; pdf-view
  (after! pdf-tools
    (map! :map pdf-view-mode-map
          :localleader
          (:prefix "G"
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; nov (epub)
  (after! nov
    (map! :map nov-mode-map
          :localleader
          (:prefix "G"
           :desc "Summarize" "s" #'+gptel-summarize-buffer
           :desc "Translate" "t" #'+gptel-translate-buffer)))

  ;; org-mode
  (after! org
    (map! :map org-mode-map
          :localleader
          "RET" #'gptel-mode
          "1" #'gptel-menu
          "5" #'my/gptel-org-toggle-branching-context
          "TAB" #'gptel-menu
          "M-s" #'gptel-save-as-org-with-denote-metadata
          "M-l" #'gptel-clear-buffer+
          ))

  ) ; end of after! gptel — 버퍼 요약/번역

;;;; Embark + gptel 통합

;; 코드 파일에서 영역 선택 → M-o (embark) → prompt 선택 → gptel 전달
;; Claude Code 호출 없이 gptel로 빠른 수정/주석화
;;
;; [embark-region-map 키바인딩] (keybindings-config.el)
;; | Key | 함수                            | 설명           |
;; |-----|---------------------------------|----------------|
;; | p   | my/gptel-apply-prompt-to-region | prompt 파일 선택 |
;; | [   | my/gptel-quick-region           | 빠른 질의      |
;; | t   | my/gptel-translate-region       | 번역 (한↔영)   |
;; | s   | my/gptel-summarize-region       | 요약           |
;; | e   | my/gptel-explain-region         | 코드 설명      |
;; | r   | my/gptel-rewrite-region         | 재작성         |
;;
;; [embark-file-map 키바인딩] (keybindings-config.el)
;; | Key | 함수                            | 설명                    |
;; |-----|---------------------------------|-------------------------|
;; | p   | my/gptel-apply-prompt-to-file   | prompt 파일 선택        |
;; | t   | my/gptel-translate-file         | immersive-translate 번역|
;; | s   | my/gptel-summarize-file         | 파일 요약               |
;;
;; [프롬프트 디렉토리]
;; ~/sync/org/resources/prompts/
;; - immersive-translate.poet : 문단별 번역 (원문 형식 유지)
;; - instant-korean.poet      : 즉시 한국어 번역
;; - instant-english.poet     : 즉시 영어 번역
;; - code-review-ko.poet      : 코드 리뷰 (한국어)
;; - summarize.md             : 요약

(after! gptel

;;;;; 프롬프트 선택 함수

  (defun my/gptel--list-prompts ()
    "gptel-prompts-directory에서 프롬프트 파일 목록 반환."
    (when (and (boundp 'gptel-prompts-directory)
               (file-directory-p gptel-prompts-directory))
      (directory-files gptel-prompts-directory nil
                       "\\.\\(org\\|md\\|txt\\|poet\\)$")))

  (defun my/gptel--read-prompt-file (filename)
    "FILENAME에서 프롬프트 내용 읽기."
    (let ((filepath (expand-file-name filename gptel-prompts-directory)))
      (when (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (buffer-string)))))

;;;;; embark-region용 함수

  (defun my/gptel-apply-prompt-to-region (beg end)
    "선택 영역에 gptel prompt 적용.
프롬프트 파일 선택 후 선택된 텍스트와 함께 gptel에 전달."
    (interactive "r")
    (let* ((text (buffer-substring-no-properties beg end))
           (prompts (my/gptel--list-prompts))
           (prompt-file (completing-read "Prompt: " prompts nil t))
           (prompt-content (my/gptel--read-prompt-file prompt-file))
           (full-prompt (format "%s\n\n---\n\n%s" prompt-content text)))
      (if prompt-content
          (progn
            (gptel full-prompt)
            (message "gptel에 전달: %s" prompt-file))
        (user-error "프롬프트 파일을 읽을 수 없습니다: %s" prompt-file))))

  (defun my/gptel-quick-region (beg end)
    "선택 영역으로 gptel-quick 호출."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-quick text)))

  (defun my/gptel--region-or-paragraph-bounds ()
    "활성 region이 있으면 그 bounds, 없으면 현재 문단 bounds를 반환.
반환: (BEG . END). 없으면 user-error."
    (cond
     ((use-region-p)
      (cons (region-beginning) (region-end)))
     ((bounds-of-thing-at-point 'paragraph))
     (t (user-error "No region or paragraph at point"))))

  (defun my/gptel-translate-region (beg end)
    "선택 영역(또는 현재 문단)을 한↔영 양방향 번역하여
*gptel-translate* 버퍼에 표시. 긴 글 검토용.
SNS·Emacs Everywhere 즉시 변환은 `my/gptel-translate-region-inline' 참고."
    (interactive (let ((b (my/gptel--region-or-paragraph-bounds)))
                   (list (car b) (cdr b))))
    (let ((text (buffer-substring-no-properties beg end))
          (gptel-backend gptel-openai-sub-backend)
          (gptel-model my/gptel-model-fast))
      (message "번역 중... (%s)" my/gptel-model-fast)
      (gptel-request text
        :system +gptel-translate-bidirectional-system-message
        :callback (lambda (response info)
                    (if response
                        (with-current-buffer (get-buffer-create "*gptel-translate*")
                          (erase-buffer)
                          (insert response)
                          (display-buffer (current-buffer))
                          (message "번역 완료"))
                      (message "번역 실패: %s" (plist-get info :status)))))))

  (defun my/gptel-translate-region-inline (beg end &optional arg)
    "선택 영역(또는 현재 문단)을 한↔영 번역하여 결과를 그 자리에 표시.
기본: 영역 바로 아래에 번역문 삽입 (대화·메모 워크플로).
C-u: 영역을 번역 결과로 교체 (Emacs Everywhere SNS 글 즉시 변환).
모델은 `my/gptel-model-fast' 고정 (속도 우선)."
    (interactive (let ((b (my/gptel--region-or-paragraph-bounds)))
                   (list (car b) (cdr b) current-prefix-arg)))
    (let ((text (buffer-substring-no-properties beg end))
          (gptel-backend gptel-openai-sub-backend)
          (gptel-model my/gptel-model-fast)
          (replace-mode (and arg t))
          (target-buf (current-buffer))
          (insert-pos (copy-marker end t)))
      (message "번역 중... (%s%s)" my/gptel-model-fast
               (if replace-mode " · 교체" " · 아래 삽입"))
      (gptel-request text
        :system +gptel-translate-bidirectional-system-message
        :callback
        (lambda (response info)
          (cond
           ((not response)
            (message "번역 실패: %s" (plist-get info :status)))
           ((buffer-live-p target-buf)
            (with-current-buffer target-buf
              (save-excursion
                (if replace-mode
                    (progn
                      (delete-region beg end)
                      (goto-char beg)
                      (insert response))
                  (goto-char insert-pos)
                  (unless (bolp) (insert "\n"))
                  (insert response)
                  (unless (eolp) (insert "\n")))))
            (message "번역 완료")))))))

  (defun my/gptel-summarize-region (beg end)
    "선택 영역 요약."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
          (format "다음 텍스트를 간결하게 요약해주세요:\n\n%s" text)
        :system +gptel-summarize-system-message
        :callback (lambda (response info)
                    (if response
                        (with-current-buffer (get-buffer-create "*gptel-summary*")
                          (erase-buffer)
                          (insert response)
                          (display-buffer (current-buffer)))
                      (message "요약 실패: %s" (plist-get info :status)))))))

  (defun my/gptel-explain-region (beg end)
    "선택 영역 (코드) 설명."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
          (format "다음 코드/텍스트를 설명해주세요:\n\n```\n%s\n```" text)
        :callback (lambda (response info)
                    (if response
                        (with-current-buffer (get-buffer-create "*gptel-explain*")
                          (erase-buffer)
                          (insert response)
                          (display-buffer (current-buffer)))
                      (message "설명 실패: %s" (plist-get info :status)))))))

  (defun my/gptel-rewrite-region (beg end)
    "선택 영역 재작성/개선."
    (interactive "r")
    (let ((text (buffer-substring-no-properties beg end)))
      (gptel-request
          (format "다음 텍스트를 더 명확하고 간결하게 재작성해주세요:\n\n%s" text)
        :callback (lambda (response info)
                    (if response
                        (with-current-buffer (get-buffer-create "*gptel-rewrite*")
                          (erase-buffer)
                          (insert response)
                          (display-buffer (current-buffer)))
                      (message "재작성 실패: %s" (plist-get info :status)))))))

;;;;; embark-file용 함수

  (defun my/gptel-apply-prompt-to-file (file)
    "FILE에 gptel prompt 적용.
프롬프트 파일 선택 후 파일 내용과 함께 gptel에 전달."
    (interactive "fFile: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (prompts (my/gptel--list-prompts))
           (prompt-file (completing-read "Prompt: " prompts nil t))
           (prompt-content (my/gptel--read-prompt-file prompt-file))
           (full-prompt (format "%s\n\n---\n\n%s" prompt-content content)))
      (if prompt-content
          (progn
            (gptel full-prompt)
            (message "gptel에 전달: %s → %s" (file-name-nondirectory file) prompt-file))
        (user-error "프롬프트 파일을 읽을 수 없습니다: %s" prompt-file))))

  (defun my/gptel-translate-file (file)
    "FILE을 immersive-translate 프롬프트로 번역.
~/sync/org/resources/prompts/immersive-translate.poet 사용."
    (interactive "fFile to translate: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (prompt-file (expand-file-name "immersive-translate.poet"
                                          gptel-prompts-directory))
           (output-buffer (get-buffer-create
                           (format "*gptel-translate: %s*"
                                   (file-name-nondirectory file)))))
      (if (file-exists-p prompt-file)
          (progn
            (message "번역 중: %s..." (file-name-nondirectory file))
            (gptel-request
                content
              :system (with-temp-buffer
                        (insert-file-contents prompt-file)
                        (goto-char (point-min))
                        (if (re-search-forward "role: system\\s-*\n\\s-*content:\\s-*>-?\\s-*\n" nil t)
                            (let ((start (point)))
                              (if (re-search-forward "^- name:" nil t)
                                  (buffer-substring-no-properties start (match-beginning 0))
                                (buffer-substring-no-properties start (point-max))))
                          (buffer-string)))
              :callback (lambda (response info)
                          (if response
                              (progn
                                (with-current-buffer output-buffer
                                  (erase-buffer)
                                  (insert response)
                                  (goto-char (point-min)))
                                (display-buffer output-buffer)
                                (message "번역 완료: %s" (file-name-nondirectory file)))
                            (message "번역 실패: %s" (plist-get info :status))))))
        (user-error "immersive-translate.poet 파일이 없습니다: %s" prompt-file))))

  (defun my/gptel-summarize-file (file)
    "FILE 내용 요약."
    (interactive "fFile to summarize: ")
    (let* ((content (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))
           (output-buffer (get-buffer-create
                           (format "*gptel-summary: %s*"
                                   (file-name-nondirectory file)))))
      (message "요약 중: %s..." (file-name-nondirectory file))
      (gptel-request
          (format "다음 파일 내용을 간결하게 요약해주세요:\n\n%s" content)
        :system +gptel-summarize-system-message
        :callback (lambda (response info)
                    (if response
                        (progn
                          (with-current-buffer output-buffer
                            (erase-buffer)
                            (insert response)
                            (goto-char (point-min)))
                          (display-buffer output-buffer)
                          (message "요약 완료: %s" (file-name-nondirectory file)))
                      (message "요약 실패: %s" (plist-get info :status)))))))

  ) ; end of after! gptel — embark 통합

;;;; 알림 (Peon-ping)

(defvar my/gptel-peon-ping-script
  (expand-file-name "~/.claude/hooks/peon-ping/peon.sh")
  "peon-ping 스크립트 경로.")

(defun my/gptel-peon-ping-notify (_beg _end)
  "GPTel 응답 완료 시 peon-ping 사운드를 재생한다."
  (when (file-executable-p my/gptel-peon-ping-script)
    (let* ((proc (start-process "gptel-peon-ping" nil
                                my/gptel-peon-ping-script)))
      (process-send-string proc "{\"hook_event_name\":\"Stop\"}\n")
      (process-send-eof proc))))

;;;; Gemini 이미지 생성

;; gptel 의존성 없음 — url.el + json.el 만 사용
;;
;; [사용법]
;;   M-x my/gemini-generate-image       → 프롬프트 입력, 비율 기본 16:9
;;   C-u M-x my/gemini-generate-image   → 비율 선택 메뉴 포함
;;
;; [저장 위치] ~/screenshot/YYYYMMDDTHHMMSS--슬러그__brand_nanobanana.png
;; [환경변수]  GEMINI_API_KEY  (~/.env.local 또는 export로 직접 설정)

(defconst my/gemini-image-model "gemini-3.1-flash-image-preview"
  "Gemini 이미지 생성 기본 모델. pi extension과 동일.")

(defconst my/gemini-aspect-ratios
  '("1:1" "16:9" "9:16" "4:3" "3:4" "3:2" "2:3")
  "Gemini 이미지 생성 지원 비율 목록.")

;;;;;; 헬퍼 함수

(defun my/gemini--get-api-key ()
  "GEMINI_API_KEY 반환. password-store 우선, 환경변수 폴백."
  (or (ignore-errors (password-store-get "api/google/gemini-junghanacs"))
      (getenv "GEMINI_API_KEY")))

(defun my/gemini--kst-timestamp ()
  "현재 KST 타임스탬프 반환 (Denote 형식: YYYYMMDDTHHMMSS).
KST = UTC+9: 현재 시각에 9시간 더해 UTC 포맷으로 출력."
  (format-time-string "%Y%m%dT%H%M%S"
                      (time-add (current-time) (* 9 3600))
                      t))

(defun my/gemini--slugify (text &optional max-len)
  "TEXT를 파일명용 슬러그로 변환. 최대 MAX-LEN자 (기본 30).
한글+영숫자 허용 (Emacs에서 한글 프롬프트 사용하므로).
Denote 파일명 규칙: YYYYMMDDTHHMMSS--한글-제목__태그.org"
  (let* ((n (or max-len 30))
         (s (downcase text)))
    (setq s (replace-regexp-in-string "[^가-힣a-z0-9 ]" " " s))
    (setq s (replace-regexp-in-string " +"              "-" s))
    (setq s (replace-regexp-in-string "-+"              "-" s))
    (setq s (replace-regexp-in-string "^-"              ""  s))
    (when (> (length s) n)
      (setq s (substring s 0 n)))
    (setq s (replace-regexp-in-string "-$" "" s))
    (if (string-empty-p s) "generated" s)))

(defun my/gemini--mime-to-ext (mime-type)
  "MIME-TYPE 문자열에서 파일 확장자 반환."
  (let ((m (or mime-type "")))
    (cond
     ((string-match-p "jpeg\\|jpg" m) "jpg")
     ((string-match-p "webp"       m) "webp")
     ((string-match-p "gif"        m) "gif")
     (t                               "png"))))

;;;;;; 핵심 함수

(defun my/gemini-generate-image (prompt &optional aspect-ratio)
  "Gemini API로 이미지 생성, ~/screenshot/에 저장, 현재 위치에 org 링크 삽입.

PROMPT: 생성할 이미지 설명.
ASPECT-RATIO: 화면 비율 (기본 \"16:9\"). nil이면 기본값 사용.
prefix arg (C-u): 비율 선택 메뉴를 표시한다.

저장 파일명: YYYYMMDDTHHMMSS--슬러그__brand_nanobanana.png  (Denote 형식, KST)
환경변수: GEMINI_API_KEY  (~/.env.local 또는 export로 설정)

검증:
  (my/gemini-generate-image \"a cute penguin\" \"16:9\")"
  (interactive
   (let* ((p (read-string "Prompt: "))
          (r (if current-prefix-arg
                 (completing-read "Aspect ratio: " my/gemini-aspect-ratios
                                  nil t nil nil "16:9")
               "16:9")))
     (list p r)))
  (require 'url)
  (require 'json)
  (let ((orig-buf (current-buffer))
        (orig-pos (point)))

    ;; ── 1. 입력 검증 ──────────────────────────────────────────────────────────
    (when (string-empty-p (string-trim (or prompt "")))
      (user-error "프롬프트가 비어있습니다"))

    (let ((api-key (my/gemini--get-api-key)))
    (unless api-key
      (user-error "GEMINI_API_KEY가 없습니다. ~/.env.local에 GEMINI_API_KEY=... 를 추가하세요"))

    ;; ── 2. 요청 구성 ──────────────────────────────────────────────────────────
    (let* ((ratio   (or aspect-ratio "16:9"))
           (model   my/gemini-image-model)
           (api-url (format
                     "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
                     model api-key))
           (body    (json-encode
                     `((contents
                        . [((role  . "user")
                            (parts . [((text . ,prompt))]))])
                       (generationConfig
                        . ((responseModalities . ["TEXT" "IMAGE"])
                           (imageConfig
                            . ((aspectRatio . ,ratio))))))))
           (url-request-method         "POST")
           (url-request-extra-headers  '(("Content-Type" . "application/json")))
           (url-request-data           (encode-coding-string body 'utf-8))
           (url-show-status            nil))

      (message "Gemini 이미지 생성 중... (모델: %s, 비율: %s)" model ratio)

      ;; ── 3. HTTP 요청 ──────────────────────────────────────────────────────────
      (let ((resp-buf
             (condition-case err
                 (url-retrieve-synchronously api-url t nil 60)
               (error (user-error "HTTP 요청 실패: %s" (error-message-string err))))))
        (unless resp-buf
          (user-error "응답 없음: 타임아웃 또는 네트워크 문제"))

        (unwind-protect
            (with-current-buffer resp-buf
              (goto-char (point-min))

              ;; HTTP 상태코드 확인 (첫 번째 줄에서 200 여부 검사)
              (let ((status (buffer-substring-no-properties
                             (point-min) (line-end-position))))
                (unless (string-match-p "200" status)
                  (re-search-forward "^\r?\n" nil t)
                  (let ((err-body (buffer-substring-no-properties (point) (point-max))))
                    (user-error "HTTP 에러: %s\n%s"
                                status
                                (substring err-body 0 (min 300 (length err-body)))))))

              ;; 헤더 건너뛰기 → JSON 본문으로 이동
              (goto-char (point-min))
              (re-search-forward "^\r?\n" nil t)

              ;; ── 4. JSON 파싱 ────────────────────────────────────────────────
              (let* ((json-object-type  'alist)
                     (json-array-type   'vector)
                     (json-key-type     'symbol)
                     (data              (json-read))
                     (api-err           (alist-get 'error          data))
                     (feedback          (alist-get 'promptFeedback data))
                     (block-reason      (and feedback
                                             (alist-get 'blockReason feedback))))

                ;; API 레벨 에러 (200 응답이지만 error 필드 있는 경우)
                (when api-err
                  (user-error "Gemini API 에러 [%s]: %s"
                              (or (alist-get 'code    api-err) "?")
                              (or (alist-get 'message api-err) "알 수 없는 에러")))

                ;; 안전 정책 차단 (promptFeedback.blockReason)
                (when block-reason
                  (user-error "프롬프트 차단됨: %s  %s"
                              block-reason
                              (or (alist-get 'blockReasonMessage feedback) "")))

                ;; ── 5. parts 추출 ──────────────────────────────────────────────
                (let* ((candidates    (alist-get 'candidates data))
                       (candidate     (and (vectorp candidates)
                                           (> (length candidates) 0)
                                           (aref candidates 0)))
                       (content-node  (and candidate
                                           (alist-get 'content candidate)))
                       (parts         (and content-node
                                           (alist-get 'parts content-node)))
                       (finish-reason (and candidate
                                           (alist-get 'finishReason candidate)))
                       text-parts  ; (list of strings, 역순 수집)
                       image-b64   ; base64 string
                       image-mime) ; "image/png" 등

                  (unless (and parts (vectorp parts) (> (length parts) 0))
                    (user-error "응답에 parts 없음 (finishReason: %s)" finish-reason))

                  ;; text / inlineData 파트 분리
                  (seq-do (lambda (part)
                            (when-let ((txt (alist-get 'text part)))
                              (push (if (stringp txt) txt (format "%s" txt))
                                    text-parts))
                            (when-let ((inline (alist-get 'inlineData part)))
                              (setq image-b64  (alist-get 'data     inline))
                              (setq image-mime (alist-get 'mimeType inline))))
                          parts)

                  (unless image-b64
                    (user-error "이미지 데이터 없음 (finishReason: %s). 안전 정책 차단 가능성 있음"
                                (or finish-reason "unknown")))

                  ;; ── 6. 파일 저장 ────────────────────────────────────────────
                  (let* ((ts    (my/gemini--kst-timestamp))
                         (slug  (my/gemini--slugify prompt))
                         (ext   (my/gemini--mime-to-ext (or image-mime "image/png")))
                         (fname (format "%s--%s__brand_nanobanana.%s" ts slug ext))
                         (dir   (expand-file-name "~/screenshot/"))
                         (fpath (expand-file-name fname dir)))
                    (make-directory dir t)
                    (let ((coding-system-for-write 'binary))
                      (write-region (base64-decode-string
                                     (if (stringp image-b64)
                                         image-b64
                                       (format "%s" image-b64)))
                                    nil fpath))

                    ;; ── 7. org 링크 삽입 (원래 버퍼의 커서 위치) ─────────────────
                    (with-current-buffer orig-buf
                      (goto-char orig-pos)
                      (insert (format "[[file:%s]]\n" fpath))

                      ;; ── 8. 인라인 이미지 표시 ──────────────────────────────────
                      (when (derived-mode-p 'org-mode)
                        (org-display-inline-images)))

                    (message "✅ 이미지 생성 완료: %s%s"
                             fname
                             (if text-parts
                                 (format " | %s"
                                         (string-join (nreverse text-parts) " "))
                               ""))))))

          ;; 응답 버퍼 정리 (unwind-protect)
          (when (buffer-live-p resp-buf)
            (kill-buffer resp-buf))))))))

;;; Provide

(provide 'ai-gptel)

;;; ai-gptel.el ends here
