;;; $DOOMDIR/lisp/present-config.el --- 라이브 발표 모드 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;; 살아있는 발표 (Living Presentation)
;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;
;; 컨셉:
;;   PPT 없이 Emacs org-tree-slide로 발표한다.
;;   에이전트(pi/eca)가 별도 프레임이나 터미널에서 대기하며
;;   Q&A 시 실시간 보조한다.
;;
;; 아키텍처:
;;   프로젝터 → Emacs 프레임 1: org-tree-slide (풀스크린, 깔끔)
;;   노트북   → 터미널 pi 세션 또는 Emacs 프레임 2 (발표자만)
;;
;; 기존 자산:
;;   - org-tree-slide: Doom +present 모듈 (init.el)
;;   - pi-coding-agent: ai-pi-agent.el (SPC j)
;;   - whisper STT: ai-stt-eca-whisper.el (M-a)
;;   - edge-tts: ai-tts-edge.el (SPC -)
;;
;; 히스토리:
;;   2026-04-06  v0.1  초기 — org-tree-slide 기본 + send-slide
;;
;; TODO:
;;   - [ ] 실제 발표 리허설 후 폰트/테마 조정
;;   - [ ] send-slide → pi control socket 연동 검증
;;   - [ ] 발표자 노트 (COMMENT 블록 or 서브트리) 표시 방식
;;   - [ ] 슬라이드 타이머/진행률 표시

;;; Code:

;;;; org-tree-slide 발표 설정

(defvar my/present-text-scale 6
  "발표 모드 텍스트 스케일. 프로젝터 해상도에 따라 조정.
5=일반 세미나, 6=대형 스크린, 7=원거리.")

(defvar my/present--active nil
  "발표 모드 활성 여부.")

(defvar my/present--wconf nil
  "발표 시작 전 윈도우 설정 백업.")

(after! org-tree-slide
  ;; Doom +present 기본이 로드된 후 오버라이드
  (setq org-tree-slide-heading-emphasis t
        org-tree-slide-activate-message "🎤 발표 시작"
        org-tree-slide-deactivate-message "📋 발표 종료"
        org-tree-slide-modeline-display nil
        ;; 컨텐츠 헤딩만 슬라이드로 (레벨 1은 섹션 구분)
        org-tree-slide-skip-outline-level 0))

;;;; 시작/종료

(defun my/present-start ()
  "발표 시작 — org-tree-slide + 풀스크린 세팅."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode 버퍼에서 실행하세요"))
  (setq my/present--active t)
  (setq my/present--wconf (current-window-configuration))
  ;; 정리
  (delete-other-windows)
  (org-fold-show-all)
  ;; 탭바 끄기
  (when (bound-and-true-p centaur-tabs-mode)
    (centaur-tabs-mode -1))
  ;; org-tree-slide 시작
  (org-tree-slide-mode 1)
  ;; 시각 설정 (Doom +present 훅이 처리 못하는 경우 보완)
  (text-scale-set my/present-text-scale)
  (hide-mode-line-mode 1)
  (set-window-fringes nil 0 0)
  (message "🎤 C-→ 다음 / C-← 이전 / SPC j p q 종료"))

(defun my/present-stop ()
  "발표 종료 — 원상 복구."
  (interactive)
  (setq my/present--active nil)
  (org-tree-slide-mode -1)
  (text-scale-set 0)
  (hide-mode-line-mode -1)
  ;; 탭바 복원
  (when (fboundp 'centaur-tabs-mode)
    (centaur-tabs-mode 1))
  ;; 윈도우 복구
  (when my/present--wconf
    (set-window-configuration my/present--wconf)
    (setq my/present--wconf nil)))

;;;; 슬라이드 → 에이전트 전송

(defun my/present--current-slide-text ()
  "현재 슬라이드(narrowed 영역)의 텍스트 반환."
  (if (buffer-narrowed-p)
      (buffer-substring-no-properties (point-min) (point-max))
    (org-get-entry)))

(defun my/present-send-slide ()
  "현재 슬라이드 내용을 pi 에이전트에 전송.
pi-coding-agent 입력 버퍼에 삽입 후 전송."
  (interactive)
  (let ((text (my/present--current-slide-text)))
    (if-let ((input-buf
              (cl-some (lambda (buf)
                         (with-current-buffer buf
                           (when (derived-mode-p 'pi-coding-agent-input-mode)
                             buf)))
                       (buffer-list))))
        (with-current-buffer input-buf
          (erase-buffer)
          (insert (format "현재 발표 슬라이드 내용입니다. 핵심 포인트를 정리하고, 예상 질문과 답변을 준비해주세요:\n\n%s" text))
          (pi-coding-agent-send)
          (message "📤 슬라이드 → 에이전트 전송"))
      ;; pi 세션 없으면 킬링에라도 복사
      (kill-new text)
      (message "📋 슬라이드 텍스트 복사됨 (pi 세션 없음)"))))

(defun my/present-send-question (question)
  "QUESTION을 pi 에이전트에 전송. Q&A용."
  (interactive "s질문: ")
  (if-let ((input-buf
            (cl-some (lambda (buf)
                       (with-current-buffer buf
                         (when (derived-mode-p 'pi-coding-agent-input-mode)
                           buf)))
                     (buffer-list))))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert (format "발표 Q&A 질문입니다. 간결하게 답변해주세요:\n\n%s" question))
        (pi-coding-agent-send)
        (message "📤 질문 → 에이전트 전송"))
    (message "⚠ pi 세션이 없습니다")))

;;;; 키바인딩

(map! :leader
      (:prefix ("j" . "pi-agent")
       (:prefix ("p" . "present")
        :desc "발표 시작"         "p" #'my/present-start
        :desc "발표 종료"         "q" #'my/present-stop
        :desc "슬라이드→에이전트" "s" #'my/present-send-slide
        :desc "질문→에이전트"     "a" #'my/present-send-question)))

(provide 'present-config)

;;; present-config.el ends here
