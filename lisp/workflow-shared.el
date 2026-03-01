;;; $DOOMDIR/lisp/workflow-shared.el --- 워크플로우 공유 설정 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; 인간(Doom)과 에이전트(agent-server)가 공유하는 설정.
;; denote 로드 이후에 평가되어야 함.
;;
;; 로딩 조건:
;;   - org-config.el: (after! denote (require 'workflow-shared))
;;   - agent-server.el: denote require 이후 (load ...)
;;
;; "워크플로우 공유(Workflow Sharing)":
;; 인간과 에이전트가 동일한 ~/org를 읽고 쓴다.
;; 어느 쪽에서 agenda를 호출해도 같은 뷰를 본다.
;; aprj(active project) 태그로 agenda 포함 여부를 제어한다.

;;; Code:

;;;; org-agenda-files 동적 구성

;; _aprj (active project) 태그가 있는 denote 파일 + botlog/agenda/
;; 프로젝트를 agenda에 올리려면 denote 태그에 aprj 추가
(defun my/org-agenda-files-rebuild ()
  "Rebuild org-agenda-files from _aprj files + botlog/agenda/ + current journal."
  (setq org-agenda-files
        (append
         ;; 1. _aprj 태그 파일
         (denote-directory-files "_aprj")
         ;; 2. 에이전트 어젠다 디렉토리
         (let ((agent-dir (expand-file-name "botlog/agenda/" denote-directory)))
           (when (file-directory-p agent-dir) (list agent-dir)))
         ;; 3. 현재 주 journal 파일
         (when (fboundp 'org-journal--get-entry-path)
           (let ((jp (ignore-errors (org-journal--get-entry-path))))
             (when (and jp (file-exists-p jp)) (list jp)))))))

;; 초기 구성
(my/org-agenda-files-rebuild)

(provide 'workflow-shared)
;;; workflow-shared.el ends here
