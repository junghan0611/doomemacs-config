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

;;;; org-tag-re — 태그에 하이픈/밑줄 불허

;; Denote filetags와 일관성 유지: [a-z0-9] only.
;; 기본값 "[[:alnum:]_@#%]+" → '_' 제거.
;; org-tag-line-re, org-tag-group-re는 org-tag-re를 참조하지 않고
;; 하드코딩되어 있으므로 3개 모두 동기화 필수.
(setq org-tag-re "[[:alnum:]@#%]+")
(setq org-tag-line-re
      "^\\*+ \\(?:.*[ \t]\\)?\\(:\\([[:alnum:]@#%:]+\\):\\)[ \t]*$")
(setq org-tag-group-re
      "[ \t]+\\(:\\([[:alnum:]@#%:]+\\):\\)[ \t]*$")

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

;; 초기 구성: Doom에서는 즉시, agent-server에서는 init 후 지연 실행
;; (denote 캐시가 준비된 후 rebuild해야 _aprj 파일이 잡힘)
(if (bound-and-true-p doom-init-time)
    ;; Doom: 이미 초기화 완료
    (my/org-agenda-files-rebuild)
  ;; agent-server: init 완료 후 rebuild
  (add-hook 'emacs-startup-hook #'my/org-agenda-files-rebuild))

;;;;; my/org-journal — agenda 통합 강화

;; 문제: org-journal-new-entry는 plain text 시간만 넣어서 org-agenda에서 안 보임.
;; org-journal-enable-agenda-integration은 future 파일까지 전체 스캔 → 느림.
;; 해결: 현재 주 파일만 agenda-files에 추가 + active timestamp 삽입.

;; (defun my/org-journal--ensure-agenda-file ()
;;   "현재 주 journal 파일을 org-agenda-files에 추가 (없으면)."
;;   (when-let* ((entry-path (org-journal--get-entry-path)))
;;     (let ((truename (file-truename entry-path)))
;;       (unless (member truename (mapcar #'file-truename (org-agenda-files)))
;;         (org-agenda-file-to-front entry-path)
;;         (message "agenda-files에 추가: %s" (file-name-nondirectory entry-path))))))

(defun my/org-journal-new-entry (&optional prefix)
  "org-journal-new-entry + agenda 파일 등록 + active timestamp 삽입.
PREFIX가 있으면 엔트리 생성 없이 파일만 열기 (org-journal 기본 동작)."
  (interactive "P")
  ;; 1. agenda-files 등록
  ;; (my/org-journal--ensure-agenda-file)
  (my/org-agenda-files-rebuild)

  ;; 2. 기본 new-entry (timestamp는 org-journal이 HH:MM 넣음)
  (org-journal-new-entry prefix)
  ;; 3. active timestamp를 다음 줄에 삽입 (엔트리 생성 시에만)
  ;; 형식: ** 11:33 제목\n<2026-03-01 Sun 11:33>
  (unless prefix
    (save-excursion
      (end-of-line)
      (insert "\n" (format-time-string "<%Y-%m-%d %a %H:%M>")))))

(defun my/org-journal-last-entry ()
  "현재 주 journal 파일을 열고 마지막 시간 엔트리로 이동.
** HH:MM 패턴만 매칭 (** [검색어:...] 등 제외)."
  (interactive)
  (let* ((entry-path (org-journal--get-entry-path))
         (buf (find-file entry-path)))
    (with-current-buffer buf
      (goto-char (point-max))
      ;; ** HH:MM 또는 ** TODO/NEXT/DONE/DONT HH:MM 패턴
      (when (re-search-backward
             "^\\*\\* \\(?:TODO \\|NEXT \\|DONE \\|DONT \\)?[0-9]\\{2\\}:[0-9]\\{2\\}"
             nil t)
        (org-show-entry)
        (org-show-children)
        (recenter 3)))))

(provide 'workflow-shared)
;;; workflow-shared.el ends here
