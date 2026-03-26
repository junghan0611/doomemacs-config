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

;;;; org-agenda 주차 — %W 통일 (ISO %V → Monday-start count)

;; NOTE 2026-03-17: org-agenda 내부가 ISO 8601 주차(%V)를 사용하는데,
;; 위클리저널 파일명은 %W(월요일 시작 카운트) 기준. 1주 차이 발생.
;; (예: 2026-03-17 → ISO=W12, %W=W11, 저널=week11)
;; org-days-to-iso-week를 advice로 %W 기준으로 교체하여
;; 헤더 "Day-agenda (W##):"와 날짜 줄 모두 한번에 통일.
(define-advice org-days-to-iso-week (:override (days) use-monday-week-number)
  "Return %W week number (Monday-start count) instead of ISO 8601.
Matches `org-journal-file-format' week%W convention."
  (let* ((date (calendar-gregorian-from-absolute days))
         (month (car date))
         (day (cadr date))
         (year (nth 2 date))
         (time (encode-time 0 0 0 day month year)))
    (string-to-number (format-time-string "%W" time))))

;;;; org-agenda-redo — 외부 변경 반영

;; 에이전트가 bash로 agenda 파일을 수정하면 Emacs 버퍼는 디스크와 불일치.
;; org-agenda-redo-all은 열린 버퍼 기반이라 변경사항이 안 보임.
;; → redo 전에 agenda 관련 org 버퍼를 자동 revert.
(defun my/org-agenda-revert-files (&rest _)
  "Revert all org-agenda file buffers from disk before agenda rebuild."
  (dolist (file (org-agenda-files))
    (when-let* ((buf (get-file-buffer file)))
      (with-current-buffer buf
        (when (and (not (buffer-modified-p))
                   (file-exists-p (buffer-file-name)))
          (revert-buffer t t t))))))

(advice-add 'org-agenda-redo :before #'my/org-agenda-revert-files)
(advice-add 'org-agenda-redo-all :before #'my/org-agenda-revert-files)

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

;;;; 존재 데이터 (Being Data) — 사용자 현황 수치

;; 인간(Doom)과 에이전트(agent-server)가 동일한 데이터를 조회하는 단일 소스.
;; denote-export-server: 정적 org 매크로 {{{notes-count}}} 등에 사용.
;; agent-server: (agent-being-data) API로 에이전트가 런타임 조회에 사용.
;; 서버 시작 시 1회 계산 후 캐시. (workflow-shared-compute-being-data) 로 갱신 가능.

(defvar workflow-shared--being-data nil
  "Cached being data plist. Computed once per session.
Keys: :notes :journal-days :garden :bib
      :notes-formatted :journal-days-formatted :garden-formatted :bib-formatted
      :computed-at
Call `workflow-shared-compute-being-data' to refresh.")

(defun workflow-shared--format-with-commas (n)
  "Format integer N with thousands separators. e.g. 3328 → \"3,328\"."
  (let* ((s (number-to-string n))
         (groups nil)
         (i (length s)))
    (while (> i 0)
      (push (substring s (max 0 (- i 3)) i) groups)
      (setq i (- i 3)))
    (mapconcat #'identity groups ",")))

(defun workflow-shared--shell-count (cmd)
  "Run shell CMD and return integer. Returns 0 on error."
  (condition-case err
      (with-temp-buffer
        (call-process "sh" nil t nil "-c" cmd)
        (string-to-number (string-trim (buffer-string))))
    (error
     (message "[workflow-shared] shell-count error (%s): %S" cmd err)
     0)))

(defun workflow-shared-compute-being-data ()
  "Compute being data and store in `workflow-shared--being-data'.
Safe to call multiple times — recomputes and updates cache.
Requires denote to be loaded for best results; falls back to shell counts."
  (unless (featurep 'denote)
    (message "[workflow-shared] WARNING: denote not loaded, being-data uses shell fallback"))
  (let* ((notes   (workflow-shared--shell-count
                   "find ~/org/ -name '*.org' | wc -l"))
         (garden  (workflow-shared--shell-count
                   "find ~/repos/gh/notes/content -name '*.md' | wc -l"))
         (jdays   (- (time-to-days (current-time))
                     (time-to-days (encode-time 0 0 0 10 3 2022))))
         (bib     (workflow-shared--shell-count
                   "ls ~/org/bib/*.org 2>/dev/null | wc -l"))
         (now-str (format-time-string "%Y-%m-%d %a %H:%M")))
    (setq workflow-shared--being-data
          (list :notes                  notes
                :journal-days           jdays
                :garden                 garden
                :bib                    bib
                :notes-formatted        (workflow-shared--format-with-commas notes)
                :journal-days-formatted (workflow-shared--format-with-commas jdays)
                :garden-formatted       (workflow-shared--format-with-commas garden)
                :bib-formatted          (workflow-shared--format-with-commas bib)
                :computed-at            now-str))
    (message "[workflow-shared] being-data: notes=%d journal=%d garden=%d bib=%d (at %s)"
             notes jdays garden bib now-str)
    workflow-shared--being-data))

(defun workflow-shared-being-data (&optional as-json)
  "Return being data plist. Computes on first call, then returns cache.
With AS-JSON non-nil, returns JSON string.

Example plist:
  (:notes 3328 :journal-days 1477 :garden 2178 :bib 671
   :notes-formatted \"3,328\" :journal-days-formatted \"1,477\"
   :garden-formatted \"2,178\" :bib-formatted \"671\"
   :computed-at \"2026-03-26 Thu 10:00\")"
  (unless workflow-shared--being-data
    (workflow-shared-compute-being-data))
  (if as-json
      (progn
        (require 'json nil t)
        (json-encode
         `((notes        . ,(plist-get workflow-shared--being-data :notes))
           (journal_days . ,(plist-get workflow-shared--being-data :journal-days))
           (garden       . ,(plist-get workflow-shared--being-data :garden))
           (bib          . ,(plist-get workflow-shared--being-data :bib))
           (computed_at  . ,(plist-get workflow-shared--being-data :computed-at)))))
    workflow-shared--being-data))

(provide 'workflow-shared)
;;; workflow-shared.el ends here
