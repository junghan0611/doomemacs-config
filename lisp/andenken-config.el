;;; $DOOMDIR/lisp/andenken-config.el --- Andenken sessions client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; andenken sessions 트랙 한정 얇은 이맥스 클라이언트.
;; 백엔드: ~/.claude/skills/semantic-memory/semantic-memory (wrapper)
;;        → ~/repos/gh/andenken/cli.ts (npx tsx).
;;
;; ─────────────────────────────────────────────────────────────────────
;; [2026-05-28] 왜 sessions today/this-week 두 명령만 남았는가
;; ─────────────────────────────────────────────────────────────────────
;;
;; 사용자 본 그림: "최근 1주일간 내가 에이전트로 무엇을 했는가" — 매일/주간
;; 자기 활동 회고. session 은 사실/대화/결정의 명확한 정보라 retrieval 기대치
;; 높음. md 처럼 글 형식 추상화 없음.
;;
;; 사용자 결정 (둘 다 명시):
;;   (1) 인터페이스 더 안 늘림. "복잡하면 사용하지 않겠다는 것."
;;   (2) md 트랙은 일단 안 본다. 글 형식이라 retrieval 기대치가 다름.
;;
;; 그래서 시간축 두 명령만 살림:
;;   andenken-search-sessions-today      — 오늘 KST 일일창
;;   andenken-search-sessions-this-week  — 이번 주 (Mon~Sun) KST
;; + 인덱스 살아있나 확인용 andenken-status.
;;
;; ─────────────────────────────────────────────────────────────────────
;; Boundary — andenken 책임 vs recall skill 책임
;; ─────────────────────────────────────────────────────────────────────
;;
;; | 책임                                  | Owner          |
;; |---------------------------------------+----------------|
;; | windowed single-axis retrieval        | andenken       |
;; | 다축 합성 (commits + journal + ...)    | /recall skill  |
;; | 자연어 시간 파싱                       | day-query      |
;;
;; 이 wrapper 는 단축만. "멀티리포 조망" / "어제 vs 오늘 비교" / "회사 vs
;; 개인" 같은 다축 합성은 /recall (~/.claude/skills/recall/) 자리.
;; 흐릿하게 두면 surface 키우는 방향으로 발산하니 분리 유지.
;;
;; ─────────────────────────────────────────────────────────────────────
;; 알고 있는 한계 (백엔드 측에서 풀리는 자리)
;; ─────────────────────────────────────────────────────────────────────
;;
;; 지금 한 프로젝트 메시지가 화면 잡아먹어 multi-repo 조망 안 됨. limit 30
;; 으로 어제 윈도우 봐도 같은 sessionFile chunk 가 반복됨. 이건 wrapper
;; 측이 아니라 andenken 백엔드 자리 — NEXT.md 2e "session-as-unit
;; windowed view" 진입점.
;;
;; 2e 머지 후 이 파일에서 할 일은 한 줄: search-sessions 호출에
;; `--view session' 추가. 인터페이스는 그대로.
;;
;; ─────────────────────────────────────────────────────────────────────
;; 패턴
;; ─────────────────────────────────────────────────────────────────────
;;
;; - 시간 윈도우 인터페이스는 my/denote-links-today / -this-week 와 동일.
;;   prefix C-u = calendar 픽. helper (my/org--date-to-time / --week-range)
;;   재활용.
;; - query 비우면 --mode recent (timestamp DESC, embedding skip).
;;   채우면 --mode hybrid (윈도우 안 semantic + BM25).
;; - 결과 선택 → *andenken-session* preview 버퍼 → RET 원본 JSONL / q 닫기.
;; - dynamic-collection 안 함 (OpenRouter 호출 비용/지연).
;;
;; ─────────────────────────────────────────────────────────────────────
;; 참조 노트
;; ─────────────────────────────────────────────────────────────────────
;;
;; ~/sync/org/notes/20250214T145633--§andenken-이맥스-임베딩-검색...org
;;   설계 결정 / boundary / 진행 상황 / 다음 한 걸음.
;;   임베딩이 별로면 기록이라도 잘 남겨야 개선된다.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)

;;;; Customization

(defgroup andenken nil
  "Andenken sessions client."
  :group 'tools)

(defcustom andenken-cli
  (expand-file-name "~/.claude/skills/semantic-memory/semantic-memory")
  "Path to the andenken CLI wrapper."
  :type 'file
  :group 'andenken)

(defcustom andenken-snippet-width 100
  "Width of the body snippet column in completion display."
  :type 'integer
  :group 'andenken)

(defcustom andenken-window-limit 30
  "Default result limit for windowed session searches.
Once andenken NEXT.md 2e ships a `--view session' mode, this becomes
sessions-per-window rather than chunks-per-window."
  :type 'integer
  :group 'andenken)

(defvar andenken-search-history nil
  "History of andenken queries.")

;;;; CLI invocation

(defun andenken--call (subcommand &rest args)
  "Call andenken CLI SUBCOMMAND with ARGS. Return parsed JSON as alist.
Stderr is discarded; only stdout is parsed."
  (unless (file-executable-p andenken-cli)
    (user-error "andenken CLI not found or not executable: %s" andenken-cli))
  (with-temp-buffer
    (let* ((exit (apply #'call-process andenken-cli nil
                        (list (current-buffer) nil) nil
                        subcommand args))
           (output (string-trim (buffer-string))))
      (unless (zerop exit)
        (error "andenken %s failed (exit %s): %s"
               subcommand exit (andenken--truncate output 200)))
      (condition-case err
          (json-parse-string output
                             :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil)
        (error
         (error "andenken JSON parse failed: %s — output head: %s"
                (error-message-string err)
                (andenken--truncate output 200)))))))

;;;; Helpers

(defun andenken--truncate (text width)
  "Flatten newlines in TEXT and truncate to WIDTH chars with an ellipsis."
  (let ((flat (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))
    (truncate-string-to-width flat width nil ?\s "…")))

(defun andenken--format-date (ts)
  "Return the YYYY-MM-DD prefix of ISO timestamp TS, or empty string.
Prevents `2026-05-…' truncation when the original ISO is 20+ chars."
  (cond ((null ts) "")
        ((>= (length ts) 10) (substring ts 0 10))
        (t ts)))

(defun andenken--open-file-at-line (path line)
  "Open PATH and move to LINE (1-based)."
  (find-file path)
  (when (and line (> line 0))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun andenken--completing-read (prompt candidates)
  "Like `completing-read' over CANDIDATES (string list) with PROMPT,
but preserve the input order so score-sorted results are not re-sorted
by vertico / orderless."
  (let ((table
         (lambda (str pred action)
           (if (eq action 'metadata)
               '(metadata (display-sort-function . identity)
                          (cycle-sort-function . identity))
             (complete-with-action action candidates str pred)))))
    (completing-read prompt table nil t)))

;;;; Session preview buffer

(defvar andenken--session-buffer "*andenken-session*"
  "Buffer name for displaying a session chunk preview.")

(defvar-local andenken--session-result nil
  "Result alist for the chunk shown in the current session preview buffer.")

(defvar andenken-session-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'andenken-session-result-open-jsonl)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `andenken-session-result-mode'.")

(define-derived-mode andenken-session-result-mode special-mode "andenken-session"
  "Major mode for andenken session chunk preview.
\\{andenken-session-result-mode-map}")

(defun andenken--format-session-result (result idx)
  "Format session RESULT line for completion display."
  (let ((score (or (alist-get 'score result) 0.0))
        (project (or (alist-get 'project result) "?"))
        (role (or (alist-get 'role result) "?"))
        (src (or (alist-get 'source result) "?"))
        (date (andenken--format-date (alist-get 'timestamp result)))
        (text (alist-get 'text result)))
    (format "%2d  %5.2f  %-18s %-10s %-6s  %-10s  │ %s"
            idx score
            (andenken--truncate project 18)
            (andenken--truncate role 10)
            (andenken--truncate src 6)
            date
            (andenken--truncate text andenken-snippet-width))))

(defun andenken--show-session-result (result)
  "Display session chunk RESULT in `andenken--session-buffer'."
  (let ((buf (get-buffer-create andenken--session-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# andenken session chunk\n\n")
        (insert (format "project   : %s\n" (or (alist-get 'project result) "?")))
        (insert (format "role      : %s\n" (or (alist-get 'role result) "?")))
        (insert (format "source    : %s\n" (or (alist-get 'source result) "?")))
        (insert (format "timestamp : %s\n" (or (alist-get 'timestamp result) "")))
        (insert (format "score     : %s\n" (alist-get 'score result)))
        (insert (format "file      : %s\n" (alist-get 'file result)))
        (insert (format "line      : %s\n" (alist-get 'line result)))
        (insert "\n--- text ---\n\n")
        (insert (or (alist-get 'text result) "")))
      (andenken-session-result-mode)
      (setq andenken--session-result result)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "RET: 원본 JSONL  q: 닫기")))

(defun andenken-session-result-open-jsonl ()
  "Open the source JSONL file of the current session result at its line."
  (interactive)
  (unless andenken--session-result
    (user-error "No andenken result in this buffer"))
  (let ((file (alist-get 'file andenken--session-result))
        (line (alist-get 'line andenken--session-result)))
    (andenken--open-file-at-line file (or line 1))))

;;;; Time-axis session search

;; Mirrors `my/denote-links-today' / `my/denote-links-this-week':
;;   no prefix = today / this-week, prefix C-u = pick a date via calendar.
;; KST = local TZ. `my/org--date-to-time' encodes at local midnight, so
;; emitting ISO Z strings with `t' lands boundaries on KST midnights.

(declare-function my/org--read-date-or-today "denote-functions" (&optional arg))
(declare-function my/org--date-to-time "denote-functions" (date))
(declare-function my/org--week-range "denote-functions" (time))

(defun andenken--day-range-utc (time)
  "Return (FROM . TO) UTC ISO strings for the local day containing TIME."
  (let ((end (time-add time (days-to-time 1))))
    (cons (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)
          (format-time-string "%Y-%m-%dT%H:%M:%SZ" end t))))

(defun andenken--week-range-utc (time)
  "Return (FROM . TO) UTC ISO strings for the local Mon~Sun week of TIME."
  (let* ((dow (string-to-number (format-time-string "%u" time)))
         (monday (time-subtract time (days-to-time (1- dow))))
         (sunday-end (time-add monday (days-to-time 7))))
    (cons (format-time-string "%Y-%m-%dT%H:%M:%SZ" monday t)
          (format-time-string "%Y-%m-%dT%H:%M:%SZ" sunday-end t))))

(defun andenken--search-sessions-window (query from to label)
  "Search sessions in UTC half-open window [FROM, TO).
Empty QUERY → `--mode recent' (timestamp DESC, no embedding call).
Non-empty QUERY → `--mode hybrid' (semantic + BM25 within the window).
LABEL is shown in the completion prompt."
  (let* ((mode (if (string-empty-p query) "recent" "hybrid"))
         (q (if (string-empty-p query) "." query))
         (resp (andenken--call "search-sessions" q
                               "--limit" (number-to-string andenken-window-limit)
                               "--date-from" from
                               "--date-to" to
                               "--mode" mode))
         (results (alist-get 'results resp))
         (count (or (alist-get 'count resp) 0)))
    (if (zerop count)
        (message "andenken sessions [%s]: no results" label)
      (let* ((candidates
              (cl-loop for r in results
                       for i from 1
                       collect (cons (andenken--format-session-result r i) r)))
             (choice (andenken--completing-read
                      (format "sessions [%s] (%d) > " label count)
                      (mapcar #'car candidates)))
             (result (cdr (assoc choice candidates))))
        (when result
          (andenken--show-session-result result))))))

;;;###autoload
(defun andenken-search-sessions-today (&optional arg)
  "Search sessions for today (local day = KST window).
With prefix ARG, prompt for a date via the calendar.
Prompts for a query; leave empty to list all sessions in that day."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (range (andenken--day-range-utc time))
         (label (format-time-string "%Y-%m-%d" time))
         (query (read-string
                 (format "andenken sessions (%s, empty=all) > " label)
                 nil 'andenken-search-history)))
    (andenken--search-sessions-window query (car range) (cdr range) label)))

;;;###autoload
(defun andenken-search-sessions-this-week (&optional arg)
  "Search sessions for this week (local Mon~Sun window).
With prefix ARG, prompt for a date via the calendar (uses the week of that date).
Prompts for a query; leave empty to list all sessions in that week."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (range (andenken--week-range-utc time))
         (week-dates (my/org--week-range time))
         (label (format "%s~%s" (car week-dates) (cdr week-dates)))
         (query (read-string
                 (format "andenken sessions (week %s, empty=all) > " label)
                 nil 'andenken-search-history)))
    (andenken--search-sessions-window query (car range) (cdr range) label)))

;;;; Status

;;;###autoload
(defun andenken-status ()
  "Show andenken index statistics in the echo area."
  (interactive)
  (let* ((resp (andenken--call "status"))
         (s (alist-get 'sessions resp))
         (m (alist-get 'md resp))
         (o (alist-get 'org resp)))
    (message
     "andenken — sessions: %s chunks / %s files · md: %s chunks / %s files · org: %s (%s)"
     (alist-get 'chunks s) (alist-get 'indexed_files s)
     (alist-get 'chunks m) (alist-get 'indexed_files m)
     (alist-get 'chunks o)
     (or (alist-get 'production o) "unknown"))))

;;;; Evil integration

;; In Doom, evil's normal-state map shadows local mode maps for derived
;; `special-mode' buffers. Without overriding registration, RET / q
;; defined on the session preview mode map are unreachable.

(with-eval-after-load 'evil
  (evil-make-overriding-map andenken-session-result-mode-map 'normal))

(provide 'andenken-config)
;;; andenken-config.el ends here
