;;; $DOOMDIR/lisp/ai-tts-edge.el --- Edge TTS 설정 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Microsoft Edge TTS (edge-tts) 통합 설정
;; - 무료로 사용 가능한 고품질 음성 합성
;; - 한국어, 영어 등 다국어 지원
;; - 로컬 실행, API 키 불필요
;;
;; 필수 패키지:
;;   - edge-tts: pip install edge-tts
;;   - mpv or vlc: 오디오 재생용

;;; Code:

;;;; 기본 설정

(defgroup edge-tts nil
  "Edge TTS (Text-to-Speech) 설정."
  :group 'ai
  :prefix "edge-tts-")

(defcustom edge-tts-voice "ko-KR-HyunsuMultilingualNeural"
  "기본 TTS 음성. 사용 가능한 음성 목록:
- ko-KR-HyunsuMultilingualNeural (남성, 다국어) ★ 추천
- ko-KR-SunHiNeural (여성, 친근함)
- ko-KR-InJoonNeural (남성, 친근함)
- en-US-JennyNeural (영어 여성)
- en-US-GuyNeural (영어 남성)"
  :type 'string
  :group 'edge-tts)

(defcustom edge-tts-player "mpv"
  "오디오 재생 프로그램. mpv 또는 vlc 권장."
  :type '(choice (const "mpv")
                 (const "vlc")
                 (string :tag "Custom"))
  :group 'edge-tts)

(defcustom edge-tts-player-args '("--really-quiet" "--no-video")
  "재생 프로그램 추가 인자.
mpv 기본값: --really-quiet --no-video
vlc 권장값: --intf dummy --play-and-exit"
  :type '(repeat string)
  :group 'edge-tts)

(defcustom edge-tts-rate "+25%"
  "말하기 속도. 예: +50% (1.5배속), -20% (0.8배속), +25% (1.25배속 권장)"
  :type 'string
  :group 'edge-tts)

(defcustom edge-tts-volume "+0%"
  "음량. 예: +20% (크게), -10% (작게)"
  :type 'string
  :group 'edge-tts)

(defcustom edge-tts-pitch "+0Hz"
  "음높이. 예: +10Hz (높게), -5Hz (낮게)"
  :type 'string
  :group 'edge-tts)

(defcustom edge-tts-output-dir (expand-file-name "~/sync/org/transcript/")
  "TTS 오디오/텍스트 파일 저장 디렉터리."
  :type 'directory
  :group 'edge-tts)

;;;; 내부 변수

(defvar edge-tts--process nil
  "현재 실행 중인 TTS 프로세스.")

(defvar edge-tts--last-file nil
  "마지막으로 생성된 오디오 파일 경로.")

;;;; 핵심 함수

(defun edge-tts--ensure-output-dir ()
  "출력 디렉터리가 없으면 생성."
  (unless (file-directory-p edge-tts-output-dir)
    (make-directory edge-tts-output-dir t)))

(defun edge-tts--generate-filename ()
  "임시 오디오 파일명 생성 (타임스탬프 기반)."
  (edge-tts--ensure-output-dir)
  (expand-file-name
   (format "tts-%s.mp3" (format-time-string "%Y%m%d-%H%M%S"))
   edge-tts-output-dir))

(defvar edge-tts--temp-text-file nil
  "임시 텍스트 파일 경로 (긴 텍스트용).")

(defun edge-tts--build-command (text output-file)
  "edge-tts 명령어 빌드.
TEXT: 변환할 텍스트
OUTPUT-FILE: 출력 파일 경로
긴 텍스트(1000자 초과)는 임시 파일로 저장 후 --file 옵션 사용."
  (if (> (length text) 1000)
      ;; 긴 텍스트: 임시 파일로 저장
      (let ((temp-file (make-temp-file "edge-tts-" nil ".txt")))
        (with-temp-file temp-file
          (insert text))
        (setq edge-tts--temp-text-file temp-file)
        `("edge-tts"
          "--voice" ,edge-tts-voice
          "--rate" ,edge-tts-rate
          "--volume" ,edge-tts-volume
          "--pitch" ,edge-tts-pitch
          "--file" ,temp-file
          "--write-media" ,output-file))
    ;; 짧은 텍스트: 직접 전달
    `("edge-tts"
      "--voice" ,edge-tts-voice
      "--rate" ,edge-tts-rate
      "--volume" ,edge-tts-volume
      "--pitch" ,edge-tts-pitch
      "--text" ,text
      "--write-media" ,output-file)))

(defun edge-tts--play-file (file)
  "오디오 파일 재생.
FILE: 재생할 파일 경로"
  (let ((cmd (append (list edge-tts-player)
                     edge-tts-player-args
                     (list file))))
    (apply #'start-process "edge-tts-player" nil cmd)))

;;;; 사용자 명령어

;;;###autoload
(defun edge-tts-speak (text)
  "TEXT를 음성으로 변환하고 재생.
현재 설정된 음성(edge-tts-voice)을 사용."
  (interactive "sText to speak: ")
  (if (string-empty-p text)
      (message "텍스트가 비어있습니다.")
    (let ((output-file (edge-tts--generate-filename)))
      (message "TTS 변환 중... (%s)" edge-tts-voice)
      (setq edge-tts--process
            (make-process
             :name "edge-tts"
             :buffer "*edge-tts*"
             :command (edge-tts--build-command text output-file)
             :sentinel
             (lambda (proc event)
               (when (string-match-p "finished" event)
                 (if (file-exists-p output-file)
                     (progn
                       (setq edge-tts--last-file output-file)
                       (edge-tts--play-file output-file)
                       (message "TTS 재생: %s" (file-name-nondirectory output-file)))
                   (message "TTS 변환 실패: 파일 생성되지 않음"))))))
      (message "TTS 명령 전송 완료"))))

;;;###autoload
(defun edge-tts-speak-region (start end)
  "선택 영역의 텍스트를 음성으로 변환.
START: 시작 위치
END: 끝 위치"
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (edge-tts-speak text)))

;;;###autoload
(defun edge-tts-speak-buffer ()
  "현재 버퍼 전체를 음성으로 변환."
  (interactive)
  (edge-tts-speak (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun edge-tts-speak-line ()
  "현재 라인을 음성으로 변환."
  (interactive)
  (edge-tts-speak
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

;;;###autoload
(defun edge-tts-speak-sentence ()
  "현재 문장을 음성으로 변환."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-sentence) (point)))
          (start (progn (backward-sentence) (point))))
      (edge-tts-speak (buffer-substring-no-properties start end)))))

;;;###autoload
(defun edge-tts-speak-paragraph ()
  "현재 문단을 음성으로 변환."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (start (progn (backward-paragraph) (point))))
      (edge-tts-speak (buffer-substring-no-properties start end)))))

;;;###autoload
(defun edge-tts-stop ()
  "현재 재생 중인 TTS 중지."
  (interactive)
  (when (process-live-p edge-tts--process)
    (kill-process edge-tts--process)
    (message "TTS 중지됨"))
  (call-process "pkill" nil 0 nil "-f" edge-tts-player))

;;;###autoload
(defun edge-tts-replay-last ()
  "마지막으로 생성된 오디오 파일 재생."
  (interactive)
  (if (and edge-tts--last-file (file-exists-p edge-tts--last-file))
      (progn
        (edge-tts--play-file edge-tts--last-file)
        (message "TTS 재재생: %s" (file-name-nondirectory edge-tts--last-file)))
    (message "재생할 이전 파일이 없습니다.")))

;;;###autoload
(defun edge-tts-set-voice ()
  "TTS 음성 선택 (대화형)."
  (interactive)
  (let ((voice (completing-read
                "음성 선택: "
                '("ko-KR-HyunsuMultilingualNeural" ; 다국어 (추천)
                  "ko-KR-SunHiNeural"      ; 여성
                  "ko-KR-InJoonNeural"     ; 남성
                  "en-US-JennyNeural"      ; 영어 여성
                  "en-US-GuyNeural"        ; 영어 남성
                  )
                nil t nil nil edge-tts-voice)))
    (setq edge-tts-voice voice)
    (message "TTS 음성 변경: %s" voice)))

;;;###autoload
(defun edge-tts-set-rate ()
  "말하기 속도 조정 (대화형)."
  (interactive)
  (let ((rate (completing-read
               "속도 선택: "
               '("+0%" "+25%" "+50%" "+75%" "+100%"
                 "-10%" "-20%" "-30%")
               nil nil edge-tts-rate)))
    (setq edge-tts-rate rate)
    (message "TTS 속도 변경: %s" rate)))

;;;###autoload
(defun edge-tts-cleanup-old-files (&optional days)
  "오래된 TTS 파일 삭제.
DAYS: 기본값 7일"
  (interactive "P")
  (let ((days (or days 7))
        (count 0))
    (when (file-directory-p edge-tts-output-dir)
      (dolist (file (directory-files edge-tts-output-dir t "^tts-.*\\.mp3$"))
        (when (> (time-to-days (time-since (file-attribute-modification-time (file-attributes file))))
                 days)
          (delete-file file)
          (setq count (1+ count)))))
    (message "%d개의 오래된 TTS 파일 삭제 완료" count)))

;;;; Org TTS 태그 저장

(defun edge-tts--get-denote-id ()
  "현재 버퍼의 Denote ID 추출. 없으면 nil 반환."
  (when buffer-file-name
    (if (fboundp 'denote-retrieve-filename-identifier)
        (denote-retrieve-filename-identifier buffer-file-name)
      ;; fallback: 파일명에서 직접 추출
      (when (string-match "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                          (file-name-nondirectory buffer-file-name))
        (match-string 1 (file-name-nondirectory buffer-file-name))))))

(defun edge-tts--generate-denote-filename (source-id)
  "Denote 스타일 TTS 파일명 생성.
SOURCE-ID: 원본 파일의 Denote ID (역추적용)
파일명 형식: 새ID--원본ID__tts.mp3"
  (edge-tts--ensure-output-dir)
  (let ((new-id (format-time-string "%Y%m%dT%H%M%S")))
    (expand-file-name
     (if source-id
         (format "%s--%s__tts.mp3" new-id source-id)
       (format "%s__tts.mp3" new-id))
     edge-tts-output-dir)))

(defun edge-tts--extract-tts-heading-text ()
  "`:TTS:` 태그가 있는 헤딩 아래 텍스트 추출.
타임스탬프 `[0:00] - [0:30]:` 제거하고 순수 텍스트만 반환."
  (save-excursion
    ;; TTS 태그 있는 헤딩 찾기
    (goto-char (point-min))
    (if (re-search-forward "^\\*+.*:TTS:" nil t)
        (let* ((heading-end (line-end-position))
               (content-start (1+ heading-end))
               (content-end (or (save-excursion
                                  (outline-next-heading)
                                  (point))
                                (point-max)))
               (raw-text (buffer-substring-no-properties content-start content-end)))
          ;; 타임스탬프 제거: [0:00] - [0:30]: 패턴
          (setq raw-text (replace-regexp-in-string
                          "\\[\\([0-9]+:[0-9]+\\)\\] *- *\\[\\([0-9]+:[0-9]+\\)\\]:?"
                          ""
                          raw-text))
          ;; 연속 공백/줄바꿈 정리
          (setq raw-text (replace-regexp-in-string "[\n\r]+" " " raw-text))
          (setq raw-text (replace-regexp-in-string "  +" " " raw-text))
          (string-trim raw-text))
      nil)))

;;;###autoload
(defun edge-tts-export-org-tts-to-txt ()
  "`:TTS:` 태그 헤딩 텍스트를 txt 파일로 추출.
타임스탬프 제거 후 Denote ID 기반 파일명으로 저장.
반환값: 생성된 txt 파일 경로"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode 버퍼에서만 사용 가능합니다"))
  (let* ((text (edge-tts--extract-tts-heading-text))
         (source-id (edge-tts--get-denote-id))
         (new-id (format-time-string "%Y%m%dT%H%M%S"))
         (txt-filename (if source-id
                           (format "%s--%s__tts.txt" new-id source-id)
                         (format "%s__tts.txt" new-id)))
         (txt-file (expand-file-name txt-filename edge-tts-output-dir)))
    (if (or (null text) (string-empty-p text))
        (progn
          (message "TTS 태그(:TTS:)가 있는 헤딩을 찾을 수 없거나 내용이 없습니다.")
          nil)
      (edge-tts--ensure-output-dir)
      (with-temp-file txt-file
        (insert text))
      (message "TTS 텍스트 추출 완료: %s (%d자)" txt-filename (length text))
      (kill-new txt-file)  ; 클립보드에 경로 복사
      txt-file)))

(defvar edge-tts-python-script
  (expand-file-name "bin/edge-tts-convert.py" doom-user-dir)
  "edge-tts 변환 Python 스크립트 경로.")

;;;###autoload
(defun edge-tts-convert-txt-to-mp3 (txt-file &optional output-file)
  "TXT 파일을 MP3로 변환 (Python 스크립트 사용, tqdm 진행 표시).
TXT-FILE: 입력 텍스트 파일
OUTPUT-FILE: 출력 MP3 파일 (생략 시 자동 생성)"
  (interactive "fTXT 파일: ")
  (unless (file-exists-p txt-file)
    (user-error "파일이 존재하지 않습니다: %s" txt-file))
  (unless (file-exists-p edge-tts-python-script)
    (user-error "Python 스크립트가 없습니다: %s" edge-tts-python-script))
  (let* ((mp3-file (or output-file
                       (concat (file-name-sans-extension txt-file) ".mp3")))
         (cmd (list "python3" edge-tts-python-script
                    "--voice" edge-tts-voice
                    "--rate" edge-tts-rate
                    "--volume" edge-tts-volume
                    "--pitch" edge-tts-pitch
                    txt-file mp3-file)))
    (message "TTS 변환 시작... (터미널에서 진행 상황 확인)")
    ;; compile 버퍼에서 실행 (진행 상황 실시간 표시)
    (compile (mapconcat #'shell-quote-argument cmd " "))
    mp3-file))

;;;###autoload
(defun edge-tts-org-tts-full-workflow ()
  "org TTS 태그 → txt 추출 → MP3 변환 전체 워크플로우.
1. TTS 태그 텍스트 추출하여 txt 저장
2. Python 스크립트로 MP3 변환 (tqdm 진행 표시)"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode 버퍼에서만 사용 가능합니다"))
  (let ((txt-file (edge-tts-export-org-tts-to-txt)))
    (when txt-file
      (edge-tts-convert-txt-to-mp3 txt-file))))

;;;###autoload
(defun edge-tts-save-org-tts-heading (&optional show-progress)
  "`:TTS:` 태그 헤딩 아래 텍스트를 MP3로 저장.
타임스탬프를 제거하고 Denote ID 기반 파일명으로 저장.
SHOW-PROGRESS가 non-nil이면 진행 상황 버퍼를 표시."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode 버퍼에서만 사용 가능합니다"))
  (let* ((text (edge-tts--extract-tts-heading-text))
         (source-id (edge-tts--get-denote-id))
         (output-file (edge-tts--generate-denote-filename source-id)))
    (if (or (null text) (string-empty-p text))
        (message "TTS 태그(:TTS:)가 있는 헤딩을 찾을 수 없거나 내용이 없습니다.")
      ;; 버퍼 초기화
      (with-current-buffer (get-buffer-create "*edge-tts*")
        (erase-buffer))
      (message "TTS 저장 중... (약 %d자, 예상 %d분) - *edge-tts* 버퍼에서 진행 상황 확인"
               (length text)
               (max 1 (/ (length text) 150)))
      ;; C-u 로 호출 시 진행 상황 버퍼 표시
      (when show-progress
        (display-buffer "*edge-tts*"))
      (setq edge-tts--process
            (make-process
             :name "edge-tts-save"
             :buffer "*edge-tts*"
             :command (edge-tts--build-command text output-file)
             :sentinel
             (lambda (proc event)
               (when (string-match-p "finished" event)
                 (if (file-exists-p output-file)
                     (progn
                       (setq edge-tts--last-file output-file)
                       (message "TTS 저장 완료: %s (%.1f MB)"
                                (file-name-nondirectory output-file)
                                (/ (file-attribute-size (file-attributes output-file)) 1048576.0)))
                   (message "TTS 저장 실패")))))))))

;;;###autoload
(defun edge-tts-save-and-play-org-tts-heading ()
  "`:TTS:` 태그 헤딩 텍스트를 MP3로 저장하고 재생."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode 버퍼에서만 사용 가능합니다"))
  (let* ((text (edge-tts--extract-tts-heading-text))
         (source-id (edge-tts--get-denote-id))
         (output-file (edge-tts--generate-denote-filename source-id)))
    (if (or (null text) (string-empty-p text))
        (message "TTS 태그(:TTS:)가 있는 헤딩을 찾을 수 없거나 내용이 없습니다.")
      (message "TTS 변환 및 저장 중...")
      (setq edge-tts--process
            (make-process
             :name "edge-tts-save-play"
             :buffer "*edge-tts*"
             :command (edge-tts--build-command text output-file)
             :sentinel
             (lambda (proc event)
               (when (string-match-p "finished" event)
                 (if (file-exists-p output-file)
                     (progn
                       (setq edge-tts--last-file output-file)
                       (edge-tts--play-file output-file)
                       (message "TTS 저장 및 재생: %s" output-file))
                   (message "TTS 저장 실패")))))))))

;;;; ECA/GPTel 통합

;;;###autoload
(defun edge-tts-speak-eca-last-response ()
  "ECA 채팅의 마지막 응답을 음성으로 읽기."
  (interactive)
  (require 'eca nil t)
  (if (and (fboundp 'eca-session) (eca-session))
      (let* ((session-id (eca--session-id (eca-session)))
             (eca-buf (get-buffer (format "<eca-chat:%s>" session-id))))
        (if eca-buf
            (with-current-buffer eca-buf
              (save-excursion
                (goto-char (point-max))
                (let ((end (point))
                      (start (or (re-search-backward "^assistant: " nil t)
                                 (point-min))))
                  (when (> end start)
                    (edge-tts-speak (buffer-substring-no-properties start end))))))
          (message "ECA 채팅 버퍼를 찾을 수 없습니다.")))
    (message "ECA 세션이 없습니다.")))

;;;###autoload
(defun edge-tts-speak-gptel-last-response ()
  "GPTel 채팅의 마지막 응답을 음성으로 읽기."
  (interactive)
  (when (derived-mode-p 'gptel-mode)
    (save-excursion
      (goto-char (point-max))
      (let ((end (point))
            (start (or (re-search-backward gptel-prompt-prefix-alist nil t)
                       (point-min))))
        (when (> end start)
          (edge-tts-speak (buffer-substring-no-properties start end)))))))

;;;; 키바인딩

(map! :leader
      (:prefix ("-" . "voice")
       :desc "TTS: 텍스트 입력" "t" #'edge-tts-speak
       :desc "TTS: 영역 읽기" "r" #'edge-tts-speak-region
       :desc "TTS: 라인 읽기" "l" #'edge-tts-speak-line
       :desc "TTS: 문장 읽기" "s" #'edge-tts-speak-sentence
       :desc "TTS: 문단 읽기" "p" #'edge-tts-speak-paragraph
       :desc "TTS: 버퍼 읽기" "b" #'edge-tts-speak-buffer
       :desc "TTS: 중지" "q" #'edge-tts-stop
       :desc "TTS: 재재생" "R" #'edge-tts-replay-last
       :desc "TTS: 음성 선택" "v" #'edge-tts-set-voice
       :desc "TTS: 속도 조정" "V" #'edge-tts-set-rate
       :desc "TTS: 파일 정리" "c" #'edge-tts-cleanup-old-files
       :desc "TTS: Org TTS태그 저장(직접)" "o" #'edge-tts-save-org-tts-heading
       :desc "TTS: Org TTS태그 저장+재생" "O" #'edge-tts-save-and-play-org-tts-heading
       :desc "TTS: Org→txt 추출" "x" #'edge-tts-export-org-tts-to-txt
       :desc "TTS: txt→mp3 변환" "m" #'edge-tts-convert-txt-to-mp3
       :desc "TTS: Org 전체 워크플로우" "w" #'edge-tts-org-tts-full-workflow))

;; AI 채팅 응답 읽기
(map! :leader
      (:prefix ("=" . "AI")
       :desc "TTS: ECA 응답" "t" #'edge-tts-speak-eca-last-response
       :desc "TTS: GPTel 응답" "T" #'edge-tts-speak-gptel-last-response))

;; 전역 키바인딩 (선택적)
(global-set-key (kbd "C-c t") #'edge-tts-speak-region)

;;; 사용법
;;
;; 기본 명령어:
;;   SPC - t     텍스트 직접 입력
;;   SPC - r     선택 영역 읽기
;;   SPC - l     현재 라인 읽기
;;   SPC - s     현재 문장 읽기
;;   SPC - p     현재 문단 읽기
;;   SPC - b     버퍼 전체 읽기
;;   SPC - q     재생 중지
;;   SPC - R     마지막 파일 재재생
;;
;; Org TTS 워크플로우 (긴 텍스트용, 진행 상황 표시):
;;   SPC - w     전체 워크플로우 (txt 추출 → mp3 변환)
;;   SPC - x     Org TTS 태그 → txt 추출만
;;   SPC - m     txt → mp3 변환만 (Python 스크립트)
;;
;; Org TTS 직접 변환 (짧은 텍스트용):
;;   SPC - o     Org TTS 태그 → mp3 직접 저장
;;   SPC - O     Org TTS 태그 → mp3 저장 + 재생
;;
;; 설정 변경:
;;   SPC - v     음성 선택
;;   SPC - V     속도 조정
;;   SPC - c     오래된 파일 삭제
;;
;; AI 채팅 통합:
;;   SPC = t     ECA 마지막 응답 읽기
;;   SPC = T     GPTel 마지막 응답 읽기
;;
;; 필수 패키지:
;;   - edge-tts: pip install edge-tts
;;   - tqdm: pip install tqdm (진행 표시용, 선택)
;;   - mpv: pkg install mpv (Termux)
;;
;; 설정 커스터마이징:
;;   M-x customize-group RET edge-tts RET
;;

(provide 'ai-tts-edge)
;;; ai-tts-edge.el ends here
