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

(defcustom edge-tts-rate "+0%"
  "말하기 속도. 예: +50% (1.5배속), -20% (0.8배속)"
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

(defcustom edge-tts-output-dir (expand-file-name "~/tmp/tts/")
  "TTS 오디오 파일 저장 디렉터리."
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

(defun edge-tts--build-command (text output-file)
  "edge-tts 명령어 빌드.
TEXT: 변환할 텍스트
OUTPUT-FILE: 출력 파일 경로"
  `("edge-tts"
    "--voice" ,edge-tts-voice
    "--rate" ,edge-tts-rate
    "--volume" ,edge-tts-volume
    "--pitch" ,edge-tts-pitch
    "--text" ,text
    "--write-media" ,output-file))

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
       :desc "TTS: 파일 정리" "c" #'edge-tts-cleanup-old-files))

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
;;   - mpv: pkg install mpv (Termux)
;;
;; 설정 커스터마이징:
;;   M-x customize-group RET edge-tts RET
;;

(provide 'ai-tts-edge)
;;; ai-tts-edge.el ends here
