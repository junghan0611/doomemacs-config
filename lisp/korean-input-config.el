;;; $DOOMDIR/lisp/korean-input-config.el --- Korean Input NFD to NFC Normalization -*- lexical-binding: t; -*-

;;; Commentary:

;; 한글 입력 시스템 전체 설정
;; - Input method: korean-hangul (Emacs 내장 입력기)
;; - 터미널 한영키: term-keys + wezterm 조합으로 RightAlt/S-SPC 전달
;; - Evil 모드 연동 (Normal→입력기 OFF, Insert→복원)
;; - NFD→NFC: 수동 변환만 지원 (M-x korean/nfc-normalize-buffer)
;; - 폰트/이모지: Noto Emoji + Noto Sans Symbols 1/2 + Noto Sans Math fallback
;;
;; 터미널 한영키 흐름:
;;   Right Alt → xkb(us) Alt_R → fcitx5 통과 → wezterm RightAlt
;;   → wezterm.lua SendString \x1b\x1f\x50\x60\x1f
;;   → Emacs term-keys input-decode-map → <Hangul> → toggle-input-method
;;
;; 전제조건:
;;   - fcitx5 기본그룹(Default/영어)로 두어야 Alt_R가 통과됨
;;   - i3-fcitx5-group.sh가 wezterm 포커스 시 자동 전환
;;   - undo-fu C-M-_ 충돌 해결 필수 (config.el 참조)
;;
;; References:
;; - https://github.com/junghan0611/term-keys (Hangul → RIGHT_ALT/RightAlt 포크)
;; - https://github.com/wezterm/wezterm/issues/2482
;; - https://github.com/org-roam/org-roam/issues/1423
;; - [[denote:20260410T203723][터미널 이맥스 한글 입력 삽질기]]

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+
;; | 📊🎉✨⭐🔥 | abcdefghij |
;; +------------+------------+

;;; Code:

;;;; Input System : Hangul

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")

;; char-width-table 좁히기 — "왜 둘 다 필요한가"
;;
;; Korean 언어 환경이 use-cjk-char-width-table 로 EA Ambiguous 전체를 2-cell 로
;; 올려버린다(역사적으로 한·영 혼용 표 정렬을 위한 디폴트). 지금 워크플로에선
;; em-dash(—)·hyphen 계열·Box Drawing(─│) 이 1-cell 이길 원함.
;;
;; 중요: 이건 WezTerm 과 "중복" 이 아니라 "합의".
;;   GUI Emacs  → 이 char-width-table 만이 레이아웃을 결정. WezTerm 무관.
;;   TTY Emacs  → Emacs 가 "컬럼 N+1에 그려라"할 때 쓰는 기준이 이 테이블.
;;                터미널 실제 렌더 폭과 다르면 커서·다음 글자 침범 발생.
;;                즉 WezTerm cell-widths.lua 와 이 테이블이 **같은 값**이어야 함.
;;   한쪽만 바꾸면 TTY 드리프트 재발. 둘 다 1 로 맞춰야 정합.
;;
;; 비-WezTerm 터미널(Termux/S26 네이티브/kitty/foot 등):
;;   대부분 dash·Box Drawing 을 1-cell, 이모지 범위를 2-cell 로 렌더 → 이 테이블
;;   값이 "공약수" 역할. WezTerm 없이도 합리적.
(set-char-table-range char-width-table '(#x2010 . #x2015) 1)  ; ‐‑‒–—― (dash 계열)
(set-char-table-range char-width-table '(#x2500 . #x257F) 1)  ; ─│┌┐└┘├┤ (Box Drawing)

;; 이모지/기호 2-cell widen — configs/cell-widths.lua 와 **동일 범위**.
;; 원래 my/set-emoji-symbol-font 안 after-setting-font-hook 에 있었으나, 부트
;; 타이밍에 따라 1F5E9(🗩)·1F5EA(🗪)·1F6FD-1F6FF 등 일부 codepoint 가 실제로는
;; w=1 로 남는 현상 관찰 → 하위 sub-table 이 이미 singleton 을 쥐고 있으면
;; set-char-table-range 가 cons range 전체를 덮지 못하는 경우가 있다.
;;
;; 따라서:
;; 1. top-level 에서 deterministic 하게 실행 (font hook 의존 제거)
;; 2. aset 반복으로 codepoint 별 강제 쓰기 → sub-table singleton 우회
;;
;; VS-16 (FE0F) 결합문자는 width 0. base(2) + FE0F(0) = 2 로 정합.
(set-char-table-range char-width-table '(#xFE00 . #xFE0F) 0)

(dolist (range '((#x2190 . #x21FF)    ; Arrows
                 (#x2300 . #x23FF)    ; Miscellaneous Technical (⌚⌛⏰⏳)
                 (#x2460 . #x24FF)    ; Enclosed Alphanumerics
                 (#x2600 . #x26FF)    ; Miscellaneous Symbols
                 (#x2700 . #x27BF)    ; Dingbats
                 (#x2B50 . #x2B55)    ; ⭐⭕
                 (#x1F000 . #x1F0FF)  ; Mahjong/Domino/Playing Cards
                 (#x1F100 . #x1F1FF)  ; Enclosed Alphanumeric Supplement + Regional Indicators
                 (#x1F300 . #x1F6FF)  ; Misc Symbols and Pictographs
                 (#x1F700 . #x1F7FF)  ; Alchemical / Geometric Shapes Ext
                 (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
                 (#x1FA00 . #x1FAFF))) ; Chess + Symbols and Pictographs Ext-A
  (cl-loop for c from (car range) to (cdr range)
           do (aset char-width-table c 2)))

(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(set-clipboard-coding-system 'utf-8)

;; NOTE: coding-system-for-read/write는 let-bind 전용 변수.
;; 전역 setq하면 모든 파일/프로세스 I/O에서 자동 인코딩 탐지를 무시하여
;; 바이너리 프로세스, comint, 이미지 등에서 부작용 발생 가능.
;; prefer-coding-system + set-default-coding-systems로 이미 UTF-8이 기본값.
;; (setq coding-system-for-read 'utf-8)  ; REMOVED — 부작용 방지
;; (setq coding-system-for-write 'utf-8) ; REMOVED — 부작용 방지

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3)

(setq system-time-locale "C") ;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.

;; IS-TERMUX: Doom에서 defconst로 정의됨. 미정의 환경(agent Emacs 등)에서 에러 방지.
(defvar IS-TERMUX (bound-and-true-p IS-TERMUX)
  "Doom Emacs IS-TERMUX 호환. 미정의 시 nil.")

(when IS-TERMUX
  (setenv "LANG" "C.UTF-8")
  (setenv "LC_ALL" "C.UTF-8"))

;; 2. 입력 메서드 시각적 피드백 최소화 (모바일 최적화)
(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

;; 3. 한영 전환 키 바인딩 (Emacs 입력 메서드 전용)
;; 안드로이드 IME 한영 전환 사용 안 함!
(global-set-key (kbd "C-\\") 'toggle-input-method)  ; Emacs 기본 (가장 중요!)

;; KKP (Kitty Keyboard Protocol) 한영 전환 키
;; +korean-input-fix.el에서 Alt_R → <Hangul> 매핑 처리
(global-set-key (kbd "<S-SPC>") 'toggle-input-method)  ; GUI 호환
(global-set-key (kbd "<Hangul>") 'toggle-input-method) ; 한글 키 (Alt_R)
(global-set-key (kbd "<Alt_R>") 'toggle-input-method) ; 한글 키 (Alt_R)
(global-set-key (kbd "<menu>") 'toggle-input-method) ;; caps lock as <menu>
(add-hook 'context-menu-mode-hook '(lambda () (define-key context-menu-mode-map (kbd "<menu>") #'toggle-input-method)))

;; Termux/모바일 전용: 추가 토글 키
(when IS-TERMUX
  (global-set-key (kbd "M-SPC") 'toggle-input-method) ; Alt+Space (긴급용)
  (global-set-key (kbd "C-c \\") 'toggle-input-method) ; 최후 수단

  ;; 선택적: 특정 입력 메서드로 즉시 전환
  (global-set-key (kbd "C-c k")
    (lambda () (interactive) (set-input-method "korean-hangul")))
  (global-set-key (kbd "C-c e")
    (lambda () (interactive) (deactivate-input-method))))

;;;; Font and Emoji Settings

(unless (string-equal system-type "android")
;;;###autoload
  (defun my/set-emoji-symbol-font ()
    (interactive)

    (set-fontset-font "fontset-default" 'hangul (font-spec :family (face-attribute 'default :family)))

    (when (display-graphic-p) ; gui
      ;; Symbola 제거 — Noto 심볼 시리즈로 통합 fallback.
      ;; prepend 누적: 마지막 prepend가 최우선. 아래 순서면
      ;; Noto Sans Symbols → Symbols 2 → Math 순으로 조회.
      (set-fontset-font t 'mathematical (font-spec :family "Noto Sans Math") nil 'prepend)
      (set-fontset-font t 'unicode (font-spec :family "Noto Sans Math") nil 'prepend)
      (set-fontset-font t 'unicode (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
      (set-fontset-font t 'unicode (font-spec :family "Noto Sans Symbols") nil 'prepend)
      ;; 기존 'emoji 폰트 폴백 전부 제거 후 Noto Emoji만 등록 —
      ;; 시스템 fontconfig가 fontset에 심어둔 Noto Color Emoji를 명시적으로 쫓아낸다.
      (set-fontset-font t 'emoji nil)
      (set-fontset-font "fontset-default" 'emoji nil)
      (set-fontset-font t 'emoji (font-spec :family "Noto Emoji")))

    (unless (display-graphic-p) ; terminal
      ;; 터미널에서는 Noto Emoji (monochrome) 사용
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil)
      ;; Symbola 제거 — Noto 시리즈로 유니코드 보완.
      ;; append 순서가 조회 순서. Symbols(넓은 범위) → Symbols 2 → Math.
      (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Sans Symbols") nil 'append)
      (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Sans Symbols 2") nil 'append)
      (set-fontset-font "fontset-default" 'unicode (font-spec :family "Noto Sans Math") nil 'append)

      ;; 터미널 폰트 스케일 — 이모지만 0.9 로 축소 (인접 셀 침범 방지).
      ;; Noto Sans Symbols/Math 는 산스 계열이라 모노 advance 근사, 스케일 불필요.
      (setq face-font-rescale-alist
            '(("Noto Emoji" . 0.9)))

      ;; NOTE: char-width-table widen 은 top-level 로 이동 (file 상단 참조).
      ;; font hook 타이밍 의존 문제 해결 + aset 로 sub-table singleton 우회.
      )

    ;; 'symbol 대역 fallback — Symbola 제거, Noto Sans Math 추가.
    ;; 최종 조회 순서: Noto Sans Symbols → Symbols 2 → Math.
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Math") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend))

  (add-hook 'after-setting-font-hook #'my/set-emoji-symbol-font))

;;;; Raw UTF-8 Byte Reassembly (터미널 한글 깨짐 방지)

;; 문제: 시스템 IME(kime)가 터미널로 UTF-8 바이트를 보내면
;; Emacs 이벤트 루프 인터럽트로 바이트 분할 → raw byte 깨짐 발생.
;;
;; 근본 해결: Emacs 내장 korean-hangul 입력기 사용 (섹션 8 참조).
;; 이 함수는 kime 등 시스템 IME로 깨진 텍스트를 수동 복원하는 안전망.
;;
;; 사용법: M-x korean/fix-raw-bytes-in-buffer
;;
;; NOTE: 이전에 after-change-functions + idle-timer로 자동 실행했으나,
;; 모든 버퍼(프로세스 포함)에서 발동 → 수천 건 메시지 폭탄 + 버벅임.
;; 타이머 방식은 좌표 썩음(stale position), 마지막 변경만 생존 등 구조 문제.
;; 자동 실행이 필요하면 before-save-hook 또는 post-self-insert-hook 검토.

(defun korean/eight-bit-char-p (ch)
  "CH가 Emacs eight-bit 문자(디코딩 안 된 raw byte)이면 t."
  (and ch (characterp ch) (>= ch #x3FFF80) (<= ch #x3FFFFF)))

(defun korean/do-reassemble-raw-bytes (search-beg search-end)
  "SEARCH-BEG ~ SEARCH-END 범위에서 raw byte → UTF-8 재디코딩."
  (save-excursion
    (save-match-data
      (let ((inhibit-modification-hooks t)
            (count 0))
        (goto-char search-beg)
        (while (< (point) (min search-end (point-max)))
          (if (korean/eight-bit-char-p (char-after))
              (let ((start (point))
                    (bytes nil))
                ;; 연속 raw byte 수집
                (while (and (< (point) (point-max))
                            (korean/eight-bit-char-p (char-after)))
                  (push (- (char-after) #x3FFF00) bytes)
                  (forward-char 1))
                (setq bytes (nreverse bytes))
                ;; UTF-8로 디코딩 시도
                (condition-case nil
                    (let ((decoded (decode-coding-string
                                    (apply #'unibyte-string bytes)
                                    'utf-8)))
                      (when (and (not (string-empty-p decoded))
                                 ;; 디코딩 결과가 원래 바이트와 다르면 (즉, 실제로 디코딩됨)
                                 (not (string-equal decoded
                                                    (buffer-substring-no-properties start (point)))))
                        (delete-region start (point))
                        (insert decoded)
                        (setq count (1+ count))
                        ;; search-end 조정 (길이 변경)
                        (setq search-end (+ search-end
                                            (- (length decoded) (length bytes))))))
                  (error nil))) ; 디코딩 실패 시 그대로 유지
            (forward-char 1)))
        (when (> count 0)
          (message "한글 raw byte %d건 복원" count))))))

(defun korean/fix-raw-bytes-in-buffer ()
  "현재 버퍼 전체에서 raw byte를 UTF-8로 재디코딩.
kime 등 시스템 IME로 깨진 한글을 수동 복원할 때 사용."
  (interactive)
  (korean/do-reassemble-raw-bytes (point-min) (point-max)))

;;;; NFD to NFC Normalization

(require 'ucs-normalize)

;;; 1. 한글 자모 패턴 정의

(defconst korean-jamo-choseong-regexp
  "[\u1100-\u115F\uA960-\uA97C]"
  "한글 초성 유니코드 범위 (Hangul Choseong)")

(defconst korean-jamo-jungseong-regexp
  "[\u1160-\u11A7\uD7B0-\uD7C6]"
  "한글 중성 유니코드 범위 (Hangul Jungseong)")

(defconst korean-jamo-jongseong-regexp
  "[\u11A8-\u11FF\uD7CB-\uD7FB]?"
  "한글 종성 유니코드 범위 (Hangul Jongseong), 선택적")

(defconst korean-jamo-pattern
  (concat korean-jamo-choseong-regexp
          korean-jamo-jungseong-regexp
          korean-jamo-jongseong-regexp)
  "한글 조합형 자모 전체 패턴 (초성+중성+종성)")

;;; 2. 버퍼 전체 변환 (수동 실행용)

(defun korean/convert-jamo-to-syllable ()
  "현재 버퍼에서 조합형 자모를 완성형 음절로 변환합니다.
NFD(분해형) → NFC(완성형) 변환."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward korean-jamo-pattern nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (ucs-normalize-NFC-region start end)
          (setq count (1+ count))))
      (when (called-interactively-p 'any)
        (message "✅ %d개 자모 → 음절 변환 완료" count)))))

;;; 3. 실시간 변환 (자동 실행)

(defvar-local korean/nfc-timer nil
  "NFC 변환 디바운스 타이머")

(defvar korean/nfc-delay 0.1
  "NFC 변환 지연 시간 (초).
너무 짧으면 성능 저하, 너무 길면 사용자 경험 저하")

(defun korean/after-change-nfc-normalize (beg end _len)
  "텍스트 변경 후 자동으로 NFD → NFC 정규화.
`after-change-functions'에서 호출됨.

BEG: 변경 시작 위치
END: 변경 끝 위치
_LEN: 삭제된 문자 수 (사용 안 함)"
  ;; 디바운스: 짧은 시간 내 연속 입력 시 마지막만 처리
  (when korean/nfc-timer
    (cancel-timer korean/nfc-timer))

  (setq korean/nfc-timer
        (run-with-idle-timer
         korean/nfc-delay nil
         (lambda ()
           (when (buffer-live-p (current-buffer))
             (save-excursion
               (save-match-data
                 ;; 변경된 영역 + 앞뒤 10자 (조합 중인 글자 포함)
                 (let* ((search-begin (max (point-min) (- beg 10)))
                        (search-end (min (point-max) (+ end 10)))
                        (inhibit-modification-hooks t)) ; 재귀 방지
                   (goto-char search-begin)
                   (while (re-search-forward korean-jamo-pattern search-end t)
                     (let ((match-start (match-beginning 0))
                           (match-end (match-end 0)))
                       (ucs-normalize-NFC-region match-start match-end)))))))))))

;;; 4. 저장 전 변환 (안전망)

(defun korean/before-save-nfc-normalize ()
  "파일 저장 전 버퍼 전체를 NFC 정규화.
`before-save-hook'에서 호출됨."
  (when (and buffer-file-name
             (derived-mode-p 'text-mode 'prog-mode))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((count 0)
              (inhibit-modification-hooks t)) ; 재귀 방지
          (while (re-search-forward korean-jamo-pattern nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (ucs-normalize-NFC-region start end)
              (setq count (1+ count))))
          (when (> count 0)
            (message "💾 저장 전 %d개 자모 → 음절 변환" count)))))))

;;; 5. 파일 열기 시 변환 (기존 파일 정리)

(defun korean/find-file-nfc-normalize ()
  "파일 열기 시 자동으로 NFC 정규화.
`find-file-hook'에서 호출됨."
  (when (and buffer-file-name
             (derived-mode-p 'text-mode 'prog-mode))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((count 0)
              (modified-before (buffer-modified-p)))
          (while (re-search-forward korean-jamo-pattern nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (ucs-normalize-NFC-region start end)
              (setq count (1+ count))))
          ;; 변환했지만 원래 수정되지 않은 파일이면 modified 플래그 복원
          (unless modified-before
            (set-buffer-modified-p nil))
          (when (> count 0)
            (message "📂 파일 열기: %d개 자모 정리됨" count)))))))

;;; 6. 글로벌 마이너 모드 정의

(define-minor-mode global-korean-nfc-mode
  "한글 NFD → NFC 자동 변환 글로벌 마이너 모드.
모든 버퍼에서 한글 입력 시 NFD(분해형)를 NFC(완성형)로 변환합니다.

성능 최적화:
- 디바운스: 0.1초 지연 (연속 입력 시 마지막만 처리)
- 제한된 영역: 변경 위치 ±10자만 검사
- 패턴 매칭: 한글 자모가 있을 때만 정규화 실행

활성화 이유:
- Termux/macOS 등에서 IME가 NFD로 입력
- 클립보드에서 NFD 인코딩 텍스트 붙여넣기
- AI API 호출 시 NFD 한글이 문제 발생"
  :lighter " 한"
  :global t
  :group 'korean
  ;; Emacs 내장 입력기(korean-hangul)는 항상 NFC 출력 → 자동 변환 불필요
  ;; NFD 정리 필요 시 M-x korean/nfc-normalize-buffer 명시 호출
  (message "%s 한글 NFC 모드 (%s)" (if global-korean-nfc-mode "✅" "❌")
           "M-x korean/nfc-normalize-buffer"))

;; 하위 호환성: 기존 korean-nfc-mode 호출 시 글로벌 모드로 리다이렉트
(defalias 'korean-nfc-mode 'global-korean-nfc-mode)

;;; 7. NFC 모드

;; Emacs 내장 입력기(korean-hangul)가 항상 NFC 출력하므로 자동 변환 불필요.
;; NFD 정리 필요 시 M-x korean/nfc-normalize-buffer 명시 호출.

;;; 8. 터미널 Emacs 입력기 전략

;; 핵심 원칙: 터미널 Emacs에서는 시스템 IME(kime/fcitx)를 쓰지 않고
;; Emacs 내장 korean-hangul 입력기를 사용한다.
;;
;; 이유: 시스템 IME는 터미널로 UTF-8 바이트를 보내는데,
;; Emacs 이벤트 루프(타이머, 프로세스 출력)가 끼어들면
;; 바이트가 분할 도착 → 디코딩 실패 → \353 \213 같은 깨진 문자.
;; Emacs 입력기는 내부에서 ㄱ→가→간 조합하므로 바이트 문제가 없다.
;;
;; 사용법:
;;   C-\      : 한영 전환 (Emacs 입력기 토글)
;;   <menu>   : 한영 전환 (Caps Lock → Menu 매핑 시)
;;
;; kime는 English 상태로 두고, Emacs 입력기로 한글 입력한다.

(defun korean/evil-normal-deactivate-im ()
  "Evil Normal 모드 진입 시 입력기 끄기."
  (when current-input-method
    (setq-local korean/saved-input-method current-input-method)
    (deactivate-input-method)))

(defun korean/evil-insert-restore-im ()
  "Evil Insert 모드 진입 시 이전 입력기 복원."
  (when (and (bound-and-true-p korean/saved-input-method)
             (not current-input-method))
    (activate-input-method korean/saved-input-method)))

(defvar-local korean/saved-input-method nil
  "Normal 모드 진입 전 활성 입력기 저장")

(after! evil
  (add-hook 'evil-normal-state-entry-hook #'korean/evil-normal-deactivate-im)
  (add-hook 'evil-insert-state-entry-hook #'korean/evil-insert-restore-im))

;;; 9. Android 특화 설정

(when (eq system-type 'android)
  (setq overriding-text-conversion-style nil)
  (setq-default text-conversion-style nil))

(provide 'korean-input-config)

;;; korean-input-config.el ends here
