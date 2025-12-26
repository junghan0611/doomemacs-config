;;; $DOOMDIR/lisp/korean-input.el --- Korean Input NFD to NFC Normalization -*- lexical-binding: t; -*-

;;; Commentary:

;; 한글 입력 시스템 전체 설정
;; - Input method: korean-hangul
;; - 터미널 환경에서 NFD → NFC 실시간 변환
;; - Termux/Kitty term-keys 프로토콜 지원
;; - Evil 모드 연동
;; - 폰트 및 이모지 설정
;;
;; References:
;; - https://github.com/wezterm/wezterm/issues/2482
;; - https://github.com/org-roam/org-roam/issues/1423
;; - https://jyun.rbind.io/post/utf_hfs/

;;; Code:

;;;; Input System : Hangul

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important
(set-clipboard-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; 멀티바이트 모드 활성화 (필요시)
(set-buffer-multibyte t)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3)

(setq system-time-locale "C") ;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.

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
      (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) ;; 2024-09-16 테스트 -- 𝑀＜1
      (set-fontset-font t 'mathematical (font-spec :family "Symbola") nil 'prepend) ; best
      ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
      (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top
      )

    (unless (display-graphic-p) ; terminal
      ;; 터미널에서는 Noto Color Emoji 사용 (컬러 이모지 지원시)
      (set-fontset-font "fontset-default" 'emoji (font-spec :family "Noto Emoji") nil)
      ;; 폴백 폰트 설정 (Noto Emoji가 없는 경우)
      ;; (set-fontset-font "fontset-default" 'unicode (font-spec :family "DejaVu Sans Mono") nil 'append)

      ;; 터미널에서 폰트 스케일 조정 (이모지 크기 일정하게)
      (setq face-font-rescale-alist
            '(
              ;; ("Noto Color Emoji" . 0.9)
              ("Noto Emoji" . 0.9)
              ("Symbola" . 0.9)))

      ;; 이모지 문자의 너비를 2로 고정 (double-width)
      ;; 주요 이모지 범위들
      (dolist (range '((#x1F300 . #x1F6FF)  ; Misc Symbols and Pictographs
                       (#x1F700 . #x1F77F)  ; Alchemical Symbols
                       (#x1F780 . #x1F7FF)  ; Geometric Shapes Extended
                       (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
                       (#x1FA00 . #x1FA6F)  ; Chess Symbols
                       (#x1FA70 . #x1FAFF)  ; Symbols and Pictographs Extended-A
                       (#x2600 . #x26FF)    ; Miscellaneous Symbols
                       (#x2700 . #x27BF)    ; Dingbats
                       (#xFE00 . #xFE0F)    ; Variation Selectors
                       (#x1F000 . #x1F02F)  ; Mahjong Tiles
                       (#x1F030 . #x1F09F)  ; Domino Tiles
                       (#x1F0A0 . #x1F0FF))) ; Playing Cards
        (set-char-table-range char-width-table range 2))
      ;; 특정 이모지들을 유니코드 코드포인트로 너비 설정
      (dolist (codepoint '(#x1F600 #x1F603 #x1F604 #x1F601 #x1F606 #x1F605 #x1F602 #x1F923 #x1F60A #x1F607
                           #x1F642 #x1F643 #x1F609 #x1F60C #x1F60D #x1F970 #x1F618 #x1F617 #x1F619 #x1F61A
                           #x1F60B #x1F61B #x1F61C #x1F92A #x1F61D #x1F911 #x1F917 #x1F92D #x1F92B #x1F914
                           #x1F525 #x1F4AF #x2728 #x2B50 #x1F31F #x1F4AB #x1F308 #x2600 #x1F31E #x1F31D
                           #x2764 #x1F9E1 #x1F49B #x1F49A #x1F499 #x1F49C #x1F5A4 #x1F90D #x1F90E #x1F494
                           #x2705 #x274C #x2B55 #x1F534 #x1F7E0 #x1F7E1 #x1F7E2 #x1F535 #x1F7E3 #x26AB
                           #x26AA #x1F7E4 #x1F536 #x1F537 #x1F538 #x1F539 #x1F53A #x1F53B #x1F4A0 #x1F532))
        (set-char-table-range char-width-table codepoint 2)))

    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend))

  (add-hook 'after-setting-font-hook #'my/set-emoji-symbol-font))

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
  (if global-korean-nfc-mode
      (progn
        (add-hook 'after-change-functions #'korean/after-change-nfc-normalize)
        (add-hook 'before-save-hook #'korean/before-save-nfc-normalize)
        (message "✅ 글로벌 한글 NFC 모드 활성화"))
    (progn
      (remove-hook 'after-change-functions #'korean/after-change-nfc-normalize)
      (remove-hook 'before-save-hook #'korean/before-save-nfc-normalize)
      (message "❌ 글로벌 한글 NFC 모드 비활성화"))))

;; 하위 호환성: 기존 korean-nfc-mode 호출 시 글로벌 모드로 리다이렉트
(defalias 'korean-nfc-mode 'global-korean-nfc-mode)

;;; 7. 자동 활성화

;; Termux/Android에서는 항상 활성화 (IME가 NFD로 입력하는 경우가 많음)
(when IS-TERMUX
  (global-korean-nfc-mode 1)
  (add-hook 'find-file-hook #'korean/find-file-nfc-normalize))

;; 터미널 Emacs (-nw)에서도 활성화
;; GUI Emacs에서는 시스템 IME가 NFC로 정상 입력되므로 조건부
(when (and (not IS-TERMUX)
           (not (display-graphic-p)))
  (global-korean-nfc-mode 1)
  (add-hook 'find-file-hook #'korean/find-file-nfc-normalize))

;;; 8. 키바인딩 (옵션)

(defun korean/setup-keybindings ()
  "한글 변환 관련 키바인딩 설정"
  (when (featurep 'evil)
    ;; SPC m k로 korean 네임스페이스
    (map! :localleader
          :desc "한글 자모 → 음절 변환" "k n" #'korean/convert-jamo-to-syllable
          :desc "한글 NFC 모드 토글" "k t" #'korean-nfc-mode)))

(with-eval-after-load 'doom-keybinds
  (korean/setup-keybindings))

;;; 9. KKP (Kitty Keyboard Protocol) 한영 키 지원
;;
;; 필요한 키:
;;   - S-SPC: Shift+Space (한영 전환)
;;   - Alt_R: 시스템 언어 en일 때 한글 키 위치 (한영 전환)
;;   - M-v: Meta+v (추가 시 사용)
;;
;; 디버깅:
;;   M-x korean/test-raw-input → 키 누르면 hex 시퀀스 확인
;;   M-x kkp-status → KKP 활성화 상태 확인

(defun korean/test-raw-input ()
  "터미널에서 raw 키 입력 확인 (input-decode-map 우회).
새 키 추가 시 이 함수로 hex 시퀀스를 확인한 후 아래에 매핑 추가."
  (interactive)
  (let ((input-decode-map (make-sparse-keymap))
        (events nil)
        (key (read-event "Press a key (raw mode): "))
        (timeout 0.5)
        evt)
    (push key events)
    (while (setq evt (read-event nil nil timeout))
      (push evt events)
      (setq timeout 0.1))
    (setq events (nreverse events))
    (message "RAW events: %s\nHex: %s"
             events
             (mapconcat (lambda (e) (format "0x%x" e)) events " "))))

(defun korean/setup-kkp-hangul-key ()
  "KKP 키 매핑 설정: S-SPC, Alt_R (한글 키), M-v.

주의:
  - report-all-keys-as-escape-codes 플래그 사용 금지!
    (a-z 키가 escape code로 전송되면 한글 입력 불가)
  - modifyOtherKeys 호환 시퀀스만 사용
  - 시스템 언어 en일 때 한글 키는 Alt_R로 전송됨"
  (when (and (featurep 'kkp)
             (not (display-graphic-p)))

    ;; ========================================
    ;; Kitty term-keys 시퀀스 매핑 (함수 방식)
    ;; ========================================

    ;; ESC 뒤에 오는 시퀀스를 읽어서 처리하는 함수
    (define-key input-decode-map [?\e ?\x1f]
      (lambda (&optional _prompt)
        (let ((char (read-event nil nil 0.01)))
          (cond
           ;; P! 시퀀스 시작: S-SPC 또는 Hangul
           ((eq char ?P)
            (let ((next (read-event nil nil 0.01)))
              (cond
               ;; S-SPC: \x1b\x1fP!\x1f
               ((eq next ?!)
                (let ((end (read-event nil nil 0.01)))
                  (if (eq end ?\x1f)
                      (kbd "S-SPC")
                    [?\e ?\x1f ?P ?! end])))
               ;; Hangul: \x1b\x1fP`\x1f (96 = 백틱)
               ((eq next 96)
                (let ((end (read-event nil nil 0.01)))
                  (if (eq end ?\x1f)
                      (kbd "<Hangul>")
                    [?\e ?\x1f ?P 96 end])))
               ;; 다른 시퀀스
               (t [?\e ?\x1f ?P next]))))
           ;; 다른 시퀀스는 그대로 전달
           (t [?\e ?\x1f char])))))

    ;; M-v: Meta+v (필요 시 추가)
    ;; 대부분 터미널에서 M-v는 이미 작동하므로 명시적 매핑 불필요
    ;; 작동 안 하면 아래 주석 해제:
    ;; (define-key input-decode-map "\e[27;3;118~" (kbd "M-v"))  ; modifyOtherKeys

    ;; Modifier 키 단독 입력 무시 (undefined 메시지 방지)
    ;; (global-set-key (kbd "<SHIFT_L>") 'ignore)
    ;; (global-set-key (kbd "<SHIFT_R>") 'ignore)
    ;; (global-set-key (kbd "<Control_L>") 'ignore)
    ;; (global-set-key (kbd "<Control_R>") 'ignore)
    ;; (global-set-key (kbd "<Alt_L>") 'ignore)

    (message "✅ KKP: S-SPC, Alt_R (Hangul) 매핑 완료")))

;; KKP 로드 후 자동 실행
(with-eval-after-load 'kkp
  (korean/setup-kkp-hangul-key))

(add-hook 'tty-setup-hook #'korean/setup-kkp-hangul-key)

;;;; evil + hangul

;; 4. Evil 모드 연동: 자동 한영 전환
(after! evil
  ;; 버퍼별 입력 메서드 상태 저장
  (defvar-local my/saved-input-method nil
    "Normal 모드 진입 전 입력 메서드 상태")

  (defun my/evil-normal-state-korean-off (&rest _)
    "Normal 모드 진입: 한글 OFF, 상태 저장"
    (when (and (boundp 'current-input-method) current-input-method)
      (setq my/saved-input-method current-input-method)
      (deactivate-input-method)))

  (defun my/evil-insert-state-korean-restore ()
    "Insert 모드 진입: 이전 한글 상태 복원"
    (when (and my/saved-input-method
               (not current-input-method))
      (activate-input-method my/saved-input-method)))

  ;; Hook 등록
  (add-hook 'evil-normal-state-entry-hook #'my/evil-normal-state-korean-off)
  (add-hook 'evil-insert-state-entry-hook #'my/evil-insert-state-korean-restore)

  ;; Evil escape 후에도 확실히 끄기
  (advice-add 'evil-normal-state :after #'my/evil-normal-state-korean-off)

  ;; Shift+Space 메시지 (motion/normal/visual 모드에서)
  (mapc (lambda (mode)
          (let ((keymap (intern (format "evil-%s-state-map" mode))))
            (define-key (symbol-value keymap) [?\S- ]
                        #'(lambda () (interactive)
                            (message
                             (format "Input method is disabled in %s state." evil-state))))))
        '(motion normal visual))
  )

;; 5. Emacs 입력 메서드 추가 최적화
(with-eval-after-load 'quail
  ;; 한글 입력 모드 표시 (모드라인)
  (setq-default mode-line-mule-info
    '((:eval (if current-input-method
                 (propertize " [한] " 'face '(:foreground "green"))
               " [En] "))))

  ;; 2벌식 기본 사용 (3벌식 원하면 변경)
  ;; (setq default-korean-keyboard "390") ; 3벌식 최종
  )

;; 6. 안드로이드 Emacs 특화 설정 (해당시)
(when (string-equal system-type "android")
  ;; Android Emacs의 IME 간섭 차단
  (setq overriding-text-conversion-style nil)
  (setq-default text-conversion-style nil))

(provide 'korean-input)
;;; korean-input.el ends here
