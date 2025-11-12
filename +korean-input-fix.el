;;; ~/.doom.d/+korean-input-fix.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Termux Emacs 터미널 환경에서 한글 입력 시 NFD → NFC 실시간 변환
;;
;; 문제: Termux IME가 한글을 NFD(분해형)로 입력하여 스페이스 입력 시
;;       조합형 자모가 완성형 음절로 변환되지 않고 분리되어 저장됨
;;
;; 해결: after-change-functions 훅으로 입력 즉시 NFC 정규화 적용
;;
;; Author: junghanacs (with Claude Code)
;; Date: 2025-11-12
;; Version: 2.0.0
;;
;; References:
;; - https://github.com/wezterm/wezterm/issues/2482
;; - https://github.com/org-roam/org-roam/issues/1423
;; - https://jyun.rbind.io/post/utf_hfs/

;;; Code:

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

;;; 6. 마이너 모드 정의

(define-minor-mode korean-nfc-mode
  "한글 NFD → NFC 자동 변환 마이너 모드.
Termux 터미널 환경에서 한글 입력 문제를 해결합니다."
  :lighter " 한"
  :global nil
  (if korean-nfc-mode
      (progn
        (add-hook 'after-change-functions #'korean/after-change-nfc-normalize nil t)
        (add-hook 'before-save-hook #'korean/before-save-nfc-normalize nil t)
        (message "✅ 한글 NFC 모드 활성화"))
    (progn
      (remove-hook 'after-change-functions #'korean/after-change-nfc-normalize t)
      (remove-hook 'before-save-hook #'korean/before-save-nfc-normalize t)
      (when korean/nfc-timer
        (cancel-timer korean/nfc-timer)
        (setq korean/nfc-timer nil))
      (message "❌ 한글 NFC 모드 비활성화"))))

;;; 7. 전역 활성화 (Termux 환경에서만)

(defun korean/enable-nfc-mode-if-needed ()
  "Termux 환경이면 자동으로 korean-nfc-mode 활성화"
  (when (and (not (display-graphic-p))
             (or (getenv "TERMUX_VERSION")
                 (string-match-p "termux" (or (getenv "PREFIX") ""))))
    (korean-nfc-mode 1)))

;; text-mode와 prog-mode에서 자동 활성화
(add-hook 'text-mode-hook #'korean/enable-nfc-mode-if-needed)
(add-hook 'prog-mode-hook #'korean/enable-nfc-mode-if-needed)

;; 파일 열기 시에도 체크
(add-hook 'find-file-hook #'korean/find-file-nfc-normalize)

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

(provide '+korean-input-fix)
;;; +korean-input-fix.el ends here
