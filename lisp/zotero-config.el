;;; zotero-config.el --- Zotero Translation Server integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Zotero Translation Server를 통한 URL 저장 기능
;; Zotero Desktop 없이 웹 라이브러리에 직접 저장
;;
;; 요구사항:
;;   - Translation Server 실행 중 (localhost:1969)
;;   - ZOTERO_API_KEY, ZOTERO_USER_ID 환경변수 또는 .envrc 설정
;;
;; 사용법:
;;   SPC n z u - URL 입력해서 저장
;;   SPC n z p - 커서 위치 URL 저장
;;   SPC n z l - Org 링크 저장
;;   SPC n z c - 클립보드 URL 저장
;;   SPC n z s - 서버 상태 확인

;;; Code:

(defvar zotero-save-el-path "~/sync/emacs/zotero-config/scripts/zotero-save.el"
  "Path to zotero-save.el file.")

(defvar zotero-translation-server-url "http://localhost:1969"
  "Zotero Translation Server URL.")

(defun zotero--server-running-p ()
  "Check if Translation Server is running."
  (condition-case nil
      (let ((url-request-method "GET"))
        (with-current-buffer (url-retrieve-synchronously
                              zotero-translation-server-url
                              t nil 2)  ; silent, no-cookies, 2 sec timeout
          (prog1 t (kill-buffer))))
    (error nil)))

(defun zotero-start-server ()
  "Start Translation Server using run.sh script."
  (interactive)
  (let ((script "~/sync/emacs/zotero-config/scripts/run.sh"))
    (if (file-exists-p (expand-file-name script))
        (progn
          (async-shell-command (concat script " start"))
          (message "Starting Translation Server..."))
      (message "run.sh not found. Start manually: docker run -d -p 1969:1969 zotero/translation-server"))))

(defun zotero-stop-server ()
  "Stop Translation Server."
  (interactive)
  (let ((script "~/sync/emacs/zotero-config/scripts/run.sh"))
    (if (file-exists-p (expand-file-name script))
        (progn
          (shell-command (concat script " stop"))
          (message "Translation Server stopped"))
      (message "run.sh not found"))))

;; 조건부 로딩: 파일 존재 + 서버 실행 중
(when (file-exists-p (expand-file-name zotero-save-el-path))
  (load-file (expand-file-name zotero-save-el-path))

  ;; 환경변수에서 자동 로드 (.envrc 대응)
  (when-let ((api-key (getenv "ZOTERO_API_KEY")))
    (setq zotero-api-key api-key))
  (when-let ((user-id (getenv "ZOTERO_USER_ID")))
    (setq zotero-user-id user-id))

  ;; Doom Emacs 키바인딩 (zotero-save.el의 것을 덮어쓰기)
  (map! :leader
        (:prefix-map ("n" . "notes")
         (:prefix ("z" . "zotero")
          :desc "Save URL" "u" #'zotero-save-url
          :desc "Save URL at point" "p" #'zotero-save-url-at-point
          :desc "Save Org link" "l" #'zotero-save-org-link
          :desc "Save clipboard" "c" #'zotero-save-clipboard
          :desc "Check server" "s" #'zotero-check-server
          :desc "Start server" "S" #'zotero-start-server
          :desc "Stop server" "q" #'zotero-stop-server)))

  (message "Zotero save loaded (server check on first use)"))

(provide 'zotero-config)
;;; zotero-config.el ends here
