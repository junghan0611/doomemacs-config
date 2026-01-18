;;; $DOOMDIR/lisp/editing-config.el --- EDITING configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; [2025-01] Terminal Focus Issue - 아래 패키지들이 터미널 모드에서 문제 발생
;; SSH + Ghostty/터미널에서 창 포커스 전환 시 "I" "O" 문자가 입력되는 현상
;; 원인: Focus reporting (DECSET 1004) 관련 ESC[I / ESC[O 시퀀스 충돌
;; 제거된 패키지: olivetti, redacted, logos, focus, centered-cursor-mode
;; 터미널 Emacs 호환성을 위해 제거 결정 (정확한 원인 패키지는 미확인)

;;; Code:

;;;; Ten - Personal Glossary System (핵심 패키지)

;; Ten 典 (ten): 개인 용어 사전 관리 및 자동 하이라이트
;; https://git.sr.ht/~nobiot/ten
;;
;; 핵심 기능:
;; 1. Glossary 파일에서 용어 정의 (org/md/txt 지원)
;; 2. 노트 작성 시 정의된 용어 자동 fontification
;; 3. M-. (xref-find-definitions)로 용어 정의로 점프
;; 4. etags 기반으로 빠른 검색 (TAGS 파일 인덱스 사용)
;; 5. consult-ten으로 용어 사전 빠른 접근
;;
;; 사용법:
;; - M-x ten-tags-create: glossary 파일에서 TAGS 생성/업데이트
;; - M-. 용어 위에서: 정의로 점프
;; - SPC b g: consult-buffer에서 glossary 항목 접근
;;
;; 파일 위치:
;; - Glossary: ~/org/dict/*.{org,md,txt}
;; - TAGS: ~/org/dict/ten-TAGS (636KB, 16536 terms)

;; gavinok-dotfiles/init.el
;; Getting added in emacs 30 https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67687
;; (use-package etags-regen
;;   :when (executable-find "etags")
;;   :custom (etags-regen-tags-file "/tmp/TAGS")
;;   :commands etags-regen-mode
;;   :bind (("C-c t" . complete-tag)
;;          ("C-c M-." . my/goto-etags))
;;   :init
;;   (defvar etags-regen-mode-map (make-sparse-keymap))
;;   (add-to-list 'minor-mode-map-alist (cons 'etags-regen-mode etags-regen-mode-map)))

;; (defun my/goto-etags ()
;;   (interactive)
;;   (let ((xref-backend-functions '(etags--xref-backend t)))
;;     (call-interactively 'xref-find-definitions)))

(when (locate-library "ten")
  (require 'ten)
  (setq ten-glossary-file-extensions '("org" "md" "txt"))
  (setq ten-glossary-exclude-regexps '("/\\."))

  ;; FIX: "Keep current list of tags tables also?" 프롬프트 제거
  ;;
  ;; 문제:
  ;; - ten-font-lock-mode가 활성화될 때마다 visit-tags-table 호출
  ;; - org-mode 파일 열 때마다 "Keep current list?" 프롬프트 표시
  ;;
  ;; 해결책:
  ;; - Emacs 시작 시 tags-file-name과 tags-table-list를 미리 설정
  ;; - visit-tags-table이 같은 파일 인식하여 프롬프트 우회
  ;;
  ;; 성능 특성:
  ;; - 첫 로딩: ~0.11초 (TAGS 파일 읽기 + 버퍼 생성)
  ;; - 재사용: ~0.000초 (버퍼 캐시됨)
  ;; - 메모리: ~449KB (전체 TAGS 파일 버퍼로 로드)
  ;;
  ;; 부작용 검증:
  ;; - xref-backend-functions: 정상 작동 (etags 백엔드 인식)
  ;; - completion-at-point: 정상 작동 (tags 기반 completion)
  ;; - dumb-jump: 영향 없음 (독립적인 xref 백엔드)
  ;; - Doom lookup (gd): 영향 없음 (LSP → xref → dumb-jump 우선순위 유지)
  (when (file-exists-p user-ten-tags-file)
    (setq ten-tags-file-default user-ten-tags-file)
    (setq tags-file-name user-ten-tags-file)
    (setq tags-table-list (list user-ten-tags-file))

    ;; org-mode에서만 활성화 (text-mode 전체는 너무 광범위)
    ;; Info-mode는 Emacs 문서 읽을 때 용어 하이라이트에 유용
    (add-hook 'org-mode-hook 'ten-font-lock-mode)
    (add-hook 'Info-mode-hook 'ten-font-lock-mode)

    ;; consult-buffer (SPC b b)에서 glossary 항목 접근
    (with-eval-after-load 'consult
      (require 'consult-ten)
      (add-to-list 'consult-buffer-sources 'consult-ten-glossary 'append) ; g
      )
    )
  )

;;;; outli

(use-package! outli
  :defer 1
  :init
  (setq outli-speed-commands nil)
  (add-hook 'prog-mode-hook #'outli-mode)
  (add-hook 'conf-mode-hook #'outli-mode)
  :config
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(zig-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(yaml-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
  (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))
  )

;;; pinentry

;; GPG Pinentry 모드 설정
;;
;; IMPORTANT: epa-pinentry-mode는 반드시 nil로 설정해야 함!
;;
;; 이유:
;; - 'loopback: GPG agent를 우회하여 Emacs가 직접 passphrase 처리
;;   → GPG agent 캐시 무시, Emacs 시작할 때마다 passphrase 입력 필요
;;   → NixOS gpg-agent.conf의 캐시 설정(1년)이 무효화됨
;;
;; - nil: GPG agent와 협력하여 passphrase 처리
;;   → GPG agent 캐시 활용, 캐시 기간 동안 재입력 불필요
;;   → Emacs minibuffer에서 입력, 시스템 전체 캐시 공유
;;
;; 참고:
;; - pinentry-start: Emacs를 GPG agent의 pinentry로 등록
;; - 시스템 gpg-agent.conf 설정을 존중하는 것이 올바른 방식

(use-package! pinentry
  :config
  (setq epa-pinentry-mode nil)  ; GPG agent 캐시 활용 (loopback 사용 금지!)
  (pinentry-start))

;;;; imenu-list

;;;###autoload
(defun spacemacs/imenu-list-smart-focus ()
  "Focus the `imenu-list' buffer, creating as necessary.
If the imenu-list buffer is displayed in any window, focus it, otherwise create and focus.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (imenu-list-show)
    (imenu-list-smart-toggle)))

;; Show an outline summary of the current buffer.
(use-package! imenu-list
  :init
  (add-hook 'imenu-list-major-mode-hook #'toggle-truncate-lines)
  ;; (setq imenu-list-focus-after-activation nil)
  (setq imenu-list-size 40)
  (setq imenu-list-auto-resize nil))

;;;; imenu-list-mode-map

(after! imenu-list
  (map! :map imenu-list-major-mode-map
        :n "f"      #'hs-toggle-hiding
        :n "g"      #'imenu-list-refresh
        :n "r"      #'imenu-list-refresh
        :n "d"      #'imenu-list-display-dwim
        :n "RET"    #'imenu-list-ret-dwim
        :n "u"      #'imenu-list-up-level
        :n "z u"    #'imenu-list-up-level ; outline-up-heading
        :n "^"      #'imenu-list-up-level  ; dired style
        :n "C-S-p"  #'imenu-list-up-level  ; sync org-mode markdown-mode
        :n "M-j"    #'imenu-list-next-entry-same-level
        :n "M-k"    #'imenu-list-previous-entry-same-level
        :n "M-n"    #'evil-next-line
        :n "M-p"    #'evil-previous-line))

;;;; provide

(provide 'editing-config)

;;; editing-config end here
