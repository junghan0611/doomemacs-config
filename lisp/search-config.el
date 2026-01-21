;;; $DOOMDIR/lisp/search-config.el --- Search Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; 검색 관련 패키지 설정
;; - recent-rgrep: 최근 파일 기반 grep

;;; Code:

;;;; recent-rgrep

;; 최근 수정된 파일에서 grep 검색
;; $ recent-rgrep -f '*.org' 'happy to see you'
(use-package! recent-rgrep
  :defer t
  :commands (recent-rgrep)
  :init
  (map! "M-s g" #'recent-rgrep
        "M-F"   #'recent-rgrep))

;;;; consult-ripfd

;; consult-ripfd: 통합 파일 + 컨텐츠 검색 인터페이스
;; https://github.com/jdtsmith/consult-ripfd
;;
;; [특징]
;; - ripgrep (rg): 파일 내용 검색
;; - fd: 파일명 검색
;; - 단일 인터페이스에서 두 검색 결과 통합 표시
;; - consult 기반 → 기존 preview, embark 액션 자동 지원
;;
;; [사용법]
;; 1. s-F 또는 M-x consult-ripfd 실행
;; 2. 검색어 입력 (파일명 + 컨텐츠 동시 검색)
;; 3. C-SPC로 preview, M-o로 embark 액션
;;
;; [Embark 연계]
;; consult-ripfd는 consult-grep과 동일한 category를 사용하므로
;; embark-file-map과 embark-consult-map의 액션이 자동 적용됨:
;; - embark-file-map: 파일 열기, dired, external 등
;; - embark-consult-map: export, wgrep 편집 등
;; - 커스텀 액션: gptel-quick (`[`), project search (`/`) 등
;;
;; [gptel 연계 시나리오]
;; 1. consult-ripfd로 코드/문서 검색
;; 2. M-o → `[` (gptel-quick) 또는 커스텀 액션
;; 3. 선택한 파일/내용을 gptel에 전달하여:
;;    - 코드 설명 요청
;;    - 리팩토링 제안
;;    - 문서 요약/번역
;;    - 버그 분석
;;
;; [TODO] embark-consult-map에 다음 액션 추가 검토:
;; - 선택 파일들을 gptel context로 일괄 추가
;; - org-mode heading/src-block을 gptel에 직접 전달
;; - 검색 결과를 임시 버퍼로 collect → gptel 분석
(use-package! consult-ripfd
  :after consult
  :bind ("s-F" . consult-ripfd))

;;; provide

(provide 'search-config)

;;; search-config.el ends here
