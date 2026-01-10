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

;;; provide

(provide 'search-config)

;;; search-config.el ends here
