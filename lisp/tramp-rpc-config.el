;;; $DOOMDIR/lisp/tramp-rpc-config.el --- TRAMP-RPC configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; tramp-rpc: 고성능 TRAMP 백엔드 (JSON-RPC → MessagePack-RPC)
;; https://github.com/ArthurHeymans/emacs-tramp-rpc
;;
;; 사용법: /rpc:user@host:/path/to/file
;; 첫 연결 시 Rust 서버 바이너리 자동 배포 (~850KB)
;;
;; 성능: 기존 TRAMP 대비 2-38x 빠름
;; - 연결 설정: 38x, file-exists: 12x, 디렉토리 목록: 27x

;;; Code:

;;;; msgpack (의존성)

(use-package! msgpack)

;;;; tramp-rpc

(use-package! tramp-rpc
  :after tramp)

;;; Provide

(provide 'tramp-rpc-config)

;;; tramp-rpc-config.el ends here
