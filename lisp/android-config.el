;;; $DOOMDIR/lisp/android-config.el --- Android/Kotlin 개발 환경 -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Android/Kotlin CLI 기반 개발 환경 설정
;;
;; 철학: 의존성 최소화 + 에이전트 투명성
;;   - 모든 작업은 CLI로 수행 (에이전트 검토 가능)
;;   - IDE 의존성 제거 (Android Studio 선택적)
;;   - 텍스트 기반 설정만 사용 (Git 추적 가능)
;;
;; 환경 요구사항 (Ubuntu 24.04 검증됨):
;;   - OpenJDK 21
;;   - Kotlin 2.1.10 (/usr/lib/kotlinc/bin)
;;   - Android SDK cmdline-tools 12.0 (~/Android)
;;   - Android NDK 28.2.13676358
;;   - kotlin-language-server (선택적)
;;
;; 환경 변수 (~/.bashrc):
;;   export ANDROID_HOME=~/Android
;;   export ANDROID_SDK_ROOT=~/Android
;;   export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin
;;   export PATH=$PATH:$ANDROID_HOME/platform-tools
;;   export PATH=$PATH:$ANDROID_HOME/build-tools/34.0.0
;;   export PATH=$PATH:/usr/lib/kotlinc/bin

;;; Gradle CLI 빌드 예시:
;;
;;   cd ~/repos/work/kyungdong-rockchip/kd-matter-wallpad
;;   ./gradlew assembleDebug
;;   ./gradlew test
;;   ./gradlew lint
;;
;; ADB 명령어 예시:
;;
;;   adb devices
;;   adb install -r app/build/outputs/apk/debug/app-debug.apk
;;   adb logcat -s KdWallpad:* Matter:* Thread:*

;;; 사용자 정의 함수 예시 (필요시 추가):
;;
;; (defvar kd/project-root "~/repos/work/kyungdong-rockchip/kd-matter-wallpad")
;;
;; (defun kd/gradle-build ()
;;   "Gradle debug APK 빌드."
;;   (interactive)
;;   (let ((default-directory kd/project-root))
;;     (compile "./gradlew assembleDebug")))
;;
;; (defun kd/adb-logcat ()
;;   "ADB logcat 실행."
;;   (interactive)
;;   (async-shell-command
;;    "adb logcat -s KdWallpad:* Matter:* Thread:*"
;;    "*adb-logcat*"))

;;; Code:

;;;; Kotlin LSP (eglot)

(after! eglot
  (add-to-list 'eglot-server-programs
               '(kotlin-mode . ("kotlin-language-server"))))

;;;; Provide

(provide 'android-config)

;;; android-config.el ends here
