;;; +claude-code-ide-mcp-tools.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: October 23, 2025
;; Modified: October 23, 2025
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/junghan0611/CLAUDE-CODE-IDE-MCP
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description

;; **English Translation:**
;; "The agent called the mcp__emacs-tools__claude-code-ide-mcp-treesit-info tool and confirmed that sendCommonConnectShadow is a function_definition node, but it returned character offset (163588-165817), requiring line number conversion. It was a mistake for me to try to go back to Python. The correct approach is to either convert tree-sitter's character offset to line numbers, or check if tree-sitter can directly provide line numbers. Question: Is there a way for tree-sitter to provide line numbers directly, or do we need character offset → line number conversion?"

;;; Code:


;; The agent wants to return functions. Could you give me some ideas for creating more tools with claude-code-ide-make-tool?
;; 완벽한 상황 파악입니다! `treesit-info`가 **character offset**을 반환하는 문제네요. 에이전트가 원하는 **줄 번호**를 직접 제공하는 커스텀 도구들을 만들어 드리겠습니다.

(progn


;;;; ## 📦 **전체 설정 예시**

;; ;; init.el 또는 config.el에 추가
;; ;; 1. 기본 도구들 활성화
;; (claude-code-ide-emacs-tools-setup)
;; ;; 2. 위의 커스텀 함수들 정의 (모두 복사/붙여넣기)
;; ;; 3. 모든 도구 다시 설정
;; (setq claude-code-ide-enable-mcp-server t)

;; ## 🎯 **사용 예시**
;; 에이전트에게 이렇게 요청할 수 있습니다:
;; 1. "sendCommonConnectShadow 함수의 줄 번호를 알려줘"
;;    → c_function_info 도구 사용
;; 2. "이 파일의 모든 함수를 줄 번호와 함께 나열해줘"
;;    → list_c_functions 도구 사용

;; 3. "현재 커서가 어느 함수 안에 있어?"
;;    → current_c_function 도구 사용

;; 4. "1000번째 줄에는 어떤 함수가 있어?"
;;    → c_function_at_line 도구 사용

;; 5. "offset 163588이 몇 번째 줄이야?"
;;    → offset_to_line 도구 사용

;; ## 💡 **추가 개선 아이디어**

;;;; ### **1. C 함수 정보 (줄 번호 포함) 도구**

(defun my-c-function-info (function-name)
  "C 함수의 정보를 줄 번호와 함께 반환"
  (claude-code-ide-mcp-server-with-session-context nil
    (save-excursion
      (goto-char (point-min))
      (let (result)
        (while (and (null result)
                    (re-search-forward
                     ;; 함수 정의만 찾기: 줄 시작부터 타입 + 함수명 + (
                     (format "^\\(static\\s-+\\)?\\w+\\s-+\\*?%s\\s-*("
                             (regexp-quote function-name))
                     nil t))
          (when (treesit-node-p (treesit-node-at (point)))
            (let* ((node (treesit-node-at (point)))
                   (func-node (treesit-parent-until
                               node
                               (lambda (n)
                                 (equal (treesit-node-type n)
                                        "function_definition")))))
              (when func-node
                (let* ((start-pos (treesit-node-start func-node))
                       (end-pos (treesit-node-end func-node))
                       (start-line (line-number-at-pos start-pos))
                       (end-line (line-number-at-pos end-pos))
                       (code (buffer-substring-no-properties start-pos end-pos))
                       ;; 함수명 추출 (정확하게)
                       (declarator (treesit-node-child-by-field-name
                                   func-node "declarator"))
                       (name-node (treesit-search-subtree
                                  declarator
                                  (lambda (n)
                                    (equal (treesit-node-type n) "identifier"))
                                  t))
                       (actual-name (when name-node (treesit-node-text name-node))))
                  ;; 찾은 함수명이 요청한 함수명과 일치하는지 확인
                  (when (and actual-name (string= actual-name function-name))
                    (setq result (list
                                 :name function-name
                                 :start-line start-line
                                 :end-line end-line
                                 :start-char start-pos
                                 :end-char end-pos
                                 :line-count (- end-line start-line -1)
                                 :code code
                                 :file (buffer-file-name)))))))))
        (or result
            (format "함수 '%s'를 찾을 수 없습니다." function-name))))))

(claude-code-ide-make-tool
 :function #'my-c-function-info
 :name "c_function_info"
 :description "C 함수의 시작/끝 줄 번호와 코드를 반환합니다"
 :args '((:name "function_name"
          :type string
          :description "찾을 함수 이름")))

;;;; ### **2. 모든 C 함수 목록 (줄 번호 포함)**

(defun my-list-c-functions ()
  "현재 버퍼의 모든 C 함수를 줄 번호와 함께 나열"
  (claude-code-ide-mcp-server-with-session-context nil
    (when (treesit-ready-p 'c)
      (let* ((root (treesit-buffer-root-node 'c))
             (functions '())
             (query (treesit-query-compile
                     'c
                     '((function_definition
                        declarator: (function_declarator
                                    declarator: (identifier) @func-name))))))
        (treesit-query-capture root query nil nil
          (lambda (node name)
            (when (eq name 'func-name)
              (let* ((parent (treesit-parent-until
                             node
                             (lambda (n)
                               (equal (treesit-node-type n)
                                      "function_definition"))))
                     (start-pos (treesit-node-start parent))
                     (end-pos (treesit-node-end parent))
                     (start-line (line-number-at-pos start-pos))
                     (end-line (line-number-at-pos end-pos))
                     (func-name (treesit-node-text node)))
                (push (list :name func-name
                           :start-line start-line
                           :end-line end-line
                           :lines (- end-line start-line -1))
                      functions)))))
        (nreverse functions)))))

(claude-code-ide-make-tool
 :function #'my-list-c-functions
 :name "list_c_functions"
 :description "현재 C 파일의 모든 함수를 줄 번호와 함께 나열합니다"
 :args '())

;;;; ### **3. 커서 위치의 함수 정보**

(defun my-current-c-function-info ()
  "현재 커서가 있는 C 함수의 정보 반환"
  (claude-code-ide-mcp-server-with-session-context nil
    (when (treesit-ready-p 'c)
      (let* ((node (treesit-node-at (point)))
             (func-node (treesit-parent-until
                         node
                         (lambda (n)
                           (equal (treesit-node-type n)
                                  "function_definition")))))
        (if func-node
            (let* ((start-pos (treesit-node-start func-node))
                   (end-pos (treesit-node-end func-node))
                   (start-line (line-number-at-pos start-pos))
                   (end-line (line-number-at-pos end-pos))
                   ;; 함수 이름 추출
                   (declarator (treesit-node-child-by-field-name
                               func-node "declarator"))
                   (name-node (treesit-search-subtree
                              declarator
                              (lambda (n)
                                (equal (treesit-node-type n) "identifier"))
                              t))
                   (func-name (treesit-node-text name-node)))
              (list :name func-name
                   :start-line start-line
                   :end-line end-line
                   :current-line (line-number-at-pos)
                   :file (buffer-file-name)))
          "커서가 함수 안에 있지 않습니다.")))))

(claude-code-ide-make-tool
 :function #'my-current-c-function-info
 :name "current_c_function"
 :description "현재 커서가 위치한 C 함수의 정보를 반환합니다"
 :args '())

;;;; ### **4. 파일 내 특정 줄의 함수 찾기**

(defun my-c-function-at-line (line-number)
  "특정 줄 번호에 있는 C 함수 정보 반환"
  (claude-code-ide-mcp-server-with-session-context nil
    (when (treesit-ready-p 'c)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line-number))
        (let* ((node (treesit-node-at (point)))
               (func-node (treesit-parent-until
                           node
                           (lambda (n)
                             (equal (treesit-node-type n)
                                    "function_definition")))))
          (if func-node
              (let* ((start-pos (treesit-node-start func-node))
                     (end-pos (treesit-node-end func-node))
                     (start-line (line-number-at-pos start-pos))
                     (end-line (line-number-at-pos end-pos))
                     (declarator (treesit-node-child-by-field-name
                                 func-node "declarator"))
                     (name-node (treesit-search-subtree
                                declarator
                                (lambda (n)
                                  (and (equal (treesit-node-type n) "identifier")
                                       ;; 첫 번째 identifier가 함수명
                                       (equal (treesit-node-parent n) declarator)))
                                t))
                     (func-name (when name-node (treesit-node-text name-node)))
                     (code (buffer-substring-no-properties start-pos end-pos)))
                (list :name (or func-name "unknown")
                     :start-line start-line
                     :end-line end-line
                     :line-count (- end-line start-line -1)
                     :query-line line-number
                     :code code
                     :file (buffer-file-name)))
            (format "줄 %d에는 함수가 없습니다." line-number)))))))

(claude-code-ide-make-tool
 :function #'my-c-function-at-line
 :name "c_function_at_line"
 :description "특정 줄 번호에 있는 C 함수를 찾습니다"
 :args '((:name "line_number"
          :type integer
          :description "조회할 줄 번호")))

;;;; ### **5. Character Offset → Line Number 변환 도구**

(defun my-offset-to-line (char-offset)
  "Character offset을 줄 번호로 변환"
  (claude-code-ide-mcp-server-with-session-context nil
    (save-excursion
      (goto-char char-offset)
      (list :line (line-number-at-pos)
           :column (current-column)
           :offset char-offset
           :file (buffer-file-name)))))

(claude-code-ide-make-tool
 :function #'my-offset-to-line
 :name "offset_to_line"
 :description "Character offset을 줄 번호로 변환합니다"
 :args '((:name "char_offset"
          :type integer
          :description "변환할 character offset")))

;;;; ### **6. 함수 시그니처만 추출**

(defun my-c-function-signature (function-name)
  "함수 시그니처만 반환 (선언부만)"
  (claude-code-ide-mcp-server-with-session-context nil
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format "\\b%s\\s-*(" (regexp-quote function-name))
             nil t)
        (let* ((node (treesit-node-at (point)))
               (func-node (treesit-parent-until
                           node
                           (lambda (n)
                             (equal (treesit-node-type n)
                                    "function_definition"))))
               (declarator (treesit-node-child-by-field-name
                           func-node "declarator"))
               (type (treesit-node-child-by-field-name
                     func-node "type"))
               (sig-end (treesit-node-end declarator))
               (sig-start (treesit-node-start func-node))
               (signature (buffer-substring-no-properties sig-start sig-end)))
          (list :signature (string-trim signature)
               :line (line-number-at-pos sig-start)))))))

(claude-code-ide-make-tool
 :function #'my-c-function-signature
 :name "c_function_signature"
 :description "C 함수의 시그니처(선언부)만 반환합니다"
 :args '((:name "function_name"
          :type string
          :description "함수 이름")))

)


;; 이 도구들로 에이전트는 더 이상 Python 스크립트를 만들 필요 없이, **Emacs의
;; tree-sitter + eglot**를 직접 활용하여 정확한 줄 번호를 얻을 수 있습니다!

;;; CLAUDE-CODE-IDE-MCP.el ends here
