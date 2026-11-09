;;; test-gptel-model-policy.el --- Explicit gptel model selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim
;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Read the shipping definitions without loading Doom, gptel or OAuth.
;; Model policy tests make no requests and do not change a running editor.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

(defconst test-gptel-policy/source
  (expand-file-name "../lisp/ai-gptel.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun test-gptel-policy/form (kind name)
  "Find the unique form of KIND defining NAME in the shipping config."
  (let (matches)
    (cl-labels ((walk (form)
                  (when (consp form)
                    (if (and (eq (car form) kind) (eq (cadr form) name))
                        (push form matches)
                      (walk (car form))
                      (walk (cdr form))))))
      (with-temp-buffer
        (insert-file-contents test-gptel-policy/source)
        (goto-char (point-min))
        (condition-case nil
            (while t (walk (read (current-buffer))))
          (end-of-file nil))))
    (should (= (length matches) 1))
    (car matches)))

(dolist (name '(my/gptel-models my/gptel-model-default my/gptel-model-fast))
  (eval (test-gptel-policy/form 'defconst name) t))
(defvar +gptel-buffer-model)
(eval (test-gptel-policy/form 'defun 'my/gptel-buffer-model-toggle) t)

(ert-deftest test-gptel-policy/allowlist ()
  (should (equal my/gptel-models '(gpt-6.1-terra gpt-6.1-sol))))

(ert-deftest test-gptel-policy/automatic-slots ()
  (should (eq my/gptel-model-default 'gpt-6.1-terra))
  (should (eq my/gptel-model-fast 'gpt-6.1-terra)))

(ert-deftest test-gptel-policy/automatic-consumers ()
  (dolist (name '(gptel-model gptel-quick-model gptel-magit-model))
    (should (eq (eval (nth 2 (test-gptel-policy/form 'setq name)) t)
                'gpt-6.1-terra)))
  (should (eq (eval (nth 2 (test-gptel-policy/form 'defvar '+gptel-buffer-model)) t)
              'gpt-6.1-terra)))

(ert-deftest test-gptel-policy/manual-buffer-toggle ()
  "Explicitly toggling picks the other allowed model, then returns to default."
  (let ((+gptel-buffer-model my/gptel-model-default))
    (my/gptel-buffer-model-toggle)
    (should (eq +gptel-buffer-model (cadr my/gptel-models)))
    (my/gptel-buffer-model-toggle)
    (should (eq +gptel-buffer-model my/gptel-model-default))))

;;; test-gptel-model-policy.el ends here
