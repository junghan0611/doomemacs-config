;;; test-telega-rich.el --- Txx matrix for telega-rich-md -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>

;;; Commentary:

;; Characterization tests for `lisp/telega-rich-md.el' — the pure TDLib
;; richMessage → markdown serializer behind the telega messageRichMessage
;; advice in `ai-bot-config.el'.
;;
;; Fixtures live in tests/fixtures/rich/ as the Txx support matrix:
;;   TNN.eldata    real TDLib 1.8.65 messageRichMessage :content (OpenClaw bot,
;;                 text properties stripped)
;;   TNN.golden.md the markdown source the serializer must produce (text only;
;;                 face/display props are not asserted here)
;;
;; This is the regression line for telega/TDLib drift: when a TDLib bump renames
;; a PageBlock field or adds a RichText subtype, re-capture the fixture and the
;; diff against the golden shows exactly what moved.  See AGENTS.md "telega 주기
;; 관찰" and tests/fixtures/rich/MATRIX.org.

;;; Code:

(require 'ert)
(require 'subr-x)

;; The module lives under lisp/; add it to load-path then require.
(let ((lisp-dir (expand-file-name
                 "../lisp/"
                 (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path lisp-dir))
(require 'telega-rich-md)

(defconst test-telega-rich/fixtures-dir
  (expand-file-name "fixtures/rich/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding TNN.eldata / TNN.golden.md fixtures.")

(defun test-telega-rich/read-content (eldata-file)
  "Read the messageRichMessage :content plist from ELDATA-FILE."
  (with-temp-buffer
    (insert-file-contents eldata-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun test-telega-rich/render (content)
  "Render CONTENT to a property-free markdown string via the serializer."
  (substring-no-properties
   (my/telega--rich-blocks->md
    (plist-get (plist-get content :message) :blocks))))

;; Generate one ERT test per fixture so a single Txx failure is isolated.
(dolist (eldata (directory-files test-telega-rich/fixtures-dir t "\\.eldata\\'"))
  (let* ((tag (file-name-base eldata))
         (golden (expand-file-name (concat tag ".golden.md")
                                   test-telega-rich/fixtures-dir)))
    (eval
     `(ert-deftest ,(intern (concat "test-telega-rich/" tag)) ()
        ,(format "Serializer output for fixture %s matches its golden." tag)
        (let ((content (test-telega-rich/read-content ,eldata))
              (expected (with-temp-buffer
                          (insert-file-contents ,golden)
                          (buffer-string))))
          (should (equal (test-telega-rich/render content) expected))))
     t)))

(provide 'test-telega-rich)
;;; test-telega-rich.el ends here
