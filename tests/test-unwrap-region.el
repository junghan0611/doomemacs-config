;;; test-unwrap-region.el --- Terminal-copy repair tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Mechanical repair only: no Korean word-boundary or shell syntax inference.
;; Keep explicit spaces; never invent a separator at a terminal wrap.

;;; Code:

(require 'ert)
(load (expand-file-name "../lisp/functions.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest my/unwrap-text-korean-wraps ()
  (should (equal (my/unwrap-text "    해\n    보자. 비\n    용 호출")
                 "해보자. 비용 호출")))

(ert-deftest my/unwrap-text-keeps-explicit-spaces ()
  (should (equal (my/unwrap-text "    one  two \n    three")
                 "one  two three")))

(ert-deftest my/unwrap-text-does-not-infer-missing-spaces ()
  ;; A lost space and a mid-word wrap are indistinguishable in plain text.
  (should (equal (my/unwrap-text "엇나가게\n    갈") "엇나가게갈")))

(ert-deftest my/unwrap-text-preserves-paragraphs-and-final-newline ()
  (should (equal (my/unwrap-text "    첫\n    째\n  \t\n    둘\n    째\n")
                 "첫째\n\n둘째\n"))
  (should (equal (my/unwrap-text "\n\n a\n\n\n b\n\n")
                 "\n\na\n\n\nb\n\n")))

(ert-deftest my/unwrap-text-crlf-and-tabs ()
  (should (equal (my/unwrap-text "\t비\r\n\t용\r\n") "비용\n")))

(ert-deftest my/unwrap-text-url ()
  (should (equal (my/unwrap-text "    https://example.org/auth?token=ab\n    cd&state=xy\n    z")
                 "https://example.org/auth?token=abcd&state=xyz")))

(ert-deftest my/unwrap-text-compact-url ()
  (should (equal (my/unwrap-text " https://example.org/auth?token=ab \n cd\t&state=xyz\n" t)
                 "https://example.org/auth?token=abcd&state=xyz")))

(ert-deftest my/unwrap-text-single-shell-command ()
  (should (equal (my/unwrap-text "    printf '%s' \n    'hello world'")
                 "printf '%s' 'hello world'"))
  ;; Backslashes are real bytes, not decorations to remove automatically.
  (should (equal (my/unwrap-text "cmd \\\n  --flag") "cmd \\--flag")))

(ert-deftest my/unwrap-text-empty-and-unicode ()
  (should (equal (my/unwrap-text "") ""))
  (should (equal (my/unwrap-text "    ") ""))
  (should (equal (my/unwrap-text "  a b\n  c​d") "a bc​d")))

(ert-deftest my/unwrap-region-bounds-and-undo ()
  (with-temp-buffer
    (buffer-enable-undo)
    (insert "BEFORE\n    비\n    용\nAFTER")
    (setq buffer-undo-list nil)
    (let ((original (buffer-string)))
      (my/unwrap-region 8 20)
      (should (equal (buffer-string) "BEFORE\n비용\nAFTER"))
      (undo-boundary)
      (undo 1)
      (should (equal (buffer-string) original)))))

(ert-deftest my/unwrap-region-read-only ()
  (with-temp-buffer
    (insert "비\n용")
    (setq buffer-read-only t)
    (should-error (my/unwrap-region (point-min) (point-max))
                  :type 'buffer-read-only)
    (should (equal (buffer-string) "비\n용"))))

(ert-deftest my/unwrap-region-requires-active-selection ()
  (with-temp-buffer
    (insert "비\n용")
    (should-error (call-interactively #'my/unwrap-region) :type 'user-error)
    (should (equal (buffer-string) "비\n용"))))

(provide 'test-unwrap-region)
;;; test-unwrap-region.el ends here
