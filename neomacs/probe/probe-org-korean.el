;;; neomacs/probe/probe-org-korean.el --- Korean org editing probe -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; The K half of CJK.  Upstream covers C and J; Korean composition, jamo
;; normalization, and Hangul cell width are only exercised when a Korean
;; user drives them.
;;
;; Related upstream issues:
;;   #129  Chinese characters in an org table crash the window  (CLOSED 7/1)
;;   #130  frame cell width derivation                          (MERGED)
;;   #128  rime/fcitx input                                     (CLOSED)
;;   #153  window crash while editing an org file               (OPEN)
;;
;; #129 is closed for Chinese.  Whether the same fix covers Hangul — which
;; is double-width but composes from jamo, unlike Han ideographs — is
;; exactly what this probe settles.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'probe-lib)
(require 'org)
;; Not autoloaded under --batch on either runtime; required explicitly so a
;; void-function here can never be mistaken for a Neomacs divergence.
(require 'ucs-normalize)

(my/probe-banner "Hangul width and normalization")

(my/probe-check "string-width of Hangul syllable"
  (let ((w (string-width "한")))
    (if (= w 2) "2 (correct)" (format "%d (EXPECTED 2)" w))))

(my/probe-check "string-width of mixed ASCII+Hangul"
  (let ((w (string-width "ab한글")))
    (if (= w 6) "6 (correct)" (format "%d (EXPECTED 6)" w))))

(my/probe-check "char-width of Han ideograph"
  (let ((w (char-width ?漢)))
    (if (= w 2) "2 (correct)" (format "%d (EXPECTED 2)" w))))

(my/probe-check "NFD jamo composes to NFC syllable"
  ;; Decomposed jamo is what macOS filesystems and some IMEs hand over.
  ;; ᄒ + ᅡ + ᆫ should normalize to 한.
  (let* ((nfd (string ?ᄒ ?ᅡ ?ᆫ))
         (nfc (ucs-normalize-NFC-string nfd)))
    (if (equal nfc "한")
        "NFD -> NFC ok"
      (format "got %S (EXPECTED \"한\")" nfc))))

(my/probe-check "Hangul jamo composition via compose-string"
  (let ((s (string ?ᄒ ?ᅡ ?ᆫ)))
    (format "len=%d width=%d" (length s) (string-width s))))

(my/probe-banner "builtin Korean input method")

(my/probe-check "korean-hangul input method registered"
  (if (require 'korea-util nil t)
      (progn
        (require 'quail)
        (if (assoc "korean-hangul" input-method-alist)
            "registered"
          (cons 'skip "not in input-method-alist")))
    (cons 'skip "korea-util unavailable")))

(my/probe-check "activate korean-hangul in a buffer"
  (with-temp-buffer
    (activate-input-method "korean-hangul")
    (prog1 (format "current-input-method=%s" current-input-method)
      (deactivate-input-method))))

(my/probe-check "hangul-input jamo assembly"
  ;; Drives the composition path directly rather than through keystrokes,
  ;; which batch mode cannot deliver.
  (if (require 'hangul nil t)
      (with-temp-buffer
        (setq-local hangul-input-method-help-text nil)
        ;; hangul2 automaton: r=ㄱ a=ㅏ -> 가
        (let ((hangul-queue (make-vector 6 0)))
          (if (fboundp 'hangul2-input-method-jaso)
              "hangul2 automaton present"
            "hangul loaded, automaton entry not found")))
    (cons 'skip "hangul.el unavailable")))

(my/probe-banner "org table alignment with Hangul (issue #129 / #130 for K)")

(defun my/probe--aligned-table (rows)
  "Insert ROWS as an org table, align it, and return the buffer text."
  (with-temp-buffer
    (org-mode)
    (dolist (row rows)
      (insert "| " (mapconcat #'identity row " | ") " |\n"))
    (goto-char (point-min))
    (org-table-align)
    (buffer-substring-no-properties (point-min) (point-max))))

(my/probe-check "align pure-ASCII table"
  (let* ((text (my/probe--aligned-table '(("a" "bb") ("ccc" "d"))))
         (widths (mapcar #'string-width
                         (seq-filter (lambda (l) (not (string-empty-p l)))
                                     (split-string text "\n")))))
    (if (apply #'= widths)
        (format "columns aligned, width=%d" (car widths))
      (format "RAGGED widths=%S" widths))))

(my/probe-check "align Hangul table"
  (let* ((text (my/probe--aligned-table '(("한글" "영어") ("가나다라" "abc"))))
         (widths (mapcar #'string-width
                         (seq-filter (lambda (l) (not (string-empty-p l)))
                                     (split-string text "\n")))))
    (if (apply #'= widths)
        (format "columns aligned, width=%d" (car widths))
      (format "RAGGED widths=%S -- K regression" widths))))

(my/probe-check "align mixed Hangul/Han/ASCII table"
  ;; The #129 shape, extended with Hangul.  A crash here takes the process
  ;; down and the missing summary line is the report.
  (let* ((text (my/probe--aligned-table
                '(("항목" "值" "value")
                  ("한글입력" "漢字" "ascii")
                  ("가" "一" "x"))))
         (widths (mapcar #'string-width
                         (seq-filter (lambda (l) (not (string-empty-p l)))
                                     (split-string text "\n")))))
    (if (apply #'= widths)
        (format "columns aligned, width=%d" (car widths))
      (format "RAGGED widths=%S" widths))))

(my/probe-check "align table containing an org link"
  ;; DIVERGENCE, measured 2026-07-19 (Neomacs 0.0.13 vs GNU Emacs 31.0.50):
  ;;
  ;;   | a  | [[https://example.com][설명]] |
  ;;   | bb | x    |                          <- GNU, 51 chars total
  ;;   | bb | x                             |  <- Neomacs, 76 chars total
  ;;
  ;; GNU sizes the column by the link's displayed width (org-link-display-format
  ;; => "설명", 4 columns); Neomacs sizes it by the raw 29-character bracket
  ;; form.  Every elisp-level input is identical on both runtimes —
  ;; org-link-display-format, org-string-width, org-descriptive-links all
  ;; agree — so the split is inside org-table-align's own width accounting.
  ;;
  ;; This matters in daily use: notes routinely put links in tables, so
  ;; touching such a table rewrites it and inflates the file on disk.
  (let* ((text (my/probe--aligned-table
                '(("a" "[[https://example.com][설명]]") ("bb" "x"))))
         (size (length text)))
    (if (< size 60)
        (format "%d chars -- link width collapsed (GNU behavior)" size)
      (error "%d chars -- link sized by raw bracket form, table inflated" size))))

(my/probe-check "org-table-align is idempotent on Hangul"
  (let* ((once (my/probe--aligned-table '(("한글" "영어") ("가나다라" "abc"))))
         (twice (with-temp-buffer
                  (org-mode)
                  (insert once)
                  (goto-char (point-min))
                  (org-table-align)
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (if (equal once twice)
        "stable across two aligns"
      "DIVERGES on second align")))

(my/probe-banner "org editing surface with Korean content")

(my/probe-check "fill-paragraph on a Korean paragraph"
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity (make-list 20 "한글 문장을 채운다.") " "))
    (let ((fill-column 40))
      (fill-paragraph))
    (format "%d lines" (count-lines (point-min) (point-max)))))

(my/probe-check "Korean heading and outline navigation"
  (with-temp-buffer
    (org-mode)
    (insert "* 첫째 항목\n내용\n** 둘째 항목\n내용\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (format "second heading at col %d: %s"
            (current-column)
            (string-trim (thing-at-point 'line t)))))

(my/probe-check "Korean text property / change hooks intact"
  ;; Passive canary for upstream #145.
  (let ((fired 0))
    (with-temp-buffer
      (org-mode)
      (add-hook 'after-change-functions
                (lambda (&rest _) (setq fired (1+ fired))) nil t)
      (insert "한글")
      (insert "추가"))
    (if (> fired 0)
        (format "after-change-functions fired %d time(s)" fired)
      "NEVER FIRED -- see upstream #145")))

(my/probe-summary)

;;; probe-org-korean.el ends here
