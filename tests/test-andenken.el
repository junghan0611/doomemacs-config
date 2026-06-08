;;; test-andenken.el --- Tests for andenken-config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>

;;; Commentary:

;; Characterization tests for the Doom-free pure core of andenken-config.el
;; (Tier A — see TESTING-GUIDELINES.org).  These PIN the current behavior so a
;; later refactor (e.g. andenken NEXT 2e `--view session') is provably safe:
;; the asserted values were OBSERVED from the running functions, not derived
;; from the spec.
;;
;; Out of scope under `emacs -Q' (no Doom, no network):
;;   andenken--call (subprocess), the interactive search commands, and the
;;   buffer/completion UI helpers.  Only input->output logic lives here.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

;; andenken-config.el lives in lisp/; put it on the load-path, then load.
;; `nil t' so a load failure leaves the functions unbound and every test
;; below skips cleanly rather than erroring the whole file.
(let ((lisp-dir (expand-file-name
                 "lisp"
                 (file-name-directory
                  (directory-file-name
                   (file-name-directory (or load-file-name buffer-file-name)))))))
  (add-to-list 'load-path lisp-dir))
(require 'andenken-config nil t)

;;;; andenken--format-date — ISO date prefix with length guards

(ert-deftest test-andenken-format-date--iso-20plus ()
  "A full ISO timestamp returns only its YYYY-MM-DD prefix.
Guards against `2026-05-…' display truncation of the 20+ char original."
  (skip-unless (fboundp 'andenken--format-date))
  (should (equal (andenken--format-date "2026-05-28T14:56:33Z") "2026-05-28")))

(ert-deftest test-andenken-format-date--nil ()
  "A nil timestamp returns the empty string."
  (skip-unless (fboundp 'andenken--format-date))
  (should (equal (andenken--format-date nil) "")))

(ert-deftest test-andenken-format-date--shorter-than-10 ()
  "A string shorter than 10 chars is returned unchanged."
  (skip-unless (fboundp 'andenken--format-date))
  (should (equal (andenken--format-date "2026-05") "2026-05")))

(ert-deftest test-andenken-format-date--exactly-10 ()
  "An exactly-10-char date is returned unchanged."
  (skip-unless (fboundp 'andenken--format-date))
  (should (equal (andenken--format-date "2026-05-28") "2026-05-28")))

;;;; andenken--truncate — flatten whitespace, then pad/cut to width

(ert-deftest test-andenken-truncate--flattens-whitespace ()
  "Runs of whitespace (incl. newlines/tabs) collapse to a single space,
and the result is right-padded with spaces to WIDTH."
  (skip-unless (fboundp 'andenken--truncate))
  (should (equal (andenken--truncate "hello\n\tworld  foo" 100)
                 (concat "hello world foo" (make-string 85 ?\s)))))

(ert-deftest test-andenken-truncate--cuts-with-ellipsis ()
  "Over-width text is cut and gets a trailing ellipsis within WIDTH."
  (skip-unless (fboundp 'andenken--truncate))
  (should (equal (andenken--truncate "abcdefghij" 5) "abcd…")))

(ert-deftest test-andenken-truncate--nil-is-blank-padding ()
  "A nil text yields WIDTH spaces."
  (skip-unless (fboundp 'andenken--truncate))
  (should (equal (andenken--truncate nil 10) (make-string 10 ?\s))))

;;;; andenken--day-range-utc — local day -> half-open UTC window

;; Inputs are constructed at NOON UTC so the result is identical in every
;; runner timezone (midnight-UTC inputs would let the date math drift with TZ).

(ert-deftest test-andenken-day-range-utc ()
  "A day range is [TIME, TIME+1day) as UTC ISO strings."
  (skip-unless (fboundp 'andenken--day-range-utc))
  (let ((noon (encode-time (list 0 0 12 8 6 2026 nil nil 0))))
    (should (equal (andenken--day-range-utc noon)
                   '("2026-06-08T12:00:00Z" . "2026-06-09T12:00:00Z")))))

;;;; andenken--week-range-utc — local Mon~Sun -> half-open UTC window

(ert-deftest test-andenken-week-range-utc--from-monday ()
  "From a Monday, the week window starts that day and spans 7 days."
  (skip-unless (fboundp 'andenken--week-range-utc))
  (let ((mon (encode-time (list 0 0 12 8 6 2026 nil nil 0))))
    (should (equal (andenken--week-range-utc mon)
                   '("2026-06-08T12:00:00Z" . "2026-06-15T12:00:00Z")))))

(ert-deftest test-andenken-week-range-utc--midweek-snaps-to-monday ()
  "A Wednesday resolves to the same Mon~Sun window as its Monday."
  (skip-unless (fboundp 'andenken--week-range-utc))
  (let ((wed (encode-time (list 0 0 12 10 6 2026 nil nil 0))))
    (should (equal (andenken--week-range-utc wed)
                   '("2026-06-08T12:00:00Z" . "2026-06-15T12:00:00Z")))))

(ert-deftest test-andenken-week-range-utc--sunday-stays-in-week ()
  "A Sunday belongs to the week that began on the prior Monday."
  (skip-unless (fboundp 'andenken--week-range-utc))
  (let ((sun (encode-time (list 0 0 12 14 6 2026 nil nil 0))))
    (should (equal (andenken--week-range-utc sun)
                   '("2026-06-08T12:00:00Z" . "2026-06-15T12:00:00Z")))))

;;;; andenken--format-session-result — result alist -> completion line

(ert-deftest test-andenken-format-session-result ()
  "A result alist renders as a fixed-column line; the body has its newlines
flattened.  Trailing padding is trimmed so the assertion ignores
`andenken-snippet-width'."
  (skip-unless (fboundp 'andenken--format-session-result))
  (let ((r '((score . 0.87)
             (project . "doomemacs-config")
             (role . "user")
             (source . "claude")
             (timestamp . "2026-06-08T08:30:00Z")
             (text . "line one\ntwo"))))
    (should (equal (string-trim-right (andenken--format-session-result r 3))
                   " 3   0.87  doomemacs-config   user       claude  2026-06-08  │ line one two"))))

;;; test-andenken.el ends here
