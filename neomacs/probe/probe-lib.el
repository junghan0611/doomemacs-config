;;; neomacs/probe/probe-lib.el --- Shared probe reporting helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Reporting helpers shared by the Neomacs K-review probes.
;;
;; Probes run under `--batch' and must survive the thing they are probing.
;; Every check is therefore wrapped so that a signal becomes a FAIL line
;; rather than a dead batch process — a crash that kills the process is
;; itself the finding, and the surviving output shows where it stopped.
;;
;; Output is line-oriented and greppable so a run can be pasted into an
;; upstream issue verbatim.

;;; Code:

(require 'subr-x)

(defvar my/probe-results nil
  "Accumulated results for the current probe, newest first.
Each entry is an alist with :status, :name, and :detail.")

(defun my/probe-banner (title)
  "Print a section banner for TITLE."
  (princ (format "\n=== %s ===\n" title)))

(defun my/probe-report (status name detail)
  "Record and print one result line: STATUS, NAME, DETAIL."
  (push (list (cons :status status) (cons :name name) (cons :detail detail))
        my/probe-results)
  (princ (format "[%s] %-38s %s\n" status name detail)))

(defmacro my/probe-check (name &rest body)
  "Run BODY as probe NAME, reporting OK, FAIL, or SKIP.
BODY returns either a detail string (OK), or the symbol `skip' or a cons
of (skip . REASON) to report SKIP.  A signal is caught and reported as
FAIL with the error message, so the batch process keeps going."
  (declare (indent 1))
  `(condition-case err
       (let ((result (progn ,@body)))
         (cond
          ((eq result 'skip) (my/probe-report "SKIP" ,name ""))
          ((and (consp result) (eq (car result) 'skip))
           (my/probe-report "SKIP" ,name (cdr result)))
          (t (my/probe-report "OK" ,name (or result "")))))
     (error
      (my/probe-report "FAIL" ,name (error-message-string err)))))

(defun my/probe-summary ()
  "Print a tallied summary and exit non-zero when anything failed."
  (let* ((statuses (mapcar (lambda (r) (alist-get :status r)) my/probe-results))
         (ok (seq-count (lambda (s) (equal s "OK")) statuses))
         (fail (seq-count (lambda (s) (equal s "FAIL")) statuses))
         (skip (seq-count (lambda (s) (equal s "SKIP")) statuses)))
    (princ (format "\n-- %d OK, %d FAIL, %d SKIP\n" ok fail skip))
    (when (> fail 0)
      (kill-emacs 1))))

(defun my/probe-runtime-line ()
  "Return a one-line runtime identity for report headers."
  (format "%s | treesit=%s | modules=%s"
          (car (split-string (emacs-version) "\n"))
          (if (and (fboundp 'treesit-available-p) (treesit-available-p)) "yes" "no")
          (if (bound-and-true-p module-file-suffix) "yes" "no")))

(provide 'probe-lib)
;;; probe-lib.el ends here
