;;; $DOOMDIR/lisp/denote-functions.el --- Denote Helper Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Custom functions for Denote workflow.

;;; Code:

;;;; my/denote-links-this-week

;; [[denote:20250904T084109][#LLM: 20250904T084109]]
(defun my/denote-links-this-week (&optional arg)
  "Insert a =denote-links= block for this week.
Default is the week containing today.
With universal argument ARG (\\[universal-argument]), prompt for a date
via =calendar-read-date=."
  (interactive "P")
  (let* ((date (if arg
                   (calendar-read-date) ;; (month day year)
                 (list (string-to-number (format-time-string "%m"))
                       (string-to-number (format-time-string "%d"))
                       (string-to-number (format-time-string "%Y")))))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (time  (encode-time 0 0 0 day month year))
         ;; ISO weekday: 1=Monday … 7=Sunday
         (dow (string-to-number (format-time-string "%u" time)))
         (monday (time-subtract time (days-to-time (1- dow))))
         (dates (mapcar (lambda (i)
                          (format-time-string "%Y%m%d"
                                              (time-add monday (days-to-time i))))
                        (number-sequence 0 6)))
         (regexp (mapconcat (lambda (d) (concat d "T*")) dates "\\\\|")))
    (insert
     (format "#+BEGIN: denote-links :regexp \"%s\" :not-regexp nil :excluded-dirs-regexp \"\\\\(journal\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\" :sort-by-component nil :reverse-sort t :id-only nil :include-date t\n#+END:\n"
             regexp))))

;;; provide

(provide 'denote-functions)

;;; denote-functions.el ends here
