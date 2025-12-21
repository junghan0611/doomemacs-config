;;; $DOOMDIR/lisp/time-config.el --- Time and Calendar Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Time display and calendar settings for Doom Emacs.

;;; Code:

;;;; Time and Day Calendar

;;;;; Time

(require 'time)

;; (setq display-time-format "%l:%M %p %b %d W%U") ;; dw-dotfiles
(setq display-time-format " W%U |%a %e %b, %H:%M| ") ; Ⓦ 🅆 🆆

;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (setq display-time-interval 30) ; default 60
(setq display-time-default-load-average nil)

;; NOTE 2022-09-21: For all those, I have implemented my own solution
;; that also shows the number of new items, although it depends on
;; notmuch: the `notmuch-indicator' package.
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;;;;; Calendar

(require 'calendar)
;; (setq org-agenda-start-on-weekday nil)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq
 calendar-date-style 'iso ;; YYYY/MM/DD
 calendar-mark-holidays-flag t
 calendar-week-start-day 1 ;; 0 Sunday, 1 Monday
 calendar-mark-diary-entries-flag nil
 calendar-latitude user-calendar-latitude
 calendar-longitude user-calendar-longitude
 calendar-location-name user-calendar-location-name
 calendar-time-display-form
 '(24-hours
   ":" minutes
   (if time-zone
       " (")
   time-zone
   (if time-zone
       ")")))

;;;; calfw

(when (modulep! :app calendar)
  (after! calfw
    (setq truncate-string-ellipsis "...")
    ;; Default setting
    (setq calfw-fchar-junction ?+
          calfw-fchar-vertical-line ?|
          calfw-fchar-horizontal-line ?-
          calfw-fchar-left-junction ?+
          calfw-fchar-right-junction ?+
          calfw-fchar-top-junction ?+
          calfw-fchar-top-left-corner ?+
          calfw-fchar-top-right-corner ?+ ))
  )

;;; provide

(provide 'time-config)

;;; time-config.el ends here
