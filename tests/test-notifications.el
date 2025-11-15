;;; test-notifications.el --- Tests for notification system -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;;; Commentary:

;; Tests for my/notify function and dunst integration

;;; Code:

(require 'test-helper)

;; Load my/notify function
(defun my/notify (title message &optional urgency duration)
  "Send system notification via notify-send (dunst).

TITLE: notification title
MESSAGE: notification body
URGENCY: low, normal, critical (default: normal)
DURATION: milliseconds (default: 4000)

Works in both GUI and terminal (emacs -nw) environments.
Requires notify-send (libnotify) and dunst daemon.

Returns t on success, nil if notify-send is not available."
  (when (executable-find "notify-send")
    (let ((urgency (or urgency "normal"))
          (duration (or duration 4000)))
      (ignore-errors
        (call-process "notify-send" nil 0 nil
                      "-u" urgency
                      "-t" (format "%d" duration)
                      title
                      message))
      t)))

(ert-deftest test-notify-basic ()
  "Test basic notification with default parameters."
  (skip-unless (executable-find "notify-send"))
  (should (my/notify "Test Title" "Test Message")))

(ert-deftest test-notify-with-urgency ()
  "Test notification with different urgency levels."
  (skip-unless (executable-find "notify-send"))
  (should (my/notify "Low Priority" "Message" "low"))
  (should (my/notify "Normal Priority" "Message" "normal"))
  (should (my/notify "Critical Priority" "Message" "critical")))

(ert-deftest test-notify-with-duration ()
  "Test notification with custom duration."
  (skip-unless (executable-find "notify-send"))
  (should (my/notify "Quick" "Message" "normal" 1000))
  (should (my/notify "Long" "Message" "normal" 10000)))

(ert-deftest test-notify-returns-t ()
  "Test that my/notify returns t on success."
  (skip-unless (executable-find "notify-send"))
  (should (eq t (my/notify "Test" "Message"))))

(ert-deftest test-notify-without-notify-send ()
  "Test graceful handling when notify-send is not available."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_) nil)))
    ;; Should not error even if notify-send is missing
    (should (null (my/notify "Test" "Message")))))

;;; test-notifications.el ends here
