;;; $DOOMDIR/autoload/junghan.el -*- lexical-binding: t; -*-

;;; Autoloaded functions for junghan's config

;;;; my/consult-fd

;;;###autoload
(defun my/consult-fd ()
  (interactive)
  (consult-fd "."))

;; spacemacs/layers/+completion/compleseus/funcs.el
;;;###autoload
(defun my/compleseus-search (use-initial-input initial-directory)
  (let* ((initial-input
          (if use-initial-input
              (doom-pcre-quote ;; rxt-quote-pcre
               (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (or (thing-at-point 'symbol t) ""))) ""))
         (default-directory
          (or initial-directory
              (read-directory-name "Start from directory: "))))
    (consult-ripgrep default-directory initial-input)))

;;;###autoload
(defun +default/search-cwd-symbol-at-point ()
  "Search current folder."
  (interactive)
  (my/compleseus-search t default-directory))

;;;###autoload
(defun my/org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

;;; Window and Layout

;;;; spacemacs/window-layout-toggle

;;;###autoload
(defun spacemacs/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;; my/open/workspaces

;;;;###autoload
(defun my/open-workspaces ()
  (interactive)

  (message "my/open-workspaces")
  (+workspace/new-named "work")
  (find-file "~/repos/work")

  (+workspace/new-named "repos")
  (find-file user-project-directory)

  (+workspace/new-named "dots")
  (find-file doom-user-dir)

  (+workspace/new-named "feed")
  (elfeed)

  (+workspace/switch-to-0))

;; (unless IS-DEMO
;;   (when (display-graphic-p) ; gui
;;     (add-hook 'doom-first-input-hook #'my/open-workspaces)))

;;; py3status integration
;; Based on ElleNajit's org-clock integration for i3status
;; https://github.com/ElleNajt/emacs-config

(defun junghan/org-text-element->string (elt)
  "Convert org text element to string."
  (cond
   ((stringp elt) elt)
   ((and (consp elt)
         (symbolp (car elt)))
    (-> elt (caddr) (junghan/org-text-element->string) (s-trim) (concat " ")))))

(defun junghan/org-element-title (elt)
  "Get title from org element."
  (let ((title (org-element-property :title elt)))
    (cond
     ((stringp title) title)
     ((listp title)
      (->> title
           (mapcar #'junghan/org-text-element->string)
           (s-join "")
           (s-trim))))))

(defun junghan/minutes->hours:minutes (minutes)
  "Convert MINUTES to H:MM format."
  (format "%d:%02d"
          (floor (/ minutes 60))
          (mod minutes 60)))

(defmacro junghan/at-org-clocked-in-item (&rest body)
  "Execute BODY at currently clocked-in org item."
  `(when (org-clocking-p)
     (let ((m org-clock-marker))
       (with-current-buffer (marker-buffer m)
         (save-mark-and-excursion
           (goto-char m)
           (org-back-to-heading t)
           ,@body)))))

(defun junghan/org-element-clocked-in-task ()
  "Get org element of currently clocked-in task."
  (junghan/at-org-clocked-in-item
   (org-element-at-point)))

(defun junghan/org-current-clocked-in-task-message ()
  "Return current clocked-in task with time. Format: (Task) [H:MM]"
  (interactive)
  (if (org-clocking-p)
      (format "(%s) [%s]"
              (->> (junghan/org-element-clocked-in-task)
                   (junghan/org-element-title)
                   (substring-no-properties)
                   (s-trim))
              (junghan/minutes->hours:minutes
               (org-clock-get-clocked-time)))
    ""))

(defun junghan/update-org-clocked-in-task-file ()
  "Write current task to file for py3status."
  (interactive)
  (let ((current-task (junghan/org-current-clocked-in-task-message))
        (task-file (expand-file-name "current-task" doom-emacs-dir)))
    (with-temp-file task-file
      (insert current-task))))

;;;; nbsp-ascii

(progn
  ;; ;;;###autoload
  (defun +replace-in-buffer (old new)
    "Replace OLD with NEW in the current buffer."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (matches 0))
        (while (re-search-forward old nil t)
          (replace-match new)
          (cl-incf matches))
        matches)))

  ;; ;;;###autoload
  (defun my/clear-nbsp-and-ascii-punctuations ()
    "Replace french ponctuations (like unsectable space) by regular ones."
    (interactive)
    (let ((chars
           '(("[\u00a0\u200b]" . "") ;; Non-breaking and zero-width spaces - nbsp
             ;; Special spaces and quads
             ("[\u2000-\u200A\u202F\u205F\u3000]" . " ")
             ("[\{\$]" . "")
             ("[\$\}]" . "")
             ;; ("[‘’‚’]" . "'")
             ;; ("[“”„”«»]" . "\"")
             ("[‘’]" . "'")
             ("[“”]" . "\"")
             ))
          (matches 0))
      (dolist (pair chars)
        (cl-incf matches (+replace-in-buffer (car pair) (cdr pair))))
      (message "Replaced %d match%s." matches (if (> matches 1) "es" "")))
    )

  (defun my/insert-nbsp-simple-all ()
    "한글 조사, 라틴-한글, 기호-텍스트 사이에 NBSP 삽입 (3가지 패턴 통합)"
    (interactive)
    (let ((word-list '()))
      (save-excursion
        (goto-line 10)
        ;; 1. 라틴 문자와 한글 사이 NBSP 삽입
        ;; 2. 조직모드 기호(=,*,_,+) 뒤 한글 또는 라틴 문자에 NBSP 삽입
        (while (re-search-forward "\\([A-Za-z*+=_]\\)\\([가-힣]\\)" nil t)
          (unless (save-excursion
                    (goto-char (match-beginning 1))
                    (looking-back "\\s-" 1))
            (goto-char (match-beginning 2))
            (insert " ")
            (goto-char (match-end 2))))

        ;; 4. 한글 조사 NBSP 삽입 - '1단어'
        ;; (goto-line 10)
        ;; (while (re-search-forward
        ;;         "\\([가-힣]\\{2,\\}\\)\\(이\\|가\\|은\\|는\\|을\\|의\\|를\\|와\\|과\\|란\\)\\(\x20\\)" ; [[:space:]]
        ;;         nil t)
        ;;   (when (>= (length (match-string 1)) 2)
        ;;     (push (match-string 1) word-list))
        ;;   (replace-match "\\1 \\2 \\3"))
        ) ; end save-excursion
      ))
  )

;;; provide

(provide 'functions)
