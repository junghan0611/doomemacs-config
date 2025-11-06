;;; +functions.el -*- lexical-binding: t; -*-

;;; core fuctions

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


;;; marginalia with vertico-sort

(after! vertico
  (require 'marginalia)
  (defun gr/marginalia--annotate-local-file (cand)
    "Annotate local file CAND.
Removes modes, which I’ve never needed or wanted."
    (marginalia--in-minibuffer
      (when-let (attrs (ignore-errors
                         ;; may throw permission denied errors
                         (file-attributes (substitute-in-file-name
                                           (marginalia--full-candidate cand))
                                          'integer)))
        (marginalia--fields
         ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
         ((marginalia--time (file-attribute-modification-time attrs))
          :face 'marginalia-date :width -12)
         ;; File owner at the right
         ((marginalia--file-owner attrs) :face 'marginalia-file-owner)))))

  (defun gr/marginalia-annotate-file (cand)
    "Annotate file CAND with its size, modification time and other attributes.
These annotations are skipped for remote paths."
    (if-let (remote (or (marginalia--remote-file-p cand)
                        (when-let (win (active-minibuffer-window))
                          (with-current-buffer (window-buffer win)
                            (marginalia--remote-file-p (minibuffer-contents-no-properties))))))
        (marginalia--fields (remote :format "*%s*" :face 'marginalia-documentation))
      (gr/marginalia--annotate-local-file cand)))

  ;; M-A 순서를 바꾸면 된다.
  (add-to-list 'marginalia-annotator-registry
               '(file gr/marginalia-annotate-file marginalia-annotate-file builtin none))

;;;;;; vertico sort modified

  ;; (setq vertico-multiform-categories nil)
  ;; (setq vertico-multiform-categories
  ;;       '(
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         ;; (file (vertico-sort-function . gr/sort-modified))
  ;;         (file (+vertico-transform-functions . +vertico-highlight-directory)) ; doom default
  ;;         ))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  (defun gr/sort-modified (list)
    "Sort LIST of files for latest modified."
    (let ((ht (make-hash-table :test #'equal :size 5000)))
      (dolist (x list)
        (puthash x (file-attribute-modification-time (file-attributes x)) ht))
      (sort list
            (lambda (a b)
              (let ((one
                     (gethash a ht))
                    (two
                     (gethash b ht)))
                (time-less-p two one))))))

  (defun vertico-sort-modified ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'gr/sort-modified)
                vertico--input t))

  (keymap-set vertico-map "M-," #'vertico-sort-modified))

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

(provide '+functions)
