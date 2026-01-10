;;; $DOOMDIR/lisp/denote-functions.el --- Denote Helper Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Custom functions for Denote workflow.
;; Prefix: my/ for interactive, my-- for internal

;;; Code:

(require 'denote)

;;;; Find & Grep

;;;###autoload
(defun my/denote-find-file ()
  "Open a denote file with completion."
  (interactive)
  (find-file (denote-file-prompt)))

;;;###autoload
(defun my/denote-find-file-by-modified ()
  "Open a denote file, sorted by modification time (newest first).
Shows flat list of all notes without folder grouping."
  (interactive)
  (let* ((files (denote-directory-files nil nil t)) ; only text files
         ;; Sort by modification time (newest first)
         (sorted-files
          (sort files
                (lambda (a b)
                  (let ((time-a (file-attribute-modification-time (file-attributes a)))
                        (time-b (file-attribute-modification-time (file-attributes b))))
                    (time-less-p time-b time-a)))))
         ;; Create display names (keep as simple list for proper ordering)
         (display-names
          (mapcar #'denote-get-file-name-relative-to-denote-directory sorted-files))
         ;; Create lookup table
         (file-table (make-hash-table :test #'equal))
         (_ (cl-loop for f in sorted-files
                     for d in display-names
                     do (puthash d f file-table)))
         ;; Disable vertico sorting to preserve our order
         (vertico-sort-function nil)
         (vertico-sort-override-function nil)
         ;; Use completion-extra-properties to prevent re-sorting
         (completion-extra-properties
          '(:display-sort-function identity
            :cycle-sort-function identity))
         ;; Let user select
         (selected (completing-read "Denote (by modified): " display-names nil t))
         (file (gethash selected file-table)))
    (when file
      (find-file file))))

;;;###autoload
(defun my/denote-grep ()
  "Search within denote directory using ripgrep."
  (interactive)
  (consult-ripgrep denote-directory))

;;;###autoload
(defun my/denote-howmish-find-file ()
  "Find denote file in howm-ish style (by date)."
  (interactive)
  ;; TODO: implement howm-style finder
  (my/denote-find-file))

;;;; Random Notes

(defun my/denote-random-note-from-directory (directory)
  "Open a random denote from DIRECTORY."
  (let* ((denote-directory directory)
         (files (denote-directory-files)))
    (when files
      (find-file (nth (random (length files)) files)))))

;;;###autoload
(defun my/denote-random-note ()
  "Open a random denote from the main directory."
  (interactive)
  (my/denote-random-note-from-directory denote-directory))

;;;###autoload
(defun my/denote-random-notes ()
  "Open a random note from notes subdirectory."
  (interactive)
  (my/denote-random-note-from-directory (concat denote-directory "notes")))

;;;###autoload
(defun my/denote-random-bib ()
  "Open a random note from bib subdirectory."
  (interactive)
  (my/denote-random-note-from-directory (concat denote-directory "bib")))

;;;###autoload
(defun my/denote-random-meta ()
  "Open a random note from meta subdirectory."
  (interactive)
  (my/denote-random-note-from-directory (concat denote-directory "meta")))

;;;; Links & Store

;;;###autoload
(defun my/denote-org-store-link-to-heading (&optional arg)
  "Store link with toggled `denote-org-store-link-to-heading' value.
With ARG, passed to `org-store-link'."
  (interactive "P")
  (let ((denote-org-store-link-to-heading
         (not denote-org-store-link-to-heading)))
    (org-store-link arg :interactive)))

;;;###autoload
(defun my/denote-update-link-descriptions (confirmp)
  "Recreate denote link descriptions in the current buffer.
If CONFIRMP is non-nil, prompt user to confirm each replacement.
Interactively, CONFIRMP is non-nil by default; use prefix to skip confirmation."
  (interactive (list (not current-prefix-arg)))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (denote-org--get-link-type-regexp 'denote) nil :no-error)
      (condition-case err
          (save-match-data
            (let* ((link-beg (match-beginning 0))
                   (link-end (match-end 0))
                   (s (match-string-no-properties 0))
                   (link (with-temp-buffer
                           (let ((org-inhibit-startup nil))
                             (insert s)
                             (org-mode)
                             (goto-char (point-min))
                             (org-element-link-parser))))
                   (path (org-element-property :path link))
                   (file (denote-get-path-by-id (car (string-split path "::"))))
                   (heading-custom-id (cadr (string-split path "::")))
                   (new-link-text
                    (if (and denote-org-store-link-to-heading heading-custom-id)
                        (format "[[denote:%s::#%s][%s]]"
                                (denote-retrieve-filename-identifier file)
                                (string-remove-prefix "#" heading-custom-id)
                                (concat (denote--link-get-description file)
                                        "::"
                                        (save-excursion
                                          (with-current-buffer (find-file-noselect file)
                                            (org-link-search heading-custom-id)
                                            (org-link-display-format
                                             (denote-link-ol-get-heading))))))
                      (format "[[denote:%s][%s]]"
                              (denote-retrieve-filename-identifier file)
                              (denote--link-get-description file))))
                   (current-link-text (buffer-substring link-beg link-end)))
              (when (and (not (string= (substring-no-properties current-link-text) new-link-text))
                         (or (not confirmp)
                             (yes-or-no-p (concat "Replace this link? " current-link-text))))
                (goto-char link-beg)
                (delete-region link-beg link-end)
                (insert new-link-text))))
        (error (message "[my/denote-update-link-descriptions] Error: %s"
                        (error-message-string err))))))
  (message "Corrected links in %s"
           (propertize (denote-retrieve-front-matter-title-value
                        (buffer-file-name)
                        (denote-filetype-heuristics (buffer-file-name)))
                       'face 'denote-faces-title)))

;;;; Dblock Insert

;;;###autoload
(defun my/denote-org-dblock-insert-backlinks ()
  "Create Org dynamic block to insert Denote backlinks to current file."
  (interactive nil org-mode)
  (org-create-dblock (list :name "denote-backlinks"
                           :excluded-dirs-regexp nil
                           :sort-by-component nil
                           :reverse-sort nil
                           :id-only nil
                           :this-heading-only nil
                           :include-date nil))
  (org-update-dblock))

;;;###autoload
(defun my/denote-org-dblock-insert-links (regexp)
  "Create Org dynamic block to insert Denote links matching REGEXP."
  (interactive
   (list (denote-files-matching-regexp-prompt)) org-mode)
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :not-regexp nil
                           :excluded-dirs-regexp
                           "\\(meta\\|elisp\\|journal\\|posts\\|docs\\|md\\|dict\\|private\\|ekg\\)"
                           :sort-by-component nil
                           :reverse-sort t
                           :id-only nil
                           :include-date nil))
  (org-update-dblock))

;;;###autoload
(defun my/denote-org-dblock-insert-bib-links (regexp)
  "Create Org dynamic block to insert bib links matching REGEXP."
  (interactive
   (list (denote-files-matching-regexp-prompt)) org-mode)
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :not-regexp nil
                           :excluded-dirs-regexp
                           "\\(meta\\|office\\|notes\\|journal\\|posts\\|docs\\|md\\|dict\\|private\\|ekg\\)"
                           :sort-by-component nil
                           :reverse-sort t
                           :id-only nil
                           :include-date nil))
  (org-update-dblock))

;;;###autoload
(defun my/denote-org-dblock-insert-notes-links (regexp)
  "Create Org dynamic block to insert notes links matching REGEXP."
  (interactive
   (list (denote-files-matching-regexp-prompt)) org-mode)
  (org-create-dblock (list :name "denote-links"
                           :regexp regexp
                           :not-regexp nil
                           :excluded-dirs-regexp
                           "\\(meta\\|bib\\|office\\|journal\\|posts\\|docs\\|md\\|dict\\|private\\|ekg\\)"
                           :sort-by-component nil
                           :reverse-sort t
                           :id-only nil
                           :include-date nil))
  (org-update-dblock))

;;;###autoload
(defun my/denote-org-dblock-insert-meta-links ()
  "Create Org dynamic block to insert meta links by selection."
  (interactive)
  (let* ((topics (mapcar (lambda (file)
                           (denote-retrieve-front-matter-title-value file 'org))
                         (denote-directory-files "_meta")))
         (selected (completing-read-multiple "Select meta: " topics nil t)))
    (org-create-dblock (list :name "denote-links"
                             :regexp
                             (mapconcat #'identity
                                        (mapcar (lambda (s) (replace-regexp-in-string "#" "" s)) selected)
                                        "\\|")
                             :not-regexp nil
                             :excluded-dirs-regexp
                             "\\(bib\\|notes\\|office\\|elisp\\|docs\\|posts\\|md\\|journal\\|dict\\|private\\|ekg\\)"
                             :sort-by-component nil
                             :reverse-sort t
                             :id-only nil
                             :include-date nil))
    (org-update-dblock)))

;;;###autoload
(defun my/denote-links-this-week (&optional arg)
  "Insert a denote-links block for this week.
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (if arg
                   (calendar-read-date)
                 (list (string-to-number (format-time-string "%m"))
                       (string-to-number (format-time-string "%d"))
                       (string-to-number (format-time-string "%Y")))))
         (month (nth 0 date))
         (day   (nth 1 date))
         (year  (nth 2 date))
         (time  (encode-time 0 0 0 day month year))
         (dow (string-to-number (format-time-string "%u" time)))
         (monday (time-subtract time (days-to-time (1- dow))))
         (dates (mapcar (lambda (i)
                          (format-time-string "%Y%m%d"
                                              (time-add monday (days-to-time i))))
                        (number-sequence 0 6)))
         (regexp (mapconcat (lambda (d) (concat d "T*")) dates "\\\\|")))
    (insert
     (format "#+BEGIN: denote-links :regexp \"%s\" :not-regexp nil :excluded-dirs-regexp \"\\\\(journal\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\" :sort-by-component nil :reverse-sort t :id-only nil :include-date nil\n#+END:\n"
             regexp))))

;;;; Refile & Extract

(require 'org-archive)

;;;###autoload
(defun my/refile-to-current-file (arg &optional file)
  "Refile current heading to elsewhere in FILE.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 1)))
        (org-refile-use-outline-path 'file)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

;;;###autoload
(defun my/refile-heading-to-denote-file (arg)
  "Refile current heading to a denote file.
If prefix ARG, move instead of copy. Adds refile metadata."
  (interactive "P")
  (org-set-tags "REFILED")
  (org-todo "DONE")
  (let ((selected-file (denote-file-prompt)))
    (when selected-file
      (my/refile-to-current-file (not arg) selected-file)
      (org-set-property "REFILED" (format-time-string "%Y-%m-%d %H:%M:%S"))
      (org-set-property "REFILED_TO" selected-file))))

;;;###autoload
(defun my/denote-org-extract-subtree (&optional silo)
  "Create new Denote note using current Org subtree.
With prefix SILO argument, ask user to select a silo directory.
Uses subtree title and tags. Deletes the original subtree."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Select a silo: " my-denote-silo-directories nil t))))
  (if-let ((text (org-get-entry))
           (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
      (let ((element (org-element-at-point))
            (tags (org-get-tags))
            (denote-user-enforced-denote-directory silo))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading
                tags
                'org
                nil
                (or (org-element-property :CREATED element)
                    (org-element-property :DATE element)
                    (org-element-property :raw-value
                                          (org-element-property :closed element))))
        (insert text))
    (user-error "No subtree to extract; aborting")))

;;;; Attach & Info

;;;###autoload
(defun my/denote-attach (file &optional description)
  "Save FILE in .attach directory and add a link.
The link will contain DESCRIPTION as text."
  (interactive "*fSelect file to attach: \nMDescription: " org-mode)
  (let ((target-dir (expand-file-name ".attach" denote-directory)))
    (unless (file-directory-p target-dir)
      (make-directory target-dir))
    (let* ((target-basename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (target-filename (make-temp-file
                             (expand-file-name (concat target-basename ".") target-dir)
                             nil
                             (concat "." (file-name-extension file)))))
      (copy-file file target-filename t)
      (org-insert-link nil (concat "file:" target-filename) description)
      (when (yes-or-no-p "Delete the initial file? ")
        (delete-file file t)))))

;;;###autoload
(defun my/denote-info ()
  "Count number of Denote text files, keywords and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
         (denote-files (length (denote-directory-files nil nil t)))
         (attachments (- all-files denote-files))
         (keywords (length (denote-keywords))))
    (message "%s Denote files (%s Attachments), %s Distinct Keywords."
             denote-files attachments keywords)))

;;;; PARA Method

(defvar my/denote-para-keywords '("projects" "areas" "resources" "archives")
  "Keywords for PARA method with Denote.")

;;;###autoload
(defun my/denote-assign-para ()
  "Move your note to either Project, Area, Resource or Archive (PARA)."
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-filename-is-note-p file))
            (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
            (keywords (seq-remove (lambda (keyword)
                                    (member keyword my/denote-para-keywords))
                                  all-keywords))
            (para (completing-read "Select category: " my/denote-para-keywords))
            (new-keywords (push para keywords)))
      (denote-rename-file
       file
       (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
       new-keywords
       (denote-retrieve-filename-signature file))
    (message "Current buffer is not a Denote file.")))

;;;; Org Utilities

;;;###autoload
(defun my/org-insert-notes-drawer ()
  "Generate or open a NOTES drawer under the current heading."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (forward-line)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (org-fold-hide-drawer-toggle 'off)
        (re-search-forward "^[ \t]*:END:" nil t)
        (forward-line -1)
        (org-end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (org-unlogged-message "Press <C-u C-SPACE> to return to the previous position."))

;;;###autoload
(defun my/org-count-words ()
  "Add word count to each heading property drawer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (count-words start end)))
       (org-set-property "WORDCOUNT" (number-to-string word-count))))))

;;;###autoload
(defun my/org-insert-screenshot ()
  "Take a screenshot with ImageMagick and insert as Org link."
  (interactive)
  (let ((filename (read-file-name "Enter filename for screenshot: " default-directory)))
    (unless (string-equal "png" (file-name-extension filename))
      (setq filename (concat (file-name-sans-extension filename) ".png")))
    (call-process-shell-command (format "import %s" filename))
    (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
    (insert (format "[[file:%s]]" filename))
    (org-redisplay-inline-images)))

;;;; Dired Utilities

;;;###autoload
(defun my/dired-narrow (selection)
  "Mark files in dired using SELECTION regexp, then hide unmarked."
  (interactive "sMark files (regexp):")
  (dired-mark-files-regexp selection)
  (dired-toggle-marks)
  (dired-do-kill-lines))

;;;; Olivetti

;;;###autoload
(defun my/olivetti ()
  "Toggle distraction-free writing environment with Olivetti."
  (interactive)
  (if (equal olivetti-mode nil)
      (progn
        (window-configuration-to-register 1)
        (delete-other-windows)
        (text-scale-set 1)
        (olivetti-mode t))
    (progn
      (if (eq (length (window-list)) 1)
          (jump-to-register 1))
      (olivetti-mode 0)
      (text-scale-set 0))))

;;;; Split and Indirect orgtree

;; copy from writers-dot-spacemaccs
(defun my/split-and-indirect-orgtree ()
  "Splits window to the right and opens an org tree section in it"
  (interactive)
  (split-window-right)
  (org-tree-to-indirect-buffer)
  (windmove-right))

(defun my/kill-and-unsplit-orgtree ()
  "Kills the cloned buffer and deletes the window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;;; TODO 정리 대기중
;;;;; org-toggle-emphasis-markers

(defun my/org-toggle-emphasis-markers ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-fontify-buffer :interactively))

;;;; Provide

(provide 'denote-functions)

;;; denote-functions.el ends here
