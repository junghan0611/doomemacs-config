;;; $DOOMDIR/lisp/denote-functions.el --- Denote Helper Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Junghan Kim

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

;; GFM compatible CUSTOM_ID: h-UUID (not h:UUID)
;; denote upstream uses org-id-new "h" → "h:UUID" (colon breaks github-slugger)
;; Override to use h-UUID format at creation time
(defun denote-link-ol-get-id ()
  "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one with h-UUID format (GFM slug-safe)."
  (let* ((pos (point))
         (id (org-entry-get pos "CUSTOM_ID")))
    (if (and (stringp id) (string-match-p "\\S-" id))
        id
      (setq id (concat "h-" (org-id-uuid)))
      (org-entry-put pos "CUSTOM_ID" id)
      id)))

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
                           :include-date t))
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
                           :reverse-sort nil
                           :id-only nil
                           :include-date t))
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
                           "\\(meta\\|office\\|botlog\\|llmlog\\|notes\\|journal\\|posts\\|docs\\|md\\|dict\\|private\\|ekg\\)"
                           :sort-by-component nil
                           :reverse-sort nil
                           :id-only nil
                           :include-date t))
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
                           :reverse-sort nil
                           :id-only nil
                           :include-date t))
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
                             "\\(bib\\|notes\\|office\\|elisp\\|botlog\\|llmlog\\|docs\\|posts\\|md\\|journal\\|dict\\|private\\|ekg\\)"
                             :sort-by-component nil
                             :reverse-sort nil
                             :id-only nil
                             :include-date t))
    (org-update-dblock)))

;;;###autoload
(defun my/denote-links-today (&optional arg)
  "Insert a denote-links block for today (by creation date).
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (ymd (format-time-string "%Y%m%d" time))
         (regexp (concat ymd "T*")))
    (insert
     (format "#+BEGIN: denote-links :regexp \"%s\" :not-regexp nil :excluded-dirs-regexp \"\\\\(journal\\\\|transcript\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\" :sort-by-component nil :reverse-sort nil :id-only nil :include-date t\n#+END:\n"
             regexp))))

;;;###autoload
(defun my/denote-links-this-week (&optional arg)
  "Insert a denote-links block for this week (by creation date).
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (dates (my/org--week-dates time))
         (regexp (mapconcat (lambda (d) (concat d "T*")) dates "\\\\|")))
    (insert
     (format "#+BEGIN: denote-links :regexp \"%s\" :not-regexp nil :excluded-dirs-regexp \"\\\\(journal\\\\|transcript\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\" :sort-by-component nil :reverse-sort nil :id-only nil :include-date t\n#+END:\n"
             regexp))))

;;;###autoload
(defun my/denote-lastmod-today (&optional arg)
  "Insert a denote-lastmod block for today (by #+hugo_lastmod: date).
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (ymd (format-time-string "%Y-%m-%d" time)))
    (insert
     (format "#+BEGIN: denote-lastmod :from \"%s\" :to \"%s\" :excluded-dirs-regexp \"\\\\(journal\\\\|transcript\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\"\n#+END:\n"
             ymd ymd))))

;;;###autoload
(defun my/denote-lastmod-this-week (&optional arg)
  "Insert a denote-lastmod block for this week (by #+hugo_lastmod: date).
With prefix ARG, prompt for a date via calendar.
Shows notes modified this week, not just newly created ones."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (range (my/org--week-range time)))
    (insert
     (format "#+BEGIN: denote-lastmod :from \"%s\" :to \"%s\" :excluded-dirs-regexp \"\\\\(journal\\\\|transcript\\\\|office\\\\|archive\\\\|md\\\\|dict\\\\|posts\\\\|private\\\\|ekg\\\\)\"\n#+END:\n"
             (car range) (cdr range)))))

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
(defun my/org-insert-screenshot (&optional name)
  "Take a screenshot with ImageMagick and insert as Org link.
File saved to ~/org/images/ with YYYYMMDD-NAME.png naming.
NAME defaults to interactive input."
  (interactive)
  (let* ((name (or name (read-string "Screenshot name (no ext): ")))
         (date (format-time-string "%Y%m%d"))
         (filename (expand-file-name
                    (format "%s-%s.png" date name)
                    "~/org/images/"))
         (caption (read-string "Caption: " name)))
    (message "Select area to capture...")
    (call-process-shell-command (format "import %s" (shell-quote-argument filename)))
    (when (file-exists-p filename)
      (insert (format "#+caption: %s\n" caption))
      (insert (format "[[file:%s]]" filename))
      (org-redisplay-inline-images))))

;;;###autoload
(defun my/org--read-date-or-today (&optional arg)
  "Read date from calendar if ARG, otherwise today. Returns (MONTH DAY YEAR)."
  (if arg
      (calendar-read-date)
    (list (string-to-number (format-time-string "%m"))
          (string-to-number (format-time-string "%d"))
          (string-to-number (format-time-string "%Y")))))

(defun my/org--date-to-time (date)
  "Convert (MONTH DAY YEAR) to time value."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun my/org--week-dates (time)
  "Return list of YYYYMMDD strings for the week containing TIME (Mon-Sun)."
  (let* ((dow (string-to-number (format-time-string "%u" time)))
         (monday (time-subtract time (days-to-time (1- dow)))))
    (mapcar (lambda (i)
              (format-time-string "%Y%m%d" (time-add monday (days-to-time i))))
            (number-sequence 0 6))))

(defun my/org--week-range (time)
  "Return (FROM . TO) as YYYY-MM-DD strings for the week containing TIME."
  (let* ((dow (string-to-number (format-time-string "%u" time)))
         (monday (time-subtract time (days-to-time (1- dow))))
         (sunday (time-add monday (days-to-time 6))))
    (cons (format-time-string "%Y-%m-%d" monday)
          (format-time-string "%Y-%m-%d" sunday))))

;;;;; Screenshots

(defun my/org--insert-screenshots-for-dates (date-strings label)
  "Insert screenshot links for DATE-STRINGS (YYYYMMDD list). LABEL for empty message."
  (let* ((img-dir (expand-file-name "~/screenshot/"))
         (files (when (file-directory-p img-dir)
                  (seq-filter
                   (lambda (f)
                     (let ((name (file-name-nondirectory f)))
                       (seq-some (lambda (d)
                                   (or (string-prefix-p d name)        ; Denote: 20260401T...
                                       (string-match-p (concat "_" d) name))) ; Android: Screenshot_20260401_...
                                 date-strings)))
                   (directory-files img-dir t "\\.\\(png\\|jpg\\|jpeg\\|gif\\|webp\\)$"))))
         (sorted (seq-sort (lambda (a b) (string> a b)) files)))
    (if (null sorted)
        (insert (format "%s 스크린샷 없음.\n" label))
      (dolist (f sorted)
        (let ((name (file-name-sans-extension (file-name-nondirectory f))))
          (insert (format "- [[file:%s][%s]]\n" f name)))))))

;;;###autoload
(defun my/org-insert-screenshots-today (&optional arg)
  "Insert links to today's screenshots. With prefix ARG, pick date from calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (ymd (format-time-string "%Y%m%d" time)))
    (my/org--insert-screenshots-for-dates
     (list ymd)
     (format "%s(%s)" (format-time-string "%Y-%m-%d %a" time) ymd))))

;;;###autoload
(defun my/org-insert-screenshots-this-week (&optional arg)
  "Insert links to screenshots taken this week (by filename date).
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (dates (my/org--week-dates time)))
    (my/org--insert-screenshots-for-dates
     dates
     (format "이번 주(%s ~ %s)" (car dates) (car (last dates))))))

;;;;; Citations (urldate)

(defun my/org--insert-citations-for-range (from to)
  "Insert org-cite links for BibTeX entries with urldate between FROM and TO."
  (let* ((bib-files (bound-and-true-p org-cite-global-bibliography))
         (script (expand-file-name "bin/bib-urldate.pl" doom-user-dir))
         (bib-args (mapconcat (lambda (f) (shell-quote-argument (expand-file-name f)))
                              (seq-filter #'file-exists-p bib-files) " "))
         (output (shell-command-to-string
                  (format "perl %s %s %s %s"
                          (shell-quote-argument script) from to bib-args)))
         (results nil))
    (dolist (line (split-string output "\n" t))
      (let* ((parts (split-string line "\t" t))
             (key (car parts))
             (title (cadr parts)))
        (when (and key title)
          (push (cons key title) results))))
    (if (null results)
        (insert (format "%s ~ %s 추가된 서지 없음.\n" from to))
      (insert (format "** [urldate: %s ~ %s]\n\n" from to))
      (dolist (entry (nreverse results))
        (insert (format "- %s [cite:@%s]\n" (cdr entry) (car entry)))))))

;;;###autoload
(defun my/org-insert-citations-today (&optional arg)
  "Insert org-cite links for BibTeX entries added today (by urldate).
With prefix ARG, pick date from calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (ymd (format-time-string "%Y-%m-%d" time)))
    (my/org--insert-citations-for-range ymd ymd)))

;;;###autoload
(defun my/org-insert-citations-this-week (&optional arg)
  "Insert org-cite links for BibTeX entries added this week (by urldate).
With prefix ARG, prompt for a date via calendar."
  (interactive "P")
  (let* ((date (my/org--read-date-or-today arg))
         (time (my/org--date-to-time date))
         (range (my/org--week-range time)))
    (my/org--insert-citations-for-range (car range) (cdr range))))


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
