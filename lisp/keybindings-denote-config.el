;;; $DOOMDIR/lisp/keybindings-denote-config.el --- Denote Keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Denote keybindings for Doom Emacs
;; Access via M-e or C-c n

;;; Code:

;;;; Define denote-map for dual prefix binding

(defvar my/denote-map (make-sparse-keymap)
  "Keymap for Denote commands, accessible via M-e and C-c n.")

;;;; Bind to M-e and C-c n

(map! "M-e" nil)   ; unbind default
(map! "C-c n" nil) ; unbind default

(map! "M-e" my/denote-map)
(map! "C-c n" my/denote-map)

;;;; Which-key descriptions (Doom style)

(after! which-key
  ;; Top-level prefix descriptions
  (which-key-add-key-based-replacements
    "M-e"   "denote"
    "C-c n" "denote")

  ;; Sub-prefix descriptions for my/denote-map
  (which-key-add-keymap-based-replacements my/denote-map
    "a"   "annotate"
    "b"   "bibliography"
    "e"   "extra"
    "p"   "publish"
    "s"   "search"
    "M-s" "sequence"
    "x"   "explore"
    "T"   "themes")

  ;; Nested prefixes under explore (x)
  (which-key-add-key-based-replacements
    "M-e x c" "count"
    "M-e x b" "barchart"
    "M-e x r" "random"
    "M-e x j" "janitor"
    "M-e x n" "network"
    "C-c n x c" "count"
    "C-c n x b" "barchart"
    "C-c n x r" "random"
    "C-c n x j" "janitor"
    "C-c n x n" "network"))

;;;; Annotate (a)

(map! :map my/denote-map
      (:prefix ("a" . "annotate")
       :desc "Archive"              "a" #'orgabilize-org-archive
       :desc "Insert link"          "l" #'orgabilize-insert-org-link
       :desc "Find archived"        "f" #'orgabilize-org-find-archived-file
       :desc "URL as org"           "t" #'org-web-tools-read-url-as-org
       :desc "Links to entries"     "T" #'org-web-tools-convert-links-to-page-entries
       :desc "Remember"             "r" #'remember
       :desc "Remember notes"       "R" #'remember-notes
       :desc "Pandoc import"        "o" #'org-pandoc-import-as-org
       :desc "Inactive timestamp"   "i" #'bh/insert-inactive-timestamp
       :desc "EWW to org"           "e" #'jao-eww-to-org
       :desc "Remark mark"          "m" #'org-remark-mark))

;;;; Bibliography (b)

(map! :map my/denote-map
      (:prefix ("b" . "bibliography")
       :desc "Insert citation"      "b" #'citar-insert-citation
       :desc "Create note"          "c" #'citar-create-note
       :desc "Create silo note"     "C" #'citar-denote-create-silo-note
       :desc "Open note"            "n" #'citar-denote-open-note
       :desc "Open"                 "o" #'citar-open
       :desc "Open entry"           "e" #'citar-open-entry
       :desc "Add reference"        "a" #'citar-denote-add-reference
       :desc "To reading list"      "0" #'my/citar-org-to-reading-list
       :desc "Find citation"        "1" #'citar-denote-find-citation
       :desc "No cite"              "2" #'citar-denote-nocite
       :desc "No bib"               "3" #'citar-denote-nobib
       :desc "Cite no cite"         "4" #'citar-denote-cite-nocite
       :desc "Insert citation"      "i" #'citar-insert-citation
       :desc "Open links"           "O" #'citar-open-links
       :desc "Find reference"       "f" #'citar-denote-find-reference
       :desc "Link reference"       "l" #'citar-denote-link-reference
       :desc "Create silo"          "S" #'citar-denote-create-silo-note
       :desc "Remove reference"     "k" #'citar-denote-remove-reference
       :desc "Open ref entry"       "r" #'citar-denote-open-reference-entry
       :desc "DWIM"                 "SPC" #'citar-denote-dwim
       :desc "Search citations"     "M-i" #'my/insert-citations-by-search))

;;;; Denote Extra (e)

(map! :map my/denote-map
      (:prefix ("e" . "extra")
       :desc "Random notes"         "1" #'my/denote-random-notes
       :desc "Random bib"           "2" #'my/denote-random-bib
       :desc "Random meta"          "3" #'my/denote-random-meta
       :desc "Explore random"       "4" #'denote-explore-random-note
       :desc "Denote info"          "0" #'my/denote-info
       :desc "Attach"               "a" #'my/denote-attach
       ;; :desc "Eshell export"        "e" #'prot-eshell-export
       :desc "Assign PARA"          "P" #'my/denote-assign-para
       :desc "Dblock insert files"  "M-i" #'denote-org-dblock-insert-files
       :desc "Add sensitive"        "S" #'my/add-sensitive-string))

;;;; Publish (p)

(map! :map my/denote-map
      (:prefix ("p" . "publish")
       :desc "Hugo lastmod"         "1" #'my/insert-hugo-lastmod-time-stamp
       :desc "Open in hugo"         "o" #'my/org-open-exported-markdown-in-hugo-content
       :desc "Export directory"     "E" #'my/org-hugo-export-directory
       :desc "Update dblocks"       "u" #'my/org-update-all-dblocks
       :desc "Update garden all"    "U" #'my/update-dblock-garden-all
       :desc "Export garden all"    "M-p" #'my/update-dblock-export-garden-all
       :desc "Screenshot links"     "M-s" #'my/insert-screenshot-links-by-date
       :desc "Search citations"     "M-i" #'my/insert-citations-by-search
       :desc "Delete blank lines"   "M-d" #'my/delete-multiple-blank-lines
       :desc "Update link desc"     "l" #'my/denote-update-link-descriptions
       :desc "Reveal export"        "r" #'org-re-reveal-export-to-html-and-browse
       :desc "Hugo export"          "e" #'org-hugo-export-wim-to-md
       :desc "Replace in content"   "X" #'my/export-replace-in-notes-content-dir))

;;;; Search (s)

(map! :map my/denote-map
      (:prefix ("s" . "search")
       :desc "Search"               "SPC" #'denote-search
       :desc "Search"               "f" #'denote-search
       :desc "Marked dired"         "d" #'denote-search-marked-dired-files
       :desc "Referenced in region" "r" #'denote-search-files-referenced-in-region
       :desc "Find parent"          "p" #'(lambda () (interactive) (denote-sequence-find 'parent))
       :desc "Find siblings"        "s" #'(lambda () (interactive) (denote-sequence-find 'siblings))
       :desc "Find children"        "c" #'(lambda () (interactive) (denote-sequence-find 'children))))

;;;; Sequence (M-s)

(map! :map my/denote-map
      (:prefix ("M-s" . "sequence")
       :desc "New sequence"         "n" #'denote-sequence
       :desc "Find"                 "F" #'denote-sequence-find
       :desc "Link"                 "l" #'denote-sequence-link
       :desc "Dired"                "d" #'denote-sequence-dired
       :desc "Reparent"             "r" #'denote-sequence-reparent
       :desc "New child"            "c" #'denote-sequence-new-child-of-current
       :desc "Convert"              "C" #'denote-sequence-convert
       :desc "New sibling"          "s" #'denote-sequence-new-sibling-of-current
       :desc "Rename signature"     "z" #'denote-rename-file-signature))

;;;; Explore (x)

(map! :map my/denote-map
      (:prefix ("x" . "explore")
       ;; Statistics
       (:prefix ("c" . "count")
        :desc "Count notes"         "n" #'denote-explore-count-notes
        :desc "Count keywords"      "k" #'denote-explore-count-keywords)
       ;; Barchart
       (:prefix ("b" . "barchart")
        :desc "Filetypes"           "f" #'denote-explore-barchart-filetypes
        :desc "Keywords"            "k" #'denote-explore-barchart-keywords
        :desc "Timeline"            "t" #'denote-explore-barchart-timeline
        :desc "Degree"              "d" #'denote-explore-barchart-degree
        :desc "Backlinks"           "b" #'denote-explore-barchart-backlinks)
       ;; Random
       (:prefix ("r" . "random")
        :desc "Random note"         "n" #'denote-explore-random-note
        :desc "Random regex"        "r" #'denote-explore-random-regex
        :desc "Random link"         "l" #'denote-explore-random-link
        :desc "Random keyword"      "k" #'denote-explore-random-keyword)
       ;; Janitor
       (:prefix ("j" . "janitor")
        :desc "Duplicate notes"     "d" #'denote-explore-duplicate-notes
        :desc "Duplicate dired"     "D" #'denote-explore-duplicate-notes-dired
        :desc "Missing links"       "l" #'denote-explore-missing-links
        :desc "Zero keywords"       "z" #'denote-explore-zero-keywords
        :desc "Single keywords"     "s" #'denote-explore-single-keywords
        :desc "Rename keywords"     "r" #'denote-explore-rename-keywords
        :desc "Sync metadata"       "y" #'denote-explore-sync-metadata
        :desc "Isolated files"      "i" #'denote-explore-isolated-files)
       ;; Network
       (:prefix ("n" . "network")
        :desc "Network"             "n" #'denote-explore-network
        :desc "Regenerate"          "r" #'denote-explore-network-regenerate)))

;;;; Themes (T) - Modus/Ef/Doric

(map! :map my/denote-map
      (:prefix ("T" . "themes")
       :desc "Modus toggle"         "m" #'modus-themes-toggle
       :desc "Modus select"         "M" #'modus-themes-select
       :desc "Ef toggle"            "e" #'ef-themes-toggle
       :desc "Ef select"            "E" #'ef-themes-select
       :desc "Ef random light"      "," #'ef-themes-load-random-light
       :desc "Ef random dark"       "." #'ef-themes-load-random-dark
       :desc "Doric random"         "r" #'doric-themes-load-random
       :desc "Doric select"         "R" #'doric-themes-select))

;;;; Main bindings

(map! :map my/denote-map
      :desc "Backlinks for heading" "B" #'denote-org-backlinks-for-heading
      :desc "Create note"           "d" #'denote-create-note
      :desc "Find file"             "f" #'my/denote-find-file
      :desc "Find by modified"      "F" #'my/denote-find-file-by-modified
      :desc "Grep"                  "g" #'my/denote-grep
      :desc "Consult grep"          "SPC" #'consult-denote-grep
      :desc "Insert citation"       "1" #'citar-insert-citation
      :desc "Assign evergreen"      "E" #'my/denote-assign-evergreen
      :desc "Howmish find"          "o" #'my/denote-howmish-find-file
      :desc "Store link heading"    "h" #'my/denote-org-store-link-to-heading
      :desc "Link to heading"       "H" #'denote-org-link-to-heading
      :desc "Dblock links"          "i" #'my/denote-org-dblock-insert-links
      :desc "Dblock backlinks"      "I" #'my/denote-org-dblock-insert-backlinks
      :desc "Dblock bib links"      "M-," #'my/denote-org-dblock-insert-bib-links
      :desc "Dblock notes links"    "M-." #'my/denote-org-dblock-insert-notes-links
      :desc "Dblock meta links"     "M-i" #'my/denote-org-dblock-insert-meta-links
      :desc "Link"                  "l" #'denote-link
      :desc "Link after creating"   "L" #'denote-link-after-creating-with-command
      :desc "Consult notes"         "n" #'consult-notes
      :desc "Screenshot"            "!" #'my/consult-org-screenshot
      :desc "Search all notes"      "G" #'consult-notes-search-in-all-notes
      :desc "Insert meta links"     "m" #'my/denote-insert-meta-links
      :desc "Create meta note"      "M" #'my/denote-create-meta-note
      :desc "Type"                  "t" #'denote-type
      :desc "Region"                "r" #'denote-region
      :desc "Rename front-matter"   "," #'denote-rename-file-using-front-matter
      :desc "Rename title"          "<" #'denote-rename-file-title
      :desc "Random note"           "/" #'my/denote-random-note
      :desc "Backlinks buffer"      "-" #'denote-show-backlinks-buffer
      :desc "Journal current"       "TAB" #'org-journal-open-current-journal-file
      :desc "Journal new"           "j" #'org-journal-new-entry
      :desc "Rename keywords"       "k" #'denote-rename-file-keywords
      :desc "Rename signature"      "z" #'denote-rename-file-signature
      :desc "Assign zettel"         "Z" #'my/denote-assign-zettel
      :desc "Find link"             "M-f" #'denote-find-link
      :desc "Find backlink"         "M-b" #'denote-find-backlink)

;;;; Org-mode specific bindings

(after! org
  (map! :map org-mode-map
        "C-x n b" #'org-cite-insert
        "C-x n -" #'bh/insert-inactive-timestamp
        "M-s ,"   #'denote-rename-file-using-front-matter
        "M-s <"   #'denote-rename-file-title
        "C-x n 0" #'my/org-insert-notes-drawer
        "C-x n 9" #'my/org-count-words
        "C-x n l" #'my/denote-org-store-link-to-heading))

;;;; Provide

(provide 'keybindings-denote-config)

;;; keybindings-denote-config.el ends here
