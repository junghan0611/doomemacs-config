;; -*- mode: emacs-lisp -*-
;; ~/.spacemacs.d/tempel-templates.eld
;; Commentary
;;
;; Organize this file by mode then by abbreviation.  Multi-modes have their own
;; section

;;; Single Mode Entries

;;;; prog-mode
prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

;;;; emacs-lisp-mode
emacs-lisp-mode

(now ";; " (format-time-string "[%Y-%m-%d %a %H:%M]"))
(autoload ";;;###autoload")
(pt "(point)")
(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(package ";;; " (file-name-base (or (buffer-file-name) (buffer-name))) " --- " p " -*- lexical-binding: t -*-"
         n n
         ";; Copyright (C) " (format-time-string "%Y") " Junghanacs " n
         ";; Author: Junghanacs <junghanacs@gmail.com>" n n
         ";; This file is NOT part of GNU Emacs." n
         ";;; Commentary" n p n
         ";;; Code" n p n
         "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)

;;;; fundamental-mode
fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))
(did (format-time-string "%Y%m%d")) ; 20241122T
(dayid (format-time-string "%Y%m%dT")) ; 20241122T
(now (format-time-string "[%Y-%m-%d %a %H:%M]"))
;; (format-time-string "%y-%m-%d %-I:%M %p ") ; 23-02-03 6:58 AM
(nowl (format-time-string "- %y-%m-%d %-I:%M %p :: ")) ; - 23-02-03 6:58 AM ::

;;;; markdown-mode
markdown-mode markdown-ts-mode

(jh "[Junghanacs](https://www.junghanacs.com)")

(header "---
last_modified_at:" (format-time-string " %Y-%m-%d") n
"---" n)

(imagelink "![name](https://raw.githubusercontent.com/junghan0611/graphic-design/main/practicalli-desktop-redux.png)")

;;;; org-mode
org-mode

;; (@ (p (jf/denote-link--people)))
;; ;; Blockquote structure for Emacs
;; (bq "#+attr_shortcode:"
;;     (p "Author (e.g. \"pre\" keyword): " pre t) (unless (s-blank? pre) (concat " :pre " pre))
;;     (p "Cite: " cite t) (unless (s-blank? cite) (concat " :cite " cite))
;;     (p "Cite URL: " cite_url t) (unless (s-blank? cite_url) (concat " :cite_url " cite_url))
;;     n "#+begin_blockquote"
;;     n q
;;     n "#+end_blockquote" n)

;; (cite "{{{cite(" (p (completing-read "Cite: " (jf/org-macro-value-list "cite"))) ")}}}")


(verb_block
  (p (completing-read "Method: " '("get" "post" "put") nil t) method t)
  (p (read-string "URL: ") url t)
  (p (completing-read "Content-Type: " '("text/html" "text/plain" "application/json" "application/xml") nil t) type t)
  "#+begin_src verb :wrap src ob-verb-response" n
  > method  " " url n
  > "Content-Type: " type "; charset=utf-8" n
  > q
  "#+end_src")

(date "[[date:" p "][" p "]]")

;; (tag-meta "{{{meta(" (p (completing-read "meta: " (my/org-tag-value-list "meta"))) ")}}}")

(meta "{{{meta(" (p (completing-read "Meta: " (jf/org-macro-value-list "meta"))) ")}}}")

(keyboard "{{{kbd(" (p (completing-read "Keyboard: " (jf/org-macro-value-list "kbd"))) ")}}}")
(idiomatic "{{{i(" (p (completing-read "Idiom: " (jf/org-macro-value-list "i"))) ")}}}")
(shell "#+begin_src bash :results scalar replace :exports both :tangle yes" n> q n> "#+end_src" n>)
(rpgs "[[abbr-plural:20221009T115226][RPGs]]")
(rpg "[[abbr:20221009T115226][RPGs]]")

;; (dtoday "[[denote:" (format-time-string "%Y%m%dT000000") "][" (format-time-string "%Y-%m-%d") "]]") ; for daily journal

(dtoday "[[denote:"
        (format-time-string "%Y%m%dT000000"
                            (org-journal--convert-time-to-file-type-time
                             (time-subtract (current-time)
                                            (* 3600 org-extend-today-until))))
        "::#"
        (downcase (format-time-string "%Y-%m-%d-%a"))
        "]["
        (format-time-string "|%Y-%m-%d %a %H:%M|") "]]") ; for weekly journal

(dtoday-prompt "** [[denote:"
        (format-time-string "%Y%m%dT000000"
                            (org-journal--convert-time-to-file-type-time
                             (time-subtract (current-time)
                                            (* 3600 org-extend-today-until))))
        "::#"
        (downcase (format-time-string "%Y-%m-%d-%a"))
        "]["
        (format-time-string "|%Y-%m-%d %a %H:%M|") "]]\n"
        "*** @user ")

(dtoday-full-prompt "* 로그 :LLMLOG:\n** [[denote:"
        (format-time-string "%Y%m%dT000000"
                            (org-journal--convert-time-to-file-type-time
                             (time-subtract (current-time)
                                            (* 3600 org-extend-today-until))))
        "::#"
        (downcase (format-time-string "%Y-%m-%d-%a"))
        "]["
        (format-time-string "|%Y-%m-%d %a %H:%M|") "]]\n"
        "*** @user ")

(edit "Edit" (format-time-string " [%Y-%m-%d %a %H:%M]: "))

(conn-notes "\n* 연결노트\n + -> ")

(Today "[[date:" (format-time-string "%Y-%m-%d") "][Today]]")
(did (format-time-string "%Y%m%d")) ; remove T
;; (tor "[[https://takeonrules.com][Take on Rules]]")
(jgi "[[https://junghanacs.github.io][junghanacs.github.io]]")
(uml "#+begin_src plantuml" n> "@startuml" n> "!theme amiga" n> q n> "@enduml" n> "#+end_src" n>)
(localvars "\* Local Variables :noexport:
-----
# Local Variables:
# fill-column: 80
# org-num-max-level: 2
# eval: (auto-fill-mode 1)
# eval: (org-num-mode 1)
# End:" n>)

(localvarg "\* Local Variables :noexport:
-----
# Local Variables:
# fill-column: 80
# eval: (org-num-mode 1)
# End:" n>)

(project n "#+PROJECT_NAME: " (p "Project Name: " project-name)
         n "#+PROJECT_PATHS: (\"Code, Local\" . \"" p "\")"
         n "#+PROJECT_PATHS: (\"Notes, " (s project-name) "\" . \"" (buffer-file-name)  "\")"
         n "#+PROJECT_PATHS: (\"Project Board\" . \"" p "\")"
         n "#+PROJECT_PATHS: (\"Code, Remote\" . \"" (p "Remote Code: " remote) "\")"
         n "#+PROJECT_PATHS: (\"Actions\" . \"" (s remote) "actions/\")"
         n "#+PROJECT_PATHS: (\"Issues\" . \"" (s remote) "issues/\")"
         n "#+PROJECT_PATHS: (\"Pull Requests\" . \"" (s remote) "pulls/\")")

(update "#+attr_shortcode: :date " (format-time-string "%Y-%m-%d")
        " :mode " (p (completing-read "Mode: " '(inline paragraph sidenote section fallback) nil t) mode)
        n "#+begin_update"
        n q
        n "#+end_update" n)

;;;; Denote

;; :regexp "2023.*_emacs"
(d-notes-only
 "#+BEGIN: denote-links :regexp \"202502..T*" p
 "\" :excluded-dirs-regexp \"\\\\(meta\\\\|journal\\\\|bib\\\\|docs\\\\|posts\\\\|ekg\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

(d-bib-only
 "#+BEGIN: denote-links :regexp \"202502..T*" p
 "\" :excluded-dirs-regexp \"\\\\(meta\\\\|journal\\\\|notes\\\\|docs\\\\|posts\\\\|ekg\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

(d-meta-only
 "#+BEGIN: denote-links :regexp \"" p
 "\" :excluded-dirs-regexp \"\\\\(bib\\\\|journal\\\\|notes\\\\|docs\\\\|posts\\\\|ekg\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

(d-docs-only
 "#+BEGIN: denote-links :regexp \"202..*" p
 "\" :excluded-dirs-regexp \"\\\\(bib\\\\|journal\\\\|notes\\\\|meta\\\\|posts\\\\|ekg\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

(d-posts-only
 "#+BEGIN: denote-links :regexp \"202..*" p
 "\" :excluded-dirs-regexp \"\\\\(bib\\\\|journal\\\\|notes\\\\|docs\\\\|meta\\\\|ekg\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

(d-ekg-only
 "#+BEGIN: denote-links :regexp \"202..*" p
 "\" :excluded-dirs-regexp \"\\\\(bib\\\\|journal\\\\|notes\\\\|meta\\\\|docs\\\\|posts\\\\)\" :sort-by-component identifier :reverse-sort t :include-date t" n
 "#+END:" n)

;;;; Export Org-roam to Jecklle Markdown

;; (header "
;; #+EXPORT_FILE_NAME: ~/git/blog/_pages/
;; # #+options: broken-links:mark toc:nil author:nil last-modified:nil
;; # #+gfm_tags: markdown zenn
;; #+gfm_custom_front_matter: :last_modified_at 2023-08-15
;; # #+gfm_custom_front_matter: :tags update markdown
;; ")

;; ~/sync/man/dotsamples/vanilla/jousimies-dotfiles/template/tempel
;; (blog
;;  "#+DESCRIPTION: " p n>
;;  "#+DATE: " n>
;;  "#+AUTHOR: Junganacs" n)

;; 한글태그, SLUG(URI), 타이틀, 요약/설명,
;; (hugofront
;;  "#+HUGO_TAGS: \"태그\"
;; # #+HUGO_SLUG: abc
;; #+HUGO_SERIES:
;; #+HUGO_CATEGORIES:
;; #+HUGO_DRAFT: nil

;; #+hugo_custom_front_matter: :title \"title\"

;; #+hugo: more

;; * #Keyword :noexport:
;; #+BEGIN: denote-links :regexp \"000\" :excluded-dirs-regexp nil :sort-by-component nil :reverse-sort nil :id-only nil :include-date t
;; #+END:

;; * #Backlink :noexport:
;; #+BEGIN: denote-backlinks :excluded-dirs-regexp nil :sort-by-component nil :reverse-sort nil :id-only nil :this-heading-only nil :include-date t
;; #+END:

;; * Related-Notes
;; #+print_bibliography:")

(hugofull
 "#+DESCRIPTION: " p n>
 "#+DATE: " p n>
 "#+AUTHOR: Jousimies" p n>
 "#+OPTIONS: author:nil" p n>
 "#+AUTO_EXPORT: t" p n>
 "#+HUGO_BASE_DIR: ~/Documents/hugo-source" n>
 "#+HUGO_AUTO_SET_LASTMOD: t" n>
 "#+HUGO_TAGS:" n>)

(hugodraft "#+hugo_draft: t")
(hugotags
 "#+description: 요약
#+hugo_tags: notes
#+print_bibliography:")

(ai "#+begin_ai :temperature 1
[SYS]: You are a helpful assistant. Please write in Korean language.
[ME]: " r> n> "#+end_ai")

;;;; ob-gptel 템플릿

;; 기본 호출 (현재 기본 백엔드)
(gpt "#+begin_src gptel" n> r> n> "#+end_src")

;; Claude Proxy — 빠른 채팅 ($0, ~2초)
(gpt-proxy "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6" n> r> n> "#+end_src")

;; OpenRouter Gemini Flash — 번역/요약 기본 (~4초)
(gpt-flash "#+begin_src gptel :backend \"OpenRouter\" :model google/gemini-3-flash-preview" n> r> n> "#+end_src")

;; DeepSeek — 폴백
(gpt-deep "#+begin_src gptel :backend \"DeepSeek\" :model deepseek-chat" n> r> n> "#+end_src")

;; 번역 (Gemini Flash + 번역 프롬프트)
(gpt-translate "#+begin_src gptel :backend \"OpenRouter\" :model google/gemini-3-flash-preview :system \"You are a professional translator. Translate to natural Korean. Keep technical terms in English. No preamble.\" :temperature 0.1" n> r> n> "#+end_src")

;; 요약 (Gemini Flash + 요약 프롬프트)
(gpt-summarize "#+begin_src gptel :backend \"OpenRouter\" :model google/gemini-3-flash-preview :system \"You are a helpful reading assistant. Generate a concise TLDR summary with bullet points. Respond in Korean.\" :temperature 0.4" n> r> n> "#+end_src")

;; 코드 생성 (Sonnet + :wrap)
(gpt-code "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6 :wrap src " (p "언어 (elisp/sh/nix/python): ") n> r> n> "#+end_src")

;; 대화 세션
(gpt-session "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6 :session " (p "세션명: ") n> r> n> "#+end_src")

;; 시스템 프롬프트 커스텀
(gpt-system "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6 :system \"" (p "시스템 프롬프트: ") "\"" n> r> n> "#+end_src")

;; 디버깅 (dry-run)
(gpt-dry "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6 :dry-run yes" n> r> n> "#+end_src")

;; org 형식 출력
(gpt-org "#+begin_src gptel :backend \"Claude-Proxy\" :model claude-sonnet-4-6 :format org" n> r> n> "#+end_src")

(export-html "#+begin_export HTML" n> r> n> "#+end_export")

(tagskeywords "#+hugo_front_matter_key_replace: tags>keywords")

;;;; for org-roam and hugo

(hugovideo "#+attr_html: :preload metadata :style center :width 700 :controls t
#+begin_video
<source src=\"/images/" r> "\" type=\"video/mp4\">
Your browser does not support the video tag.
#+end_video")

;; (lastmodified "#+last-modified: Time-stamp: <>")
(hugolastmod "#+hugo_lastmod: Time-stamp: <>")

(hugotitle "#+hugo_front_matter_key_replace: title>today
#+EXPORT_HUGO_FRONT_MATTER_FORMAT: yaml
#+begin_src yaml :front_matter_extra t
title: \"title\"
alias:
 - \"abc\"
#+end_src")

(glossary "#+glossary_sources: ithink\n")

;; (hugofront "
;; # #+EXPORT_HUGO_PANDOC_CITATIONS: t
;; #+cite_export: csl
;; #+glossary_sources: ithink

;; #+description: dd
;; #+options: ':t toc:4 num:t H:8
;; # #+hugo_custom_front_matter: :toc false
;; # #+hugo_front_matter_key_replace: aliases>nil
;; # #+hugo_front_matter_key_replace: tags>keywords
;; # #+print_bibliography:

;; #+hugo: more

;; ")

;; shortcode hugo
;; (hugoside "#+begin_sidenote" n> r> n> "#+end_sidenote")
;; (hugoinfo "#+attr_shortcode: info" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugowarning "#+attr_shortcode: warning" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugodanger "#+attr_shortcode: danger" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugodetails "#+attr_shortcode: \"Details\"" n "#+begin_details" n> r> n> "#+end_details")

;; link
(lk "[[" p "]]" n )
(zk "
===
<-
===
->
===
")

;;;;; Consider "The One Ring" minor mode to inject these.
;; (rune "{{{rune(" (completing-read "Rune: " jf/gaming/runes) ")}}}")
;; (asl "As {{{i(Loremaster)}}}, ")

;; junghanacs

;; (imagelink "
;; #+attr_html: :width 100% :align center
;; #+caption: noname
;; [[https://raw.githubusercontent.com/junghan0611/graphic-design/main/practicalli-desktop-redux.png]]")
(imglink "#+attr_html: :width 100% :align center" n "#+caption: " p n "#+attr_org: :width 640px" n "[[file:" p "]]")
(imgcaption "#+attr_html: :width 100% :align center" n "#+caption: " p n "#+attr_org: :width 640px")

(filelink "[[file:" p "][" p "]]" n )
(filelinkline "[[file:" p "::" p "][" p "]]" n )

;; (dayscreenshot "~/screenshot/" (format-time-string "%Y%m%dT")) ; 20241122T
(screenshotfilelink "[[file:~/screenshot/" p "]]")
(desktopscreenshotfilelink "[[file:~/screenshot/" (format-time-string "%Y%m%dT"))
(mobilescreenshot "[[file:~/screenshot/Screenshot_" (format-time-string "%Y%m%d_"))
(mobilescreenshotkyobo "[[file:~/screenshot/kyoboebook_" (format-time-string "%Y%m%d-"))

(export-html-quartz "#+begin_export html\n![[../images/.jpg|320]]\n#+end_export")
(export-extimages-youtube-quartz "#+begin_export html\n![]()\n#+end_export")

(attachfilelink "[[file:../.attach/" )

;; for slide / presentation
(revealimg "#+REVEAL_HTML: <div class=\"column\" style=\"float; width: 100%\">" n
           "[[file:" p "]]" n
           "#+REVEAL_HTML: </div>" n)

(revealimgleft "#+REVEAL_HTML: <div class=\"column\" style=\"float:left; width: 50%\">" n
               "[[file:" p "]]" n
               "#+REVEAL_HTML: </div>" n)

(revealimgright "#+REVEAL_HTML: <div class=\"column\" style=\"float:right; width: 50%\">" n
                "[[file:" p "]]" n
                "#+REVEAL_HTML: </div>" n)

(title "#+title: " p n "#+author: Junghan Kim" n "#+language: ko" n n)
(description "#+begin_description" n> r> n> "#+end_description")
(summary "#+begin_summary" n> r> n> "#+end_summary")
(sidenote "#+begin_sidenote" n> r> n> "#+end_sidenote")

(transclude "#+transclude: " r> " :level 2") ; org-transclusion

(mark "#+begin_mark" n> r> n> "#+end_mark")
(quote "#+begin_quote" n> r> n> "#+end_quote")

(example "#+BEGIN_EXAMPLE" n> r> n> "#+END_EXAMPLE")
(notes "#+BEGIN_NOTES" n> r> n> "#+END_NOTES") ; reveal.js slides
(center "#+BEGIN_CENTER" n> r> n> "#+END_CENTER")
(comment "#+BEGIN_COMMENT" n> r> n> "#+END_COMMENT")
(mermaid "#+begin_mermaid" n> r> n> "#+end_mermaid")
(ob-mermaid "#+begin_src mermaid exports: code" n> r> n> "#+end_src")
(ob-mermaid-png "#+begin_src mermaid :file ./test.png " n> r> n> "#+end_src")
(ob-d2 "#+begin_src d2 exports: code" n> r> n> "#+end_src")

(verse "#+BEGIN_VERSE" n> r> n> "#+END_VERSE")
(bash "#+BEGIN_SRC bash :results output" n> r> n> "#+END_SRC") ;; :eval no :exports none
(bashsudo "#+BEGIN_SRC bash :dir /sudo:: :results output" n> r> n> "#+END_SRC") ;; :eval no :exports none

(sh "#+begin_src shell :tangle no :eval no :exports none" n> r> n> "#+end_src")

(hugo-relref "@@hugo:[Title]({{< relref \"docs\" >}})@@")

;; (hugo-quote "#+attr_shortcode: emoji=\"🔮\"\n#+begin_callout" n>
;;             "\"But needless to say, Leonardo never finished executing his plan. Coming up with the conception was enough for him.\"
;; Walter Isaacson 《Leonardo da Vinci》\n#+end_callout")
;; (hugo-info "#+attr_shortcode: type=\"info\"\n#+begin_callout" n> r> n> "#+end_callout")
;; (hugo-warning "#+attr_shortcode: type=\"warning\"\n#+begin_callout" n> r> n> "#+end_callout")
;; (hugo-error "#+attr_shortcode: type=\"error\"\n#+begin_callout" n> r> n> "#+end_callout")
;; (hugo-details "#+attr_shortcode: \"Details\"\n#+begin_details" n> r> n> "#+end_details")
;; (hugo-detailshidden "#+attr_shortcode: title=\"Click me to reveal\" closed=\"true\"\n#+begin_details" n> r> n> "#+end_details")


;;;; quartz quote-block

(quartz-callout-note "#+begin_quote" n> "[!note]" n> n> r> n> "#+end_quote")
(callout-note "#+begin_quote" n> "[!note]" n> n> r> n> "#+end_quote")

(callout-tldr "#+begin_quote" n> "[!tldr]" n> n> r> n> "#+end_quote")
(callout-summary "#+begin_quote" n> "[!summary]" n> n> r> n> "#+end_quote")
(callout-abstract "#+begin_quote" n> "[!abstract] abstract" n> n> r> n> "#+end_quote")

(callout-todo "#+begin_quote" n> "[!todo]" n> n> r> n> "#+end_quote")

;; [!tip] tip hint important
(callout-tip "#+begin_quote" n> "[!tip]" n> n> r> n> "#+end_quote")
(callout-hint "#+begin_quote" n> "[!hint]" n> n> r> n> "#+end_quote")
(callout-important "#+begin_quote" n> "[!important]" n> n> r> n> "#+end_quote")

;; [!done] done check success
(callout-done "#+begin_quote" n> "[!done]" n> n> r> n> "#+end_quote")
(callout-success "#+begin_quote" n> "[!success]" n> n> r> n> "#+end_quote")

;; [!question] question help faq
(callout-question "#+begin_quote" n> "[!question]" n> n> r> n> "#+end_quote")
(callout-faq "#+begin_quote" n> "[!faq]" n> n> r> n> "#+end_quote")
(callout-help "#+begin_quote" n> "[!help]" n> n> r> n> "#+end_quote")

;; [!warning] warning caution attention
(callout-warning "#+begin_quote" n> "[!warning]" n> n> r> n> "#+end_quote")
(callout-caution "#+begin_quote" n> "[!caution]" n> n> r> n> "#+end_quote")
(callout-attention "#+begin_quote" n> "[!attention]" n> n> r> n> "#+end_quote")

;; [!failure] failure fail missing
(callout-fail "#+begin_quote" n> "[!fail]" n> n> r> n> "#+end_quote")
(callout-failure "#+begin_quote" n> "[!failure]" n> n> r> n> "#+end_quote")
(callout-missing "#+begin_quote" n> "[!missing]" n> n> r> n> "#+end_quote")

;; [!error] error danger
(callout-error "#+begin_quote" n> "[!error]" n> n> r> n> "#+end_quote")
(callout-danger "#+begin_quote" n> "[!danger]" n> n> r> n> "#+end_quote")

;; [!bug]
(callout-bug "#+begin_quote" n> "[!bug]" n> n> r> n> "#+end_quote")

;; [!example]
(callout-example "#+begin_quote" n> "[!example]" n> n> r> n> "#+end_quote")

;; [!quote] cite quote
(calloutquote "#+begin_quote" n> "[!quote] quote" n> n> r> n> "

― 저자명, 『책』
#+end_quote")

(calloutquotesimple "#+begin_quote" n> "[!quote] quote" n> n> r> n> " #+end_quote")

;;;; python

(python "# #+name:
#+begin_src python :results output" n> r> n> "#+end_src")

(jupyter-python "#+begin_src jupyter-python :session py" n> r> n> "#+end_src")

(hy "# #+name:
#+begin_src hy :results output" n> r> n> "#+end_src")

;;;; others

(coq "# #+name:
#+begin_src coq" n> r> n> "#+end_src")

(bash-0755 "#+begin_src bash :tangle run.sh :tangle-mode (identity #o755)" n> r> n> "#+end_src")

(text "#+begin_src text" n> r> n> "#+end_src")
(yaml "#+begin_src yaml" n> r> n> "#+end_src")
(json "#+begin_src json" n> r> n> "#+end_src")
(jsontangle "#+begin_src json :tangle a.json" n> r> n> "#+end_src")

(org "#+BEGIN_SRC org" n> r> n> "#+END_SRC")
(elisp "#+BEGIN_SRC elisp" n> r> n> "#+END_SRC")
(elixir "#+BEGIN_SRC elixir" n> r> n> "#+END_SRC")

(emacslisp "#+BEGIN_SRC emacs-lisp :tangle yes" n> r> n> "#+END_SRC")
;; (emacslisp "#+BEGIN_SRC emacs-lisp :tangle no :eval no" n> r> n> "#+END_SRC")
(clojure "#+BEGIN_SRC clojure :results code" n> r> n> "#+END_SRC")
;; (clj "#+BEGIN_SRC clojure :results code" n> r> n> "#+END_SRC")
(md "#+BEGIN_SRC markdown" n> r> n> "#+END_SRC")
(typescript "#+BEGIN_SRC typescript " n> r> n> "#+END_SRC")
(js "#+BEGIN_SRC javascript " n> r> n> "#+END_SRC")
;; (elispt "#+BEGIN_SRC emacs-lisp :tangle yes" n> r> n> "#+END_SRC") ; tangle yes

(src "#+begin_src" p n> r> n> "#+end_src")
(srcedit "#+begin_src " n> r> n "#+end_src" :post (org-edit-src-code))

;; (src "#+BEGIN_SRC" p n> r> n> "#+END_SRC" :post (org-edit-src-code))
;; (elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))
(clisp "#+BEGIN_SRC common-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))
(rkt "#+BEGIN_SRC racket" n> r> n "#+END_SRC" :post (org-edit-src-code))
(rktsicp "#+BEGIN_SRC racket :lang sicp :eval no-export :exports both" n> r> n "#+END_SRC" :post (org-edit-src-code))
(clj "#+BEGIN_SRC clojure :backend babashka" n> r> n "#+END_SRC" :post (org-edit-src-code))
(cljs "#+BEGIN_SRC clojurescript" n> r> n "#+END_SRC" :post (org-edit-src-code))

(scheme "#+BEGIN_SRC scheme" n> r> n "#+END_SRC" :post (org-edit-src-code))
(scheme2 "#+BEGIN_SRC scheme" n> r> n> "#+END_SRC")

(contact "
:PROPERTIES:
:TYPE:
:GITHUB:
:URL:
:EMAIL:
:NOTE:
:END:")

(bib "
:PROPERTIES:
:TITLE:
:BTYPE: webpage
:AUTHOR:
:URL:
:YEAR: 2023
:CUSTOM_ID:
:END:")

;;;; org-journal-mode

org-journal-mode

(addnewentry (format-time-string "** %H:%M "))
(now (format-time-string "%H:%M "))

;; Daily-templates
;; https://isamert.net/2021/01/25/how-i-do-keep-my-days-organized-with-org-mode-and-emacs.html
;; <2024-09-19 Thu 14:00-15:00>
;; 2024-08-27 daily journal template
;; "#+hugo_categories: Journal Note" n
;; "#+hugo_series:" n
;; "#+description:\n#+category: Journal\n#+startup: overview\n" n
;; "#+BEGIN: clocktable :scope agenda :maxlevel 2 :step day :fileskip0 true :tstart \"" (format-time-string "%Y-%m-%d") "\" :tend \"" (format-time-string "%Y-%m-%d" (time-add 86400 (current-time)))
;; "\" \n#+END:\n\n"
(template
 "#+title: " (format-time-string "%Y-%m-%d") n
 "#+hugo_lastmod: " n
 "#+filetags: :journal:\n#+date: " (format-time-string "[%Y-%m-%d %a %H:%M]") n
 "#+identifier: " (format-time-string "%Y%m%dT000000") n
 "#+export_file_name: " (format-time-string "%Y%m%dT000000") ".md" n
 "# #+print_bibliography:" n n
 "* " (format-time-string "%Y-%m-%d") n
 "<" (format-time-string "%Y-%m-%d %a") ">" n
 "\n#+begin_quote" n>
 "\"But needless to say, Leonardo never finished executing his plan. Coming up with the conception was enough for him.\"
Walter Isaacson 《Leonardo da Vinci》\n#+end_quote" n n
 "+ [ ] #영감" n
 "+ [ ] #원씽" n
 "+ [ ] #가족" n
 "+ [ ] #요약
   #+BEGIN: clocktable :scope tree1 :maxlevel 2 :block untilnow
   #+END:" n
 "+ [ ] #오늘만든노트
   #+BEGIN: denote-links :regexp \"" (format-time-string "%Y%m%dT")
 "\" :not-regexp nil :excluded-dirs-regexp nil :sort-by-component nil :reverse-sort nil :id-only nil :include-date t
   #+END:" n
 "** " (format-time-string "%H:%M 시작 ") n)

;;;; text-mode
text-mode

;; This is an example of using the value of `(s str)' to populate the above and
;; below line!
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(banner comment-start (make-string (length title) ?=) n comment-start (p "Title: " title) n comment-start (make-string (length title) ?=))

;;;; web-mode
web-mode

(bye "<% byebug %>" n)
(db "<% require \"debug\"; binding.break %>" n)
(tor "<cite><a href=\"https://takeonrules.com\">Take on Rules</a></cite>")
(today "<time datetime=\"" (format-time-string "%Y-%m-%d") "\">today</time>")

;;;; eshell-mode
eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

;;; Multi-Mode Entries

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

;;; Clojure and ClojureScript

clojure-mode clojurescript-mode

(-- ";; " (format-time-string "[%Y-%m-%d %H:%M] "))

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
