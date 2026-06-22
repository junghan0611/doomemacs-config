;;; $DOOMDIR/lisp/telega-rich-md.el --- TDLib richMessage → markdown source -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config

;;; Commentary:

;; Pure serialization of TDLib `richMessage' content (Bot API 10.1 rich
;; messages, TDLib 1.8.64+) to a markdown SOURCE string.  OpenClaw sends bot
;; output as the new `messageRichMessage' content type; telega.el has no
;; renderer for it yet, so `ai-bot-config.el' intercepts it and feeds the
;; :blocks here.
;;
;; Why a separate, telega-free file: per AGENTS.md, reusable input→output logic
;; should run under `emacs -Q' so `tests/run-tests.sh' can gate it.  The two
;; telega primitives this needs (`telega--tl-type', `telega-tl-str') are
;; reduced to local pure helpers; everything else is plain plist/vector walking.
;; The telega-dependent insert/advice glue stays in `ai-bot-config.el'.
;;
;; SSOT for "what renders well vs degrades to plain text" is the Txx support
;; matrix in tests/fixtures/rich/ (real TDLib 1.8.65 payloads + golden output).
;; When telega/TDLib drifts, that matrix is the first regression line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; telega primitive shims (pure)

(defun my/telega--rt-type (obj)
  "Return the TDLib `:@type' of OBJ as an interned symbol, or nil."
  (when-let* ((ty (and (listp obj) (plist-get obj :@type))))
    (intern ty)))

(defun my/telega--rt-str (obj key)
  "Return KEY of OBJ as a plain string, \"\" when absent.
Mirrors `telega-tl-str' for the plain-string fields used here; a nested
formattedText degrades to its own :text."
  (let ((v (plist-get obj key)))
    (cond ((stringp v) v)
          ((and (listp v) (stringp (plist-get v :text))) (plist-get v :text))
          (t ""))))

;;;; RichText (inline) → markdown

(defun my/telega--md-face (str face)
  "Return STR with FACE combined over its whole length.
Markdown markers stay literal text (markdown-mode style); the face only
adds visual emphasis and is additive so nested link/code faces survive."
  (let ((s (copy-sequence str)))
    (add-face-text-property 0 (length s) face nil s)
    s))

(defconst my/telega--superscript-map
  '((?0 . ?⁰) (?1 . ?¹) (?2 . ?²) (?3 . ?³) (?4 . ?⁴)
    (?5 . ?⁵) (?6 . ?⁶) (?7 . ?⁷) (?8 . ?⁸) (?9 . ?⁹)
    (?+ . ?⁺) (?- . ?⁻) (?= . ?⁼) (?\( . ?⁽) (?\) . ?⁾)
    (?n . ?ⁿ) (?i . ?ⁱ))
  "Char → Unicode superscript for the safe, widely-rendered set.")

(defconst my/telega--subscript-map
  '((?0 . ?₀) (?1 . ?₁) (?2 . ?₂) (?3 . ?₃) (?4 . ?₄)
    (?5 . ?₅) (?6 . ?₆) (?7 . ?₇) (?8 . ?₈) (?9 . ?₉)
    (?+ . ?₊) (?- . ?₋) (?= . ?₌) (?\( . ?₍) (?\) . ?₎))
  "Char → Unicode subscript for the safe, widely-rendered set.")

(defun my/telega--rich-script (str map)
  "Map each char of STR through MAP to a Unicode super/subscript glyph.
Unmapped chars pass through unchanged, so arbitrary content degrades to
plain text instead of dropping.  Emitted as source text (not a raise-face)
to match this module's markdown-source contract and survive re-export."
  (apply #'string (mapcar (lambda (c) (or (cdr (assq c map)) c)) str)))

(defun my/telega--rich-rt->md (rt)
  "Serialize TDLib RichText RT to a markdown source string.
Markdown markers are kept literal; bold/italic/code/strike also get a face
so they render emphasized (like markdown-mode) without changing text size.
Total over subtypes: unknown 1.8.64 additions fall back to their inner
text, so nothing can break rendering."
  (cond
   ((null rt) "")
   ((stringp rt) rt)
   (t
    (let ((inner (lambda () (my/telega--rich-rt->md (plist-get rt :text)))))
      (cl-case (my/telega--rt-type rt)
        (richTextPlain (or (my/telega--rt-str rt :text) ""))
        (richTexts (mapconcat #'my/telega--rich-rt->md (plist-get rt :texts) ""))
        (richTextBold (my/telega--md-face (concat "**" (funcall inner) "**") 'bold))
        (richTextItalic (my/telega--md-face (concat "*" (funcall inner) "*") 'italic))
        (richTextStrikethrough
         (my/telega--md-face (concat "~~" (funcall inner) "~~")
                             'telega-webpage-strike-through))
        (richTextFixed
         (my/telega--md-face (concat "`" (funcall inner) "`") 'telega-webpage-fixed))
        (richTextSpoiler (concat "||" (funcall inner) "||"))
        (richTextMarked
         (my/telega--md-face (concat "==" (funcall inner) "==")
                             'telega-webpage-marked))
        (richTextSuperscript
         (my/telega--rich-script (funcall inner) my/telega--superscript-map))
        (richTextSubscript
         (my/telega--rich-script (funcall inner) my/telega--subscript-map))
        ((richTextUrl richTextReferenceLink)
         (let ((url (my/telega--rt-str rt :url)) (txt (funcall inner)))
           (if (and url (not (string-empty-p url)))
               (format "[%s](%s)" txt url)
             txt)))
        (richTextEmailAddress
         (let ((em (my/telega--rt-str rt :email_address)) (txt (funcall inner)))
           (if (and em (not (string-empty-p em)))
               (format "[%s](mailto:%s)" txt em)
             txt)))
        (richTextCustomEmoji (or (my/telega--rt-str rt :alternative_text) ""))
        (richTextMathematicalExpression (or (my/telega--rt-str rt :expression) ""))
        ;; underline/mention/hashtag/cashtag/bot-command/phone/datetime/
        ;; anchor/... — no portable markdown marker, emit text.
        (t
         (let ((sub (plist-get rt :text)))
           (cond ((stringp sub) sub)
                 (sub (my/telega--rich-rt->md sub))
                 (t (or (my/telega--rt-str rt :alternative_text)
                        (my/telega--rt-str rt :expression) ""))))))))))

(defun my/telega--rich-blocks->md (blocks)
  "Serialize sequence BLOCKS to markdown, one blank line between blocks."
  (string-join
   (seq-remove #'string-empty-p
               (seq-map (lambda (b) (string-trim (or (my/telega--rich-pb->md b) "")))
                        blocks))
   "\n\n"))

;;;; PageBlock (block) → markdown

(defun my/telega--md-heading (level rt)
  "Markdown heading string for RichText RT at LEVEL (1-6), bold-faced."
  (my/telega--md-face
   (concat (make-string (max 1 (min 6 level)) ?#) " " (my/telega--rich-rt->md rt))
   'bold))

(defun my/telega--rich-table->md (pb)
  "Serialize a pageBlockTable PB to a markdown table string."
  (let* ((rows (seq-map (lambda (row) (append row nil))
                        (plist-get pb :cells)))
         (line (lambda (row)
                 (concat "| "
                         (mapconcat
                          (lambda (cell)
                            (string-trim (my/telega--rich-rt->md (plist-get cell :text))))
                          row " | ")
                         " |"))))
    (when rows
      (let* ((ncol (length (car rows)))
             (sep (concat "| " (string-join (make-list (max 1 ncol) "---") " | ") " |"))
             (lines (mapcar line rows))
             (caption (string-trim (my/telega--rich-rt->md (plist-get pb :caption)))))
        (concat (car lines) "\n" sep
                (when (cdr lines) (concat "\n" (string-join (cdr lines) "\n")))
                (unless (string-empty-p caption) (concat "\n\n" caption)))))))

(defun my/telega--rich-pb->md (pb)
  "Serialize TDLib rich-message PageBlock PB to a markdown source string.
Total over subtypes, and reads the 1.8.64 field names (blockQuote/
listItem/details nested blocks moved to :blocks) with old-name fallback."
  (if (null pb) ""
    (cl-case (my/telega--rt-type pb)
      (pageBlockParagraph (my/telega--rich-rt->md (plist-get pb :text)))
      ;; sectionHeading :size 는 1-6, 1 이 가장 큰 헤딩 → 그대로 markdown 레벨.
      (pageBlockSectionHeading
       (my/telega--md-heading (or (plist-get pb :size) 2) (plist-get pb :text)))
      (pageBlockTitle (my/telega--md-heading 1 (plist-get pb :title)))
      (pageBlockHeader (my/telega--md-heading 1 (plist-get pb :header)))
      (pageBlockSubtitle (my/telega--md-heading 2 (plist-get pb :subtitle)))
      (pageBlockSubheader (my/telega--md-heading 2 (plist-get pb :subheader)))
      (pageBlockKicker (my/telega--rich-rt->md (plist-get pb :kicker)))
      (pageBlockFooter (my/telega--rich-rt->md (plist-get pb :footer)))
      ;; "Thinking..." pending placeholder (rich message streaming)
      (pageBlockThinking (my/telega--rich-rt->md (plist-get pb :text)))
      (pageBlockPreformatted
       (concat "```" (or (my/telega--rt-str pb :language) "") "\n"
               (my/telega--rich-rt->md (plist-get pb :text)) "\n```"))
      (pageBlockMathematicalExpression
       (concat "$$" (or (my/telega--rt-str pb :expression) "") "$$"))
      (pageBlockTable (my/telega--rich-table->md pb))
      (pageBlockDivider "---")
      ;; 컨테이너 — 1.8.64 에서 nested 가 :blocks 로 이름 변경됨.
      (pageBlockBlockQuote
       (let ((body (my/telega--rich-blocks->md
                    (or (plist-get pb :blocks) (plist-get pb :page_blocks))))
             (credit (plist-get pb :credit)))
         (concat
          (mapconcat (lambda (l) (concat "> " l)) (split-string body "\n") "\n")
          (when credit (concat "\n> — " (my/telega--rich-rt->md credit))))))
      (pageBlockList
       (mapconcat #'my/telega--rich-pb->md (append (plist-get pb :items) nil) "\n"))
      (pageBlockListItem
       (let ((label (cond ((plist-get pb :has_checkbox)
                           (if (eq (plist-get pb :is_checked) t) "- [x]" "- [ ]"))
                          ((my/telega--rt-str pb :label)
                           (concat (my/telega--rt-str pb :label) "."))
                          (t "-")))
             (body (my/telega--rich-blocks->md
                    (or (plist-get pb :blocks) (plist-get pb :page_blocks)))))
         (concat label " " (string-trim body))))
      (pageBlockDetails
       (let ((header (plist-get pb :header))
             (body (my/telega--rich-blocks->md
                    (or (plist-get pb :blocks) (plist-get pb :page_blocks)))))
         (concat (when header (concat "**" (my/telega--rich-rt->md header) "**\n\n"))
                 body)))
      (pageBlockCover (my/telega--rich-pb->md (plist-get pb :cover)))
      (pageBlockAnchor "")
      ;; media / unknown — markdown 텍스트로 담을 수 없으니 caption 이나 타입 태그.
      (t
       (let ((rt (plist-get pb :text))
             (cap (plist-get pb :caption)))
         (cond (rt (my/telega--rich-rt->md rt))
               (cap (my/telega--rich-rt->md (plist-get cap :text)))
               (t (format "[%s]" (my/telega--rt-type pb)))))))))

(provide 'telega-rich-md)
;;; telega-rich-md.el ends here
