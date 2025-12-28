;;; $DOOMDIR/lisp/unicode-config.el --- UI Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Junghan Kim

;; Author: Junghan Kim <junghanacs@gmail.com>
;; URL: https://github.com/junghan0611/doomemacs-config
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; - 겹화살괄호(《 》): 책의 제목이나 신문 이름 등을 나타낼 때 쓰임.[11]
;; - 홑화살괄호(〈 〉): 소제목, 그림이나 노래와 같은 예술 작품의 제목, 상호, 법률, 규정 등을 나타낼 때 쓰임.[12]
;;  『 』(겹낫표),
;;  ｢ ｣(홑낫표) "≪겹화살괄호≫" - https://wikidocs.net/79912
;; 0x002012	‒	FIGURE DASH
;; 0x002013	–	EN DASH
;; 0x002014	—	EM DASH
;; 0x002015	―	QUOTATION DASH
;; 0x002015	―	HORIZONTAL BAR
;; 0x000809	ࠉ SAMARITAN LETTER YUT
;; 0x003179	ㅹ	HANGUL LETTER SSANG BIEUB SUN GYEONG EUM
;; - 0x0000A7	§	SECTION SIGN
;; - 0x0000A9	©	COPYRIGHT SIGN
;; - 0x0000AC	¬	NOT SIGN
;; - 0x0000A1	¡	INVERTED EXCLAMATION MARK
;; - 0x0000A2	¢	CENT SIGN
;; - 0x0000A3	£	POUND SIGN
;; - 0x0000A4	¤	CURRENCY SIGN
;; - 0x0000A5	¥	YEN SIGN
;; - 0x0000A6	¦	BROKEN VERTICAL BAR
;; - 0x0000B5	µ	MICRO SIGN
;; - 0x0000B6	¶	PARAGRAPH SIGN

;;; Code:

;; 2025-04-15 remove "⊨"
;; "⊢" prove, "⊨" entail , "∉" notin
(setq my/unicode-notetaking '( " "
                               "§"
                               "§ section"
                               "¶ paragraph"
                               "†"
                               "‡"
                               "№ num"
                               "↔"
                               "←"
                               "→"
                               "⊢ derive도출"
                               "⊨ entail함축"
                               "∉ notin"
                               "© company"
                               "¬ not"
                               "¤"
                               "µ micro"
                               "¡ excla"
                               "¿ ques"
                               "◊ f"
                               "◊"
                               "⁂"
                               "¥"
                               "¢"
                               "£"
                               ;; ☚ ☛ ⁂ ⸮ ※ † ‡ ¿ ¡ ❦ ◊ № ‽
                               ;; "ㅹ" "ㆅ" "ㅺ"
                               ;; "Ж" ; Greek αβγδεζηθικλμνξοπρςτυφχψω
                               "『겹낫표』"
                               "《겹화살괄호》"
                               "〈홑화살괄호 〉"
                               ;; "≪겹화살괄호≫"
                               "｢홑낫표｣"
                               "― QUOTADASH"
                               ))

(setq my/unicode-notetaking-circle '(
                                     "ⓐ a"
                                     "ⓑ b"
                                     "ⓒ c"
                                     "ⓓ d"
                                     "ⓔ e"
                                     "ⓕ f"
                                     "ⓖ g"
                                     "ⓗ h"
                                     "ⓘ i"
                                     "ⓙ j"
                                     "ⓚ k"
                                     "ⓛ l"
                                     "ⓜ m"
                                     "ⓝ n"
                                     "ⓞ o"
                                     "ⓟ p"
                                     "ⓠ q"
                                     "ⓡ r"
                                     "ⓢ s"
                                     "ⓣ t"
                                     "ⓤ y"
                                     "ⓥ v"
                                     "ⓦ w"
                                     "ⓧ x"
                                     "ⓨ y"
                                     "ⓩ z"
                                     "⓪ 0"
                                     ;; "㉼"
                                     ;; "㉽"
                                     ))

;;;; my/insert-unicode-notetaking
(defun my/insert-unicode-notetaking ()
  "Insert Unicode for NoteTaking."
  (interactive)
  (insert (completing-read "Select unicode: " my/unicode-notetaking)))


;;;; my/insert-unicode-notetaking-circle
(defun my/insert-unicode-notetaking-circle ()
  "Insert Unicode for NoteTaking2"
  (interactive)
  (insert (completing-read "Select unicode: " my/unicode-notetaking-circle)))

;;;; keybindings

(after! vertico
  (evil-define-key '(insert normal) text-mode-map (kbd "M-M") #'my/insert-unicode-notetaking)
  (evil-define-key '(insert normal) text-mode-map (kbd "M-N") #'my/insert-unicode-notetaking-circle)
  (evil-define-key '(insert normal) text-mode-map (kbd "M-m") #'my/insert-white-space)

  (define-key minibuffer-mode-map (kbd "M-M") #'my/insert-unicode-notetaking) ; needed
  (require 'grep)
  (define-key grep-mode-map (kbd "M-M") #'my/insert-unicode-notetaking) ; needed
  (define-key vertico-map (kbd "M-M") #'my/insert-unicode-notetaking)
  (define-key minibuffer-mode-map (kbd "M-N") #'my/insert-unicode-notetaking-circle) ; needed

  (define-key vertico-map (kbd "M-N") #'my/insert-unicode-notetaking-circle)
  )

;;; provide

(provide 'unicode-config)

;;; unicode-config.el ends here
