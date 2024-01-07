;;; init-ligature.el --- maps ordinary graphemes to fancy ligatures  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ligature is not available as missing HarfBuzz

;;; Code:


(require 'dash)

;; default font ligatures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun +correct-symbol-bounds-with-TAB (pretty-alist)
  "Prepend a TAB character to each symbol in this alist, this way
compose-region called by prettify-symbols-mode will use the correct
width of the symbols instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))


(defun +create-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with codepoints starting
from codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))


(setq +font-ligatures
      (let ((ligature-chars '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                              "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                              "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                              "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                              ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                              "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                              "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                              "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                              ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                              "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                              "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                              "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                              "x" ":" "+" "+" "*")))
        ;; Private Use Area
        (+correct-symbol-bounds-with-TAB (+create-ligature-list ligature-chars #Xe100))))


(defun +set-font-ligature-chars ()
  "Add fira code ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append +font-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))


(add-hook 'prog-mode-hook '+set-font-ligature-chars)
(add-hook 'sql-mode-hook '+set-font-ligature-chars)
;; (with-eval-after-load 'org
;;   (add-hook 'org-mode-hook '+set-font-ligature-chars))


;; hook ligatures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'org
  (add-hook 'org-mode-hook '+customized-org-ligature-chars))


(defun +customized-org-ligature-chars ()
  (let ((fancy-chars '(("lambda"           . ?λ)
                       ("\\pagebreak"      . 128204)
                       ("#+tblfm:"         . 8756) ;; ∴
                       ("->"               . 8594) ;; →
                       ("<-"               . 8592) ;; ←
                       ("=>"               . 8658) ;; ⇒
                       ("<="               . 8656) ;; ⇐
		               ("[ ]"              . 9744)         ; ☐
		               ("[X]"              . 9745)         ; ☑
		               ("[-]"              . 8863)         ; ⊟
                       ("::"               . ?∷)
                       ;; ("#+TITLE:"         . 10162) ;; ➲ ☺ ⊘ ⨀ Τ
                       ;; ("#+AUTHOR:"        . 9998) ;; ✎ ♥
                       ;; ("#+EMAIL:"         . ?﹫)  ;; ␤ ＠ ﹫ ⌂ ⚙
                       ;; ("#+DATE:"          . ?⌨)
                       ;; ("#+DESCRIPTION:"   . ?𝇊) ;;
                       ;; ("#+KEYWORDS:"      . ?)
                       ;; ("#+TAGS:"          . ?)
                       ;; ("#+OPTIONS:"       . ?⌥)
                       ;; ("#+STARTUP:"       . ?⑆)
		               ;; ("#+ATTR_LATEX:"    . ?🄛)
		               ;; ("#+ATTR_HTML:"     . ?🄗)
		               ;; ("#+ATTR_ORG:"      . ?🄞)
                       ("#+BLOCK_LINE: "   . ?━)
                       ("#+PROPERTY:"      . ?⚙)
                       ("#+LATEX_CLASS:"   . ?C) ;; 🄲
                       ("#+LATEX_HEADER:"  . ?⇥)
                       ("#+BEAMER_HEADER:" . ?↔)
                       ("#+CAPTION:"       . ?☰)
                       ("#+HEADER:"        . ?›)
                       ("#+begin_quote"    . 187) ;; »
                       ("#+end_quote"      . 171) ;; «
                       ("#+begin_export"   . ?↠)
                       ("#+end_export"     . ?↞)
                       ("#+RESULTS:"       . ?⚑)
                       ("#+begin_results"  . 8943) ;; ⋯
                       ("#+end_results"    . 8943)
                       ("#+begin_src"      . ?ƒ)
                       ("#+end_src"        . ?ƒ)
                       ;; ("#+begin_example"  . ?∴) ;; ⧉
                       ;; ("#+end_example"    . ?∵)
                       ;; (":PROPERTIES:"     . ?⚙)
                       ;; (":END:"            . ?∎)
                       ("[#A]"             . ?🅰)
                       ("[#B]"             . ?🅱)
                       ("[#C]"             . ?🅲)
                       ("[#D]"             . ?🅳)
                       ("[#E]"             . ?🅴)
                       ("~/haoran/gr/haoran-mc.github.io/images" . ?A)
                       ("~/haoran/no/org/export/images" . ?B))))
    (setq prettify-symbols-alist
          (append fancy-chars prettify-symbols-alist)))
  (prettify-symbols-mode))


(add-hook 'emacs-lisp-mode-hook '+customized-elisp-ligature-chars)

(defun +customized-elisp-ligature-chars ()
  (let ((fancy-chars '(("lambda"           . ?λ)
                       ("\\pagebreak"      . 128204)
                       ("#+tblfm:"         . 8756) ;; ∴
                       ("->"               . 8594) ;; →
                       ("=>"               . 8658) ;; ⇒
                       )))
    (setq prettify-symbols-alist
          (append fancy-chars prettify-symbols-alist)))
  (prettify-symbols-mode))


(provide 'init-ligature)
;;; init-ligature.el ends here
