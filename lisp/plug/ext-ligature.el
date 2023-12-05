;;; ext-ligature.el --- maps ordinary graphemes to fancy ligatures  -*- lexical-binding: t; -*-

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


(add-hook 'org-mode-hook 'my-org-ligature-mode-hook)

(defun my-org-ligature-mode-hook ()
  (setq prettify-symbols-alist
        '(("lambda"           . ?λ)
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
          ("~/haoran/no/org/export/images" . ?B)))
  (prettify-symbols-mode))


(add-hook 'lisp-mode-hook 'my-lisp-ligature-mode-hook)

(defun my-lisp-ligature-mode-hook ()
  (setq prettify-symbols-alist
        '(("lambda"           . ?λ)
          ("\\pagebreak"      . 128204)
          ("#+tblfm:"         . 8756) ;; ∴
          ("->"               . 8594) ;; →
          ("=>"               . 8658) ;; ⇒
          ))
  (prettify-symbols-mode))


(provide 'ext-ligature)
;;; ext-ligature.el ends here
