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

;; M-x list-character-sets
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
        ;; Private Use Area 57600
        (+correct-symbol-bounds-with-TAB (+create-ligature-list ligature-chars #Xe100))))


(defun +set-font-ligature-chars ()
  "Add fira code ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append +font-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))


;; affect monospace font
;; (add-hook 'prog-mode-hook '+set-font-ligature-chars)
;; (add-hook 'sql-mode-hook '+set-font-ligature-chars)

;; no use font-ligatures in org-mode
;; (with-eval-after-load 'org
;;   (add-hook 'org-mode-hook '+set-font-ligature-chars))


;; org-mode-hook ligatures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun +customized-org-ligature-chars ()
  (let ((fancy-chars '(("lambda"           . ?λ)
                       ("#+PROPERTY:"      . ?⚙)
                       ("#+LATEX_CLASS:"   . ?C) ;; 🄲
                       ("#+LATEX_HEADER:"  . ?⇥)
                       ("#+BEAMER_HEADER:" . ?↔)
                       ("#+CAPTION:"       . ?☰)
                       ("#+HEADER:"        . ?›)
                       ("#+begin_quote"    . ?»)
                       ("#+end_quote"      . ?«)
                       ("#+begin_export"   . ?↠)
                       ("#+end_export"     . ?↞)
                       ("#+RESULTS:"       . ?⚑)
                       ("#+begin_results"  . ?⋯)
                       ("#+end_results"    . 8943)
                       ("#+begin_src"      . ?ƒ)
                       ("#+end_src"        . ?ƒ)
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

(with-eval-after-load 'org
  (add-hook 'org-mode-hook '+customized-org-ligature-chars))


;; emacs-lisp-mode-hook ligatures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun +customized-elisp-ligature-chars ()
  (let ((fancy-chars '(("lambda"           . ?λ)
                       ("\\pagebreak"      . 128204)
                       ("#+tblfm:"         . ?∴)
                       ("->"               . ?→)
                       ("=>"               . ?⇒))))
    (setq prettify-symbols-alist
          (append fancy-chars prettify-symbols-alist)))
  (prettify-symbols-mode))

(add-hook 'emacs-lisp-mode-hook '+customized-elisp-ligature-chars)


(provide 'init-ligature)
;;; init-ligature.el ends here
