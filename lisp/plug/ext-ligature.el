;;; ext-ligature.el --- maps ordinary graphemes to fancy ligatures  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

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
          ("=>"               . 8658) ;; ⇒
          ("[ ]"              . ?☐)
          ("[-]"              . ?◼)
          ("[X]"              . ?☑)
          ("::"               . ?∷)
          ("#+TITLE:"         . ?T) ;; ❤ ☺ ⊘ ⨀ Τ
          ("#+AUTHOR:"        . ?♥)
          ("#+EMAIL:"         . ?⌂) ;; ＠ ﹫ ⌂ ⚙
          ("#+DATE:"          . ?⌨)
          ("#+DESCRIPTION:"   . ?━) ;; 𝇊
          ("#+BLOCK_LINE: "   . ?━)
          ("#+PROPERTY:"      . ?⚙)
          ("#+OPTIONS:"       . ?⌥)
          ("#+STARTUP:"       . ?⑆)
          ("#+KEYWORDS:"      . ?)
          ("#+TAGS:"          . ?)
          ("#+LATEX_CLASS:"   . ?C) ;; 🄲
          ("#+LATEX_HEADER:"  . ?⇥)
          ("#+BEAMER_HEADER:" . ?↔)
          ("#+ATTR_LATEX:"    . ?🄛) ;; 🄛
          ("#+ATTR_HTML:"     . ?🄗) ;; 🄗
          ("#+CAPTION:"       . ?☰)
          ("#+HEADER:"        . ?›)
          ("#+begin_quote"    . ?«)
          ("#+end_quote"      . ?»)
          ("#+begin_export"   . ?↠)
          ("#+end_export"     . ?↞)
          ("#+RESULTS:"       . ?⚑)
          (":PROPERTIES:"     . ?⚙)
          (":END:"            . ?∎)
          ("[#A]"             . ?🅰)
          ("[#B]"             . ?🅱)
          ("[#C]"             . ?🅲)
          ("[#D]"             . ?🅳)
          ("[#E]"             . ?🅴)
          ))
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
