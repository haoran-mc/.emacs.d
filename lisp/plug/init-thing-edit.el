;;; init-thing-edit.el --- Extension thing edit      -*- lexical-binding: t; -*-

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

;;; Code:



(require 'one-key)
(require 'thing-edit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Thing-Edit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(one-key-create-menu
 "THING-EDIT"
 '(
   ;; Copy.
   (("w" . "Copy Word") . thing-copy-word)
   (("s" . "Copy Symbol") . thing-copy-symbol)
   (("m" . "Copy Email") . thing-copy-email)
   (("f" . "Copy Filename") . thing-copy-filename)
   (("u" . "Copy URL") . thing-copy-url)
   (("x" . "Copy Sexp") . thing-copy-sexp)
   (("g" . "Copy Page") . thing-copy-page)
   (("t" . "Copy Sentence") . thing-copy-sentence)
   (("o" . "Copy Whitespace") . thing-copy-whitespace)
   (("i" . "Copy List") . thing-copy-list)
   (("c" . "Copy Comment") . thing-copy-comment)
   (("h" . "Copy Function") . thing-copy-defun)
   (("p" . "Copy Parentheses") . thing-copy-parentheses)
   (("l" . "Copy Line") . thing-copy-line)
   (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
   (("e" . "Copy To Line End") . thing-copy-to-line-end)
   ;; Cut.
   (("W" . "Cut Word") . thing-cut-word)
   (("S" . "Cut Symbol") . thing-cut-symbol)
   (("M" . "Cut Email") . thing-cut-email)
   (("F" . "Cut Filename") . thing-cut-filename)
   (("U" . "Cut URL") . thing-cut-url)
   (("X" . "Cut Sexp") . thing-cut-sexp)
   (("G" . "Cut Page") . thing-cut-page)
   (("T" . "Cut Sentence") . thing-cut-sentence)
   (("O" . "Cut Whitespace") . thing-cut-whitespace)
   (("I" . "Cut List") . thing-cut-list)
   (("C" . "Cut Comment") . thing-cut-comment)
   (("H" . "Cut Function") . thing-cut-defun)
   (("P" . "Cut Parentheses") . thing-cut-parentheses)
   (("L" . "Cut Line") . thing-cut-line)
   (("A" . "Cut To Line Begin") . thing-cut-to-line-beginning)
   (("E" . "Cut To Line End") . thing-cut-to-line-end)
   )
 t)


(provide 'init-thing-edit)
;;; init-thing-edit.el ends here
