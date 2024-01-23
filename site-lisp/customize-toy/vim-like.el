;;; vim-like.el ---vimlike functions                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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


;; https://qiita.com/hadashiA/items/5591e4c4c0bfdc558641

(defvar vimlike-f-recent-char nil)
(defvar vimlike-f-recent-func nil)

(defun vimlike-f (char)
  "search to forward char into current line and move point (vim 'f' command)"
  (interactive "cSearch to forward char: ")
  (when (= (char-after (point)) char)
    (forward-char))
  (search-forward (char-to-string char) (point-at-eol) nil 1)
  ;; (migemo-forward (char-to-string char) (point-at-eol) t 1)
  (backward-char)
  (setq vimlike-f-recent-search-char char
        vimlike-f-recent-search-func 'vimlike-f))

(defun vimlike-F (char)
  "search to forward char into current line and move point. (vim 'F' command)"
  (interactive "cSearch to backward char: ")
  (search-backward (char-to-string char) (point-at-bol) nil 1)
  ;; (migemo-backward (char-to-string char) (point-at-bol) t 1)
  (setq vimlike-f-recent-search-char char
        vimlike-f-recent-search-func 'vimlike-F))

(defun vimlike-semicolon ()
  "search repeat recent vimlike 'f' or 'F' search char (vim ';' command)"
  (interactive)
  (if (and vimlike-f-recent-search-char
             vimlike-f-recent-search-func)
      (funcall vimlike-f-recent-search-func vimlike-f-recent-search-char)
    (message "Empty recent search char.")))

(provide 'vim-like)
;;; vim-like.el ends here
