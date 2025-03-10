;;; lang-html.el --- use web-mode in html,jsp,vue... files  -*- lexical-binding: t; -*-

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
;; html: npm install -g emmet-ls
;;       npm i -g vscode-langservers-extracted

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-c C-n   jump
;; C-c C-f   folding
;; C-c C-s   snippet
;; C-c C-w   whitespaces
;; C-c C-i   indent
;; C-c C-e r renmae
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Require
;;
(require 'web-mode)

;;; Code:




(provide 'lang-html)
;;; lang-html.el ends here
