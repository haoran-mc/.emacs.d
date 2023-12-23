;;; init-which-key.el --- prompt keys                 -*- lexical-binding: t; -*-

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



;; Tips for next keystroke
(require 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(which-key-add-key-based-replacements
  "C-c @"   "hideshow"
  "C-x a"   "abbrev"
  "C-c f"   "file"
  "C-c e"   "eshell"
  "C-c i"   "insert"
  "C-c n"   "narrow"
  "C-c u"   "user"
  "C-c t"   "hl-todo"
  "C-c y"   "yasnippet"
  "C-c C-v" "babel"
  "C-x n"   "narrow"
  "C-x t"   "tab")
(which-key-add-major-mode-key-based-replacements 'markdown-mode
  "C-c m" "markdown")

(setq which-key-idle-delay 0.5
      which-key-add-column-padding 1)


(provide 'init-which-key)
;;; init-which-key.el ends here
