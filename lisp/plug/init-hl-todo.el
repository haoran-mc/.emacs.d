;;; init-hl-todo.el --- Highlight TODO -*- lexical-binding: t; -*-

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

;; Highlight TODO
(require 'hl-todo)

(global-hl-todo-mode)

(define-key hl-todo-mode-map (kbd "C-c t i") 'hl-todo-insert)
(define-key hl-todo-mode-map (kbd "C-c t o") 'hl-todo-occur)

(add-to-list 'hl-todo-keyword-faces '("LOCAL-PACKAGES" . "#FF0000"))
(add-to-list 'hl-todo-keyword-faces '("EXTERNAL-TOOLS" . "#FF0000"))
(add-to-list 'hl-todo-keyword-faces '("USER-DIRECTORY" . "#FF0000"))
(add-to-list 'hl-todo-keyword-faces '("CUSTOM-COLOURS" . "#FF0000"))
(add-to-list 'hl-todo-keyword-faces '("IMPORTANT"      . "#FF0000"))
(add-to-list 'hl-todo-keyword-faces '("BUILT-IN"       . "#C066DB"))


(provide 'init-hl-todo)
;;; init-hl-todo.el ends here
