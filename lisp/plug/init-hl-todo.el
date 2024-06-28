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

(setq hl-todo-keyword-faces
      '(("LOCAL-PACKAGES" . "#FF0000")
        ("EXTERNAL-TOOLS" . "#FF0000")
        ("USER-DIRECTORY" . "#FF0000")
        ("CUSTOM-COLOURS" . "#FF0000")
        ("IMPORTANT"      . "#FF0000")
        ("DEPRECATED"     . "#FF0000")
        ("BUILT-IN"       . "#C066DB")

        ;; org-todo
        ("TODO"      . "#FF0000") ;; #CC9393
        ("DONE"      . "#50a14f")
        ("CANCELLED" . "#50a14f")
        ("LONG"      . "#D0BF8F")
        ("HOLD"      . "#D0BF8F")
        ("INBOX"     . "#FF0000") ;; deprecated
        ("WORK"      . "#FF0000")))


(provide 'init-hl-todo)
;;; init-hl-todo.el ends here
