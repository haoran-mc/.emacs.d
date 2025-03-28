;;; init-variables.el --- my global variables        -*- lexical-binding: t; -*-

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

(defvar ran--os-linux (eq system-type 'gnu/linux))
(defvar ran--os-mac (eq system-type 'darwin))

(defvar ran--bookmark-file "~/haoran/no/org/bookmark-default.el")
(defvar ran--org-directory "~/haoran/no/org/sync-notes")
(defvar ran--org-agenda-files (list "~/haoran/no/org/sync-notes/org-task"
                                    "~/haoran/no/org/work-agenda/work-todo.org"))


(defvar ran--private-notes "~/haoran/no/org/wiki/index.org")
(defvar ran--public-notes "~/haoran/no/org/site/index.org")
(defvar ran--centre "~/haoran/no/org/sync-notes/centre.org")
(defvar ran--github-page "~/haoran/gr/haoran-mc.github.io")
(defvar ran--trash-dir "~/haoran/tr")




(provide 'init-variables)
;;; init-variables.el ends here
