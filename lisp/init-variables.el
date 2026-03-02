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

(defvar ran--bookmark-file "~/haoran/note/org/bookmark-default.el")
(defvar ran--org-directory "~/haoran/note/org/sync-notes")
(defvar ran--org-agenda-files (list "~/haoran/note/org/sync-notes/org-task"
                                    "~/haoran/note/org/work-agenda/work-todo.org"))

(defvar ran--private-notes (expand-file-name "~/haoran/note/org/wiki/index.org"))
(defvar ran--centre        (expand-file-name "~/haoran/note/org/sync-notes/centre.org"))
(defvar ran--github-page   (expand-file-name "~/haoran/gr/haoran-mc.github.io"))
(defvar ran--trash-dir     (expand-file-name "~/haoran/tr"))
(defvar ran--algo-file     (expand-file-name "~/haoran/note/org/wiki/algorithm.org"))
(defvar ran--pandoc-dir    (expand-file-name "~/haoran/note/org/export/pandoc-docx"))

;; site
(defvar ran--public-notes  (expand-file-name "~/haoran/note/org/site/index.org"))
(defvar ran--site-org-dir  (expand-file-name "~/haoran/note/org/site"))
(defvar ran--site-html-dir (expand-file-name "~/haoran/gr/haoran-mc.github.io"))
(defvar ran--wiki-org-dir  (expand-file-name "~/haoran/note/org/wiki"))
(defvar ran--my-export-dir (expand-file-name "~/haoran/note/org/export/org-preview"))

;; work
(defvar ran--work-journay-file (expand-file-name "~/haoran/note/org/work-agenda/work-journay.org"))
(defvar ran--work-todo-file    (expand-file-name "~/haoran/note/org/work-agenda/work-todo.org"))


(provide 'init-variables)
;;; init-variables.el ends here
