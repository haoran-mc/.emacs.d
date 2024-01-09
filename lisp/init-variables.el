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

(defvar haoran--os-linux (eq system-type 'gnu/linux))
(defvar haoran--os-mac (eq system-type 'darwin))

(defvar haoran--bookmark-file "~/haoran/no/org/bookmark-default.el")
(defvar haoran--org-directory "~/haoran/no/org/org-directory")
(defvar haoran--org-agenda-files (list "~/haoran/no/org/org-directory/tasks"
                                       "~/haoran/no/org/org-directory/agenda"
                                       "~/haoran/no/org/org-directory/work/journay.org"
                                       "~/haoran/no/org/org-directory/work/todo.org"))

(defvar haoran--font-weight "normal")

(defvar haoran--private-notes "~/haoran/no/org/wiki/index.org")
(defvar haoran--public-notes "~/haoran/no/org/site/index.org")
(defvar haoran--github-page "~/haoran/gr/haoran-mc.github.io")

(cond (haoran--os-linux
       (defvar haoran--home-directory "/home/haoran")
       (defvar haoran--font-size 11))
      (haoran--os-mac
       (defvar haoran--home-directory "/Users/haoran")
       (defvar haoran--font-size 13)))


(provide 'init-variables)
;;; init-variables.el ends here
