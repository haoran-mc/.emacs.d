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


(setq haoran/font-weight "normal")

(cond ((eq system-type 'gnu/linux)
       (setq haoran/home-directory "/home/haoran")
       (setq haoran/test-packages-dir "/home/haoran/Documents/emacs/local-packages"))
      ((eq system-type 'darwin)
       (setq haoran/home-directory "/Users/haoran")
       (setq haoran/test-packages-dir "/Users/haoran/Documents/emacs/local-packages")))


(provide 'init-variables)
;;; init-variables.el ends here
