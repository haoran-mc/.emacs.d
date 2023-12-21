;;; init-vundo.el ---visualize undo                  -*- lexical-binding: t; -*-

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




(require 'vundo)


(lazy-load-set-keys
 '(
   ("l" . vundo-forward)
   ("h" . vundo-backward)
   ("n" . vundo-next)
   ("p" . vundo-previous)
   ("j" . vundo-stem-root)
   ("k" . vundo-stem-end)
   ("," . vundo-goto-last-saved)
   ("q" . vundo-quit)
   ("C-g" . vundo-quit)
   ("f" . vundo-confirm)
   ("C-m" . vundo-confirm)
   ("i" . vundo--inspect)
   ("d" . vundo--debug)
   )
 vundo-mode-map)

(provide 'init-vundo)
;;; init-vundo.el ends here
