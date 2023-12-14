;;; init-symbol-overlay.el --- init for symbol-overlay.el  -*- lexical-binding: t; -*-

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

;;; Require
(require 'symbol-overlay)

;;; Code:

(lazy-load-set-keys
 '(
   ("s" . symbol-overlay-put)
   ("n" . symbol-overlay-jump-next)
   ("p" . symbol-overlay-jump-prev)
   ("w" . symbol-overlay-save-symbol)
   ("t" . symbol-overlay-toggle-in-scope)
   ("e" . symbol-overlay-echo-mark)
   ("d" . symbol-overlay-jump-to-definition)
   ("S" . symbol-overlay-isearch-literally)
   ("r" . symbol-overlay-rename)
   ("R" . symbol-overlay-query-replace)
   ("q" . symbol-overlay-remove-all)
   ("<" . symbol-overlay-jump-first)
   (">" . symbol-overlay-jump-last)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)
   )
 symbol-overlay-map)


(provide 'init-symbol-overlay)
;;; init-symbol-overlay.el ends here
