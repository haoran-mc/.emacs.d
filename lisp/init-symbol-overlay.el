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

;; 置空
(setq symbol-overlay-map (make-sparse-keymap))

(lazy-load-set-keys
 '(("n" . symbol-overlay-jump-next)
   ("p" . symbol-overlay-jump-prev)
   ("r" . symbol-overlay-rename)
   ("R" . symbol-overlay-query-replace)
   ("<" . symbol-overlay-jump-first)
   (">" . symbol-overlay-jump-last)
   ;; ("H" . symbol-overlay-map-help)
   ;; ("s" . symbol-overlay-put)
   ;; ("w" . symbol-overlay-save-symbol)
   ;; ("t" . symbol-overlay-toggle-in-scope)
   ;; ("e" . symbol-overlay-echo-mark)
   ;; ("d" . symbol-overlay-jump-to-definition)
   ;; ("S" . symbol-overlay-isearch-literally)
   ;; ("q" . symbol-overlay-remove-all)
   ;; ("M-n" . symbol-overlay-switch-forward)
   ;; ("M-p" . symbol-overlay-switch-backward)
   )
 symbol-overlay-map)


;; 因为打算不论什么主题都使用这些 colors，所以就和这个插件的配置放在一块
(set-face-attribute 'symbol-overlay-face-1 nil :foreground "black" :background "#A4E57E")
(set-face-attribute 'symbol-overlay-face-2 nil :foreground "black" :background "#8CCBEA")
(set-face-attribute 'symbol-overlay-face-3 nil :foreground "black" :background "#FFDB72")
(set-face-attribute 'symbol-overlay-face-4 nil :foreground "black" :background "#FF7272")
(set-face-attribute 'symbol-overlay-face-5 nil :foreground "black" :background "#FFB3FF")
(set-face-attribute 'symbol-overlay-face-6 nil :foreground "black" :background "#9999FF")
(set-face-attribute 'symbol-overlay-face-7 nil :foreground "black" :background "#1E90FF")
(set-face-attribute 'symbol-overlay-face-8 nil :foreground "black" :background "#40E0D0")


(provide 'init-symbol-overlay)
;;; init-symbol-overlay.el ends here
