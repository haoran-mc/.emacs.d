;;; init-parens.el --- parentheses                   -*- lexical-binding: t; -*-

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

;; paren ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight parenthesises
;; (dolist (mode-hook '(python-mode-hook
;;                      emacs-lisp-mode-hook
;;                      go-mode-hook))
;;   (add-hook mode-hook show-paren-mode)) ;; 显示括号匹配

;; (setq show-paren-style 'parentheses    ;; 括号匹配显示但不是烦人的跳到另一个括号。
;;       blink-matching-paren nil         ;; 当插入右括号时不显示匹配的左括号
;;       show-paren-when-point-inside-paren t
;;       show-paren-when-point-in-periphery t)


;; highlight-parenthesses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight-parentheses)
(setq hl-paren-colors '("Orange" "DeepSkyBlue" "Green"))
;; (add-hook 'find-file-hook 'highlight-parentheses-mode t) ;; 增强的括号高亮


;; elec-pair ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete parens
(require 'elec-pair)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)



(dolist (mode-hook '(cc-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook
                     python-mode-hook
                     json-mode-hook))
  (add-hook mode-hook 'electric-pair-local-mode)
  (add-hook mode-hook 'highlight-parentheses-mode))

(provide 'init-parens)
;;; init-parens.el ends here
