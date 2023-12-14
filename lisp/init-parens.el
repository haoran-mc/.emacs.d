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
(dolist (mode-hook '(python-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook))
  (add-hook mode-hook show-paren-mode))

(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)


;; elec-pair ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete parens
(dolist (mode-hook '(cc-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook
                     python-mode-hook))
  (add-hook mode-hook 'electric-pair-local-mode))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


(provide 'init-parens)
;;; init-parens.el ends here
