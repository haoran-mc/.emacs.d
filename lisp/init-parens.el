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


;; highlight-parenthesses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'highlight-parentheses)
;; (setq hl-paren-colors '("Orange" "DeepSkyBlue" "Green"))
;; (add-hook 'find-file-hook 'highlight-parentheses-mode t) ;; 增强的括号高亮


;; elec-pair ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete parens
(require 'elec-pair)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
      electric-pair-delete-adjacent-pairs t)

;; paredit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'paredit-mode)


(dolist (mode-hook '(emacs-lisp-mode-hook
                     c-mode-hook c++-mode-hook cc-mode-hook
                     go-mode-hook
                     python-mode-hook
                     json-mode-hook
                     rust-mode-hook))
  (add-hook mode-hook 'electric-pair-local-mode))

(provide 'init-parens)
;;; init-parens.el ends here
