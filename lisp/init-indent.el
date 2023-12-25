;;; init-indent.el --- indent config                 -*- lexical-binding: t; -*-

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun adjust-languages-indent (n)
  (setq-local c-basic-offset n)

  (setq-local coffee-tab-width n)
  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n)

  (setq-local typescript-indent-level n))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'sh-mode-hook
               'haskell-cabal-mode-hook
               'ruby-mode-hook
               'qml-mode-hook
               'scss-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 4)
                     )))

(dolist (hook (list
               'web-mode-hook
               'js-mode-hook
               'typescript-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 2)
                     )))

(provide 'init-indent)
;;; init-indent.el ends here
