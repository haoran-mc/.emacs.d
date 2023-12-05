;;; lang-haskell.el --- haskell -*- lexical-binding: t -*-

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

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . haskell-doc-mode))
  :custom
  (haskell-completing-read-function 'completing-read)
  (haskell-process-check-cabal-config-on-load nil)
  (haskell-process-suggest-add-package nil)
  (haskell-process-suggest-hoogle-imports nil)
  (haskell-process-suggest-language-pragmas nil)
  (haskell-process-suggest-overloaded-strings nil)
  (haskell-process-suggest-restart nil))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
