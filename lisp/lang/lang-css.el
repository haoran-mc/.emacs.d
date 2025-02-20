;;; lang-css.el --- init for css-mode                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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

;;; Require:
;;
;; css:  npm install -g emmet-ls
(require 'css-mode)

;;; Code:

(dolist (hook (list
               'css-mode-hook))
  (add-hook hook #'(lambda ()
                     (require 'rainbow-mode)
                     (rainbow-mode))))

(provide 'lang-css)
;;; lang-css.el ends here
