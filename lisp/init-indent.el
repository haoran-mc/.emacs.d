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
;; 在各个语言中设置？

;;; Code:
(setq-default indent-tabs-mode nil) ;; 使用空格缩进
(setq-default tab-width 4) ;; 制表符宽度为 4 个空格


(dolist (hook (list
               'web-mode-hook
               'js-mode-hook
               'typescript-mode-hook
               'json-mode-hook))   ;; js-indent-level 2
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (setq-local javascript-indent-level 2)
                     (setq-local js-indent-level 2)
                     (setq-local js2-basic-offset 2)

                     (setq-local web-mode-attr-indent-offset 2)
                     (setq-local web-mode-attr-value-indent-offset 2)
                     (setq-local web-mode-code-indent-offset 2)
                     (setq-local web-mode-css-indent-offset 2)
                     (setq-local web-mode-markup-indent-offset 2)
                     (setq-local web-mode-sql-indent-offset 2)

                     (setq-local css-indent-offset 2)
                     (setq-local typescript-indent-level 2))))

;; c, c++
(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (setq-local c-basic-offset 4))))

;; langs
(add-hook 'yaml-mode-hook #'(lambda () (yaml-indent-offset 2)))


(provide 'init-indent)
;;; init-indent.el ends here
