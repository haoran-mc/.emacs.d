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


(setq-default indent-tabs-mode nil) ;; 使用空格缩进
(setq-default tab-width 4) ;; 制表符宽度为 4 个空格


;; 定义一个函数统一设置
(defun adjust-languages-indent (n)
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
               'web-mode-hook
               'js-mode-hook
               'typescript-mode-hook
               'json-mode-hook))   ;; js-indent-level 2
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 2))))


;; 配置简单，直接定义就好
(setq yaml-indent-offset 2)
(setq-default c-basic-offset 4)



(provide 'init-indent)
;;; init-indent.el ends here
