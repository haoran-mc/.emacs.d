;;; init-eldoc.el --- Eldoc configuration            -*- lexical-binding: t; -*-

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

;;; Code:

(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'Info-mode-hook
               'erc-mode-hook
               'org-mode-hook))
  (add-hook hook #'(lambda ()
                     (require 'eldoc)
                     (require 'eldoc-extension)
                     (setq eldoc-idle-delay 0.05) ;; 显示一定的延迟，避免快速移动时 minibuffer 频繁闪烁
                     (setq eldoc-argument-case 'eldoc-argument-list) ;; 高亮函数参数
                     (turn-on-eldoc-mode))))

(provide 'init-eldoc)
;;; init-eldoc.el ends here
