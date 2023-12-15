;;; init-org-src.el --- org babel                    -*- lexical-binding: t; -*-

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


(setq org-src-fontify-natively t ;; syntax code on org mode
      org-src-tab-acts-natively t ;; 使用代码块自己的缩进，而不是 org-mode 的缩进
      org-src-preserve-indentation nil ;; used with the sentence above
      org-edit-src-content-indentation 2 ;; src content indent 2
      org-confirm-babel-evaluate nil ;; 执行代码块前是否确认
      org-src-window-setup 'current-window ;; 当编辑代码块时，在当前窗口显示
      org-src-lang-modes '(("C"        . c) ;; 配置代码块的 major mode
                           ("C++"      . c++)
                           ("bash"     . sh)
                           ("cpp"      . c++)
                           ("dot"      . graphviz-dot) ;; was `fundamental-mode'
                           ("elisp"    . emacs-lisp)
                           ("ocaml"    . tuareg)
                           ("shell"    . sh)
                           ("go"       . go)
                           ("plantuml" . plantuml)
                           ("sql"      . sql))
      org-babel-load-languages '((C          . t) ;; 哪些代码块可以在 org 中运行
                                 (dot        . t)
                                 (emacs-lisp . t)
                                 (eshell     . t)
                                 (python     . t)
                                 (shell      . t)
                                 (go         . t)
                                 (plantuml   . t)
                                 (jupyter    . t)
                                 (sql        . t)))

(define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)


(add-hook org-babel-after-execute-hook 'org-redisplay-inline-images)

(require 'ob-go)


(provide 'init-org-src)
;;; init-org-src.el ends here
