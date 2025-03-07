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
(define-minor-mode my-plain-mode
  "A custom plain mode with disabled indentation."
  :lighter " my-plain"
  (if my-plain-mode (setq-local indent-line-function #'ignore) (kill-local-variable 'indent-line-function)))


(setq org-src-fontify-natively t ;; syntax code on org mode
      org-src-tab-acts-natively t ;; 使用代码块自己的缩进，而不是 org-mode 的缩进
      org-src-preserve-indentation nil ;; used with the sentence above
      org-edit-src-content-indentation 2 ;; src content indent 2
      org-confirm-babel-evaluate t ;; 执行代码块前是否确认
      org-src-window-setup 'current-window ;; 当编辑代码块时，在当前窗口显示
      org-src-lang-modes '(("emacs-lisp" . emacs-lisp) ;; 指定代码块的 major-mode
                           ("C"          . c) ;; 不可以删除这里，使用 C-c ' 编辑代码块时用得到，否则 emacs 不知道该使用哪个 major-mode
                           ("C++"        . c++)
                           ("cpp"        . c++)
                           ("python"     . python)
                           ("bash"       . sh)
                           ("shell"      . sh)
                           ("plantuml"   . plantuml)
                           ("ocaml"      . tuareg)
                           ("dot"        . fundamental)

                           ;; my no indent minor mode, TODO but no htmlize
                           ("sql"      . my-plain)
                           ("diff"     . my-plain)))

(org-babel-do-load-languages 'org-babel-load-languages ;; 哪些代码块可以在 org 中运行
                             '((emacs-lisp . t)
                               (plantuml   . t)))

(define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)

;; (add-hook org-babel-after-execute-hook 'org-redisplay-inline-images)


(provide 'init-org-src)
;;; init-org-src.el ends here
