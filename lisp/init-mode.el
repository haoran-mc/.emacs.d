;;; init-mode.el --- config emacs major mode         -*- lexical-binding: t; -*-

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

;;; ### auto-mode-alist ###
;;; --- 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))


;; 已经在 auto-mode-alist 里的 (后缀 . major-mode) 注册过的，不需要重复添加在这里
;; 或者修改默认的 (后缀 . major-mode)，比如 (.json . json-mode)
(dolist (elt-cons '(("\\.md$" . markdown-mode)
                    ("\\.markdown$" . markdown-mode)
                    ("\\.go$" . go-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.rs$" . rust-mode)

                    ;; front-end language
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.js$" . js-mode)
                    ("\\.js.erb\\'" . js-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.ts$" . typescript-mode)

                    ;; configuration language
                    ("\\.rss$" . xml-mode)
                    ("\\.json$" . json-mode)
                    ("\\.toml$" . toml-mode)
                    ("\\.yaml$" . yaml-mode)
                    ("\\.yml$" . yaml-mode)
                    ("Dockerfile" . (lambda () (require 'dockerfile-mode) (dockerfile-mode)))

                    ;; other
                    ("\\.cpp$" . c++-mode) ;; .cpp 文件为 c++-mode
                    ("\\.h$" . c++-mode)))
  (add-to-alist 'auto-mode-alist elt-cons))


;; 为 major-mode 自动加载相应配置，因为不会使用 keys 加载，而且预加载需要花费长时间
(autoload 'markdown-mode "init-markdown")
(autoload 'go-mode "lang-golang")
(autoload 'lua-mode "lang-lua")
(autoload 'rust-mode "lang-rust")

(autoload 'web-mode "lang-html")
(autoload 'js-mode "lang-javascript")
(autoload 'typescript-mode "typescript-mode")
(autoload 'css-mode "lang-css")

(autoload 'xml-mode "lang-xml") ;; major-mode 是 nxml-mode
(autoload 'json-mode "lang-json")
(autoload 'toml-mode "lang-toml")
(autoload 'yaml-mode "lang-yaml")

(require 'lang-elisp) ;; directly require
(require 'lang-cpp) ;; 文件后缀多，autoload 处理麻烦，直接 load 配置文件（hook 加载真正的配置）
;; (require 'lang-sql) ;; 在 auto-mode-alist 注册过，也就是有内置的 sql-mode，autoload 被覆盖，所以直接加载 lang-sql
(require 'lang-python) ;; 因为有内置的 python-mode，autoload 被覆盖，所以直接加载 lang-python

;; ↑ require 不一定真正加载，因为可以在 lang-*.el 里使用 mode-hook

(add-hook 'prog-mode-hook #'(lambda () (require 'init-ai)))

;; conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for systemd files
;; (add-to-list 'auto-mode-alist ((rx "."
;;                                    (or "automount" "busname" "link" "mount" "netdev" "network"
;;                                        "path" "service" "slice" "socket" "swap" "target" "timer")
;;                                    string-end) . conf-mode))



;; truncate-lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq truncate-lines t        ;; 默认不折行
      word-wrap-by-category t ;; 按照中文折行
      sentence-end-double-space nil) ;; 句子结束标点视为句子的结束，不需要额外的两个空格

(defun my/no-fringe-indicators ()
  (setq
   ;; fringe-indicator 窗口边缘（fringe），折行导致的箭头
   visual-line-fringe-indicators '(nil nil)
   fringe-indicator-alist
   '((truncation . nil) (continuation . nil))))


(dolist (mode-hook '(prog-mode-hook
                     eshell-mode-hook))
  (add-hook mode-hook #'(lambda () (progn
                                (setq truncate-lines t)
                                (my/no-fringe-indicators)))))

(dolist (mode-hook '(org-mode-hook
                     term-mode-hook))
  (add-hook mode-hook #'(lambda () (progn
                                (setq truncate-lines nil) ;; 折行 visual-line-mode
                                (my/no-fringe-indicators)))))



;; encoding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-coding-alist
      (cons '("\\.txt\\'" . chinese-gbk) auto-coding-alist))


(provide 'init-mode)
;;; init-mode.el ends here
