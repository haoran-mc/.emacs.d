;;; lang-latex.el --- for latex -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:

;; 鼠标放在 LaTeX 代码上，可编辑状态，离开即显示编译后的公式效果
;; (use-package org-fragtog
;;   :hook (org-mode . org-fragtog-mode))

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup))

(provide 'lang-latex)
;;; init-latex.el ends here
