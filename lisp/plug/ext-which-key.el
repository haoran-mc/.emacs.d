;;; ext-which-key.el --- prompt keys                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:



;; Tips for next keystroke
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x t" "tab"
    "C-c !" "flycheck"
    "C-c @" "hideshow"
    "C-c c" "citre"
    "C-c e" "eshell"
    "C-c t" "hl-todo"
    "C-c y" "yasnippet")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c m" "markdown")
  (which-key-add-major-mode-key-based-replacements 'go-mode
    "C-c t" "hl-todo/go-tag/test")
  :custom
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 1))


(provide 'ext-which-key)
;;; ext-which-key.el ends here