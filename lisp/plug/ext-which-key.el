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
    "C-c @"   "hideshow"
    "C-x a"   "abbrev"
    "C-x n"   "narrow"
    "C-x t"   "tab"
    "C-c e"   "eshell"
    "C-c i"   "insert"
    "C-c u"   "user"
    "C-c t"   "hl-todo"
    "C-c y"   "yasnippet"
    "C-c C-v" "babel")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c m" "markdown")
  :custom
  (which-key-idle-delay 0.5)
  (which-key-add-column-padding 1))


(provide 'ext-which-key)
;;; ext-which-key.el ends here
