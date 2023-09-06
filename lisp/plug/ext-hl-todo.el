;;; ext-hl-todo.el --- Highlight TODO -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t i" . hl-todo-insert)
              ("C-c t o" . hl-todo-occur)
              ("C-c t s" . hl-todo-rgrep))
  :config
  (add-to-list 'hl-todo-keyword-faces '("LOCAL-PACKAGES" . "#FF0000"))
  (add-to-list 'hl-todo-keyword-faces '("EXTERNAL-TOOLS" . "#FF0000"))
  (add-to-list 'hl-todo-keyword-faces '("USER-DIRECTORY" . "#FF0000"))
  (add-to-list 'hl-todo-keyword-faces '("CUSTOM-COLOURS" . "#FF0000")))


(provide 'ext-hl-todo)
;;; ext-hl-todo.el ends here
