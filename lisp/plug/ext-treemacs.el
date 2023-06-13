;;; ext-treemacs.el --- file tree -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

;;; Commentary:
;;

;;; Code:


;; A tree layout file explorer
(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :init
  (with-eval-after-load 'treemacs
    (custom-set-faces
     '(treemacs-directory-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
     '(treemacs-file-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
     '(treemacs-git-added-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-conflict-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-deleted-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-ignored-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-modified-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-renamed-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-unmodified-face ((t (:inherit treemacs-file-face))))
     '(treemacs-git-untracked-face ((t (:inherit treemacs-file-face))))
     '(treemacs-root-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
     '(variable-pitch ((t nil)))))
  :bind (:map treemacs-mode-map
              ([mouse-1]   . treemacs-single-click-expand-action)
              ("o 1"       . treemacs-visit-node-first-window))
  :config
  (treemacs-define-RET-action 'file-node-closed
                              #'(lambda(&optional arg)
                                  (treemacs-visit-node-in-most-recently-used-window)
                                  (delete-window (treemacs-get-local-window))))

  (defun treemacs-visit-node-first-window(&optional arg)
    (interactive "P")
    (treemacs-visit-node-no-split)
    (delete-window (treemacs-get-local-window)))

  (use-package treemacs-evil
    :ensure t
    :after evil)

  :custom
  (treemacs-filewatch-mode t)
  (treemacs-git-mode nil)
  (treemacs-follow-mode nil)
  (treemacs-show-cursor t)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-missing-project-action 'remove)
  (treemacs-width 30))

(provide 'ext-treemacs)
;;; ext-treemacs.el ends here
