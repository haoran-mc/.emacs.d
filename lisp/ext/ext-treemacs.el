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
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color
   ((t (:background ,(face-foreground 'font-lock-comment-face nil t)))))
  :bind (:map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action)
         ("o 1"       . treemacs-visit-node-first-window))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30)
  (treemacs-define-RET-action 'file-node-closed
                              #'(lambda(&optional arg)
                                  (treemacs-visit-node-in-most-recently-used-window)
                                  (delete-window (treemacs-get-local-window))))
  (defun treemacs-visit-node-first-window(&optional arg)
    (interactive "P")
    (treemacs-visit-node-no-split)
    (delete-window (treemacs-get-local-window)))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-evil
    :ensure t
    :after evil)

  (use-package treemacs-projectile
    :ensure t
    :after projectile
    :bind (:map projectile-command-map
                ("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :ensure t
    :after magit
    :commands treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-persp
    :ensure t
    :after persp-mode
    :demand t
    :functions treemacs-set-scope-type
    :config (treemacs-set-scope-type 'Perspectives)))

(provide 'ext-treemacs)
;;; ext-treemacs.el ends here
