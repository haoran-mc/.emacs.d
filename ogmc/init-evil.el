;;----------------------------------------------------------------evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ff" 'find-file
  "fr" 'recentf-open-files
  "bb" 'switch-to-buffer
  "bk" 'kill-buffer
  "bq" 'save-buffer-kill-terminal
  "bs" 'create-scratch-buffer
  "be" (lambda() (interactive) (find-file "~/haoran/editing/Programming/elisp-Foundation.el"))
  "pf" 'counsel-git
  "ps" 'helm-do-ag-project-root
  ;;------------------------------
  ":" 'counsel-M-x
  "d" 'dired
  "a" (lambda() (interactive) (dired "~/haoran/work/orange/"))
  "T" (lambda() (interactive) (dired "~/haoran/editing/Markdown/"))
  "tw" (lambda() (interactive) (find-file "~/haoran/editing/Programming/+wiki-list.org") (sp-push-position-to-ring))
  "0" 'delete-window
  "1" 'delete-other-windows
  "q" 'delete-window
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right
  "`l" (lambda() (interactive) (split-window-right) (windmove-right))
  "`j" (lambda() (interactive) (split-window-below) (windmove-down))
  "rc" (lambda() (interactive) (dired "~/.emacs.d/ogmc/"))
  "go" 'evil-tabs-mode
  "xx" 'exchange-point-and-mark
  "rb" 'bookmark-jump-other-window
  "rm" 'bookmark-set
  "rl" 'bookmark-bmenu-list
  "rr" (lambda() (interactive) (find-file "~/.emacs.d/Bin/bookmarks"))
  "xns" 'org-narrow-to-subtree
  "xnw" 'widen
  "cf" 'yafolding-toggle-element
  "z" 'evil-emacs-state)

;;----------------------------------------------------------------evil
(evil-mode t);;需要先global-evil-leader-mode
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;;----------------------------------------------------------------evil-nerd-commenter
(define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(evilnc-default-hotkeys)
(setq evil-want-C-u-scroll t)

;;evil-tabs
(global-evil-tabs-mode -1)
(setq elscreen-display-screen-number nil)
(setq elscreen-display-tab nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)

;;evil-search-highlight-parsist
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "RET" 'evil-search-highlight-persist-remove-all)


;;----------------------------------------------------------------
(provide 'init-evil)
