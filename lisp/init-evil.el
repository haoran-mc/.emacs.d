;;; init-evil.org --- Set Emacs as Vim. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
             :ensure t
             :hook (after-init . evil-mode)
             :config
             (setcdr evil-insert-state-map nil)
             :bind (:map evil-motion-state-map
                         ("C-u" . scroll-down-command)
                         :map evil-insert-state-map
                         ([escape] . evil-normal-state)))

(use-package evil-leader
             :ensure t
             :config
             (global-evil-leader-mode)
             (evil-leader/set-leader "<SPC>")
             (evil-leader/set-key
               "rc" (lambda() (interactive) (dired "~/.emacs.d/"))
               "oT" (lambda() (interactive) (dired "~/haoran/Notes/Markdown/"))
               "tw" (lambda() (interactive) (find-file "~/haoran/Notes/Org/Programming/+wiki-list.org") (sp-push-position-to-ring))

               ":"  'counsel-M-x
               "SPC" 'counsel-M-x
               "TAB" 'spacemacs/alternate-buffer
               ;;------------------------------
               "wV" (lambda() (interactive) (split-window-right) (windmove-right))
               "wS" (lambda() (interactive) (split-window-below) (windmove-down))
               "wv" 'split-window-right
               "ws" 'split-window-below
               "wm" 'spacemacs/toggle-maxmize-buffer
               ;;------------------------------
               "ff" 'find-file
               "fr" 'counsel-recentf
               "fj" 'dired-jump
               "ft" 'dired-sidebar-toggle-sidebar
               "fR" 'rename-this-file-and-buffer
               ;;------------------------------
               "bb" 'switch-to-buffer
               "bk" 'kill-buffer
               "bq" 'save-buffer-kill-terminal
               "bs" 'create-scratch-buffer
               "pf" 'counsel-git
               "sp" 'helm-do-ag-project-root
               ;;------------------------------
               "ll" 'comment-line
               ;;------------------------------
               "oy" 'youdao-dictionary-search-at-point+
               ;;------------------------------
               "ad" 'dired
               "go" 'evil-tabs-mode
               "xx" 'exchange-point-and-mark
               "rb" 'bookmark-jump-other-window
               "rm" 'bookmark-set
               "rl" 'bookmark-bmenu-list
               "rr" (lambda() (interactive) (find-file "~/.emacs.d/bookmarks"))
               "xns" 'org-narrow-to-subtree
               "xnw" 'widen
               "z" 'evil-emacs-state

               "o<SPC>" 'smex

               "en" 'new-frame
               "ee" 'other-frame
               "ed" 'delete-frame

               "pw" 'save-and-publish-website
               "pf" 'save-and-publish-file
               "ps" 'save-and-publish-statics
               "pS" 'save-and-publish-rstatics
               "pb" 'preview-current-buffer-in-browser
               "pd" 'delete-org-and-html
               "pD" 'just-delete-relative-html

               "bb" 'ido-switch-buffer
               "bj" 'ibuffer-sidebar-toggle-sidebar
               "bk" 'ido-kill-buffer

               "fo" 'org-open-at-point-and-delete-other-windows
               "fO" 'org-open-at-point

               "gg" 'magit-status

               "hf" 'describe-function
               "hk" 'describe-key
               "hv" 'describe-variable
               "hp" 'describe-package
               ))

(use-package use-package-chords
             :ensure t
             :config
             (key-chord-mode 1)
             (setq-default key-chord-two-keys-delay 0.5)
             (key-chord-define evil-insert-state-map ",," 'evil-normal-state))

(use-package evil-surround
             :ensure t
             :config
             (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
             :ensure t
             :config
             (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
             (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
             (evilnc-default-hotkeys))

(use-package evil-tabs
             :ensure t
             :config
             (global-evil-tabs-mode -1)
             (setq elscreen-display-screen-number nil)
             (setq elscreen-display-tab nil)
             (setq elscreen-tab-display-control nil)
             (setq elscreen-tab-display-kill-screen nil))

(use-package evil-search-highlight-persist
             :ensure t
             :config
             (require 'evil-search-highlight-persist)
             (global-evil-search-highlight-persist t)
             (evil-leader/set-key "RET" 'evil-search-highlight-persist-remove-all))

(provide 'init-evil)
;;; init-evil.el ends here
