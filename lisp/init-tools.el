;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


;; Try out emacs package without installing
(use-package try
  :ensure t
  :commands try try-and-refresh)

;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path (append exec-path '("/root/go/bin")))
  (exec-path-from-shell-copy-envs '("PYTHONPATH")))

;; The blazing grep tool
;;
;; Press C-c s to search
(use-package rg
  :ensure t
  :hook (after-init . rg-enable-default-bindings))

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))
  ;; :custom
  ;; (hungry-delete-join-reluctantly t)
  )

;; GC optimization
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;; Translator for Emacs
;; M-x fanyi-dwim{,2}, that's all.
(use-package fanyi
  :ensure t
  :commands fanyi-dwim fanyi-dwim2)

;; Snippest
(use-package yasnippet
  :ensure t
  :bind (("C-c y s" . yas-insert-snippet)
         ("C-c y n" . yas-new-snippet)
         ("C-c y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (use-package yasnippet-snippets
    :ensure t))

;; Auto-insert templates when create file
(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (define-auto-insert "\\.org$"
    ["~/.emacs.d/templates/default-org.org" autoinsert-yas-expand])
  (define-auto-insert "\\.html$"
    ["~/.emacs.d/templates/default-html.html" autoinsert-yas-expand]))

(use-package ace-pinyin
  :ensure t
  :custom
  (ace-pinyin-global-mode +1)
  (ace-pinyin-treat-word-as-char nil)) ;; keep <leader>fw original

(require 'ext-treemacs)
(require 'ext-hl-todo)
(require 'ext-projectile)
(require 'ext-which-key)
(require 'ext-ligature)
;; (require 'ext-reader)
;; (require 'ext-eaf)

(provide 'init-tools)
;;; init-tools.el ends here
