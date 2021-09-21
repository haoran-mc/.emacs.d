;;; init-more.el --- Configurations with extra packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  ;; :bind (:map company-active-map
  ;;             ("M-n" . nil)
  ;;             ("M-p" . nil)
  ;;             ("C-n" . company-select-next)
  ;;             ("C-p" . company-select-previous))
  :config
  (progn
    (setq-default company-idle-delay 0.08)
    (setq-default company-minimum-prefix-length 2)
    )
  )

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 5
        scroll-conservatively 9999
        scroll-step 1))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                 ;; personal snippets
          ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
          ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
          )))

(use-package autoinsert
  :config
  (progn
    (setq auto-insert-query nil)
    (setq auto-insert-directory (locate-user-emacs-file "templates"))
    (add-hook 'find-file-hook 'auto-insert)
    (auto-insert-mode 1)
    (define-auto-insert "\\.el$" [ "~/.emacs.d/snippets/defaults-elisp.el" autoinsert-yas-expand ])
    (define-auto-insert "\\.org$" ["~/.emacs.d/snippets/default-org.el" autoinsert-yas-expand])
    )
  )

(use-package swiper
  :ensure t
  :init
  (defun sanityinc/swiper-at-point (sym)
    "@purcell
    Use `swiper' to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  :bind (("M-s" . sanityinc/swiper-at-point)
	     ("C-s" . swiper)))

(use-package counsel
  :ensure t)


(use-package helm-ag
  :ensure t)

(use-package youdao-dictionary
  :ensure t)

(provide 'init-plug)
;;; init-plug.el ends here
