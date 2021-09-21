;;; init-more.el --- Configurations with extra packages. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :diminish " C"
  :config
  (setq-default company-idle-delay 0.08)
  (setq-default company-minimum-prefix-length 2))

(use-package hungry-delete
  :ensure t
  :hook (after-init . global-hungry-delete-mode)
  :init
  (global-hungry-delete-mode)
  )

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
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        )))

(use-package org-bullets
  :ensure t
  :hook (org-mode-hook . #'(lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("◉" "☯" "○" "✿" "❀" "◇"))
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

(provide 'init-plug)
;;; init-plug.el ends here
