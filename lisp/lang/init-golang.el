;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :custom
  (gofmt-command "goimports"))

;; (use-package auto-complete)
;; (use-package go-autocomplete
;;   :ensure t
;;   :config
;;   (require 'auto-complete-config)
;;   (ac-config-default)
;;   )

(use-package company-go
  :init
  (progn
    (setq company-go-show-annotation t)
    (setq company-tooltip-limit 20)                      ; bigger popup window
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode)))
    )
  )

(use-package go-eldoc
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ))

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode))

;; go get -u -v golang.org/x/tools/cmd/...
;; go get -u -v github.com/rogpeppe/godef
;; go get -u -v golang.org/x/tools/cmd/goimports
;; go get -u -v golang.org/x/tools/gopls
;; go get -u -v github.com/mdempsky/gocode

(provide 'init-golang)
;;; init-golang.el ends here
