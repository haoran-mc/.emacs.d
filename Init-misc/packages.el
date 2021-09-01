(setq ogmc-misc-packages
      '(
        all-the-icons
        company
        hungry-delete
        smartparens
        )
      )

(use-package all-the-icons)

(use-package company
  :config
  (progn
    (global-company-mode t)
    (setq-default company-idle-delay 0.08)
    (setq-default company-minimum-prefix-length 2)
    )
  )

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode)
  )

(use-package smartparens
  :ensure t
  :config
  (progn
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
    (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
    )
  )
