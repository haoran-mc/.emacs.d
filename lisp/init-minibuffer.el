;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; enhance minibuffer
(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-sort-function nil))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

;; show description on minibuffer
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-o"     . embark-act)
              ("C-c C-c" . embark-export)
              ("C-c C-o" . embark-collect))
  :custom
  (prefix-help-command 'embark-prefix-help-command))

(use-package consult
  :ensure t
  :bind (([remap imenu]                  . consult-imenu)
         ([remap goto-line]              . consult-goto-line)
         ([remap bookmark-jump]          . consult-bookmark)
         ([remap evil-show-marks]        . consult-mark)
         ([remap recentf-open-files]     . consult-recent-file)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap isearch-forward]        . consult-line)
         ([remap projectile-ripgrep]     . consult-ripgrep))
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))

  (defun +consult-directory-externally (file)
    "Open FILE externally using the default application of the system."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
	         (fboundp 'w32-shell-execute))
        (shell-command-to-string
         (encode-coding-string
          (replace-regexp-in-string
           "/" "\\\\" (format "explorer.exe %s"
                              (file-name-directory
                               (expand-file-name file)))) 'gbk))
      (call-process (pcase system-type
		              ('darwin "open")
		              ('cygwin "cygstart")
		              (_ "xdg-open"))
		            nil 0 nil
		            (file-name-directory (expand-file-name file)))))
  (with-eval-after-load 'embark
    (define-key embark-file-map (kbd "E") #'+consult-directory-externally))

  (defun +open-current-directory ()
    "Open current FILE directory externally using the default application
of the system."
    (interactive)
    (+consult-directory-externally default-directory))
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after embark consult)

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
