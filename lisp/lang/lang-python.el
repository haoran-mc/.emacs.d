;;; lang-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . python-mode-delete-trailing-whitespace))
  :init
  (defun python-mode-delete-trailing-whitespace ()
    "Delete trailing whitespace before saving file."
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  :custom
  (python-indent-guess-indent-offset-verbose nil))

;; Live Coding in Python
(use-package live-py-mode
  :ensure t
  :defer t)

;; python -m venv ENV_DIR
(use-package pyvenv
  :ensure t
  :commands pyvenv-deactivate pyvenv-deactivate)


(provide 'lang-python)
;;; lang-python.el ends here
