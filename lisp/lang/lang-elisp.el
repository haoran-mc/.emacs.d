;;; lang-elisp.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-to-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-to-comment))
  :hook ((emacs-lisp-mode . elisp-mode-delete-trailing-whitespace))
  :init
  (defun elisp-mode-delete-trailing-whitespace ()
    "Delete trailing whitespace before saving file."
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
  :config
  (defconst eval-as-comment-prefix ";;=> ")

  ;; Imitate scala-mode
  ;; from https://github.com/dakra/dmacs
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    (let ((start (point)))
      (eval-print-last-sexp arg)
      (save-excursion
        (goto-char start)
        (save-match-data
          (re-search-forward "[[:space:]\n]+" nil t)
          (insert eval-as-comment-prefix))))))

(use-package ielm
  :ensure nil
  :hook (ielm-mode . lsp-bridge-mode))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
