;;; init-lsp_bridge.el --- The lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;; git clone https://github.com/manateelazycat/lsp-bridge.git etc/
(use-package lsp-bridge
  :load-path "~/.emacs.d/etc/lsp-bridge"
  :init
  (require 'lsp-bridge-jdtls)
  :hook ((go-mode . lsp-bridge-mode)
         (emacs-lisp-mode . lsp-bridge-mode))
  :config
  (defun lsp-bridge-jump ()
    "Fuses LSP-bridge find-function and dumb-jump intelligent jumps."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (let ((symb (function-called-at-point)))
        (when symb
          (find-function symb))))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))
  (defun lsp-bridge-jump-back ()
    "Jump back."
    (interactive)
    (cond
     (lsp-bridge-mode
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-back)))))


(provide 'init-lsp_bridge)
;;; init-lsp_bridge.el ends here
