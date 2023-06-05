;;; init-lsp_bridge.el --- The lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; download the dependency first
;; https://github.com/manateelazycat/lsp-bridge.git

;;; Code:

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
  :defer t
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))

(use-package posframe
  :ensure t
  :defer t)

(use-package lsp-bridge
  :load-path "~/Documents/emacs/local-packages/lsp-bridge"
  :hook ((python-mode     . lsp-bridge-mode)
         (emacs-lisp-mode . lsp-bridge-mode))
  :bind (:map lsp-bridge-mode-map
              ("C-c j" . +lsp-bridge-jump)
              ("C-c b" . +lsp-bridge-jump-back)
              ("C-c r" . lsp-bridge-find-references)
              ("C-c R" . lsp-bridge-rename))
  :init
  (defvar +lsp-bridge-jump-stack nil
    "Stack to store jump locations for +lsp-bridge-jump-back.")

  (defun +lsp-bridge-jump ()
    "Fuses LSP-bridge find-function and dumb-jump intelligent jumps."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (let ((symb (function-called-at-point)))
        (when symb
          (push (point-marker) +lsp-bridge-jump-stack)
          (find-function symb))))
     (lsp-bridge-mode
      (message "lsp-bridge-find-def")
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun +lsp-bridge-jump-back ()
    "Jump back to the previous location."
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (if (null +lsp-bridge-jump-stack)
          (message "No previous location to jump back to.")
        (let ((marker (pop +lsp-bridge-jump-stack)))
          (if (marker-buffer marker)
              (progn
                (switch-to-buffer (marker-buffer marker))
                (goto-char (marker-position marker)))
            (message "Jump location is no longer available.")))))
     (lsp-bridge-mode
      (message "lsp-bridge-find-def-return")
      (lsp-bridge-find-def-return))
     (t
      (require 'dumb-jump)
      (dumb-jump-back)))))

(provide 'init-lsp_bridge)
;;; init-lsp_bridge.el ends here
