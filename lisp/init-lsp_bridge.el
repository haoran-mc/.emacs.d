;;; init-lsp_bridge.el --- The lsp client -*- lexical-binding: t -*-

;;; Commentary:
;; installation and dependencies:
;; install
;;     git clone https://github.com/manateelazycat/lsp-bridge.git etc/
;; Python-EPC https://github.com/tkf/python-epc
;;     pip install epc
;; orjson https://github.com/ijl/orjson
;;     pip install --upgrade "pip>=20.3" # manylinux_x_y, universal2 wheel support
;;     pip install --upgrade orjson
;; postframe https://github.com/tumashu/posframe
;; markdown-mode https://github.com/jrblevin/markdown-mode
;; yasnippet https://github.com/joaotavora/yasnippet
;;     M-s package-install RET postframe RET
;;     ... for markdown-mode and yasnippet
;;     (require 'yasnippet) ...
;;     look at init-tools.el for more configuration about yasnippet

;;; Code:

(use-package lsp-bridge
  :load-path "~/.emacs.d/etc/lsp-bridge"
  :init
  (require 'lsp-bridge-jdtls)
  :hook ((go-mode . lsp-bridge-mode)
         (emacs-lisp-mode . lsp-bridge-mode))
  :bind (:map lsp-bridge-mode-map
              ("C-c j" . +lsp-bridge-jump)
              ("C-c b" . +lsp-bridge-jump-back)
              ("C-c C-r" . lsp-bridge-rename))
  :config
  (defun +lsp-bridge-jump ()
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
  (defun +lsp-bridge-jump-back ()
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
