;;; init-lsp_bridge.el --- The lsp client -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;;; Require
;;; git clone https://github.com/manateelazycat/lsp-bridge.git etc/
(add-to-list 'load-path "~/.emacs.d/etc/lsp-bridge")

(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

(dolist (hook (list
               'go-mode-hook
               'emacs-lisp-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (lsp-bridge-mode)
                     )))

(defun lsp-bridge-jump ()
  "融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转."
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
    (dumb-jump-back))))

;; (require 'yasnippet)
;; (yas-global-mode 1)

(provide 'init-lsp_bridge)
;;; init-lsp_bridge.el ends here
