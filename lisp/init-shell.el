;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(defun shell-self-destroy-sentinel (proc _exit-msg)
  "Make PROC self destroyable."
  (when (memq (process-status proc) '(exit signal stop))
    (kill-buffer (process-buffer proc))
    (ignore-errors (delete-window))))

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :config
  ;; Prevent accident typing
  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)
  (defalias 'eshell/nvim 'find-file)

  ;; git clone git@github.com:manateelazycat/aweshell.git
  (use-package aweshell
    :load-path "~/.emacs.d/etc/aweshell"
    :init
    (require 'aweshell)
    :bind(("C-c e n"   . aweshell-new)
          ("C-c e t"   . aweshell-toggle)
          ("C-c e d" . aweshell-dedicated-toggle))
    :config
    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-dakrone)))
  :custom
  (eshell-banner-message ""))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
              ([remap kill-region] . backward-kill-word)
              ([remap delete-char] . eshell-delchar-or-maybe-eof))
  :config
  ;; Delete the last "word"
  (dolist (ch '(?_ ?- ?.))
    (modify-syntax-entry ch "w" eshell-mode-syntax-table)))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook ((shell-mode . shell-mode-common-init)
         (shell-mode . revert-tab-width-to-default))
  :bind ("M-`" . shell-toggle) ;; was `tmm-menubar'
  :config
  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :custom
  (shell-kill-buffer-on-exit t))

(provide 'init-shell)
;;; init-shell.el ends here
