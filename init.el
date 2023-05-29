;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc threshold to
;; temporarily prevent it from running, and then reset it by the `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; read-process-output-max: the maximum bytes read from processs in a single chunk (default is 4kb).
;; This is too small for the LSP protocol that uses JSON communication
(setq read-process-output-max (* 4 1024 1024))

(require 'package) ;; install package
(setq package-archives
      '(
        ("gnu"   . "http://1.15.88.122/gnu/")
        ("melpa" . "http://1.15.88.122/melpa/")

        ;; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
        ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")

        ;; stop using it as wrong connect
        ;; ("melpa"  . "https://melpa.org/packages/")
        ;; ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))

;; Keep ~/.emacs.d/ clean.
(use-package no-littering
  :ensure t
  :demand t)

;; Bootstrap `quelpa'.
(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir)))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "ext" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-base)
(require 'init-utils)
(require 'init-ui)
(require 'init-tools)
(require 'init-evil)
;; (require 'init-lsp_bridge)
(require 'init-git)
(require 'init-dev)
(require 'init-dired)
(require 'init-minibuffer)
(require 'init-company)
(require 'init-keybindings)

;; standalone apps
(require 'init-org)
(require 'init-site)
(require 'init-text)
(require 'init-shell)

;; MacOS specific
(when (eq system-type 'darwin)
  (require 'init-osx))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
