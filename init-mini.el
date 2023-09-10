;;; init-mini.el --- The minimal configuration -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs minimal configuration for debugging.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

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

(setq debug-on-error t)
(setq-default lexical-binding t)
;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path (append exec-path '("/root/go/bin"))))

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  :hook (after-init . evil-mode)
  :config
  (with-eval-after-load 'evil
    (setq evil-insert-state-cursor 'bar)))

(use-package org
  :ensure nil
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images)
         (org-mode . visual-line-mode) ;; 折行
         ;; (org-mode . (lambda() (setq truncate-lines nil)))
         )
  )

(provide 'init)
;;; init.el ends here
