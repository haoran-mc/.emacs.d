;;; init-mini.el --- The minimal configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
