;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

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

;;; Code:


;; Try out emacs package without installing
(use-package try
  :ensure t
  :commands try try-and-refresh)

;; MacOS specific
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path (append exec-path '("/root/go/bin")))
  :config
  (setenv "LANG" "zh_CN.UTF-8"))

;; The blazing grep tool
;;
;; invoke rg-menu to search
(use-package rg
  :ensure t)

(use-package hungry-delete
  :ensure t
  :hook ((org-mode . hungry-delete-mode))
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))
  ;; :custom
  ;; (hungry-delete-join-reluctantly t)
  )

;; GC optimization
(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x6400000)) ;; 100 MB

;; Translator for Emacs
;; M-x fanyi-dwim{,2}, that's all.
(use-package fanyi
  :ensure t
  :commands fanyi-dwim fanyi-dwim2)

;; Snippest
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (use-package yasnippet-snippets
    :ensure t))

;; Auto-insert templates when create file
(use-package autoinsert
  :init
  ;; Don't want to be prompted before insertion:
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  :config
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (define-auto-insert "\\.org$"
    ["~/.emacs.d/templates/default-org.org" autoinsert-yas-expand])
  (define-auto-insert "\\.html$"
    ["~/.emacs.d/templates/default-html.html" autoinsert-yas-expand])
  (define-auto-insert "\\.go$"
    ["~/.emacs.d/templates/default-go.go" autoinsert-yas-expand])
  (define-auto-insert "\\.py$"
    ["~/.emacs.d/templates/default-py.py" autoinsert-yas-expand])
  (define-auto-insert "\\.c++$"
    ["~/.emacs.d/templates/default-cpp.cpp" autoinsert-yas-expand]))

(use-package ace-pinyin
  :ensure t
  :custom
  (ace-pinyin-global-mode +1)
  (ace-pinyin-treat-word-as-char nil)) ;; keep <leader>fw original

(use-package literate-calc-mode
  :ensure t
  :defer t)

(require 'ext-treemacs)
(require 'ext-hl-todo)
(require 'ext-projectile)
(require 'ext-which-key)
(require 'ext-ligature)
;; (require 'ext-telega)
;; (require 'ext-reader)
;; (if (eq system-type 'gnu/linux)
;;     (progn
;;       ;; Load the ext-eaf package here
;;       (require 'ext-eaf)))


(provide 'init-tools)
;;; init-tools.el ends here
