;;; init-lsp_bridge.el --- The lsp client -*- lexical-binding: t -*-

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
;; download the dependency first
;; https://github.com/manateelazycat/lsp-bridge.git
;; lsp-bridge 仅支持补全、跳转、重命名！
;; 代码检查使用 flycheck

;;; Code:

;; Jump to definition, I keep it anyway though it doesn't load most of the time
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

;; LOCAL-PACKAGES
(use-package lsp-bridge
  :load-path "~/Documents/emacs/local-packages/lsp-bridge"
  :hook ((python-mode     . lsp-bridge-mode)
         (emacs-lisp-mode . lsp-bridge-mode)
         (go-mode         . lsp-bridge-mode))
  :bind (:map lsp-bridge-mode-map
              ("C-j" . +lsp-bridge-jump)
              ("C-k" . +lsp-bridge-jump-back)      ;; go back
              ([tab]   . my/yas-expand))
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
      (lsp-bridge-find-def-return))
     (t
      (require 'dumb-jump)
      (dumb-jump-back))))

  :config
  ;; (setq lsp-bridge-get-project-path-by-filepath nil)
  (defun +func-eset-go-project-path()
    "Reset go project path, for multi lang server."
    (interactive)
    (setq lsp-bridge-get-project-path-by-filepath
          (lambda (filepath)
            "Set `project-path` variable to FILEPATH and return its value."
            (if (string-suffix-p ".go" filepath)
                (let ((project-path filepath))
                  project-path))
            (message "lsp-bridge single-mode"))))

  (defun my/yas-expand()
    (interactive)
    (if (use-region-p) ; indent region instead if it's active
        (indent-region (region-beginning) (region-end))
      (when (and (let ((indent (current-indentation)))
                   (funcall indent-line-function)
                   (= indent (current-indentation)))
                 (looking-at "[[:space:]\n]")) ;; TODO or "[^[:word:]]"
        (setq this-command 'yas-expand)
        (call-interactively #'yas-expand))))
  :custom
  (lsp-bridge-enable-hover-diagnostic t)
  ;; (lsp-bridge-enable-debug t)
  ;; (lsp-bridge-enable-log t)
  ;; (acm-quick-access-use-number-select nil)
  (acm-enable-quick-access nil))

(provide 'init-lsp_bridge)
;;; init-lsp_bridge.el ends here
