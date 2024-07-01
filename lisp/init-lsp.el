;;; init-lsp.el --- The lsp client -*- lexical-binding: t -*-

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

;;; Code:

;; lsp-bridge funcs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar +lsp-bridge-jump-stack nil
  "Stack to store jump locations for +lsp-bridge-jump-back.")

;;;###autoload
(defun +lsp-bridge-jump ()
  "Fuses LSP-bridge find-function intelligent jumps."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (push (point-marker) +lsp-bridge-jump-stack)
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))))

;;;###autoload
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
    (lsp-bridge-find-def-return))))

;;;###autoload
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


;; lsp-bridge conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/Documents/emacs/local-packages/lsp-bridge"))
(defun my/load-lsp-bridge ()
  "Load lsp-bridge if it hasn't been loaded yet."
  (if (featurep 'lsp-bridge)
      (lsp-bridge-mode 1)
    (progn
      (require 'lsp-bridge)
      (lsp-bridge-mode 1)))
  (message "major mode is: %s" major-mode))

(setq lsp-bridge-enable-hover-diagnostic t   ;; 允许错误悬浮显示
      acm-enable-quick-access nil            ;; 候选项数字前缀
      lsp-bridge-enable-mode-line nil        ;; 不在 modeline 中显示信息
      acm-enable-doc nil                     ;; doc 遮挡代码，影响视线
      lsp-bridge-enable-auto-format-code nil ;; 自动格式化代码
      lsp-bridge-enable-log nil              ;; 启用 LSP 消息日志，除非开发目的，平常请勿打开以避免影响性能
      lsp-bridge-enable-debug nil            ;; 启用程序调试，默认关闭
      
      lsp-bridge-c-lsp-server "clangd")

(dolist (mode-hook '(python-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook
                     c++-mode-hook
                     js-mode-hook
                     css-mode-hook
                     web-mode-hook))
  (add-hook mode-hook #'my/load-lsp-bridge))

(with-eval-after-load 'lsp-bridge
  (define-key lsp-bridge-mode-map (kbd "M-.") '+lsp-bridge-jump)
  (define-key lsp-bridge-mode-map (kbd "M-,") '+lsp-bridge-jump-back)
  (define-key lsp-bridge-mode-map (kbd "M-?") 'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "<tab>") 'my/yas-expand)
  (define-key lsp-bridge-mode-map (kbd "C-c c a") 'lsp-bridge-code-action)
  (define-key lsp-bridge-mode-map (kbd "C-c c c") 'lsp-bridge-diagnostic-copy)
  (define-key lsp-bridge-mode-map (kbd "C-c c d") 'lsp-bridge-popup-documentation)
  (define-key lsp-bridge-mode-map (kbd "C-c c h") 'lsp-bridge-toggle-sdcv-helper)
  (define-key lsp-bridge-mode-map (kbd "C-c c i") 'lsp-bridge-find-impl-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c c l") 'lsp-bridge-diagnostic-list)
  (define-key lsp-bridge-mode-map (kbd "C-c c q") 'lsp-bridge-ref-quit)
  (define-key lsp-bridge-mode-map (kbd "C-c c r") 'lsp-bridge-rename)) ;; code rename


(provide 'init-lsp)
;;; init-lsp.el ends here
