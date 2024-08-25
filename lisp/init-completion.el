;;; init-completion.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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

;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ;; Enable indentation+completion using the TAB key.
 ;; `completion-at-point' is often bound to M-TAB.
 tab-always-indent 'complete
 ;; TAB cycle if there are only few candidates
 completion-cycle-threshold 3
 ;; maybe remove as orderless and corfu work together for completions.
 completion-ignore-case t)


;; corfu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/corfu")
(require 'corfu)
(add-hook 'after-init-hook #'global-corfu-mode)
(setq corfu-auto t        ;; enable auto completion
      corfu-auto-prefix 2 ;; minimum length of prefix for completion
      corfu-auto-delay 0)
;; my-DEL-meow-delete prevent corfu-auto see `corfu-auto-commands' for more

(with-eval-after-load 'meow
  (advice-add #'meow-insert-exit :after #'corfu-quit))

(with-eval-after-load 'yasnippet
  (define-key corfu-map (kbd "<tab>") #'yas-expand))


;; flymake ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; eglot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/external-completion")
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/eglot")
(require 'eglot)
(setq eglot-ignored-server-capabilities '(:hoverProvider ;; 光标位置信息
                                          :documentHighlightProvider ;; 高亮当前 symbol
                                          :inlayHintProvider) ;; 显示 inlay hint 提示
      ;; 当最后一个源码 buffer 关闭时自动关闭 eglot server
      eglot-autoshutdown t)

(add-hook 'before-save-hook
          (lambda ()
            (call-interactively 'eglot-code-action-organize-imports))
          nil t)

(global-set-key (kbd "M-.") 'xref-find-definitions)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-?") 'xref-find-references)
(define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)

(dolist (mode-hook '(python-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook
                     c++-mode-hook))
  (add-hook mode-hook #'eglot-ensure))


(provide 'init-completion)
;;; init-completion.el ends here
