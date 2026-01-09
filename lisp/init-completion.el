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
(setq completion-ignore-case t)


;; corfu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/corfu")
(require 'corfu)
(require 'corfu-popupinfo)
(setq corfu-auto t        ;; enable auto completion
      corfu-auto-prefix 1 ;; minimum length of prefix for completion
      corfu-auto-delay 0
      corfu-scroll-margin 0
      corfu-on-exact-match nil ;; 即使只有一个 candidate 也不自动选中
      corfu-popupinfo-delay 10)
(setq corfu-auto-commands '("self-insert-command\\'" c-electric-colon c-electric-lt-gt c-electric-slash c-scope-operator))
;; my-DEL-meow-delete prevent corfu-auto see `corfu-auto-commands' for more

(dolist (mode-hook '(emacs-lisp-mode-hook
                     python-mode-hook
                     go-mode-hook
                     rust-mode-hook
                     c++-mode-hook
                     js-mode-hook
                     typescript-mode-hook))
  (add-hook mode-hook #'corfu-mode))

(with-eval-after-load 'meow
  (advice-add #'meow-insert-exit :after #'corfu-quit))

(with-eval-after-load 'yasnippet
  (define-key corfu-map (kbd "<tab>") #'yas-expand))

(define-key corfu-map (kbd "SPC") nil)
(define-key corfu-map (kbd "RET") 'corfu-complete)
(define-key corfu-map (kbd "M-<") 'corfu-first)
(define-key corfu-map (kbd "M->") 'corfu-last)
(define-key corfu-map (kbd "C-d") 'corfu-scroll-up)
(define-key corfu-map (kbd "C-u") 'corfu-scroll-down)
(define-key corfu-map (kbd "C-h") 'corfu-info-documentation)

(require 'corfu-history)
(corfu-history-mode)
;; savehist have requried in minibuffer
(add-to-list 'savehist-additional-variables 'corfu-history)


;; flymake ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (mode-hook '(python-mode-hook
                     go-mode-hook
                     rust-mode-hook))
  (add-hook mode-hook #'flymake-mode))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (flymake-mode -1)))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-P" #'flymake-goto-prev-error)
  (keymap-set flymake-mode-map "M-N" #'flymake-goto-next-error))

(add-hook 'flymake-mode-hook
          (lambda ()
            (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))


;; xref ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(advice-add #'xref-find-references :after #'(lambda (&rest args) (recenter nil)))
(advice-add #'xref-pop-marker-stack :after #'(lambda (&rest args) (recenter nil)))


;; eglot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/external-completion")
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/eglot")
(dolist (mode-hook '(python-mode-hook
                     go-mode-hook
                     rust-mode-hook
                     c++-mode-hook
                     js-mode-hook
                     typescript-mode-hook))
  (add-hook mode-hook #'(lambda () (require 'eglot) (eglot-ensure)
                          (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
                          (define-key eglot-mode-map (kbd "C-c c i") 'eglot-find-implementation)
                          (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
                          (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
                          (define-key eglot-mode-map (kbd "M-,") 'xref-pop-marker-stack)
                          (define-key eglot-mode-map (kbd "M-?") 'xref-find-references))))

(setq eglot-ignored-server-capabilities '(:hoverProvider ;; 光标位置信息
                                          :documentHighlightProvider ;; 高亮当前 symbol
                                          :inlayHintProvider) ;; 显示 inlay hint 提示
      ;; 当最后一个源码 buffer 关闭时自动关闭 eglot server
      eglot-autoshutdown t
      eglot--mode-line-format nil)

(setq-default eglot-workspace-configuration
              '( :gopls ( :buildFlags ["-tags" "wireinject"]
                          :usePlaceholders t
                          :staticcheck t)
                 :pyright ( :checkOnlyOpenFiles t
                            :typeCheckingMode "basic")
                 ))

(provide 'init-completion)
;;; init-completion.el ends here
