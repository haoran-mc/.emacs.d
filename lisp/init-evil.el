;;; init-evil.el --- vim model editing               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Haoran Liu

;; Author: Haoran Liu <haoran.mc@foxmail.com>
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
(require 'init-macros)

(setq evil-disable-insert-state-bindings t

      ;; window behavior
      evil-split-window-below t
      evil-vsplit-window-right t

      ;; ex / search behavior
      evil-ex-complete-emacs-commands nil
      evil-ex-interactive-search-highlight 'selected-window

      ;; motion / editing behavior
      evil-respect-visual-line-mode t
      evil-want-integration t
      evil-want-keybinding nil
      evil-want-fine-undo t
      evil-want-C-g-bindings t
      evil-want-C-u-scroll t
      evil-symbol-word-search t)

;; before ↑
(add-subdirs-to-load-path "~/Documents/emacs/local-packages/evil")
(require 'evil)
(add-hook 'after-init-hook #'evil-mode)

(setq evil-insert-state-cursor 'bar
      evil-emacs-state-modes '(eshell-mode term-mode vterm-mode
                                           magit-mode magit-popup-mode
                                           dirvish-mode
                                           treemacs-mode
                                           dired-mode))

;; Silence line out of range error.
(shut-up! #'evil-indent)

;; normal/visual state
(dolist (map '(evil-normal-state-map
               evil-visual-state-map
               evil-motion-state-map))
  (dolist (key '("TAB"
                 "M-x" "M-w" "M-q" "M-/" "M-." "M-,"
                 "C-k" "C-."))
    (define-key (symbol-value map) (kbd key) nil)))

;; (define-key evil-ex-map "q" #'kill-this-buffer)

(provide 'init-evil)
;;; init-evil.el ends here
