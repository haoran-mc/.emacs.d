;;; init-osx.el --- Tweaks for MacOS -*- lexical-binding: t -*-

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
;; 1. [X] change CapsLock -> Control
;;
;; 2.
;; +----------+----------+-----------+
;; | control  | option   | command   |
;; +----------+----------+-----------+
;;     ↓
;; +----------+----------+-----------+
;; | control  | super    | meta      |
;; +----------+----------+-----------+
;;
;; 3. disable ⌘-h: https://superuser.com/a/1328252/1826985


;;; Code:




;; Make titlebar dark
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Useful when use an external keyboard
(defun +osx-swap-option-and-command ()
  "Swap `mac-option-modifier' and `mac-command-modifier'."
  (interactive)
  (cl-rotatef mac-option-modifier mac-command-modifier)
  (message "mac-option-modifier: %s, mac-command-modifier: %s" mac-option-modifier mac-command-modifier))

;; Emoji support
(let ((fonts '("apple color Emoji")))
  (cl-loop with script = (if (>= emacs-major-version 28) 'emoji 'unicode)
           for font in fonts
           when (member font (font-family-list))
           return (set-fontset-font t script (font-spec :family font) nil 'prepend)))

(setq mac-option-modifier 'super ;; set option(key) as hyper(key)
      mac-command-modifier 'meta ;; set command(key) as meta(key)
      delete-by-moving-to-trash t
      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ns-use-native-fullscreen t
      ;; Visit files opened outside of Emacs in existing frame, not a new one
      ns-pop-up-frames nil
      ;; https://stackoverflow.com/a/42038174
      dired-use-ls-dired nil)


;; exec-path-from-shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 在初始化之后加载 exec-path-from-shell，并初始化环境变量
(when (eq system-type 'darwin)
  (require 'exec-path-from-shell)
  ;; https://emacs-china.org/t/exec-path-from-shell/2515/4
  (setq exec-path-from-shell-arguments '("-l")) ;; not load ~/.zshrc, only ~/.zshenv
  (setenv "LANG" "zh_CN.UTF-8")
  (exec-path-from-shell-initialize))


(provide 'init-osx)
;;; init-osx.el ends here
