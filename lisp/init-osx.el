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

;;; Code:
;; fullboth → maximized macos 固定emacs窗口位置
(modify-frame-parameters nil '((fullscreen . maximized)))
;; Make titlebar dark, but need patch
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

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

      ;; Visit files opened outside of Emacs in existing frame, not a new one
      ns-pop-up-frames nil

      ;; https://stackoverflow.com/a/42038174
      dired-use-ls-dired nil

      ;; stop cursor blinking bug when typing Chinese/Japanese on OS X
      ;; redisplay-dont-pause nil

      ;; Curse Lion and its sudden but inevitable fullscreen mode!
      ;; NOTE Meaningless to railwaycat's emacs-mac build
      ;; ns-use-native-fullscreen nil
      )

;; disable ⌘-h https://emacs.stackexchange.com/a/14825/39425
(setq mac-pass-command-to-system nil)


;; cache-path-from-shell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 在初始化之后加载 cache-path-from-shell，并初始化环境变量
;; (setenv "LANG" "zh_CN.UTF-8")
(run-with-idle-timer
 2 nil
 #'(lambda ()
     (require 'cache-path-from-shell)
     (dolist (var '("LANG" "LC_CTYPE"))
       (add-to-list 'exec-path-from-shell-variables var))
     (exec-path-from-shell-initialize)))

(provide 'init-osx)
;;; init-osx.el ends here
