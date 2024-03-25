;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-

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

;; Defer garbage collection further back in the startup process
;; A big contributor to startup times is garbage collection. We up the gc threshold to
;; temporarily prevent it from running, and then reset it by the `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default mode-line-format nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t  ;; 不缩放 frame，启动更平滑
      initial-major-mode 'fundamental-mode
      package-enable-at-startup nil
      package--init-file-ensured t)

;; https://emacs-china.org/t/emacs-utf-8/21143/28
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

;; (when (eq system-type 'windows-nt)
;;   (setq file-name-coding-system 'gbk))

(setq initial-frame-alist '((fullscreen . fullboth)))

(provide 'early-init)
;;; early-init.el ends here
