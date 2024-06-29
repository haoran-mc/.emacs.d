;;; init-font.el --- font for emacs                  -*- lexical-binding: t; -*-

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


(defvar +font-family "SFMono Nerd Font") ;; Fira Code, JetBrainsMono Nerd Font, SFMono Nerd Font, DejaVuSansMono Nerd Font
(defvar +font-unicode-family "LXGW WenKai") ;; 霞鸳文楷: LXGW WenKai
(defvar +font-size ran--font-size)
(defvar +font-weight ran--font-weight)
(defvar +font-size-list '(10 11 12 13 14 15 16 17 18))


;; 1. set base font
(defun +load-base-font ()
  (let* ((font-spec (format "%s-%d:weight=%s" +font-family +font-size +font-weight)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec))))
(+load-base-font)

;; 2. set face font
;; (defvar +line-font-family "Fira Code")
;; (defvar +fixed-pitch-family "Sarasa Mono SC Nerd") ;; 更纱黑体
;; (defvar +variable-pitch-family "LXGW WenKai")
(defun +set-face-font (&optional frame)
  (let ((font-spec (format "%s-%d" +font-family +font-size))
        (line-font-spec (format "%s-%d" +font-family +font-size))
        (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
        (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
    (set-face-attribute 'variable-pitch frame :font variable-pitch-font-spec :height 1.2) ;; pitch
    (set-face-attribute 'fixed-pitch frame :font fixed-pitch-font-spec)
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec)
    (set-face-attribute 'tab-bar frame :font font-spec :height 1.0) ;; tab-bar
    (set-face-attribute 'mode-line frame :font line-font-spec) ;; mode-line
    (set-face-attribute 'mode-line-inactive frame :font line-font-spec)))
;; (+set-face-font)


;; 3. load unicode font
(when window-system
  (let ((font (frame-parameter nil 'font))
        (font-spec (font-spec :family +font-unicode-family)))
    (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
      (set-fontset-font font charset font-spec))))


;; larger、smaller font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun +larger-font ()
  (interactive)
  (if-let ((size (--find (> it +font-size) +font-size-list)))
      (progn (setq +font-size size)
             (+load-base-font)
             (message "font size: %s" +font-size))
    (message "using largest font")))

(defun +smaller-font ()
  (interactive)
  (if-let ((size (--find (< it +font-size) (reverse +font-size-list))))
      (progn (setq +font-size size)
             (+load-base-font)
             (message "font size: %s" +font-size))
    (message "using smallest font")))

(global-set-key (kbd "s-+") #'+larger-font)
(global-set-key (kbd "s--") #'+smaller-font)


(require 'init-ligature)
;; use hyphenated font



(provide 'init-font)
;;; init-font.el ends here
