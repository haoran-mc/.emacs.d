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
;; ✦ font-spec specification（规格），基于 X Window System（或 X11）中的字体命名约定

;;; Code:
;; Fira Code, JetBrainsMono Nerd Font, SFMono Nerd Font, DejaVuSansMono Nerd Font
(let ((font-size (cond
                  (ran--os-mac 13)
                  (ran--os-linux 10)
                  (t 13)))
      (font-family (cond
                    (ran--os-mac "Liga SFMono Nerd Font")
                    (ran--os-linux "SFMono Nerd Font Mono")
                    (t "Ubuntu Mono"))))
  (defvar ran--font-family font-family)
  (defvar ran--unicode-font-family "LXGW WenKai")
  (defvar ran--font-size font-size)
  (defvar ran--font-weight "normal")
  (defvar my/font-size-list '(7 8 9 10 11 12 13 14 15 16 17 18)))

(defun my/font-setup (&optional frame)
  (when window-system
    (let* ((english-font ran--font-family)
           (chinese-font ran--unicode-font-family)
           (font-size ran--font-size)
           (font-weight ran--font-weight)
           (my-font-spec (format "%s-%d:weight=%s" english-font font-size font-weight)))

      (set-frame-parameter nil 'font my-font-spec)
      (add-to-list 'default-frame-alist `(font . ,my-font-spec))

      (dolist (charset '(kana han hangul cjk-misc bopomofo)) ;; symbol
        (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family chinese-font))))))
(my/font-setup)

;; larger、smaller font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ran/larger-font ()
  (interactive)
  (if-let ((size (--find (> it ran--font-size) my/font-size-list)))
      (progn (setq ran--font-size size)
             (my/font-setup)
             (message "font size: %s" ran--font-size))
    (message "using largest font")))

(defun ran/smaller-font ()
  (interactive)
  (if-let ((size (--find (< it ran--font-size) (reverse my/font-size-list))))
      (progn (setq ran--font-size size)
             (my/font-setup)
             (message "font size: %s" ran--font-size))
    (message "using smallest font")))

(global-set-key (kbd "s-+") #'ran/larger-font)
(global-set-key (kbd "s--") #'ran/smaller-font)

(provide 'init-font)
;;; init-font.el ends here
