;;; init-theme-dark.el --- custom set theme for dark  -*- lexical-binding: t; -*-

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
;; (set-face-attribute 'variable-pitch nil :family +font-family) ;; 变宽字体


;;; Code:
(with-eval-after-load 'meow
  ;; (set-face-attribute 'meow-keypad-cursor nil :background "#7c2902")
  ;; (set-face-attribute 'meow-insert-cursor nil :background "#014f39")
  ;; (set-face-attribute 'meow-normal-cursor nil :background "#9b034c")
  ;; (set-face-attribute 'meow-motion-cursor nil :background "#035063")
  (set-face-attribute 'meow-keypad-indicator nil :foreground "#801717" :background "#FF6666" :box '(:line-width -1 :color "#801717"))
  (set-face-attribute 'meow-insert-indicator nil :foreground "#309030" :background "#AAE9A0" :box '(:line-width -1 :color "#309030"))
  (set-face-attribute 'meow-normal-indicator nil :foreground "#6F5033" :background "#FFEE99" :box '(:line-width -1 :color "#6F5033"))
  (set-face-attribute 'meow-motion-indicator nil :foreground "#505090" :background "#AACCEE" :box '(:line-width -1 :color "#505090")))

;; org-mode
(defface org-bold nil "Face for org-mode bold.")
(with-eval-after-load 'org
  (set-face-attribute 'org-bold nil :weight 'normal :foreground "purple" :underline '(:color "red" :style line) :overline nil)
  (set-face-attribute 'org-verbatim nil :foreground "yellow" :background "#595530")
  (set-face-attribute 'org-code nil :foreground "orange")
  (set-face-attribute 'org-document-title nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal :foreground "#829CD6")
  (set-face-attribute 'org-level-2 nil :height 1.0 :weight 'normal :foreground "#5B94AB")
  (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'normal :foreground "#7EBEBD")
  (set-face-attribute 'org-level-4 nil :height 1.0 :weight 'normal :foreground "#677CAB")
  (set-face-attribute 'org-level-5 nil :height 1.0 :weight 'normal :foreground "#487688")
  (set-face-attribute 'org-level-6 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-7 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-8 nil :height 1.0 :weight 'normal))

(provide 'init-theme-dark)
;;; init-theme-dark.el ends here
