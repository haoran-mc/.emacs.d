;;; init-theme-light.el --- custom set theme for light  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>
;; keywords:

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

;; set-face-attribute    动态设置某个 face 的属性，立即生效，适用于临时修改
;; custom-set-face       通过 Emacs 自定义系统进行持久化修改，但是会修改 custom.el 文件，不建议使用
;; defface               用于复杂的面定义和主题支持
;; face-spec-set         批量设置面属性
;; custom-theme-set-face 在自定义主题中设置面属性

;; (set-face-attribute 'example-face nil :inherit 'unspecified)

;;; Code:

;; font-lock-comment-face:              #96A7A9
;; font-lock-constant-face:             #6c71c4
;; font-lock-builtin-face:              #d33682
;; font-lock-string-face:               #2aa198
;; font-lock-type-face:                 #b58900
;; font-lock-keyword-face:              #859900
;; font-lock-function-name-face:        #b58900
;; font-lock-variable-name-face:        #268bd2
;; font-lock-doc-face:                  #35a69c
;; font-lock-preprocessor-face:         #268bd2 预处理 #define
;; font-lock-regexp-grouping-construct: #268bd2 正则中的分组 []
;; font-lock-regexp-grouping-backslash: #268bd2 正则中的转义 \w
;; font-lock-negation-char-face:        #268bd2 正则中的取反 !
;; font-lock-comment-delimiter-face:    unspecified 注释符
;; font-lock-doc-markup-face:           unspecified
;; font-lock-warning-face:              unspecified

(set-face-attribute 'font-lock-comment-face nil :italic nil)
(set-face-attribute 'font-lock-builtin-face nil :italic nil)
(set-face-attribute 'font-lock-type-face nil :italic nil)
;; (set-face-attribute 'fringe nil :background "#EDE8D5")

;; (set-face-attribute 'tab-bar nil :background "#D7D7D7" :box nil :inherit 'unspecified)
;; (set-face-attribute 'tab-bar-tab nil :background "#B7B7B7" :box nil :inherit 'unspecified)
;; (set-face-attribute 'tab-bar-tab-inactive nil :box nil :inherit 'unspecified)

;; diff-hl
(with-eval-after-load 'diff-hl
  (set-face-attribute 'diff-hl-change nil :background "#BBBB00" :foreground "#BBBB00"))

;; magit
(with-eval-after-load 'magit
  (set-face-attribute 'magit-header-line nil :background "#EDE8D5"))

;; drivish
(with-eval-after-load 'dirvish
  (set-face-attribute 'dirvish-hl-line nil :foreground 'unspecified :background "#F2E6CE"))

;; meow
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
(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :height 1.0 :weight 'normal :foreground "#829CD6")
  (set-face-attribute 'org-level-2 nil :height 1.0 :weight 'normal :foreground "#5B94AB")
  (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'normal :foreground "#7EBEBD")
  (set-face-attribute 'org-level-4 nil :height 1.0 :weight 'normal :foreground "#677CAB")
  (set-face-attribute 'org-level-5 nil :height 1.0 :weight 'normal :foreground "#487688")
  (set-face-attribute 'org-level-6 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-7 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-8 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-document-title nil :height 1.0 :weight 'normal))

(provide 'init-theme-light)
;;; init-theme-light.el ends here
