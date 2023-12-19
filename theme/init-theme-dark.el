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

;;; Code:


(custom-set-faces
 ;; basics
 ;; `(hl-line                        ((((type graphic)) :background "#424868") ;; 高亮行
 ;;                                   (((type tty)))))
 '(hl-line                        ((t (:background "#424868"))))
 '(region                         ((t (:background "grey" :foreground "black"))))

 ;; org-mode
 '(org-level-1 ((t (:inherit outline-1 :weight normal :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :weight normal :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :weight normal :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :weight normal :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :weight normal :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :weight normal :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :weight normal :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :weight normal :height 1.0))))
 '(org-document-title ((t (:weight normal :height 1.0))))
 '(org-link ((t (:inherit link :foreground "#2AA1AE" :weight normal))))

 ;; highlight-thing
 '(highlight-thing ((t (:background "#606689"))))

 ;; symbol-thing
 '(symbol-overlay-face-1 ((t (:foreground "black" :background "#A4E57E"))))
 '(symbol-overlay-face-2 ((t (:foreground "black" :background "#8CCBEA"))))
 '(symbol-overlay-face-3 ((t (:foreground "black" :background "#FFDB72"))))
 '(symbol-overlay-face-4 ((t (:foreground "black" :background "#FF7272"))))
 '(symbol-overlay-face-5 ((t (:foreground "black" :background "#FFB3FF"))))
 '(symbol-overlay-face-6 ((t (:foreground "black" :background "#9999FF"))))
 '(symbol-overlay-face-7 ((t (:foreground "black" :background "#1E90FF"))))
 '(symbol-overlay-face-8 ((t (:foreground "black" :background "#40E0D0"))))

 ;; vertico
 '(vertico-current ((t (:background "#424868"))))
 )




;; (deftheme painting "A minimal dark theme.")

;; ;; 设置 org-mode 字体
;; (let ((font-weight "normal"))
;;   (custom-theme-set-faces
;;    `painting
;;    `(org-level-1 ((t (:inherit outline-1 :weight ,font-weight :height 1.0))))
;;    `(org-level-2 ((t (:inherit outline-2 :weight ,font-weight :height 1.0))))
;;    `(org-level-3 ((t (:inherit outline-3 :weight ,font-weight :height 1.0))))
;;    `(org-level-4 ((t (:inherit outline-4 :weight ,font-weight :height 1.0))))
;;    `(org-level-5 ((t (:inherit outline-5 :weight ,font-weight :height 1.0))))
;;    `(org-level-6 ((t (:inherit outline-6 :weight ,font-weight :height 1.0))))
;;    `(org-level-7 ((t (:inherit outline-7 :weight ,font-weight :height 1.0))))
;;    `(org-level-8 ((t (:inherit outline-8 :weight ,font-weight :height 1.0))))
;;    `(org-document-title ((t (:weight ,font-weight :height 1.0))))
;;    `(org-link ((t (:inherit link :foreground "#2AA1AE" :weight ,font-weight))))))


;; 设置代码块用上下边线包裹
;; (custom-set-faces
;;  '(org-block-begin-line ((t (:underline t :background unspecified))))
;;  '(org-block-end-line ((t (:overline t :underline nil :background unspecified)))))


(provide 'init-theme-dark)
;;; init-theme-dark.el ends here
