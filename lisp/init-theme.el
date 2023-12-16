;;; init-theme.el --- may be go out on my own someday  -*- lexical-binding: t; -*-

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
   '(org-level-1 ((t (:inherit outline-1 :weight normal :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :weight normal :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :weight normal :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :weight normal :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :weight normal :height 1.0))))
   '(org-level-6 ((t (:inherit outline-6 :weight normal :height 1.0))))
   '(org-level-7 ((t (:inherit outline-7 :weight normal :height 1.0))))
   '(org-level-8 ((t (:inherit outline-8 :weight normal :height 1.0))))
   '(org-document-title ((t (:weight normal :height 1.0))))
   '(org-link ((t (:inherit link :foreground "#2AA1AE" :weight normal)))))






(deftheme painting "A minimal dark theme.")

;; 设置 org-mode 字体
(let ((font-weight "normal"))
  (custom-theme-set-faces
   `painting
   `(org-level-1 ((t (:inherit outline-1 :weight ,font-weight :height 1.0))))
   `(org-level-2 ((t (:inherit outline-2 :weight ,font-weight :height 1.0))))
   `(org-level-3 ((t (:inherit outline-3 :weight ,font-weight :height 1.0))))
   `(org-level-4 ((t (:inherit outline-4 :weight ,font-weight :height 1.0))))
   `(org-level-5 ((t (:inherit outline-5 :weight ,font-weight :height 1.0))))
   `(org-level-6 ((t (:inherit outline-6 :weight ,font-weight :height 1.0))))
   `(org-level-7 ((t (:inherit outline-7 :weight ,font-weight :height 1.0))))
   `(org-level-8 ((t (:inherit outline-8 :weight ,font-weight :height 1.0))))
   `(org-document-title ((t (:weight ,font-weight :height 1.0))))
   `(org-link ((t (:inherit link :foreground "#2AA1AE" :weight ,font-weight))))))


;; 设置代码块用上下边线包裹
;; (custom-set-faces
;;  '(org-block-begin-line ((t (:underline t :background unspecified))))
;;  '(org-block-end-line ((t (:overline t :underline nil :background unspecified)))))


(provide 'init-theme)
;;; init-theme.el ends here
