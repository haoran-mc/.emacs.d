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

;; 设置 org-mode 字体
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :weight semi-bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :weight semi-bold :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :weight semi-bold :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :weight semi-bold :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :weight semi-bold :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :weight semi-bold :height 1.0))))
 '(org-document-title ((t (:weight semi-bold :height 1.0))))
 '(org-link ((t (:inherit link :foreground "#2AA1AE" :weight semi-bold)))))

;; 设置代码块用上下边线包裹
;; (custom-set-faces
;;  '(org-block-begin-line ((t (:underline t :background unspecified))))
;;  '(org-block-end-line ((t (:overline t :underline nil :background unspecified)))))


(provide 'init-theme)
;;; init-theme.el ends here
