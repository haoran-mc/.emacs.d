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

;;; Code:


(custom-set-faces
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
 '(highlight-thing ((t (:background "#ECEFF1"))))

 ;; symbol-thing
 '(symbol-overlay-face-1 ((t (:foreground "black" :background "#A4E57E"))))
 '(symbol-overlay-face-2 ((t (:foreground "black" :background "#8CCBEA"))))
 '(symbol-overlay-face-3 ((t (:foreground "black" :background "#FFDB72"))))
 '(symbol-overlay-face-4 ((t (:foreground "black" :background "#FF7272"))))
 '(symbol-overlay-face-5 ((t (:foreground "black" :background "#FFB3FF"))))
 '(symbol-overlay-face-6 ((t (:foreground "black" :background "#9999FF"))))
 '(symbol-overlay-face-7 ((t (:foreground "black" :background "#1E90FF"))))
 '(symbol-overlay-face-8 ((t (:foreground "black" :background "#40E0D0"))))

 ;; diff-hl
 '(diff-hl-change ((t (:background "#bbbb00" :foreground "#bbbb00"))))
 )


(provide 'init-theme-light)
;;; init-theme-light.el ends here
