;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

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


(defvar pretty-dark-themes
  (list 'doom-ayu-dark 'doom-city-lights 'doom-dracula
        'doom-gruvbox 'doom-molokai 'doom-old-hope 'doom-one
        'doom-snazzy 'doom-tomorrow-night 'doom-vibrant
        'doom-wilmersdorf 'doom-xcode 'doom-zenburn))

(defun random-choice (items)
  "Random choice in ITEMS."
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun +load-theme-random ()
  "Load random all of user's themes."
  (interactive)
  (let* ((selected-theme (random-choice (custom-available-themes))))
    (message "Current random theme is: %s" selected-theme)
    (load-theme selected-theme t)))

(defun +load-theme-from-selected ()
  "Load random from pretty-dark-themes."
  (interactive)
  (let* ((selected-theme (random-choice pretty-dark-themes)))
    (message "Current random theme is: %s" selected-theme)
    (load-theme selected-theme t)))

;; TODO
(defun +load-theme-accoriding-time ()
  (interactive)
  (load-theme 'modus-vivendi t))

;; load-theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'doom-themes)
(setq doom-themes-enable-bold nil
      doom-themes-enable-italic nil)

(require 'doom-themes-ext-org)
(doom-themes-org-config)

;; spacemacs-theme
;; (require 'spacemacs-dark-theme)

;; (+load-theme-from-selected)
;; (load-theme 'doom-solarized-light t)
(load-theme 'doom-old-hope t)

;; 当前窗口透明度 (活动区 非活动区)
;; (set-frame-parameter (selected-frame) 'alpha (list 95 95))
;; 新窗口透明度 (活动区 非活动区)
;; (add-to-list 'default-frame-alist (cons 'alpha (list 95 95)))


;; all-the-icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'all-the-icons)


;; hl-todo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hl-todo)
(global-hl-todo-mode)

(setq hl-todo-keyword-faces
      '(("DEPRECATED" . "#FF0000")
        ("BUILT-IN"   . "#C066DB")

        ;; org-todo
        ("TODO"   . "#FF0000") ;; #CC9393
        ("DONE"   . "#5B6268")
        ("CANCEL" . "#5B6268") ;; #50a14f
        ("LONG"   . "#D0BF8F")
        ("HOLD"   . "#D0BF8F")
        ("WORK"   . "#FF0000")))


(provide 'init-ui)
;;; init-ui.el ends here
