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

(defun random-choice (items)
  "Random choice in ITEMS."
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun ran/load-theme-random ()
  "Load random all of user's themes."
  (interactive)
  (let* ((selected-theme (random-choice (custom-available-themes))))
    (message "Current random theme is: %s" selected-theme)
    (load-theme selected-theme t)))

;; TODO
(defun my/load-theme-accoriding-time ()
  (interactive)
  (load-theme 'modus-vivendi t))


;; load-theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my/theme "~/Documents/emacs/local-packages/painting-theme")
(add-to-list 'load-path (concat my/theme "/doom-extensions"))
(add-to-list 'custom-theme-load-path my/theme)

(require 'doom-themes-ext-org)
(doom-themes-org-config)

(require 'doom-themes-ext-treemacs)
(setq doom-themes-treemacs-theme "doom-colors")
(doom-themes-treemacs-config)
(set-face-attribute 'doom-themes-treemacs-root-face nil
                    :height 1
                    :weight 'normal)

;; (require 'spacemacs-dark-theme)
;; (load-theme 'spacemacs-dark t)

;; (require 'lazycat-dark-theme)
;; (load-theme 'lazycat-dark t)

(load-theme 'monokai t)
(setq frame-background-mode 'dark)

;; (add-to-list 'load-path "~/Documents/emacs/local-packages/doom-themes")
;; (require 'doom-themes)
;; (setq doom-themes-enable-bold nil
;;       doom-themes-enable-italic nil)
;; (load-theme 'doom-molokai t)

;; 当前窗口透明度 (活动区 非活动区)
;; (set-frame-parameter (selected-frame) 'alpha (list 95 95))
;; 新窗口透明度 (活动区 非活动区)
;; (add-to-list 'default-frame-alist (cons 'alpha (list 95 95)))


;; all-the-icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'all-the-icons)


;; hl-todo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'hl-todo)

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

(add-hook 'prog-mode-hook #'hl-todo-mode)
(add-hook 'conf-mode-hook #'hl-todo-mode)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'hl-todo-mode))


(provide 'init-ui)
;;; init-ui.el ends here
