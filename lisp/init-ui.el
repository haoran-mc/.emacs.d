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
  (list 'spacemacs-dark 'doom-ayu-dark 'doom-ayu-mirage 'doom-badger
        'doom-challenger-deep  'doom-city-lights 'doom-dracula 'doom-gruvbox
        'doom-horizon 'doom-Iosvkem 'doom-material-dark 'doom-molokai
        'doom-monokai-spectrum
        'doom-moonlight 'doom-old-hope 'doom-one 'doom-opera
        'doom-outrun-electric 'doom-palenight 'doom-peacock 'doom-rouge
        'doom-snazzy 'doom-solarized-dark 'doom-solarized-dark-high-contrast
        'doom-sourcerer 'doom-spacegrey 'doom-tokyo-night 'doom-tomorrow-night
        'doom-vibrant 'doom-wilmersdorf 'doom-xcode 'doom-zenburn))

(defvar pretty-light-themes
  (list 'doom-ayu-light 'doom-flatwhite 'doom-gruvbox-light
        'doom-homage-white 'doom-monokai-classic 'doom-one-light
        'doom-opera-light 'doom-solarized-light 'doom-tomorrow-day
        'modus-operandi))

(defvar strange-themes
  (list 'doom-1337 'doom-dark+ 'doom-lantern 'doom-laserwave 'doom-manegarm
        'doom-miramare 'doom-nord-light 'doom-plain 'doom-plain-dark
        'doom-shades-of-purple))

(defvar bad-themes
  (list 'doom-monokai-octagon ;; the highlight is inconspicuous
        'doom-monokai-pro ;; ripgrep highlight bad
        'doom-acario-light ;; comment not obvious
        'doom-acario-dark ;; bad org-link
        ))

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

(defun +load-theme-accoriding-time ()
  (interactive)
  (load-theme 'modus-vivendi t)
  ) ;; TODO

;; load-theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'doom-themes)
;; (doom-themes-treemacs-config)
;; (setq doom-themes-treemacs-theme "doom-colors")

;; nano
;; (require 'nano)

;; lazycat-theme
;; (require 'lazycat-dark-theme)

;; spacemacs-theme
(require 'spacemacs-dark-theme)

;; painting-theme
(add-to-list 'load-path "/Users/haoran/Documents/emacs/local-packages/painting-theme")
(require 'painting-theme)


(if (eq system-type 'gnu/linux)
    (+load-theme-from-selected)
  ;; (load-theme 'doom-solarized-light t)
  ;; (+load-theme-from-selected)
  ;; (load-theme 'painting-dark-theme t)
  (load-theme 'spacemacs-dark t)
  )


;; shackle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize popwin behavior
(require 'shackle)
(setq shackle-default-size 0.5
      shackle-default-alignment 'below
      shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
                      (magit-log-mode       :select t :inhibit-window-quit t :same t)
                      (vc-annotate-mode     :select t :inhibit-window-quit t :same t)
                      ("*quickrun*"         :select t :inhibit-window-quit t :same t)
                      (profiler-report-mode :select t)
                      (xwidget-webkit-mode  :select t :same t)
                      (comint-mode          :select t :align t :size 0.4)
                      (grep-mode            :select t :align t)
                      (rg-mode              :select t :align t)
                      ;; See also `help-window-select'
                      (apropos-mode         :select nil :align t :size 0.4)
                      (help-mode            :select nil :align t :size 0.4)
                      ("*Flycheck errors*"         :select t   :align t :size 10)
                      ("*Backtrace*"               :select t   :align t :size 15)
                      ("*Shell Command Output*"    :select nil :align t :size 0.4)
                      ("*Async Shell Command*"     :select nil :align t :size 0.4)
                      ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
                      ("*package update results*"  :select nil :align t :size 10)
                      ("*Process List*"            :select t   :align t :size 0.3)
                      ("*Occur*"                   :select t   :align t)
                      ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t)))

;; all-the-icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'all-the-icons)

;; rainbow-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-mode)

(provide 'init-ui)
;;; init-ui.el ends here
