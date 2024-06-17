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
        'doom-horizon 'doom-material-dark 'doom-molokai 'doom-monokai-spectrum
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
        'doom-Iosvkem ;; bad highlight-thing
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
(require 'doom-themes)
(setq doom-themes-treemacs-theme "doom-colors"
      doom-themes-enable-bold nil
      doom-themes-enable-italic t)

(require 'doom-themes-ext-treemacs)
(require 'doom-themes-ext-org)
(doom-themes-treemacs-config)
(doom-themes-org-config)


;; nano
;; (require 'nano)

;; lazycat-theme
;; (require 'lazycat-dark-theme)

;; spacemacs-theme
;; (require 'spacemacs-dark-theme)

;; painting-theme
;; (add-to-list 'load-path (concat haoran/test-packages-dir "/painting-theme"))
;; (require 'painting-theme)


(if (eq system-type 'gnu/linux)
    (+load-theme-from-selected)
  ;; (load-theme 'doom-solarized-dark-high-contrast t)
  (load-theme 'doom-solarized-light t)
  ;; (+load-theme-from-selected)
  ;; (load-theme 'painting t)
  ;; (load-theme 'spacemacs-dark t)
  ;; (require 'nano)
  )


;; 当前窗口透明度 (活动区 非活动区)
(set-frame-parameter (selected-frame) 'alpha (list 95 95))
;; 新窗口透明度 (活动区 非活动区)
(add-to-list 'default-frame-alist (cons 'alpha (list 95 95)))


;; all-the-icons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'all-the-icons)


(provide 'init-ui)
;;; init-ui.el ends here
