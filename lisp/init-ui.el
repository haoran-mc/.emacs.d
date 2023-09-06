;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; LOCAL-PACKAGES
(use-package awesome-tray
  :ensure nil
  :load-path "~/Documents/emacs/local-packages/awesome-tray"
  :when (display-graphic-p)
  :hook (after-init . awesome-tray-mode)
  :init
  (require 'awesome-tray)
  :custom
  (awesome-tray-active-modules '("location" "buffer-name"))
  (awesome-tray-update-interval 0.5))

;; LOCAL-PACKAGES
(use-package doom-themes
  :ensure t
  :when (display-graphic-p)
  :init
  (add-to-list 'load-path "~/Documents/emacs/local-packages/spacemacs-theme")
  (add-to-list 'load-path "~/Documents/emacs/local-packages/lazycat-theme")
  (add-to-list 'load-path "~/Documents/emacs/local-packages/nano-emacs")
  (require 'spacemacs-dark-theme)
  (require 'lazycat-dark-theme)
  ;; (require 'nano) ;; a great theme
  :config
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")

  (defvar pretty-dark-themes
    (list 'painting-dark 'modus-vivendi 'spacemacs-dark
          'doom-ayu-dark 'doom-ayu-mirage 'doom-badger
          'doom-challenger-deep  'doom-city-lights 'doom-dracula 'doom-gruvbox
          'doom-horizon 'doom-Iosvkem 'doom-material-dark 'doom-molokai
          'doom-monokai-spectrum
          'doom-moonlight 'doom-old-hope 'doom-one 'doom-opera
          'doom-outrun-electric 'doom-palenight 'doom-peacock 'doom-rouge
          'doom-snazzy 'doom-solarized-dark 'doom-solarized-dark-high-contrast
          'doom-sourcerer 'doom-spacegrey 'doom-tokyo-night 'doom-tomorrow-night
          'doom-vibrant 'doom-wilmersdorf 'doom-xcode 'doom-zenburn
          ))

  (defvar pretty-light-themes
    (list 'doom-ayu-light 'doom-flatwhite 'doom-gruvbox-light
          'doom-homage-white 'doom-monokai-classic 'doom-one-light
          'doom-opera-light 'doom-solarized-light 'doom-tomorrow-day
          'modus-operandi
          ))

  (defvar strange-themes
    (list 'doom-1337 'doom-dark+ 'doom-lantern 'doom-laserwave 'doom-manegarm
          'doom-miramare 'doom-nord-light 'doom-plain 'doom-plain-dark
          'doom-shades-of-purple
          ))

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

  :config
  ;; (+load-theme-from-selected)
  ;; (+load-theme-random)
  ;; (load-theme 'spacemacs-dark t)
  ;; (load-theme 'doom-one t)
  (lazycat-theme-load-dark)
  ;; (lazycat-theme-load-light)
  )

;; Customize popwin behavior
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules '((magit-status-mode    :select t :inhibit-window-quit t :same t)
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
                   ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t))))

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

;; show color when css
(use-package rainbow-mode
  :ensure t
  :defer t)

(use-package rainbow-identifiers
  :ensure t
  :defer t)

(provide 'init-ui)
;;; init-ui.el ends here
