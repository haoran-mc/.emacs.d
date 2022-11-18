;;; init-ui.el --- Theme, modeline and window behavior -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package doom-modeline
  :ensure t
  :unless (display-graphic-p)
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-project-detection 'relative-to-project)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-lsp nil)
  (doom-modeline-github nil)
  (doom-modeline-time t)
  (doom-modeline-env-version nil)
  (doom-modeline-height 21)
  (doom-modeline-persp-icon nil)
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

;; git clone --depth=1 https://github.com/manateelazycat/awesome-tray
(use-package awesome-tray
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/awesome-tray/"
  :when (display-graphic-p)
  :hook (after-init . awesome-tray-mode)
  :init
  (require 'awesome-tray)
  (custom-set-variables
   '(awesome-tray-active-modules '("location" "belong" "file-path" "mode-name" "date"))
   '(awesome-tray-file-path-show-filename t)))

(use-package emacs
  :ensure nil
  :unless (display-graphic-p)
  :config
  (load-theme 'doom-molokai t))

(use-package doom-themes
  :ensure t
  :when (display-graphic-p)
  :init
  (add-to-list 'load-path "~/.emacs.d/site-lisp/spacemacs-theme")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/lazycat-theme")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/painting-theme")
  (require 'painting-dark-theme)
  (require 'spacemacs-dark-theme)
  (require 'lazycat-dark-theme)
  (defvar pretty-dark-themes
    (list 'painting-dark 'modus-vivendi 'spacemacs-dark
          'doom-acario-dark 'doom-ayu-dark 'doom-ayu-mirage 'doom-badger
          'doom-challenger-deep  'doom-city-lights 'doom-dracula 'doom-gruvbox
          'doom-horizon 'doom-Iosvkem 'doom-material-dark 'doom-molokai
          'doom-monokai-octagon 'doom-monokai-pro 'doom-monokai-spectrum
          'doom-moonlight 'doom-old-hope 'doom-one 'doom-opera
          'doom-outrun-electric 'doom-palenight 'doom-peacock 'doom-rouge
          'doom-snazzy 'doom-solarized-dark 'doom-solarized-dark-high-contrast
          'doom-sourcerer 'doom-spacegrey 'doom-tokyo-night 'doom-tomorrow-night
          'doom-vibrant 'doom-wilmersdorf 'doom-xcode 'doom-zenburn
          ))
  (defvar pretty-light-themes
    (list 'doom-acario-light 'doom-ayu-light 'doom-flatwhite 'doom-gruvbox-light
          'doom-homage-white 'doom-monokai-classic 'doom-one-light
          'doom-opera-light 'doom-solarized-light 'doom-tomorrow-day
          'modus-operandi
          ))
  (defvar strange-themes
    (list 'doom-1337 'doom-dark+ 'doom-lantern 'doom-laserwave 'doom-manegarm
          'doom-miramare 'doom-nord-light 'doom-plain 'doom-plain-dark
          'doom-shades-of-purple
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
  (defun +load-theme-accoriding-time ()) ;; TODO
  :config
  ;; (+load-theme-random)
  ;; (+load-theme-from-selected)
  ;; (load-theme 'painting-dark t)
  (load-theme 'modus-vivendi t)
  ;; (load-theme 'modus-operandi t)
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

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-enable-variable-value-editing t))

;; Windows layout recorder
;;
;; You can still use `winner-mode' on Emacs 26 or early. On Emacs 27, it's
;; prefered over `winner-mode' for better compatibility with `tab-bar-mode'.
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-history-mode)
  :init
  (with-eval-after-load 'doom-themes
    (custom-set-faces
     '(tab-line-tab ((t (:foreground red))))))
  :custom
  (tab-bar-history-buttons-show nil))

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p)
  :demand t)

(use-package dashboard
  :ensure t
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "mark-github"      :height 1.0 :v-adjust  0.0) "★")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'all-the-icons-octicon) (all-the-icons-octicon "heart"            :height 1.1 :v-adjust  0.0) "♥")
                                        "Stars" "Show stars" (lambda (&rest _) (browse-url stars-url)))
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "report_problem" :height 1.1 :v-adjust -0.2) "⚑")
                                        "Issue" "Report issue" (lambda (&rest _) (browse-url issue-url)) warning)
                                       (,(if (fboundp 'all-the-icons-material) (all-the-icons-material "update"         :height 1.1 :v-adjust -0.2) "♺")
                                        "Update" "Update packages synchronously" (lambda (&rest _) (package-update-all nil)) success))))

  :hook ((after-init . dashboard-setup-startup-hook)
         (dashboard-mode . (lambda ()
                             (setq-local global-hl-line-mode nil))))
  :config
  (defconst homepage-url "https://github.com/haoran-mc/.emacs.d")
  (defconst stars-url (concat homepage-url "/stargazers"))
  (defconst issue-url (concat homepage-url "/issues/new"))
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-navigator t)
  (dashboard-items '((recents   . 10)
                     (projects  . 5)
                     (bookmarks . 5))))

;; show color when css
(use-package rainbow-mode
  :defer t)

(provide 'init-ui)

;;; init-ui.el ends here
