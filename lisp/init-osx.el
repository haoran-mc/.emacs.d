;;; init-osx.el --- Tweaks for MacOS -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  ;; CUA for MacOS
  :bind (("H-h" . +scroll-right-half-page)
         ("H-l" . +scroll-left-half-page))
  :config
  ;; Make titlebar dark
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  ;; Useful when use an external keyboard
  (defun +osx-swap-option-and-command ()
    "Swap `mac-option-modifier' and `mac-command-modifier'."
    (interactive)
    (cl-rotatef mac-option-modifier mac-command-modifier)
    (message "mac-option-modifier: %s, mac-command-modifier: %s" mac-option-modifier mac-command-modifier))

  ;; Emoji support
  (let ((fonts '("Apple Color Emoji")))
    (cl-loop with script = (if (>= emacs-major-version 28) 'emoji 'unicode)
             for font in fonts
             when (member font (font-family-list))
             return (set-fontset-font t script (font-spec :family font) nil 'prepend)))
  :custom
  (mac-option-modifier 'hyper) ;; set option(key) as hyper(key)
  (mac-command-modifier 'meta) ;; set command(key) as meta(key)
  (delete-by-moving-to-trash t)
  ;; Curse Lion and its sudden but inevitable fullscreen mode!
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (ns-use-native-fullscreen t)
  ;; Visit files opened outside of Emacs in existing frame, not a new one
  (ns-pop-up-frames nil))

(provide 'init-osx)
;;; init-osx.el ends here
