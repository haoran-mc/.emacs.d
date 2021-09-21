;;; init-base.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'init-vars)
(require 'recentf)

;;;;;; Switch Git Bash
(when *is-win*
  (setq explicit-shell-file-name
        "C:/Program Files/Git/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "C:/Program Files/Git/bin"))

;;;;;; Some Better Settings

(setq user-full-name "L.M.haoran"
      user-mail-address "haoran.mc@outlook.com"
      x-select-enable-clipboard t
      scroll-margin 5 scroll-conservatively 10000
      recentf-max-menu-items 1024)

(setq inhibit-startup-screen t
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      ring-bell-function 'ignore)

(setq-default
  initial-scratch-message (concat ";; Happy hacking, Gnu emacs :)\n\n")
  line-spacing 0.1
  truncate-lines t
  indent-tabs-mode nil ;; Disable TABs.
  word-wrap t)

(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil);;indent in blank space
(setq-default tab-width 4)
(recentf-mode 1)
(save-place-mode t);;Restore cursor position

(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Turn on mouse in console mode
(xterm-mouse-mode t)

;; (progn
;;   "Show time in 24h formats."
;;   (setq display-time-24hr-format t)
;;   (setq system-time-locale "C")
;;   (display-time))

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(defalias 'list-buffers 'ibuffer)

(progn
  "Set coding system."
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

(when *is-win*
  ;; (when (member "Consolas" (font-family-list))
  ;; (set-frame-font "consolas-12" t t))
  (when (member "Monaco" (font-family-list))
    (set-frame-font "Monaco-10.5" t t))
  (when (member "楷体" (font-family-list))
    (set-fontset-font t 'han "楷体-12")))

(when *is-nux*
  (when (member "Hack" (font-family-list))
    (set-frame-font "Hack-11.5" t ))
  (when (member "WenQuanYi Micro Hei Mono" (font-family-list))
    (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-13.5")))

;;;;;; Some Basic Modes

(require 'dired-x)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)

(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'recentf-mode)
;; (add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; (add-hook 'after-init-hook 'electric-indent-mode')

;; FUCK U DESKTOP-SAVE-MODE !
;; (defvar desktop-path (list user-emacs-directory))
;; (defvar desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)

(add-hook 'after-init-hook 'show-paren-mode)
;; @zilongshanren
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
               "Highlight enclosing parens."
               (cond ((looking-at-p "\\s(") (funcall fn))
                     (t (save-excursion
                          (ignore-errors (backward-up-list))
                          (funcall fn)))))


;;;;;; Some Basic Functions

(defun org-open-at-point-and-delete-other-windows ()
  "Open link file and just keep the goal file."
  (interactive)
  (org-open-at-point)
  (delete-other-windows))

;; @purcell
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited?"))
  (when (yes-or-no-p (format "really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; @purcell
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "snew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


(provide 'init-base)
;;; init-base.el ends here
