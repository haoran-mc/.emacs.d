;;; init-upkg.el --- Init use-package. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; @purcell --- Install into separate package dirs for each Emacs version
(let ((versioned-package-dir
    (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory)))
(setq package-user-dir versioned-package-dir))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives
    '(
      ;; ("gnu" . "https://mirrors.cloud.tencent.com/elpa/gnu/")
      ;; ("melpa" . "https://mirrors.cloud.tencent.com/elpa/melpa/")
      ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
      ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn//elpa/melpa/")
      )))

(require 'cl) ;;下面有需要用到common Lisp的函数，所以调用了common Lisp的库，例如loop for等

(defvar haoran/packages '(
                          company
                          hungry-delete
                          evil
                          evil-leader
                          evil-nerd-commenter
                          evil-tabs
                          evil-search-highlight-persist
                          smooth-scrolling
                          auto-yasnippet
                          org-bullets
                          swiper
                          counsel
                          ;;---------------------以上必备
                          smartparens
                          all-the-icons
                          dashboard
                          doom-modeline
                          use-package
                          web-mode
                          emmet-mode
                          css-mode
                          helm-ag
                          youdao-dictionary
                          ;;---------------------theme
                          dracula-theme
                          ) "Default packages")

(setq package-selected-packages haoran/packages)

(defun haoran/packages-installed-p()
  "nil is returned only if all packages are installed"
  (loop for pkg in haoran/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

;; If there is a package not installed, install it
(unless (haoran/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg haoran/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(provide 'init-upkg)
;;; init-upkg.el ends here
