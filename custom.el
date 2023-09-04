;;; custom.el --- custom configuration -*- lexical-binding: t -*-

;;; Commentary:
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'smart nil nil "Customized with use-package org")
 '(package-selected-packages
   '(nov json-mode sql-indent treemacs-all-the-icons imenu-list yasnippet-snippets yaml-mode which-key webpaste vertico valign use-package undo-fu tuareg try treemacs-projectile treemacs-evil spdx simple-httpd shackle separedit rust-mode rg ranger rainbow-mode rainbow-identifiers quickrun quelpa pyvenv protobuf-mode org-superstar org-download org-contrib orderless ob-go no-littering markdown-toc marginalia live-py-mode ligature hungry-delete htmlize hl-todo haskell-mode grip-mode graphviz-dot-mode gotest go-tag go-guru go-gen-test gcmh flycheck fanyi exec-path-from-shell evil-surround evil-collection embark-consult dumb-jump doom-themes doom-modeline diredfl diff-hl devdocs dashboard company citre cargo browse-at-remote bazel auctex-latexmk atomic-chrome all-the-icons ace-pinyin))
 '(writeroom-width 0.7))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treemacs-directory-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
 '(treemacs-file-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
 '(treemacs-git-added-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-conflict-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-deleted-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-ignored-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-modified-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-renamed-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-unmodified-face ((t (:inherit treemacs-file-face))))
 '(treemacs-git-untracked-face ((t (:inherit treemacs-file-face))))
 '(treemacs-root-face ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
 '(variable-pitch ((t nil))))

;;; custom.el ends here
