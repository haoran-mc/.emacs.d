;;; custom.el --- custom configuration -*- lexical-binding: t -*-

;;; Commentary:
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sql-indent treemacs-all-the-icons imenu-list yasnippet-snippets yaml-mode which-key webpaste vertico valign use-package undo-fu tuareg try treemacs-projectile treemacs-evil spdx simple-httpd shackle separedit rust-mode rg ranger rainbow-mode rainbow-identifiers quickrun quelpa pyvenv protobuf-mode org-superstar org-download org-contrib orderless ob-go no-littering markdown-toc marginalia live-py-mode ligature hungry-delete htmlize hl-todo haskell-mode grip-mode graphviz-dot-mode gotest go-tag go-guru go-gen-test gcmh flycheck fanyi exec-path-from-shell evil-surround evil-collection embark-consult dumb-jump doom-themes doom-modeline diredfl diff-hl devdocs dashboard company citre cargo browse-at-remote bazel auctex-latexmk atomic-chrome all-the-icons ace-pinyin))
 '(writeroom-width 0.7))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-treemacs-file-face ((t nil)))
 '(epe-dir-face ((t (:inherit bold :foreground "gray"))))
 '(epe-git-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-host-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-time-face ((t (:foreground "darkorange"))))
 '(epe-pipeline-user-face ((t (:foreground "darkcyan"))))
 '(tab-line-tab ((t (:foreground red)))))

;;; custom.el ends here
