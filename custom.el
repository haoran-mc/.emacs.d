;;; custom.el --- custom configuration -*- lexical-binding: t -*-

;;; Commentary:
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(awesome-tray-active-modules '("location" "belong" "file-path" "mode-name" "date"))
 '(awesome-tray-file-path-show-filename t)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(package-selected-packages
   '(go-guru hide-mode-line modern-cpp-font-lock ob-go marginalia markdown-preview-mode impatient-mode go-gen-test go-tag protobuf-mode treemacs-evil meow rainbow-mode treemacs-persp treemacs-magit treemacs-projectile treemacs htmlize window-numbering smartparens markdown-toc grip-mode go-playground fantom-theme modus-themes srcery-theme auto-yasnippet yasnippet posframe gotest go-impl go-fill-struct go-dlv youdao-dictionary valign embark-consult consult embark vertico diredfl pyvenv haskell-mode bazel tuareg cargo rust-mode cmake-font-lock cmake-mode bison-mode rmsbolt graphviz-dot-mode yaml-mode devdocs citre dumb-jump flycheck projectile quickrun hl-todo spdx browse-at-remote diff-hl magit lsp-mode company evil-collection undo-fu evil-surround evil atomic-chrome fanyi webpaste separedit gcmh avy rg which-key dashboard all-the-icons shackle doom-modeline doom-themes exec-path-from-shell try quelpa no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epe-dir-face ((t (:inherit bold :foreground "gray"))))
 '(epe-git-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-host-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-time-face ((t (:foreground "darkorange"))))
 '(epe-pipeline-user-face ((t (:foreground "darkcyan"))))
 '(tab-line-tab ((t (:foreground red))))
 '(variable-pitch ((t (:family "Verdana")))))

;;; custom.el ends here
