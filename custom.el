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
   '(company rainbow-mode live-py-mode yasnippet-snippets ob-go markdown-toc treemacs-persp treemacs-magit treemacs-projectile treemacs-evil gotest go-gen-test go-guru go-tag quelpa no-littering use-package))
 '(warning-suppress-log-types '((use-package) (use-package)))
 '(warning-suppress-types '((use-package))))
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
