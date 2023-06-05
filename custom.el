;;; custom.el --- custom configuration -*- lexical-binding: t -*-

;;; Commentary:
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(awesome-tray-file-path-show-filename nil)
 '(custom-safe-themes
   '("a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(package-selected-packages
   '(ligature ranger company rainbow-mode live-py-mode yasnippet-snippets ob-go markdown-toc treemacs-persp treemacs-magit treemacs-projectile treemacs-evil gotest go-gen-test go-guru go-tag quelpa no-littering use-package))
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
