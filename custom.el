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
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "251ed7ecd97af314cd77b07359a09da12dcd97be35e3ab761d4a92d8d8cf9a71" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aafe3cbd4d72f793ef5d1a2e6beb0130c79df5b596baeb74336fc24bdc189e9a" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ae4aa4bf7418af9a2a8a0e9d172895a2f25fe725790fed3f259bba53159a8264" "e9b6d5c2c76f4ef5d6f514ef49de5b497f09873f4047ba30105f34eecc3ed6f6" "0662e3fbfe80821ee133053f9a68f87fa83331bc45add5f85e19633597d41ffc" "0c2d7f410f835d59a0293f2a55744e9d3be13aab8753705c6ad4a9a968fb3b28" "4cc1cc7efd5c2362ef684657eec7d7e482223b1def4edeb0fab52ba1d334d38a" "c5a81a42df109b02a9a68dfe0ed530080372c1a0bbcb374da77ee3a57e1be719" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" default))
 '(markdown-command "/usr/bin/pandoc")
 '(markdown-live-preview-delete-export 'delete-on-destroy)
 '(org-agenda-files '("~/haoran/Notes/Org/site/org/Editor-Emacs-学会emacs.org") nil nil "Customized with use-package org-agenda")
 '(package-selected-packages
   '(marginalia markdown-preview-mode impatient-mode go-gen-test go-tag protobuf-mode treemacs-evil meow rainbow-mode treemacs-persp treemacs-magit treemacs-projectile treemacs htmlize window-numbering smartparens markdown-toc grip-mode go-playground fantom-theme modus-themes srcery-theme auto-yasnippet yasnippet posframe gotest go-impl go-fill-struct go-dlv youdao-dictionary valign embark-consult consult embark vertico diredfl pyvenv haskell-mode bazel tuareg cargo rust-mode cmake-font-lock cmake-mode bison-mode rmsbolt graphviz-dot-mode yaml-mode devdocs citre dumb-jump flycheck projectile quickrun hl-todo spdx browse-at-remote diff-hl magit lsp-mode company evil-collection undo-fu evil-surround evil atomic-chrome fanyi webpaste separedit gcmh avy rg which-key dashboard all-the-icons shackle doom-modeline doom-themes exec-path-from-shell try quelpa no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfrs-border-color ((t (:background "#a8a8a8"))))
 '(epe-dir-face ((t (:inherit bold :foreground "gray"))))
 '(epe-git-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-host-face ((t (:foreground "skyblue"))))
 '(epe-pipeline-time-face ((t (:foreground "darkorange"))))
 '(epe-pipeline-user-face ((t (:foreground "darkcyan"))))
 '(tab-line-tab ((t (:foreground red)))))

;;; custom.el ends here
