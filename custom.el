;;; custom.el --- custom configuration -*- lexical-binding: t -*-

;;; Commentary:
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "0f4e8712faa97372f505cac514d11f7fa0891e1dfc8bdf03208247b31fa29a01" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(org-agenda-files
   '("~/haoran/no/org/sync-notes/e.观察世界/世界局势/宗教信仰.org" "/Users/haoran/haoran/no/org/org-directory/tasks/emacs.org" "/Users/haoran/haoran/no/org/org-directory/tasks/invest.org" "/Users/haoran/haoran/no/org/org-directory/tasks/learn.org" "/Users/haoran/haoran/no/org/org-directory/tasks/love.org" "/Users/haoran/haoran/no/org/org-directory/tasks/tasks.org" "/Users/haoran/haoran/no/org/org-directory/agenda/daily.org" "/Users/haoran/haoran/no/org/org-directory/agenda/distance.org" "/Users/haoran/haoran/no/org/org-directory/agenda/monthly.org" "/Users/haoran/haoran/no/org/org-directory/agenda/questions.org" "/Users/haoran/haoran/no/org/org-directory/agenda/toweek.org" "/Users/haoran/haoran/no/org/org-directory/agenda/weekly.org" "/Users/haoran/haoran/no/org/org-directory/agenda/yearly.org" "/Users/haoran/haoran/no/org/org-directory/work/docs.org" "/Users/haoran/haoran/no/org/org-directory/work/journay.org" "/Users/haoran/haoran/no/org/org-directory/work/todo.org") nil nil "Customized with use-package org-agenda")
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
