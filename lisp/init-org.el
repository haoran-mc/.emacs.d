;; INIT-ORG
;; Author: L.M.haoran <haoran.mc@outlook.com>
;; Copyright © 2021, L.M.haoran, all rights reserved.
;; Created: 2021-09-22 01:09 Wed
;; Summary:
;; Code:

(require 'org)
(setq org-src-fontify-natively t);;使org文件中的#+BEGIN_SRC也有高亮
(setq org-M-RET-may-split-line '((header-line . nil)));;M-RET不分割
(setq org-ellipsis "..")
(setq org-startup-folded 'content);; 只显示标题
(load-file "~/.emacs.d/ogmc/init-link.el")

;; 打开链接全屏，不然就会分屏打开
(setq org-link-frame-setup
      '((vm . vm-visit-folder)
        (vm-imap . vm-visit-imap-folder)
        (gnus . gnus)
        (file . find-file)
        (wl . wl-frame)))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0 :foreground "#FD971F")))) 
 '(org-level-2 ((t (:inherit outline-2 :height 1.0 :foreground "#A6E22E")))) 
 '(org-level-3 ((t (:inherit outline-3 :height 1.0 :foreground "#66D9EF")))) 
 '(org-level-4 ((t (:inherit outline-4 :height 1.0 :foreground "#E6DB74")))) 
 '(org-level-5 ((t (:inherit outline-5 :height 1.0 :foreground "#A1EFE4")))) 
 '(org-level-6 ((t (:inherit outline-6 :height 1.0 :foreground "#A6E22E")))) 
 '(org-level-7 ((t (:inherit outline-7 :height 1.0 :foreground "#F92672")))) 
 '(org-level-8 ((t (:inherit outline-8 :height 1.0 :foreground "#66D9EF")))))

(require 'org-tempo);;添加这一句之后就可以<s<TAB>了

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-bullet-list '("☯" "◉" "○" "✿" "❀" "◇")))

;; init-org.el ends here
;;----------------------------------------------------------------
(provide 'init-org)
