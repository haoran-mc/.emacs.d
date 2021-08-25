;;----------------------------------------------------------------config
;;company
(global-company-mode t)
(setq-default company-idle-delay 0.08)
(setq-default company-minimum-prefix-length 2)


;;hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)


;;smooth-scroll
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)


;;auto-yasnippet，自动生成org模板
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

;; 先插入defaults-elisp.el中的snippet，然后调用autoinsert-yas-expand函数
;; autoinsert-yas-expand函数会先清空文件内容，然后再插入buffer-string
;; (yas-expand-snippet ";; Bah-da $1 Bing")  ;; buffer-string可由此句生成，是根据情况自定义的
;; 如果不(yas-expand-snippet ";; Bah-da $1 Bing")，那么就只是插入default中的内容
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;autoinsert
(require 'autoinsert)
;; Don't want to be prompted before insertion:
(setq auto-insert-query nil)
(setq auto-insert-directory (locate-user-emacs-file "templates"))
(add-hook 'find-file-hook 'auto-insert)
(auto-insert-mode 1)
(define-auto-insert "\\.el$" [ "~/.emacs.d/Bin/snippets/defaults-elisp.el" autoinsert-yas-expand ])
(define-auto-insert "\\.org$" ["~/.emacs.d/Bin/snippets/default-org.el" autoinsert-yas-expand])


;; org-bullets
;; (install-pkg 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))
(if (equal window-system 'x)
    (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1))))
(setq org-bullets-bullet-list '("◉" "☯" "○" "✿" "❀" "◇"))


;;swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;----------------------------------------------------------------
(provide 'init-set)
