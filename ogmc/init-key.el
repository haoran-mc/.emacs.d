;;----------------------------------------------------------------解绑
(global-set-key (kbd "C-l") nil)
(global-set-key (kbd "C-l r") 'recentf-open-files)


;;----------------------------------------------------------------init-func
(global-set-key (kbd "C-l i") 'ogmc/init-file)
(global-set-key (kbd "C-M-\\") 'ogmc/indent-region-or-buffer);;一键格式化
(global-set-key (kbd "M-p") 'ogmc/open-in-browser)


;;----------------------------------------------------------------plug
;;swiper
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)


;;----------------------------------------------------------------evil
(define-key evil-normal-state-map (kbd "gN") 'elscreen-create)
(define-key evil-normal-state-map (kbd "gn") 'elscreen-next)
(define-key evil-normal-state-map (kbd "gp") 'elscreen-previous)
(define-key evil-normal-state-map (kbd ">") 'evil-shift-right-line)
(define-key evil-normal-state-map (kbd "<") 'evil-shift-left-line)


;;----------------------------------------------------------------org
(if (not (equal window-system 'x))
    (with-eval-after-load 'org
      (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))


;;----------------------------------------------------------------
(provide 'init-key)
