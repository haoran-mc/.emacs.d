;;; init-base.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------解绑


;;----------------------------------------------------------------自带的好用的函数
(global-set-key (kbd "C-h C-f") 'find-function);;查找函数的位置
(global-set-key (kbd "C-h C-v") 'find-variable);;查找变量的位置
(global-set-key (kbd "C-h C-k") 'find-function-on-key);;按下一个按键，会找到被绑定的函数
(global-set-key (kbd "s-/") 'hippie-expand);;windows-/，一个代码补全函数
(global-set-key (kbd "C-w") 'backward-kill-word);;回退删除


;;----------------------------------------------------------------init-func
(global-set-key (kbd "C-M-\\") 'ogmc/indent-region-or-buffer);;一键格式化
(global-set-key (kbd "M-p") 'ogmc/open-in-browser)
;;(global-set-key (kbd "M-s o") 'occur-dwim);;本来就是这个按键，优化了这个函数而已
;;(global-set-key (kbd "M-s i") 'counsel-imenu);;列出函数


;;----------------------------------------------------------------plug
;;swiper
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-k") 'dired-up-directory))


;;----------------------------------------------------------------evil
;; (define-key evil-normal-state-map (kbd "gN") 'elscreen-create)
;; (define-key evil-normal-state-map (kbd "gt") 'elscreen-next)
;; (define-key evil-normal-state-map (kbd "gT") 'elscreen-previous)
;; (define-key evil-normal-state-map (kbd ">") 'evil-shift-right-line)
;; (define-key evil-normal-state-map (kbd "<") 'evil-shift-left-line)


;;----------------------------------------------------------------org
(if (not (equal window-system 'x))
    (with-eval-after-load 'org
      (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))


;;----------------------------------------------------------------
(provide 'init-keybindings)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
