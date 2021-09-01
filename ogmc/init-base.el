(setq user-full-name "L.M.haoran")
(setq user-mail-address "haoran.mc@outlook.com")
(set-language-environment "UTF-8");;设置默认语言编码

(global-auto-revert-mode t);;自动加载外部修改
(setq make-backup-files nil);;取消备份
(setq create-lockfiles nil);; 不生成锁文件
(setq auto-save-default nil);;取消自动保存
(fset 'yes-or-no-p 'y-or-n-p);;回答 yes/no 改成回答 y/n
(setq dired-recursive-deletes 'always);;对目录删除操作时始终递归
(setq dired-recursive-copies 'always);;对目录复制操作时始终递归
(put 'dired-find-alternate-file 'disabled nil);;buffer共用
(with-eval-after-load 'dired;;所以使用这一句
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file));;配合上上一条使用，不共用buffer
(setq x-select-enable-clipboard t);;使图形化emacs与系统剪切板共用
(save-place-mode t);;在下一次打开文件时恢复上次的光标位置
(setq-default tab-width 4);;制表符长度
(setq-default indent-tabs-mode nil) ;;使用空格缩进
(setq user-emacs-directory "~/.emacs.d/ogmc/bin")
(setq save-place-file "~/.emacs.d/ogmc/bin/saveplace")
(setq desktop-dirname "~/.emacs.d/ogmc/bin/desktop-save")
(setq recentf-save-file "~/.emacs.d/ogmc/bin/recentf")
(setq abbrev-file-name "~/.emacs.d/ogmc/bin/abbrev_defs")
(setq default-major-mode 'text-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode);;高亮匹配的括号

(setq scroll-margin 5 scroll-consrvatively 10000)
(require 'recentf);;emacs有recentf文件，使用require引入这个文件的功能
(recentf-mode 1)
(setq recentf-max-menu-items 25);;最近使用的最大的文件数

;;----------------------------------------------------------------
(provide 'init-base)
