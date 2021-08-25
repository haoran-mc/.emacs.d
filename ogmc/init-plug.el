(when (>= emacs-major-version 24);;emacs版本号大于24时，会将下面源添加到emacs的package源
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(require 'cl) ;;下面有需要用到common Lisp的函数，所以调用了common Lisp的库，例如loop for等

(defvar haoran/packages '( ;;定义一个package的列表
                          company                 ;;代码补全
                          hungry-delete           ;;智能删除空格
                          evil                    ;;vim
                          evil-leader             ;;vim leader key
                          evil-nerd-commenter     ;;注释
                          evil-tabs               ;;enable evil tabs
                          evil-search-highlight-persist
                          smooth-scrolling        ;;smooth-scrolling
                          auto-yasnippet          ;;snippet，org自动生成模板需要
                          org-bullets             ;;更换org标题级别图形样式
                          swiper                  ;;C-s优化搜索，C-f优化，rencent文件优化
                          counsel                 ;;swiper依赖
                          ;;---------------------theme
                          ) "Default packages")

(setq package-selected-packages haoran/packages);;这样使用autoremove就会根据上面haoran/package中的包删除了

(defun haoran/packages-installed-p() ;;判断上面定义的package列表中package是否全部安装，如果全部安装就会返回nil
  (loop for pkg in haoran/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (haoran/packages-installed-p) ;;如果上面判断有没安装的package，那么安装它
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg haoran/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;----------------------------------------------------------------
(provide 'init-plug)
