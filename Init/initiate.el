(setq custom-file (expand-file-name "~/.emacs.d/custom.el" user-emacs-directory))
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(load-file custom-file)

(load-file "~/.emacs.d/Init/init-base.el");;这个一定要在第一个位置
(load-file "~/.emacs.d/Init/init-func.el")
(load-file "~/.emacs.d/Init/init-plug.el");;这个一定要在base之后
(load-file "~/.emacs.d/Init/init-set.el")

(load-file "~/.emacs.d/Init-org/init-org.el")
(load-file "~/.emacs.d/Init-org/funcs.el")
(load-file "~/.emacs.d/Init-org/packages.el")
(load-file "~/.emacs.d/Init-org/config.el")


(load-file "~/.emacs.d/Init-misc/init-evil.el");;这个要在plug之后
(load-file "~/.emacs.d/Init-misc/funcs.el")
(load-file "~/.emacs.d/Init-misc/packages.el")
(load-file "~/.emacs.d/Init-misc/config.el")

(load-file "~/.emacs.d/Init-ui/init-dashboard.el")
(load-file "~/.emacs.d/Init-ui/init-ui.el");;这个无所谓，就放在最后吧
(load-file "~/.emacs.d/Init-ui/funcs.el")
(load-file "~/.emacs.d/Init-ui/packages.el")
(load-file "~/.emacs.d/Init-ui/config.el")

(load-file "~/.emacs.d/Init-better-defaults/init-default.el");;这个不能在set之前，其实也可以，但是normal下左右邻近括号不高亮而已
(load-file "~/.emacs.d/Init-better-defaults/funcs.el")
(load-file "~/.emacs.d/Init-better-defaults/packages.el")
(load-file "~/.emacs.d/Init-better-defaults/config.el")

(load-file "~/.emacs.d/Init-programming/funcs.el")
(load-file "~/.emacs.d/Init-programming/packages.el")
(load-file "~/.emacs.d/Init-programming/config.el")


(load-file "~/.emacs.d/Init/keybindings.el");;这个一定要在func, plug, set, 之后

(provide 'initiate)
