(setq custom-file (expand-file-name "~/.emacs.d/Init/custom.el" user-emacs-directory))
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(load-file custom-file)

(load-file "~/.emacs.d/Init/init-base.el");;这个一定要在第一个位置
(load-file "~/.emacs.d/Plug/init-plug.el");;这个一定要在base之后
(load-file "~/.emacs.d/Init/init-func.el")
(load-file "~/.emacs.d/Init/init-org.el")
(load-file "~/.emacs.d/Plug/init-dashboard.el")
(load-file "~/.emacs.d/Plug/init-set.el")
(load-file "~/.emacs.d/Plug/init-evil.el");;这个要在plug之后
(load-file "~/.emacs.d/ogmc/init-link.el")
(load-file "~/.emacs.d/Init/init-key.el");;这个一定要在func, plug, set, 之后
(load-file "~/.emacs.d/Init/init-ui.el");;这个无所谓，就放在最后吧
(load-file "~/.emacs.d/Init/init-default.el");;这个不能在set之前，其实也可以，但是normal下左右邻近括号不高亮而已

(provide 'initiate)
