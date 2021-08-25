(setq inferior-ess-r-program "R")

(setq custom-file (expand-file-name "~/.emacs.d/ogmc/custom.el" user-emacs-directory))
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(load-file custom-file)

(load-file "~/.emacs.d/ogmc/init-ui.el")
(load-file "~/.emacs.d/ogmc/init-base.el")
(load-file "~/.emacs.d/ogmc/init-func.el")
(load-file "~/.emacs.d/ogmc/init-org.el")
(load-file "~/.emacs.d/ogmc/init-plug.el")
(load-file "~/.emacs.d/ogmc/init-set.el")
(load-file "~/.emacs.d/ogmc/init-evil.el")
(load-file "~/.emacs.d/ogmc/init-link.el")
(load-file "~/.emacs.d/ogmc/init-key.el")

;;----------------------------------------------------------------
(provide 'initiate)
