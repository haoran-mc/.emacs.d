;;; init-shell.el --- All about shell/term -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun shell-mode-common-init ()
  "The common initialization procedure for term/shell."
  (setq-local scroll-margin 0)
  (setq-local truncate-lines t)
  (setq-local global-hl-line-mode nil))

(defun shell-self-destroy-sentinel (proc _exit-msg)
  "Make PROC self destroyable."
  (when (memq (process-status proc) '(exit signal stop))
    (kill-buffer (process-buffer proc))
    (ignore-errors (delete-window))))

;; term, ansi-term, multi-term

;; the Emacs shell & friends
(use-package eshell
  :ensure nil
  :functions eshell/alias
  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)
                         (eshell/alias "e" "find-file $1")
                         (eshell/alias "fo" "find-file-other-window $1")
                         (eshell/alias "d" "dired $1")
                         (eshell/alias "l" "ls -lFh")
                         (eshell/alias "ll" "ls -l")
                         (eshell/alias "la" "ls -lAFh")
                         (eshell/alias "lr" "ls -tRFh")
                         (eshell/alias "lrt" "ls -lFcrt")
                         (eshell/alias "lsa" "ls -lah")))
  :config
  (with-no-warnings
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))
    (defalias 'eshell/c #'eshell/clear)

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
    (defalias 'eshell/e #'eshell/emacs)
    (defalias 'eshell/vi #'find-file)
    (defalias 'eshell/vim #'find-file)
    (defalias 'eshell/nvim #'find-file)
    (defalias 'eshell/q #'eshell/exit))

  ;; git clone git@github.com:manateelazycat/aweshell.git ~/.emacs.d/etc/
  (use-package aweshell
    :load-path "~/.emacs.d/site-lisp/aweshell"
    :init
    (require 'aweshell)
    (with-eval-after-load 'eshell
      (custom-set-faces
       '(epe-dir-face ((t (:inherit bold :foreground "gray"))))
       '(epe-git-face ((t (:foreground "skyblue"))))
       '(epe-pipeline-host-face ((t (:foreground "skyblue"))))
       '(epe-pipeline-user-face ((t (:foreground "darkcyan"))))
       '(epe-pipeline-time-face ((t (:foreground "darkorange"))))))
    :bind(("C-c e n"   . aweshell-new)
          ("C-c e t"   . aweshell-toggle)
          ("C-c e d"   . aweshell-dedicated-toggle)
          ("C-c e C-n" . aweshell-next)
          ("C-c e C-p" . aweshell-prev))
    :config
    (with-eval-after-load "esh-opt"
      (autoload 'epe-theme-lambda "eshell-prompt-extras")
      (setq eshell-highlight-prompt nil
            eshell-prompt-function 'epe-theme-pipeline)))
  :custom
  (eshell-banner-message ""))

(use-package em-rebind
  :ensure nil
  :commands eshell-delchar-or-maybe-eof)

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
              ([remap kill-region] . backward-kill-word)
              ([remap delete-char] . eshell-delchar-or-maybe-eof))
  :config
  ;; Delete the last "word"
  (dolist (ch '(?_ ?- ?.))
    (modify-syntax-entry ch "w" eshell-mode-syntax-table)))

;; The interactive shell.
;;
;; It can be used as a `sh-mode' REPL.
;;
;; `shell' is recommended to use over `tramp'.
(use-package shell
  :ensure nil
  :hook ((shell-mode . shell-mode-common-init)
         (shell-mode . revert-tab-width-to-default))
  :bind ("M-`" . shell-toggle) ;; was `tmm-menubar'
  :config
  ;; Correct indentation for `ls'
  (defun revert-tab-width-to-default ()
    "Revert `tab-width' to default value."
    (setq-local tab-width 8))
  :custom
  (shell-kill-buffer-on-exit t))

(provide 'init-shell)
;;; init-shell.el ends here
