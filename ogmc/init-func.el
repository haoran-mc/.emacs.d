;;----------------------------------------------------------------函数
(defun ogmc/init-file()
  (interactive)
  (find-file "~/.emacs.d/ogmc/initiate.el"))

(defun ogmc/open-in-browser ()
  (lambda ())
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


;;使终端中的emacs -nw剪切板与系统共用，需要下载xsel, xclip
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)))


(defun ogmc/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ogmc/indent-region-or-buffer () ;;一键格式化
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion;;记忆光标位置
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (ogmc/indent-buffer)
        (message "Indented buffer.")))))


(defun ogmc/open-in-browser ()
  (lambda ())
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


(defun ogmc/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;----------------------------------------------------------------
(provide 'init-func)
