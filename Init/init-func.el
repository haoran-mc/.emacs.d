;;----------------------------------------------------------------函数
(defun haoran/init-file()
  (interactive)
  (find-file "~/.emacs.d/Init/initiate.el"))

;;在浏览器中打开当前文件
(defun ogmc/open-in-browser()
  (lambda ())
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

(defun ogmc/open-in-browser()
  (lambda ())
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

(defun ogmc/create-scratch-buffer()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

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


;;C-M-\能够直接对全文格式化，不使用这个函数与下面一个函数的话，只能对选中的地方格式化
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

;;删除DOS系统下的^M，没有绑定按键，手动调用函数
(defun ogmc/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min));;跳转到文件开头
  (while (search-forward "\r" nil t) (replace-match "")));;windows下的换行符替换为空字符串

;;----------------------------------------------------------------
(provide 'init-func)
