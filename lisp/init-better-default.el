;;; init-better-default.el --- Basic configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------dired-mode
(setq dired-recursive-deletes 'always);;对目录删除操作时始终递归
(setq dired-recursive-copies 'always);;对目录复制操作时始终递归
(put 'dired-find-alternate-file 'disabled nil);;buffer共用
(with-eval-after-load 'dired;;所以使用这一句
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file));;配合上上一条使用，不共用buffer
(with-eval-after-load 'dired
  (define-key dired-mode-map "n" 'evil-search-next)
  (define-key dired-mode-map "N" 'evil-search-previous))


;;----------------------------------------------------------------括号匹配
(setq show-paren-delay 0);;默认情况下高亮匹配括号会有延迟
(show-paren-mode t);;高亮匹配括号

;; normal模式下仍然匹配邻近括号
(defadvice show-paren-function (around fix-show-paren-function activate)
  (cond ((looking-at-p "\\s(") ad-do-it)
        (t (save-excursion
             (ignore-errors (backward-up-list))
             ad-do-it))))


;;好像是增强补全，而且还很不错的样子，绑定了一个快捷键s-/
(setq hippie-expand-try-functions-list '(try-expand-dabbrev ;;路径补全
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

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


;; 先插入defaults-elisp.el中的snippet，然后调用autoinsert-yas-expand函数
;; autoinsert-yas-expand函数会先清空文件内容，然后再插入buffer-string
;; (yas-expand-snippet ";; Bah-da $1 Bing")  ;; buffer-string可由此句生成，是根据情况自定义的
;; 如果不(yas-expand-snippet ";; Bah-da $1 Bing")，那么就只是插入default中的内容
(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;----------------------------------------------------------------occur
;;自动抓取光标停留的单词
(defun occur-dwim()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))


(provide 'init-better-default)
;;; init-better-default.el ends here
