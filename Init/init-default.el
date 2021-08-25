;; INIT-DEFAULT
;; Author: L.M.haoran <haoran.mc@outlook.com>
;; Copyright © 2021, L.M.haoran, all rights reserved.
;; Created: 2021-07-26 09:07 一
;; Summary:
;; Code:

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

;;----------------------------------------------------------------occur
;;自动抓取光标停留的单词，至于为什么叫这个函数名，dwim = do what I mean
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

;; init-default.el ends here
;;----------------------------------------------------------------
(provide 'init-default)
