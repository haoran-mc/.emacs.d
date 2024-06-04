;;; init-idle.el --- load file when emacs idle       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(setq max-lisp-eval-depth 40000)        ;; lisp 最大执行深度
(setq max-specpdl-size 10000)           ;; 最大容量
(setq kill-ring-max 1024)               ;; 用一个很大的 kill ring. 防止不小心删掉重要的东西
(setq mark-ring-max 1024)               ;; 设置的 mark ring 容量
(setq eval-expression-print-length nil) ;; 设置执行表达式的长度没有限制
(setq eval-expression-print-level nil)  ;; 设置执行表达式的深度没有限制
(auto-compression-mode 1)               ;; 打开压缩文件时自动解压缩
(setq read-quoted-char-radix 16)        ;; 设置 引用字符 的基数
(setq global-mark-ring-max 1024)        ;; 设置最大的全局标记容量
(setq isearch-allow-scroll t)           ;; isearch 搜索时可以滚动屏幕
(setq enable-recursive-minibuffers t)   ;; minibuffer 递归调用命令
(setq history-delete-duplicates t)      ;; 删除 minibuffer 的重复历史
(setq minibuffer-message-timeout 1)     ;; 显示消息超时的时间

(setq message-log-max t)                ;; 设置 message 记录全部消息, 而不用截去
(setq require-final-newline nil)        ;; 不自动添加换行符到末尾, 有些情况会出现错误
(setq x-stretch-cursor t)               ;; 光标在 TAB 字符上会显示为一个大方块
(setq print-escape-newlines t)          ;; 显示字符窗中的换行符为 \n
(setq tramp-default-method "ssh")       ;; 设置传送文件默认的方法
(tooltip-mode -1)                       ;; 不要显示任何 tooltips
(setq package-archives                  ;; 设置中国的镜像源
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")))


(setq auto-revert-mode 1)               ;; 自动更新 buffer


;; Enable upcase and downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'cl-lib)
(require 'noflet)

(setq confirm-kill-processes nil)       ;; 退出自动杀掉进程

;; Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (require 'noflet)
  (noflet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))


;; hl-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-hl-line-mode 1)                 ;; 高亮当前行


;; hideshow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box nil))))

(defface hideshow-border-face
  '((((background light))
     :background "rosy brown" :extend t)
    (t
     :background "sandy brown" :extend t))
  "Face used for hideshow fringe."
  :group 'hideshow)

(define-fringe-bitmap 'hideshow-folded-fringe
  (vector #b00000000
          #b00000000
          #b00000000
          #b11000011
          #b11100111
          #b01111110
          #b00111100
          #b00011000))

(defun hideshow-folded-overlay-fn (ov)
  "Display a folded region indicator with the number of folded lines."
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
           (info (format " (%d)..." nlines)))
      ;; fringe indicator
      (overlay-put ov 'before-string (propertize " "
                                                 'display '(left-fringe hideshow-folded-fringe
                                                                        hideshow-border-face)))
      ;; folding indicator
      (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))

(setq hs-set-up-overlay #'hideshow-folded-overlay-fn)


;; whitespace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show trailing whitespaces
(dolist (mode-hook '(prog-mode-hook markdown-mode-hook conf-mode-hook))
  (add-hook mode-hook 'whitespace-mode))

(setq whitespace-style '(face trailing))


;; so-long ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-enable-so-long-maybe ()
  "Enable so-long-mode if the file size is greater than 1MB."
  (when (> (buffer-size) 1000000)
    (so-long-mode 1)))

(add-hook 'find-file-hook 'my-enable-so-long-maybe)


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq help-window-select t)


(provide 'init-idle)
;;; init-idle.el ends here
