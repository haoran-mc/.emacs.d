;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;; Copyright (C) 2022  Haoran Liu

;; Author: HaoRan Liu <haoran.mc@outlook.com>
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

;; absolute path of the current file
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

(setq user-full-name "Haoran Liu")
(setq user-mail-address "haoran.mc@qq.com")

;; 启用多字节字符集支持，允许使用和显示各种字符，包括中文、日文、韩文等
(setq-default default-enable-multibyte-characters t)

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment "English") ;; 也可以设为 English，逻辑上使用 UTF-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Suppress GUI features and more
(setq use-dialog-box nil          ;; never pop dialog
      inhibit-x-resources t       ;; 不使用 X 资源文件
      inhibit-default-init t      ;; 禁用默认的初始化步骤，确保用户的设置不会被覆盖
      inhibit-startup-screen t    ;; 不显示启动屏幕
      inhibit-startup-message t   ;; 不显示启动消息
      inhibit-startup-buffer-menu t) ;; 启动时不显示缓冲区消息

;; Pixelwise resize 像素级缩放
;; 保证 mac 平台最大化窗口时无空隙
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

;; 设置Fringe的宽度
(fringe-mode '(6 . 6))  ; 左右各设置为6个像素的宽度

;; 设置Fringe的显示方式
(setq-default left-fringe-width 6)   ;; 设置左侧Fringe的宽度为6个像素
(setq-default right-fringe-width 6)  ;; 设置右侧Fringe的宽度为6个像素

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(global-so-long-mode 1)

;; 包含多版本文件的目录中，emacs 在加载文件时将优先选择最新的版本
(setq load-prefer-newer t)

;; Cutting and pasting use clipboard
(setq select-enable-clipboard t)

;; Delete selected part when type
(delete-selection-mode 1)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; 完全禁用响铃
(setq ring-bell-function 'ignore)

;; Smooth scroll & friends
(setq scroll-conservatively 101   ;; 控制滚动时的行为，如果设置为一个较大的值（例如 101），则 Emacs 会尽量保持在窗口中显示的内容不变，而不是让光标移动到窗口中央。这有助于使滚动更加平滑。
      ;; scroll-step 2
      ;; hscroll-step 2
      scroll-margin 2             ;; 滚动边缘的行数
      hscroll-margin 2            ;; 水平滚动边缘的列数
      scroll-preserve-screen-position 'always ;; 尽量保持屏幕上的内容不变，即使光标在窗口的边缘
      auto-window-vscroll nil)    ;; 禁用自动垂直滚动的功能

(setq-default fill-column 80)

(blink-cursor-mode 0)                   ;; 指针闪动，帮助我快速定位光标位置
(setq blink-cursor-blinks 100)          ;; 闪动 100 次
(transient-mark-mode 1)                 ;; 标记高亮
(setq-default comment-style 'indent)    ;; 设定自动缩进的注释风格
(setq default-major-mode 'text-mode)    ;; 设置默认的主模式为 TEXT 模式

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")
            (modify-syntax-entry ?- "w")))

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(setq initial-scratch-message
      (file-to-string
       (locate-user-emacs-file "resources/initial-scratch-message.txt")))

;; Sane defaults
(setq use-short-answers t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; Use TeX as default IM
;; (setq default-input-method "TeX")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

(setq confirm-kill-processes nil)       ;; 退出自动杀掉进程
(setq warning-minimum-level :error)     ;; 提高告警级别

;; Enable upcase and downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun my/suppress-package-cl-deprecated-warning (fn type message &rest args)
      "Suppress the legacy `cl' package deprecation warning."
      (unless (string-match-p "\\`Package cl is deprecated\\'" message)
            (apply fn type message args)))

;; Keep Emacs' original load handling and suppress only this one noisy warning.
(advice-add 'display-warning :around #'my/suppress-package-cl-deprecated-warning)


;; bookmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bookmark-default-file ran--bookmark-file)


;; saveplace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back to the previous position
(add-hook 'after-init-hook 'save-place-mode)


;; recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 600
      recentf-auto-cleanup 'never
      recentf-exclude '(;; Folders on MacOS start
                        "^/private/tmp/"
                        "^/var/folders/"
                        ;; Folders on MacOS end
                        "^/tmp/"
                        "/ssh\\(x\\)?:"
                        "/su\\(do\\)?:"
                        "^/usr/include/"
                        "/TAGS\\'"
                        "COMMIT_EDITMSG\\'"))


;; newcomment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-or-uncomment ()
  "Comment or uncomment the current line or region.

If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\s-*$"))
        (comment-dwim nil)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

(global-set-key [remap comment-dwim] 'comment-or-uncomment)


;; server-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use emacsclient to connect
(add-hook 'after-init-hook 'server-mode)


;; imenu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'imenu-after-jump-hook 'recenter)


;; simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show line/column number and more
;; show line/column/filesize in modeline
(setq line-number-mode t
      column-number-mode t
      size-indication-mode nil
      ;; No visual feedback on copy/delete.
      copy-region-blink-delay 0
      delete-pair-blink-delay 0
      ;; don't save current clipboard text before replacing it
      save-interprogram-paste-before-kill nil
      ;; eliminate duplicates
      kill-do-not-save-duplicates t
      ;; include '\n' when point starts at the beginning-of-line
      kill-whole-line t
      ;; show cwd when `shell-command' and `async-shell-command'
      shell-command-prompt-show-cwd t
      ;; show the name of character in `what-cursor-position'
      what-cursor-show-names t
      ;; List only applicable commands.
      ;;
      ;; ``` elisp
      ;; (defun foo ()
      ;;   (interactive nil org-mode)
      ;;   (message "foo"))
      ;; ```
      ;;
      ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
      read-extended-command-predicate #'command-completion-default-include-p)


;; hl-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'find-file-hook (lambda () (hl-line-mode 1))) ;; 高亮当前行
(setq-default cursor-in-non-selected-windows t) ;; 分屏时，未选中的窗格也显示光标


;; autorevert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自动更新 buffer
(require 'autorevert)
(setq auto-revert-verbose t)
(global-auto-revert-mode 1)


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq help-window-select t)


(provide 'init-base)
;;; init-base.el ends here
