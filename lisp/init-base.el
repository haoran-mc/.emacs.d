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
(setq user-mail-address "haoran.mc@outlook.com")

;; 启用多字节字符集支持，允许使用和显示各种字符，包括中文、日文、韩文等
(setq-default default-enable-multibyte-characters t)
(set-language-environment "English")

;; encoding
(prefer-coding-system 'utf-8)

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

;; 包含多版本文件的目录中，emacs 在加载文件时将优先选择最新的版本
(setq load-prefer-newer t)

;; Cutting and pasting use clipboard
(setq select-enable-clipboard t)

;; Delete selected part when type
(delete-selection-mode 1)

;; 使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches t)

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


(defun fixed-do-after-load-evaluation (abs-file)
  "Override `do-after-load-evaluation' and run additional functions based on file name."
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
  (run-hook-with-args 'after-load-functions abs-file))

;; remove *Messages* warning: "Package cl is deprecated"
(advice-add 'do-after-load-evaluation :override #'fixed-do-after-load-evaluation)


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

(provide 'init-base)
;;; init-base.el ends here
