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

(setq-default default-enable-multibyte-characters t)
(set-language-environment "English")

;; encoding
(prefer-coding-system 'utf-8)

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil          ;; never pop dialog
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t    ;; inhibit start screen
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; screen pos and size on startup
(setq initial-frame-alist
      '((left . 445) ;; left 55
        (top . 50)
        (width . 141)
        (height . 40)))

;; The nano style for truncated long lines.
(setq auto-hscroll-mode t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      ;; 设置缩放的模式，避免 mac 平台最大化窗口
      ;; 以后右边和下边有空隙
      frame-resize-pixelwise t)

(setq auto-save-default nil
      ;; No backup files
      make-backup-files nil
      ;; No lock files
      create-lockfiles nil)

;; Always load the newest file
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

;; No annoying bell
(setq ring-bell-function 'ignore)

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-conservatively 101
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-preserve-screen-position 'always)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

(setq-default fill-column 80)


(blink-cursor-mode 1)                   ;; 指针闪动，帮助我快速定位光标位置
(setq blink-cursor-blinks 100)          ;; 闪动 100 次
(transient-mark-mode 1)                 ;; 标记高亮
(global-subword-mode 1)                 ;; Word移动支持 FooBar 的格式
(setq-default comment-style 'indent)    ;; 设定自动缩进的注释风格
(setq default-major-mode 'text-mode)    ;; 设置默认地主模式为 TEXT 模式
;; 按照中文折行
(setq word-wrap-by-category t
      ;; paragraphs
      sentence-end-double-space nil)


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

;; bookmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bookmark-default-file haoran/bookmark-file)


;; saveplace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back to the previous position
(add-hook 'after-init-hook 'save-place-mode)


;; recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 300
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
      ;; confusing if no fringes (GUI only).
      visual-line-fringe-indicators '(nil right-curly-arrow)
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
