;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t -*-

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


(setq minibuffer-eldef-shorten-default t) ;; shorten "(default ...)" to "[...]"


;; Persist history over Emacs restarts. Vertico sorts by history position.
(require 'savehist)
(savehist-mode 1)
(setq history-length 1000)


;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertical style minibuffer
(require 'vertico)
(add-hook 'after-init-hook 'vertico-mode)
(setq vertico-count 20)

(defun my/up-directory (path)
  "Move up a directory in PATH without affecting the kill buffer."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (let ((end (point)))
	    (re-search-backward "/.")
	    (forward-char)
	    (delete-region (point) end))))

(define-key vertico-map (kbd "S-<backspace>") #'my/up-directory)


;; orderless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'orderless)
(setq orderless-smart-case t
      completion-styles '(orderless basic)
      completion-category-defaults nil ;; use orderless only
      completion-category-overrides nil
      orderless-component-separator #'orderless-escapable-split-on-space)


;; marginalia ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show description on minibuffer, like this:
;; lisp/      drwxr-xr-x    192  19 mis ago
(require 'marginalia)
(add-hook 'vertico-mode-hook 'marginalia-mode)


;; pingyinlib ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support Pinyin first character match for orderless, avy etc.
(require 'pinyinlib)
;; make completion support pinyin, refer to
;; https://emacs-china.org/t/vertico/17913/2
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(add-to-list 'orderless-matching-styles 'completion--regex-pinyin)


;; consult ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provides commands for finding and completing
(with-eval-after-load 'consult
  ;; require
  (require 'consult-imenu)
  (with-eval-after-load 'org
    (require 'consult-org))

  ;; binding keys
  (define-key prog-mode-map (kbd "C-c C-j") 'consult-outline) ;; make sense?
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c C-j") 'consult-imenu))

  (with-eval-after-load 'org
    (define-key org-mode-map [remap org-goto] 'consult-org-heading))

  ;; config
  (consult-customize consult-git-grep
                     consult-grep ;; consult-ripgrep
                     consult-bookmark
                     consult-recent-file
                     consult-buffer
                     :sort nil
                     :preview-key nil)

  (consult-customize consult-fd
                     :sort t
                     :history 'consult--find-history) ;; remember recent find

  (consult-customize consult-theme
                     :preview-key
                     '("M-."
                       :debounce 0.5 "<up>" "<down>"
                       :debounce 1 any))

  ;; custom
  (setq consult-fontify-preserve nil      ;; 不保留文本高亮
        consult-async-min-input 2         ;; 两个字符开始搜索
        ;; 异步搜索和输入处理相关参数，我的macos性能足够实时处理，所以设置了足够小的时间间隔0.01，性能不够的机器调大间隔，给出推荐参数0.07
        consult-async-input-debounce 0.01 ;; 去抖动时间，判定用户的输入结束，然后发送给搜索软件rg。认为0.07秒已经小于用户的连续输入间隔，所以是实时搜索。性能不够时考虑增大此值，减慢发送
        consult-async-input-throttle 0.01 ;; 输入节流时间，搜索软件rg处理间隔，和抖动时间相同，所以实时搜索。搜索软件性能不够时增大此值，减慢查询
        consult-async-refresh-delay 0.01  ;; vertico列表的刷新时间间隔，这里设置的值比throttle小，所以每当throttle返回结果就刷新，vertico性能不够时增大此值，减慢刷新
        consult-ripgrep-args "rg --ignore-file=/Users/haoran/.emacs.d/.rgignore \
                                 --null             \
                                 --line-buffered    \
                                 --color=never      \
                                 --max-columns=1000 \
                                 --path-separator / \
                                 --smart-case       \
                                 --no-heading       \
                                 --with-filename    \
                                 --line-number      \
                                 --search-zip"
        consult-fd-args '((if (executable-find "fdfind" 'remote)
                              "fdfind" "fd")
                          "--ignore-file=/Users/haoran/.emacs.d/.fdignore \
                           --full-path \
                           --color=never")))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
