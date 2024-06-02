;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

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

(require 'org)

;; (add-hook 'org-mode-hook 'visual-line-mode) ;; 折行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(require 'org-tempo) ;; <s

(defun my-func/open-and-play-gif-image (file &optional link)
  "Open and play GIF image `FILE' in Emacs buffer.

Optional for Org-mode file: `LINK'."
  (let ((gif-image (create-image file))
		(tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
	(switch-to-buffer tmp-buf)
	(erase-buffer)
	(insert-image gif-image)
	(image-animate gif-image nil t)
	(local-set-key (kbd "q") 'bury-buffer)
	))
(setq org-file-apps '(("\\.png\\'"     . default)
                      (auto-mode       . emacs)
                      (directory       . emacs)
                      ("\\.mm\\'"      . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'"     . emacs)
                      ("\\.md\\'"      . emacs)
                      ("\\.gif\\'"     . my-func/open-and-play-gif-image)
                      ("\\.xlsx\\'"    . default)
                      ("\\.svg\\'"     . default)
                      ("\\.pptx\\'"    . default)
                      ("\\.docx\\'"    . default)))



;; https://github.com/lijigang/emacs.d
(defface org-bold '((t :weight normal
                       ;; :foreground "white" ;; dark color
                       ;; :background "#282C34"
                       :foreground "purple" ;; light color
                       ;; :background "#EFF1F2"
                       :underline (:color "red" :style line)
                       :overline nil))
  "Face for org-mode bold."
  :group 'org-faces)

;; Because spacemacs had different ideas about the verbatim background
;; (set-face-background 'org-bold "#fefefe")
;; (set-face-background 'org-verbatim "#fefefe")

;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
;; (add-hook 'org-babel-after-execute-hook 'org-toggle-latex-fragment 'append)
;; (add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)


;; base
(setq org-directory ran--org-directory
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-use-property-inheritance t ;; 子标题会继承父标题的属性
      org-list-allow-alphabetical t ;; 允许使用字母作为有序列表
      org-return-follows-link nil ;; 需要提前进入插入模式，没有自定义的 RET 方便
      ;; (org-special-ctrl-a/e t) ;; 插入模式下的快捷键、跳到行首、行尾
      ;; (org-special-ctrl-k t) ;; 插入模式下的快捷键，删除到行尾
      org-catch-invisible-edits 'smart ;; 对不可见编辑进行捕获的策略为 smart
      org-use-sub-superscripts '{} ;; 启用 Org Mode 的上下标功能
      org-link-frame-setup '((vm . vm-visit-folder) ;; 全屏打开链接
                             (vm-imap . vm-visit-imap-folder)
                             (gnus . gnus)
                             (file . find-file)
                             (wl . wl-frame))
      org-M-RET-may-split-line '((header-line . nil)) ;; M-RET 不分割标题
      org-startup-folded 'content ;; 打开文件时只显示标题，不显示内容
      org-hide-block-startup t ;; 打开文件时，初始状态隐藏代码块
      org-imenu-depth 6
      ;; log
      org-log-done t ;; 完成任务时自动记录时间
      org-log-into-drawer t ;; 将日志放在一个抽屉里 :LOGBOOK:
      org-log-repeat 'time ;; 任务可以重复，任务可以重复 DONE
      ;; beautify
      org-startup-indented t ;; 标题的子级会相对于父级标题进行缩进，层次结构的视觉效果
      org-ellipsis " ⤵ " ;; 设置在折叠文本或被截断的文本中显示省略号的样式 ▼
      org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")) ;; not know
      org-hide-emphasis-markers t ;; 隐藏强调标识符
      org-emphasis-alist '(("*" org-bold) ;; org-bold 在 :config 的 defface 中定义
                           ("/" italic)
                           ("_" underline)
                           ("=" (:foreground "orange" ;; light color
                                             :background "#EFF1F2"))
                           ;; ("=" (:foreground "#fef7ca")) ;; dark color
                           ("+" (:foreground "dark gray"
                                             :strike-through t))
                           ("~" org-code verbatim))
      org-goto-interface 'ortline-path-completion ;; org-goto 命令的界面样式
      org-fontify-todo-headline t ;; TODO 标签美化
      org-fontify-done-headline t ;; DONE 标签美化
      ;; priority
      org-priority-highest ?A ;; 最高优先级是字母 A
      org-priority-lowest ?E ;; 最低优先级是字母 E
      org-priority-faces '((?A . 'all-the-icons-red)
                           (?B . 'all-the-icons-orange)
                           (?C . 'all-the-icons-yellow)
                           (?D . 'all-the-icons-green)
                           (?E . 'all-the-icons-blue))
      ;; image
      org-image-actual-width '(565) ;; 统一图片的宽度
      org-startup-with-inline-images nil ;; 每次打开文件时主动加载内联图片
      ;; archive
      org-archive-location "%s_archive::datetree/"
      ;; latex
      ;; EXTERNAL-TOOLS
      org-preview-latex-default-process 'imagemagick ;; C-c C-x C-l 使用 imagemagick 作为预览公式图像的工具
      org-latex-create-formula-image-program 'imagemagick ;; 使用 imagemagick 作为生成公式图像的工具
      ;; todo
      org-todo-keywords ;; not use for todo instead of agenda
      '((sequence "TODO(t)"   ;; 优先处理
                  "WORK(w!)"  ;; 工作相关
                  "INBOX(i!)" ;; 待做盒子
                  "LONG(l!)"  ;; 长期跟踪
                  "HOLD(h!)"  ;; 做到一半，暂存后面再做，事项部分比较简单，剩余的没有能力继续完成
                  "|"
                  "DONE(d!)"        ;; 完成
                  "CANCELLED(c@/!)" ;; 删除
                  ))
      org-todo-keyword-faces '(("TODO"       :foreground "#FF0000" :weight bold)
                               ("DONE"       :foreground "#50a14f" :weight bold)
                               ("CANCELLED"  :foreground "#50a14f" :weight bold)
                               ("LONG"       :foreground "#D0BF8F" :weight bold)
                               ("HOLD"       :foreground "#D0BF8F" :weight bold)
                               
                               ("WORK"       :foreground "#FF0000" :weight bold)
                               ("INBOX"      :foreground "#FF0000" :weight bold))
      ;; tag 只用在 inbox 中，用来分类
      ;; 而「工作」只会收录在 work 文件夹下，且 agenda 也有专门入口，不需要增加 tag
      ;; 书籍不分技术书籍与其他书籍，行为（课程、阅读、学习某个技术）不做割裂
      org-tag-alist
      '(("@生活" . ?l)
        ("@课程" . ?k)
        ("@书籍" . ?b)
        ("@某项技术" . ?j))
      org-columns-default-format ;; 使用 org-columns 在表格视图查看任务
      "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}"
      org-use-fast-todo-selection 'expert ;; 快速选择代办状态，输入 "C" 来选择 "CANCEL"
      ;; tags, e.g. #+TAGS: keyword in your file
      org-use-tag-inheritance nil
      org-use-fast-tag-selection t
      org-fast-tag-selection-single-key t)


;; Keep track of tasks
(require 'init-macros)

(require 'org-agenda)

;; (add-hook org-agenda-finalize-hook 'org-agenda-to-appt)

;; update appt list every 5 minutes
(run-at-time t 300 #'org-agenda-to-appt)
(shut-up! #'org-agenda-to-appt)

(setq org-agenda-files ran--org-agenda-files
      org-agenda-block-separator ?─
      org-agenda-time-grid '((daily today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────"
      org-agenda-use-tag-inheritance nil
      org-agenda-span 'week ;; 将 agenda 的默认时间跨度设置为「一周」
      org-agenda-start-on-weekday 1 ;; 起始日期设为周一
      org-agenda-log-mode-items '(clock) ;; 仅在日程条目中显示 "clock" 类型的日志
      org-agenda-include-all-todo t ;; 将所有的待办事项包括在 agenda 中
      org-agenda-time-leading-zero t ;; 在时间格式中使用前导零，例如 "09:30" 而不是 "9:30"
      calendar-holidays nil ;; 禁用日历的假日显示，不在 agenda 中显示日历的假日
      org-agenda-columns-add-appointments-to-effort-sum t ;; 将 appointments 的时间包括在任务的工作量总和中
      org-agenda-restore-windows-after-quit t ;; 退出 agenda 视图后恢复到之前的窗口布局
      org-agenda-window-setup 'current-window ;; agenda 视图在当前窗口打开
      org-agenda-custom-commands '(("c" "The most import priority!"
                                    ((tags-todo "+PRIORITY=\"A\"")))
                                   ;; ...other commands here
                                   ))

(require 'org-capture)

;; (with-no-warnings
;;   (defun org-capture-setup ()
;;     (setq-local org-complete-tags-always-offer-all-agenda-tags t))
;;
;;   (defun project-todo-org-file (headline)
;;     (let* ((file (expand-file-name "TODO.org" (projectile-acquire-root)))
;;            (buf (find-file-noselect file)))
;;       (set-buffer buf)
;;       ;; Set to UTF-8 because we may be visiting raw file.
;;       (setq buffer-file-coding-system 'utf-8-unix)
;;       (unless (org-find-exact-headline-in-buffer headline)
;;         (goto-char (point-max))
;;         (insert "* " headline)
;;         (org-set-tags (downcase headline))))))


;; (add-hook org-capture-mode-hook 'org-capture-setup)


(setq org-capture-use-agenda-date t ;; capture 创建条目时使用 agenda 的日期
      org-capture-templates-contexts nil ;; 禁用 capture 模板的上下文功能，手动选择模板
      ;; 减少 capture 步骤：
      ;; • 减少 tag 的选择 %^g，只在 inbox 中使用 tag，做不到完全依赖文件分类
      ;; • 通过 INBOX、WORK todo关键字识别任务类别
      ;; • todo 项不要太分散，只保留少数 todo 文件
      ;; • 提示输入「优先[#A]」「学习」
      ;;
      ;; todo 关键字分类：
      ;; • WORK  工作相关
      ;; • TODO  紧急的待做
      ;; • INBOX 待做箱子，使用 [#A] [#B] ... 标记优先级
      ;; • LONG  长期跟踪，这种事情不会很多，手动管理，不通过capture增加
      org-capture-templates `(
                              ("i" "inbox" entry (file+headline "tasks/tasks.org" "inbox")
                               "* INBOX %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                               :prepend t)
                              
                              ("c" "sync-notes") ;; capture
                              ("ca" "capture stories, 故事" plain (file "~/haoran/no/org/sync-notes/b.故事/故事.org")
                               "* %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                               :prepend t) ;; 每次在最上面新增
                              ;; cb book
                              ("cd" "capture everything, 客观" plain (file "~/haoran/no/org/sync-notes/b.故事/常识（客观的）.org")
                               "%<%Y.%m.%d - %H:%M>\n%?\n."
                               :prepend t)
                              ("cc" "capture my ideas, 主观" plain (file "~/haoran/no/org/sync-notes/b.故事/观点（主观的）.org")
                               "%<%Y.%m.%d - %H:%M>\n%?\n."
                               :prepend t)
                              ("cn" "capture non-public information, 行业内幕" plain (file "~/haoran/no/org/sync-notes/e.观察世界/专业、职业发展/行业内幕.org")
                               "%<%Y.%m.%d - %H:%M>\n%?\n."
                               :prepend t)
                              ("cz" "capture trivia, 小知识" plain (file "~/haoran/no/org/sync-notes/e.观察世界/capture-小知识.org")
                               "* %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                               :prepend t)
                              
                              ("d" "diary")
                              ("dj" "diary journay" entry (file+datetree "~/haoran/no/org/diary/diary.org")
                               "* %<%H:%M>\n%?\n")
                              
                              ("e" "emacs inbox" entry (file+headline "tasks/emacs.org" "inbox")
                               "* INBOX %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n")
                              
                              ("w" "work")
                              ("wd" "work docs" plain (file "work/docs.org")
                               "* %<%Y.%m.%d %a %H:%M> - %^{title}\n%?"
                               :prepend t)
                              ("wj" "work journay" entry (file+datetree "work/journay.org") "* %<%H:%M> - %^{title}\n%?")
                              ("wt" "work todo「优先[#A]」「学习」"
                               entry (file "work/todo.org")
                               "* WORK %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                               :prepend t)
                              )
      )





(require 'init-org-superstar)
(require 'init-ox)

;; org mode 的附加包，有诸多附加功能
;; (use-package org-contrib
;;   :pin nongnu
;;   :ensure t
;;   :config
;;   (require 'org-checklist))

(require 'init-org-appear)

;; (use-package imenu-list
;;   :ensure t
;;   :commands (imenu-list-smart-toggle))

(require 'init-plantuml)

;; load when need
(require 'init-site)


;;;###autoload
(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line. Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 39 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))
;; ───────────────────────────────────────

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'xah-show-formfeed-as-line))
(add-hook 'emacs-lisp-mode-hook #'xah-show-formfeed-as-line)



(provide 'init-org)
;;; init-org.el ends here
