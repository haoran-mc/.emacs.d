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

;;; Require:
(require 'org)
(require 'org-tempo) ;; <s
(require 'org-funcs)

;;; Code:
(defvar my/theme-fg (face-foreground 'default))

(defun my/set-org-bold-face-based-on-theme ()
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (foreground-color (if (eq bg-mode 'dark) "#ff4cff" "purple"))
         (underline-color (if (eq bg-mode 'dark) "red" "red")))
    (defface org-bold `((t :weight normal
                           :foreground ,foreground-color
                           :underline (:color ,underline-color :style line)
                           :overline nil))
      "Face for org-mode bold."
      :group 'org-faces)))

(my/set-org-bold-face-based-on-theme)

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
      org-file-apps '(("\\.png\\'"     . default)
                      (auto-mode       . emacs)
                      (directory       . emacs)
                      ("\\.mm\\'"      . default)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'"     . emacs)
                      ("\\.md\\'"      . emacs)
                      ;; ("\\.gif\\'"     . vanilla/open-and-play-gif-image)
                      ("\\.xlsx\\'"    . default)
                      ("\\.svg\\'"     . default)
                      ("\\.pptx\\'"    . default)
                      ("\\.docx\\'"    . default))
      ;; log: set Local Variables in site-learn.
      ;; will generate safe-local-variable-values in custom.el
      org-log-into-drawer nil ;; 将日志放在一个抽屉里 :LOGBOOK:
      org-log-done nil ;; 完成任务时自动记录时间
      org-log-note-headings nil ;; 清空注释标题格式
      ;; beautify
      org-startup-indented t ;; 标题的子级会相对于父级标题进行缩进，层次结构的视觉效果
      org-ellipsis " ⤵ " ;; 设置在折叠文本或被截断的文本中显示省略号的样式 ▼
      org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+")) ;; not know
      org-hide-emphasis-markers t ;; 隐藏强调标识符
      org-emphasis-alist `(("*" org-bold) ;; org-bold 使用 defface 自定义
                           ("/" (:foreground ,my/theme-fg :slant italic))
                           ("_" (:foreground ,my/theme-fg :underline t))
                           ("=" (:foreground "yellow" :background "#595530"))
                           ("+" (:foreground "dark gray" :strike-through t))
                           ("~" (:foreground "orange")))
      org-goto-interface 'ortline-path-completion ;; org-goto 命令的界面样式
      org-fontify-todo-headline nil ;; TODO 标签美化
      org-fontify-done-headline nil ;; DONE 标签美化
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
      ;; archive 因为 org-archive-location 会将子树的最高 level 设成 top level，所以尽量只 archive top level，保证 level 顺序
      org-archive-location "%s_archive::"
      ;; latex
      org-preview-latex-default-process 'imagemagick ;; C-c C-x C-l org-latex-preview 使用 imagemagick 作为预览公式图像的工具
      org-latex-create-formula-image-program 'imagemagick ;; 使用 imagemagick 作为生成公式图像的工具
      ;; todo
      org-todo-keywords ;; not use for todo instead of agenda
      '((sequence "TODO(t)"   ;; 优先处理
                  "HOLD(h!)"  ;; 做到一半，暂存后面再做，事项部分比较简单，剩余的没有能力继续完成
                  "|"
                  "DONE(d!)"     ;; 完成
                  "CANCEL(c@/!)" ;; 删除
                  ))
      ;; foreground should to be consistent with hl-todo
      org-todo-keyword-faces '(("TODO"    :foreground "#FF0000" :weight normal)
                               ("DONE"    :foreground "#5B6268" :weight normal)
                               ("CANCEL"  :foreground "#5B6268" :weight normal)
                               ("HOLD"    :foreground "#D0BF8F" :weight normal))
      ;; 真实的 tag 太多了，无法维护，尽量不使用
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


(require 'org-agenda)
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
(defun +org-agenda-T ()
  "display org-agenda T view, entries withspecial
TODO kwd."
  (interactive)
  (org-agenda nil "T"))


(require 'org-capture)
(setq org-capture-use-agenda-date t ;; capture 创建条目时使用 agenda 的日期
      org-capture-templates-contexts nil ;; 禁用 capture 模板的上下文功能，手动选择模板
      ;; 减少 capture 步骤：
      ;; • 不使用 tag 的选择 %^g
      ;; • todo 项不分散，只保留少数 todo 文件
      ;;
      ;; 使用文件分别任务类型：工作、Emacs、Technology、其他
      ;;
      ;; 1. 任务型："* TODO %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
      ;; 2. 摘记型（没有标题）："%<%Y.%m.%d %H:%M %a>\n%?\n."
      ;; 3. 摘记型（有标题）："* %<%Y.%m.%d %a %H:%M> - %^{title}\n%?"
      ;; 4. 故事型： "* %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
      org-capture-templates `(("j" "journay")
                              ("jd" "journay diary" entry (file+datetree "org-diary/diary.org")
                               "* %<%H:%M>\n%?\n")
                              ("jw" "journay work" entry (file+datetree "~/haoran/no/org/work-agenda/work-journay.org")
                               "* %<%H:%M> - %^{title}\n%?")
                              

                              ("t" "todo")
                              ("ti" "todo inbox, 待做全放在这里" entry (file+headline "org-task/t-all.org" "inbox")
                               "* TODO %<%y%m%d %H:%M %a> %^{title}\n%?"
                               :prepend t)
                              ("tc" "todo cs" entry (file+headline "org-task/t-cs.org" "inbox")
                               "* TODO %<%y%m%d %H:%M %a> %^{title}\n%?"
                               :prepend t)
                              ("te" "todo emacs" entry (file+headline "org-task/t-e.org" "inbox")
                               "* TODO %<%y%m%d %H:%M %a> %^{title}\n%?"
                               :prepend t)
                              ("tw" "todo work" entry (file+headline "~/haoran/no/org/work-agenda/work-todo.org" "inbox")
                               "* TODO %<%y%m%d %H:%M %a> %^{title}\n%?"
                               :prepend t)
                              

                              ("c" "capture")
                              ("ci" "capture inbox" entry (file+headline "org-capture/c-all.org" "inbox")
                               "* %<%Y.%m.%d %H:%M %a>\n%?\n."
                               :prepend t)
                              ("cz" "capture trivia, 小知识 - 荔枝病" plain (file "org-capture/c-小知识.org")
                               "* %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
                               :prepend t)
                              ("ce" "capture emacs" entry (file+headline "org-capture/c-emacs.org" "inbox")
                               "* %<%Y.%m.%d %H:%M %a>\n%?"
                               :prepend t)))



(require 'init-org-superstar)

(require 'init-ox)

(require 'init-plantuml) ;; require before org-src

(require 'init-org-src)

(require 'init-org-appear)

(require 'init-site)

(lazy-load-local-keys '(("C-<return>" . bookmark-jump)) org-mode-map "")
(lazy-load-local-keys '(("C-," . goto-last-change)) org-mode-map "goto-last-change")
(lazy-load-local-keys '(("C-j" . vanilla/merge-line-down)) org-mode-map "basic-tookit")
;; only full paths are supported
(lazy-load-local-keys '(("C-v" . vanilla/preview-file-link)) org-mode-map "org-funcs")

(lazy-load-local-keys '(("M-." . org-open-at-point) ;; xref-find-dfinitions
                        ("M-," . org-mark-ring-goto)) ;; xref-pop-marker-stack
                      org-mode-map "")

(lazy-load-local-keys '(("s-<return>" . org-insert-heading-respect-content)) org-mode-map "")

;; e for export
(lazy-load-set-keys '(("C-c e p" . +org-preview-in-browser)) org-mode-map)

;; i for insert
(defhydra hydra-org-insert (:body-pre (require 'org-insert) :color blue)
  ("!" vanilla/org-insert-stamp-inactive "inactive time" :exit t :column "org-insert")
  ("l" vanilla/dwim-create-link-with-datetime "datetime link" :exit t)
  ("i" vanilla/org-insert-image "image with name" :exit t)
  ("s" vanilla/org-insert-image-with-timestamp "image with time" :exit t)
  ("t" hl-todo-insert "todo insert" :column "insert"))
(lazy-load-local-keys '(("C-c i" . hydra-org-insert/body)) org-mode-map "")

;; n for narrow
(defhydra hydra-narrow (:color blue)
  ("s" org-narrow-to-subtree "narrow to subtree" :exit t :column "narrow")
  ("w" widen "widen" :exit t))
(lazy-load-set-keys '(("C-c n" . hydra-narrow/body)) org-mode-map)


(provide 'init-org)
;;; init-org.el ends here
