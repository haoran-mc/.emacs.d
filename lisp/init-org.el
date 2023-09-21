;;; init-org.el --- Org mode configurations -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)
(require 'evil)

(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(use-package org
  :ensure nil
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images)
         (org-mode . visual-line-mode) ;; 折行
         (org-mode . (lambda()
                       (define-key evil-motion-state-map (kbd "RET") 'org-open-at-point)
                       (define-key evil-motion-state-map (kbd "C-c &") 'org-mark-ring-goto))))
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture))
  :init
  (require 'org-tempo) ;; <s
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :weight semi-bold :height 1.0))))
  (org-level-2 ((t (:inherit outline-2 :weight semi-bold :height 1.0))))
  (org-level-3 ((t (:inherit outline-3 :weight semi-bold :height 1.0))))
  (org-level-4 ((t (:inherit outline-4 :weight semi-bold :height 1.0))))
  (org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.0))))
  (org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.0))))
  (org-level-7 ((t (:inherit outline-7 :weight semi-bold :height 1.0))))
  (org-level-8 ((t (:inherit outline-8 :weight semi-bold :height 1.0))))
  (org-document-title ((t (:weight normal))))
  (org-link ((t (:inherit link :foreground "#2AA1AE" :weight semi-bold)))) ;; CUSTOM-COLOURS
  ;; 设置代码块用上下边线包裹
  ;; (org-block-begin-line ((t (:underline t :background unspecified))))
  ;; (org-block-end-line ((t (:overline t :underline nil :background unspecified))))
  :config
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
  :config
  ;; TODO I write it again as I don't know
  ;; why this configuration which in init-tools doesn't take effect
  (yas-global-mode 1)

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

  (add-hook 'org-mode-hook #'xah-show-formfeed-as-line)
  (add-hook 'emacs-lisp-mode-hook #'xah-show-formfeed-as-line)

  ;; learn from: https://github.com/lijigang/emacs.d
  (defface org-bold '((t :foreground "white"
                         :background "#282C34"
                         :weight bold
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
  :custom ;; base
  (org-directory "~/haoran/no/org/org-directory") ;; USER-DIRECTORY
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-use-property-inheritance t) ;; 子标题会继承父标题的属性
  (org-list-allow-alphabetical t) ;; 允许使用字母作为有序列表
  (org-return-follows-link nil) ;; 需要提前进入插入模式，没有自定义的 RET 方便
  (org-special-ctrl-a/e t) ;; 插入模式下的快捷键、跳到行首、行尾
  (org-special-ctrl-k t) ;; 插入模式下的快捷键，删除到行尾
  (org-catch-invisible-edits 'smart) ;; 对不可见编辑进行捕获的策略为 smart
  (org-use-sub-superscripts '{}) ;; 启用 Org Mode 的上下标功能
  (org-link-frame-setup '((vm . vm-visit-folder) ;; 全屏打开链接
                          (vm-imap . vm-visit-imap-folder)
                          (gnus . gnus)
                          (file . find-file)
                          (wl . wl-frame)))
  (org-M-RET-may-split-line '((header-line . nil))) ;; M-RET 不分割标题
  (org-startup-folded 'content) ;; 打开文件时只显示标题，不显示内容
  (org-hide-block-startup t) ;; 打开文件时，初始状态隐藏代码块
  (org-imenu-depth 6)
  :custom ;; log
  (org-log-done t) ;; 完成任务时自动记录时间
  (org-log-into-drawer t) ;; 将日志放在一个抽屉里 :LOGBOOK:
  (org-log-repeat 'time) ;; 任务可以重复，任务可以重复 DONE
  :custom ;; beautify
  (org-startup-indented t) ;; 标题的子级会相对于父级标题进行缩进，层次结构的视觉效果
  (org-ellipsis " ⤵ ") ;; 设置在折叠文本或被截断的文本中显示省略号的样式 ▼
  (org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))) ;; not know
  (org-hide-emphasis-markers t) ;; 隐藏强调标识符
  (org-emphasis-alist '(("*" org-bold)
                        ("/" italic)
                        ("_" underline)
                        ("=" (:foreground "#fef7ca"))
                        ("+" (
                              :foreground "dark gray"
                              :strike-through t))
                        ("~" org-code verbatim)))
  (org-goto-interface 'ortline-path-completion) ;; org-goto 命令的界面样式
  (org-fontify-todo-headline t) ;; TODO 标签美化
  (org-fontify-done-headline t) ;; DONE 标签美化
  :custom ;; priority
  (org-priority-highest ?A) ;; 最高优先级是字母 A
  (org-priority-lowest ?E) ;; 最低优先级是字母 E
  (org-priority-faces '((?A . 'all-the-icons-red)
                        (?B . 'all-the-icons-orange)
                        (?C . 'all-the-icons-yellow)
                        (?D . 'all-the-icons-green)
                        (?E . 'all-the-icons-blue)))
  :custom ;; image
  (org-image-actual-width '(500)) ;; 统一图片的宽度
  (org-startup-with-inline-images nil) ;; 每次打开文件时主动加载内联图片
  :custom ;; archive
  (org-archive-location "%s_archive::datetree/")
  :custom ;; latex
  ;; EXTERNAL-TOOLS
  (org-preview-latex-default-process 'imagemagick) ;; C-c C-x C-l 使用 imagemagick 作为预览公式图像的工具
  (org-latex-create-formula-image-program 'imagemagick) ;; 使用 imagemagick 作为生成公式图像的工具
  :custom ;; todo
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h!)" "WORK(i!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                            ("HOLD"       :foreground "#feb24c" :weight bold)
                            ("WORK"       :foreground "#0098dd" :weight bold)
                            ("DONE"       :foreground "#50a14f" :weight bold)
                            ("CANCELLED"  :foreground "#ff6480" :weight bold)))
  (org-tag-alist ;; 任务标签中添加 :w: 表示与工作相关
   '(("@工作" . ?w) ("@生活" . ?l) ("@学习" . ?s)))
  (org-columns-default-format ;; 使用 org-columns 在表格视图查看任务
   "%25ITEM %TODO %SCHEDULED %DEADLINE %3PRIORITY %TAGS %CLOCKSUM %EFFORT{:}")
  (org-use-fast-todo-selection 'expert) ;; 快速选择代办状态，输入 "C" 来选择 "CANCEL"
  :custom ;; tags, e.g. #+TAGS: keyword in your file
  (org-use-tag-inheritance nil)
  (org-agenda-use-tag-inheritance nil)
  (org-use-fast-tag-selection t)
  (org-fast-tag-selection-single-key t)
  :custom ;; export
  (org-export-in-background nil) ;; 不在后台进行导出操作，否则可能出现不一致的结果或意外的行为
  (org-export-with-sub-superscripts '{}) ;; 仅有使用 {} 包裹的表达式能够作为上下标导出
  (org-export-headline-levels 6) ;; 导出标题的层级限制
  (org-html-doctype "html5") ;; 导出的 HTML 文件的文档类型为 html5
  (org-html-coding-system 'utf-8) ;; 导出的 HTML 文件编码为 utf8
  (org-export-default-language "en") ;; 设置导出的语言为英语，在日期等地方会使用 "Monday"
  (org-export-with-section-numbers nil) ;; 不设置导出时包含章节编号
  (org-export-with-planning t) ;; 导出时是否包含计划信息
  (org-export-with-priority t) ;; 导出时是否包含优先级
  (org-export-preserve-breaks t) ;; 导出时保留换行符
  (org-export-with-toc t) ;; 导出时包含 toc
  (org-html-head-include-default-style nil) ;; 导出时不包含默认的 css 样式表，默认的样式表在 org 安装目录中
  (org-html-head-include-scripts nil) ;; 导出时不包含默认的 script 脚本文件
  (org-html-head "<link rel='stylesheet' href='org.css' type='text/css'/>")
  (org-html-text-markup-alist '((bold . "<b>%s</b>")
                                (code . "<code>%s</code>")
                                (italic . "<i>%s</i>")
                                (strike-through . "<del>%s</del>")
                                (underline . "<span class=\"underline\">%s</span>")
                                (verbatim . "<verbatim>%s</verbatim>"))))

;; beautify org star
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; ● ☰ ☷ ☲ ☵ ❖ ■ ◆ ▲ ▼ ▶ ✦ ✧ ✶ ▸ ▪ ▫ ◇ ◆ ☀ ☁ ☂ ☃ ☎ ☑ ☢ ☣ ☪ ☮ ☸ ☹ ☺ ☻ ☼
  (org-superstar-headline-bullets-list '("☯" "◉" "○" "❂" "❉" "✸"))
  (org-superstar-special-todo-items t) ;; 用于定义在标题行中特殊的待办事项标记的显示样式
  (org-superstar-prettify-item-bullets t) ;; 使用列表的美化
  (org-superstar-item-bullet-alist '((?- . ?•) (?* . ?–) (?+ . ?◦))))

;; Write codes in org-mode
(use-package org-src
  :ensure nil
  :bind (:map org-src-mode-map
              ;; consistent with separedit/magit
              ("C-c C-c" . org-edit-src-exit))
  :config
  (use-package ob-go
    :ensure t)
  :custom
  (org-src-fontify-natively t) ;; syntax code on org mode
  (org-src-tab-acts-natively t) ;; 使用代码块自己的缩进，而不是 org-mode 的缩进
  (org-src-preserve-indentation nil) ;; used with the sentence above
  (org-edit-src-content-indentation 2) ;; src content indent 2
  (org-confirm-babel-evaluate nil) ;; 执行代码块前是否确认
  (org-src-window-setup 'current-window) ;; 当编辑代码块时，在当前窗口显示
  (org-src-lang-modes '(("C"        . c) ;; 配置代码块的 major mode
                        ("C++"      . c++)
                        ("bash"     . sh)
                        ("cpp"      . c++)
                        ("dot"      . graphviz-dot) ;; was `fundamental-mode'
                        ("elisp"    . emacs-lisp)
                        ("ocaml"    . tuareg)
                        ("shell"    . sh)
                        ("go"       . go)
                        ("plantuml" . plantuml)))
  (org-babel-load-languages '((C          . t) ;; 哪些代码块可以在 org 中运行
                              (dot        . t)
                              (emacs-lisp . t)
                              (eshell     . t)
                              (python     . t)
                              (shell      . t)
                              (go         . t)
                              (plantuml   . t))))

;; Keep track of tasks
(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; update appt list every 5 minutes
  (run-at-time t 300 #'org-agenda-to-appt)
  (shut-up! #'org-agenda-to-appt)

  (define-key org-agenda-mode-map "h" 'evil-backward-char)
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)
  (define-key org-agenda-mode-map "l" 'evil-forward-char)
  (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map)
  :custom
  (org-agenda-files '("~/haoran/no/org/org-directory/tasks/"
                      "~/haoran/no/org/org-directory/agenda/"
                      "~/haoran/no/org/org-directory/work/"
                      )) ;; 此文件夹的日程将被 agenda 管理
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-span 'week) ;; 将 agenda 的默认时间跨度设置为「一周」
  (org-agenda-start-on-weekday 1) ;; 起始日期设为周一
  (org-agenda-log-mode-items '(clock)) ;; 仅在日程条目中显示 "clock" 类型的日志
  (org-agenda-include-all-todo t) ;; 将所有的待办事项包括在 agenda 中
  (org-agenda-time-leading-zero t) ;; 在时间格式中使用前导零，例如 "09:30" 而不是 "9:30"
  (calendar-holidays nil) ;; 禁用日历的假日显示，不在 agenda 中显示日历的假日
  (org-agenda-columns-add-appointments-to-effort-sum t) ;; 将 appointments 的时间包括在任务的工作量总和中
  (org-agenda-restore-windows-after-quit t) ;; 退出 agenda 视图后恢复到之前的窗口布局
  (org-agenda-window-setup 'current-window) ;; agenda 视图在当前窗口打开
  (org-agenda-custom-commands '(("c" "The most import priority!"
                                 ((tags-todo "+PRIORITY=\"A\"")))
                                ;; ...other commands here
                                )))

;; Create structured information quickly
(use-package org-capture
  :ensure nil
  :hook (org-capture-mode . org-capture-setup)
  :config
  (with-no-warnings
    (defun org-capture-setup ()
      (setq-local org-complete-tags-always-offer-all-agenda-tags t))

    (defun project-todo-org-file (headline)
      (let* ((file (expand-file-name "TODO.org" (projectile-acquire-root)))
             (buf (find-file-noselect file)))
        (set-buffer buf)
        ;; Set to UTF-8 because we may be visiting raw file.
        (setq buffer-file-coding-system 'utf-8-unix)
        (unless (org-find-exact-headline-in-buffer headline)
          (goto-char (point-max))
          (insert "* " headline)
          (org-set-tags (downcase headline))))))
  :custom
  (org-capture-use-agenda-date t) ;; capture 创建条目时使用 agenda 的日期
  (org-capture-templates-contexts nil) ;; 禁用 capture 模板的上下文功能，手动选择模板
  (org-capture-templates `(
                           ("a" "all in tasks") ;; task
                           ("ai" "inbox" entry (file+headline "tasks/tasks.org" "inbox")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           ("al" "learn" plain (file "tasks/learn.org")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n.")
                           ("ao" "love" plain (file "tasks/love.org")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n.")
                           ;; * go language -> ...
                           ;; * machine learning -> ...
                           ;; * emacs -> ...
                           ("at" "invest inbox" entry (file+headline "tasks/invest.org" "inbox")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           ;; * movie -> 《》
                           ;; * fruit
                           ;; * read news
                           ("ab" "invest book" entry (file+headline "tasks/invest.org" "book")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           ;; 汇编 -> 《汇编语言》
                           ;; 心理学 -> 《被讨厌的勇气》
                           ("af" "invest life" entry (file+headline "tasks/invest.org" "life")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           
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
                           
                           ("e" "emacs")
                           ("el" "emacs learn" entry (file+headline "tasks/emacs.org" "learn")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n.")
                           ("et" "emacs todo" entry (file+headline "tasks/emacs.org" "tasks")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           
                           ("w" "work")
                           ("wd" "work docs" plain (file "work/docs.org")
                            "* %^{title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           ("wj" "work journay" entry (file+datetree "work/journay.org") "* %<%H:%M> - %^{title}\n%?")
                           ("wt" "work todo" entry (file+headline "work/todo.org" "inbox")
                            "* TODO %^{title} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
                           
                           ("p" "project") ;; project
                           ("pb" "bug"           entry (function ,(lazy! (project-todo-org-file "Bugs")))          "* %?")
                           ("pf" "feature"       entry (function ,(lazy! (project-todo-org-file "Features")))      "* %?")
                           ("ps" "security"      entry (function ,(lazy! (project-todo-org-file "Security")))      "* %?")
                           ("pe" "enhancement"   entry (function ,(lazy! (project-todo-org-file "Enhancements")))  "* %?")
                           ("po" "optimization"  entry (function ,(lazy! (project-todo-org-file "Optimizations"))) "* %?")
                           ("pd" "documentation" entry (function ,(lazy! (project-todo-org-file "Documentation"))) "* %?")
                           ("pm" "miscellaneous" entry (function ,(lazy! (project-todo-org-file "Miscellaneous"))) "* %?"))))

(use-package ox
  :ensure nil
  :after org
  :config
  (add-to-list 'org-export-backends 'pandoc)
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-email t)
  (org-export-with-author t)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  ;; Use :eval never-export header argument to avoid evaluating.
  (org-export-use-babel t)
  (org-export-headline-levels 5)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  ;; (org-export-backends '(ascii html md icalendar man))) ; original value
  )

;; org mode 的附加包，有诸多附加功能
(use-package org-contrib
  :pin nongnu
  :ensure t
  :config
  (require 'org-checklist))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t))

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable))

(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle))

(require 'ext-plantuml)

(provide 'init-org)
;;; init-org.el ends here
