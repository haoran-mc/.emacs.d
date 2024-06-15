;;; init-ox.el --- org export                        -*- lexical-binding: t; -*-

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

;; (add-to-list 'org-export-backends 'pandoc)

(setq org-export-use-babel t
      org-export-coding-system 'utf-8
      org-export-default-language "en" ;; 设置导出的语言为英语，在日期等地方会使用 "Monday"
      org-export-in-background nil ;; 不在后台进行导出操作，否则可能出现不一致的结果或意外的行为
      org-export-preserve-breaks t ;; 导出时保留换行符
      org-export-headline-levels 6 ;; 导出标题的层级限制
      org-export-with-tags 'not-in-toc
      org-export-with-email nil
      org-export-with-author nil
      org-export-with-drawers nil
      org-export-with-footnotes t
      org-export-with-smart-quotes t
      ;; Use :eval never-export header argument to avoid evaluating.
      org-export-with-broken-links 'mark
      org-export-with-sub-superscripts '{} ;; 仅有使用 {} 包裹的表达式能够作为上下标导出
      org-export-with-section-numbers nil ;; 不设置导出时包含章节编号
      org-export-with-planning t ;; 导出时是否包含计划信息
      org-export-with-priority t ;; 导出时是否包含优先级
      org-export-with-toc t ;; 导出时包含 toc
      org-html-doctype "html5" ;; 导出的 HTML 文件的文档类型为 html5
      org-html-coding-system 'utf-8 ;; 导出的 HTML 文件编码为 utf8
      org-html-head-include-default-style nil ;; 导出时不包含默认的 css 样式表，默认的样式表在 org 安装目录中
      org-html-head-include-scripts nil ;; 导出时不包含默认的 script 脚本文件
      org-html-text-markup-alist '((bold . "<b>%s</b>")
                                   (code . "<code>%s</code>")
                                   (italic . "<i>%s</i>")
                                   (strike-through . "<del>%s</del>")
                                   (underline . "<span class=\"underline\">%s</span>")
                                   (verbatim . "<verbatim>%s</verbatim>")))


(require 'ox-pandoc)


(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (concat ran--homedir "/.emacs.d/templates/template.docx")))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))


(provide 'init-ox)
;;; init-ox.el ends here
