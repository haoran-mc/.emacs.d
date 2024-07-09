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

(require 'ox)

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
      org-html-validation-link nil
      org-html-text-markup-alist '((bold . "<b>%s</b>")
                                   (code . "<code>%s</code>")
                                   (italic . "<i>%s</i>")
                                   (strike-through . "<del>%s</del>")
                                   (underline . "<span class=\"underline\">%s</span>")
                                   (verbatim . "<verbatim>%s</verbatim>")))


;; https://emacs-china.org/t/org-docx/23409/2
(defun ran/org-pandoc-convert-to-docx ()
  "Convert current buffer file to docx format by Pandoc."
  (interactive)
  (let ((docx-file (concat (expand-file-name "~/haoran/no/org/export/pandoc-docx")
                           "/"
                           (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                           ".docx"))
        (command "pandoc")
        (refdoc (list "--reference-doc" (expand-file-name "templates/template.docx" user-emacs-directory))))
    (cond ((not buffer-file-name) (user-error "Must be visiting a file"))
          (t (let* ((buffer (generate-new-buffer " *Pandoc output*"))
                    (filename (list buffer-file-name))
                    (output (list "-o" docx-file))
                    (arguments (nconc filename refdoc output))
                    (exit-code (apply #'call-process command nil buffer nil arguments)))
               (cond ((eql 0 exit-code)
                      (kill-buffer buffer)
                      (message "Convert finished: %s" (cadr output)))
                     (t (with-current-buffer buffer
                          (goto-char (point-min))
                          (insert (format "%s\n%s\n\n" (make-string 50 ?=) (current-time-string)))
                          (insert (format "Calling pandoc with:\n\n%s\n\nFailed with error:\n\n"
                                          (mapconcat #'identity (cons command arguments) " ")))
                          (special-mode))
                        (pop-to-buffer buffer)
                        (error "Convert failed with exit code %s" exit-code))))))))

(defun ran/open-docx-with-default-app ()
  "Open DOCX file at point in Dired with the default application on macOS."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (message "Error: Not in Dired mode")
    (let ((file (dired-get-file-for-visit)))
      (if (and file (string-equal (file-name-extension file) "docx"))
          (start-process "open-docx" nil "open" (expand-file-name file))
        (message "Error: Not a DOCX file")))))

(provide 'init-ox)
;;; init-ox.el ends here
