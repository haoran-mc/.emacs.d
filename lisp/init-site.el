;;; init-site.el --- site built by org               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  HaoRan Liu

;; Author: HaoRan Liu <haoran.mc@gmail.com>
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
(require 'ox-html)

(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        ))

(eval-after-load 'ox-html
  (setq user-full-name "Haoran Liu"))

(use-package ox-publish
  :after org
  :config
  (defun +save-and-publish-website()
    "Save all buffers and publish."
    (interactive)
    (when (yes-or-no-p "Really save and publish current project?")
      (save-some-buffers t)
      (org-publish-project "website" t)
      (message "Site published done.")))

  (defun +save-and-publish-statics ()
    "Just copy statics like js, css, and image file .etc."
    (interactive)
    (org-publish-project "statics" t)
    (message "Copy statics done."))

  (defun +save-and-publish-rstatics ()
    "Just copy statics like js, css, and image file .etc.
Which is a reverse operation of `save-and-publish-statics'."
    (interactive)
    (org-publish-project "rstatics" t)
    (message "Copy rstatics done."))

  (defun +delete-org-and-html ()
    "Delete current org and the relative html when it exists."
    (interactive)
    (when (yes-or-no-p "Really delete current org and the relative html?")

      (let ((fileurl (concat "~/haoran/n/Org/site/public" (file-name-base (buffer-name)) ".html")))
        (if (file-exists-p fileurl)
            (delete-file fileurl))
        (delete-file (buffer-file-name))
        (kill-this-buffer)
        (message "Delete org and the relative html done."))))

  (defun +just-delete-relative-html ()
    "Just delete the relative html when it exists."
    (interactive)
    (when (yes-or-no-p "Really delete the relative html?")
      (let ((fileurl (concat "~/haoran/n/Org/site/public" (file-name-base (buffer-name)) ".html")))
        (if (file-exists-p fileurl)
            (progn
              (delete-file fileurl)
              (message "Delete the relative html done."))
          (message "None relative html.")))))

  (defun +save-and-publish-file ()
    "Save current buffer and publish."
    (interactive)
    (save-buffer t)
    (if (string= (file-name-directory (buffer-file-name))
                 "/home/haoran/haoran/n/Org/site/org/")
        ;; 是site文件夹
        (org-publish-current-file t)))

  (defun +preview-current-buffer-in-browser ()
    "Open current buffer as html."
    (interactive)
    (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
      (+save-and-publish-file)
      (unless (httpd-running-p) (httpd-start))
      (browse-url fileurl)))
  :custom
  (org-publish-project-alist
   '(("orgfiles"
      :base-directory "~/haoran/n/Org/site/org/"
      ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"
      :publishing-directory "~/haoran/n/Org/site/public/"
      :base-extension "org"
      ;; exclude files like "_draft-demo-1.org"
      :exclude "^_[[:word:]-]*.org"     ;; regexp
      :recursive t
      :publishing-function org-html-publish-to-html ;; Publishing action

      ;; personal information
	  :author "Haoran Liu"
	  :email "haoran.mc@outlook.com"
      ;; :html-validation-link "<a href=\"http://beian.miit.gov.cn/\">豫ICP备19900901号</a>"

      ;; generic properties
      :headline-levels 6    ;; org-export-headline-levels
      :language "zh"        ;; org-export-default-language
      :section-numbers nil  ;; org-export-with-section-numbers
      :with-planning t      ;; org-export-with-planning
      :with-priority t      ;; org-export-with-priority
      ;; :with-tags not-in-toc ;; org-export-with-tags
      :with-toc t           ;; org-export-with-toc
	  :with-sub-superscript {}     ;; 禁用 _ 转义成下标，^转义成上标。但加 {} 就可以转义了
	  :preserve-breaks t           ;; 是否保留换行符。如果设置为 nil，导出后就会多行文本显示在一行

      :html-doctype "html5" ;; org-html-doctype
      :html-checkbox-type unicode   ;; org-html-checkbox-type

      :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
      :html-head-include-default-style nil ;; org-html-head-include-default-style
      :html-head-include-scripts nil ;; org-html-head-include-scripts
      :html-head
      "<link rel=\"shortcut icon\" href=\"images/favicon.ico\" type=\"image/x-icon\" />
           <link rel=\"stylesheet\" href=\"static/css/org.css\" type=\"text/css\"  />
           <script type=\"module\" src=\"static/js/main.js\" defer></script>"
      )

     ("images"
      :base-directory "~/haoran/n/Org/site/images/"
      :base-extension any
      :publishing-directory "~/haoran/n/Org/site/public/images/"
      :recursive t
      :publishing-function org-publish-attachment
      )
     ("static"
      :base-directory "~/haoran/n/Org/site/static/"
      :base-extension any
      :publishing-directory "~/haoran/n/Org/site/public/static/"
      :recursive t
      :publishing-function org-publish-attachment
      )

     ("rimages"
      :base-directory "~/haoran/n/Org/site/public/images/"
      :base-extension any
      :publishing-directory "~/haoran/n/Org/site/images/"
      :recursive t
      :publishing-function org-publish-attachment
      )
     ("rstatic"
      :base-directory "~/haoran/n/Org/site/public/static/"
      :base-extension any
      :publishing-directory "~/haoran/n/Org/site/static/"
      :recursive t
      :publishing-function org-publish-attachment)

     ("website" :components ("orgfiles" "images" "static"))
     ("statics" :components ("images" "static"))
     ("rstatics" :components ("rimages" "rstatic"))
     ))
  )

(use-package htmlize :ensure t)

(use-package simple-httpd
  :ensure t
  :custom
  (httpd-port 9517)
  (httpd-root "~/haoran/n/Org/site/public/")
  (org-html-mathjax-options
   '((path "static/MathJax/cdn.bootcdn.net/ajax/libs/mathjax/3.1.2/es5/tex-mml-chtml.min.js")
	 (scale "100")
	 (align "center")
	 (font "TeX")
	 (linebreaks "false")
	 (autonumber "AMS")
	 (indent "0em")
	 (multlinewidth "85%")
	 (tagindent ".8em")
	 (tagside "right"))))

(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'+save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'+save-and-publish-file :local)))

(use-package auto-save-and-publish-file-mode
  :hook (org-mode))

(provide 'init-site)
;;; init-site.el ends here
