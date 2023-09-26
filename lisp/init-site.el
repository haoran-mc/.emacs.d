;;; init-site.el --- site built by org               -*- lexical-binding: t; -*-

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

(require 'ox)
(require 'ox-html)

;; "Settings of `org-export'."
(setq org-html-htmlize-output-type 'inline-css) ;; Hide html built-in style and script.

(use-package ox-publish
  :after org
  :config
  (defun +delete-org-and-html ()
    "Delete current org and the relative html when it exists."
    (interactive)
    (when (yes-or-no-p "Really delete current org and the relative html?")
      (let ((fileurl (concat "~/haoran/gr/haoran-mc.github.io" (file-name-base (buffer-name)) ".html")))
        (if (file-exists-p fileurl)
            (delete-file fileurl))
        (delete-file (buffer-file-name))
        (kill-this-buffer)
        (message "Delete org and the relative html done."))))

  (defun +just-delete-relative-html ()
    "Just delete the relative html when it exists."
    (interactive)
    (when (yes-or-no-p "Really delete the relative html?")
      (let ((fileurl (concat "~/haoran/gr/haoran-mc.github.io" (file-name-base (buffer-name)) ".html")))
        (if (file-exists-p fileurl)
            (progn
              (delete-file fileurl)
              (message "Delete the relative html done."))
          (message "None relative html.")))))

  
  (defun +save-and-publish-wiki-file ()
    "Save current buffer and publish."
    (interactive)
    (save-buffer t)
    (org-publish-current-file 'wiki)
    (message "hello wiki"))

  (defun +preview-wiki-current-buffer-in-browser ()
    "Open current wiki buffer as html."
    (interactive)
    (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
      (+save-and-publish-wiki-file)
      (httpd-stop)
      (unless (httpd-running-p)
        (progn
          (setq httpd-port 9517)
          (setq httpd-root "~/haoran/no/org/wiki/") ;; Corresponding to org-publish-project-alist - wiki
          (httpd-start)))
      (browse-url fileurl)))

  
  (defun +save-and-publish-site-file ()
    "Save current buffer and publish."
    (interactive)
    (save-buffer t)
    ;; Disable auto-save-and-publish-site-file-mode for other folders
    (if (string= (file-name-directory (buffer-file-name))
                 "/Users/haoran/haoran/no/org/site/")
        (org-publish-current-file 'site))
    (message "hello org-mode"))

  (defun +preview-site-current-buffer-in-browser ()
    "Open current site buffer as html."
    (interactive)
    (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
      (+save-and-publish-site-file)
      (httpd-stop)
      (unless (httpd-running-p)
        (progn
          (setq httpd-port 9517)
          (setq httpd-root "~/haoran/gr/haoran-mc.github.io/") ;; Corresponding to org-publish-project-alist - site
          (httpd-start)))
      (browse-url fileurl)))

  
  (defun +org-export-html-to-my-dir-and-preview()
    "Export org to my dir and preview. NOTE that org files in
other org folders should use absolute paths to define images. Those html files
use export/org-preview/org.css render style."
    (interactive)
    (save-buffer)
    (delete-directory "~/haoran/no/org/export/org-preview/images" t)
    (shell-command (format "cp -r %s/images ~/haoran/no/org/export/org-preview/images" default-directory))
    (shell-command
     (format "mv -v %s %s"
             (shell-quote-argument (org-html-export-to-html))
             "~/haoran/no/org/export/org-preview")) ;; USER-DIRECTORY
    (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
      (httpd-stop)
      (unless (httpd-running-p)
        (progn
          (setq httpd-port 9517)
          (setq httpd-root "~/haoran/no/org/export/org-preview/") ;; Corresponding to org-publish-project-alist - site
          (httpd-start)))
      (browse-url fileurl)))

  
  (defun +preview-current-buffer-in-browser ()
    "Open current buffer as html."
    (interactive)
    (let ((file-path (buffer-file-name)))
      (cond
       ((string-prefix-p "/Users/haoran/haoran/no/org/wiki/" file-path)
        (+preview-wiki-current-buffer-in-browser))
       ((string-prefix-p "/Users/haoran/haoran/no/org/site/" file-path)
        (+preview-site-current-buffer-in-browser))
       (t
        (+org-export-html-to-my-dir-and-preview)))))
  :custom
  (org-publish-project-alist
   '(("wiki"
      :base-directory "~/haoran/no/org/wiki/"
      ;; :publishing-directory "/ssh:jack@192.112.245.112:~/site/public/"
      ;; .gitignore will ignore .html files, you may not find images after changing it
      :publishing-directory "~/haoran/no/org/wiki/"
      :base-extension "org"
      :exclude "^_[[:word:]-]*.org"     ;; regexp exclude files like "_draft-demo-1.org"
      :recursive t
      :publishing-function org-html-publish-to-html ;; Publishing action
	  :author "Haoran Liu"
	  :email "haoran.mc@outlook.com"
      ;; :html-validation-link "<a href=\"http://beian.miit.gov.cn/\">豫ICP备19900901号</a>"
      ;; :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
      :html-head ;; wiki-css wiki-js
      "<link rel=\"shortcut icon\" href=\"assets/favicon.ico\" type=\"image/x-icon\" />
           <link rel=\"stylesheet\" href=\"wiki-css/org.css\" type=\"text/css\" />
           <script type=\"module\" src=\"wiki-js/main.js\" defer></script>"
      )
     ("site"
      :base-directory "~/haoran/no/org/site/"
      :publishing-directory "~/haoran/gr/haoran-mc.github.io/"
      :base-extension "org"
      :exclude "^_[[:word:]-]*.org"     ;; regexp exclude files like "_draft-demo-1.org"
      :recursive t
      :publishing-function org-html-publish-to-html ;; Publishing action
	  :author "Haoran Liu"
	  :email "haoran.mc@outlook.com"
      :html-head
      "<link rel=\"shortcut icon\" href=\"assets/favicon.ico\" type=\"image/x-icon\" />
           <link rel=\"stylesheet\" href=\"css/org.css\" type=\"text/css\"  />
           <script type=\"module\" src=\"js/main.js\" defer></script>"
      )
     ))
  (org-html-postamble nil)
  )

(use-package htmlize :ensure t)

(use-package simple-httpd
  :ensure t
  :custom
  ;; (httpd-port 9517)
  ;; (httpd-root "~/haoran/gr/haoran-mc.github.io/")
  (org-html-mathjax-options
   '((path "MathJax/cdn.bootcdn.net/ajax/libs/mathjax/3.1.2/es5/tex-mml-chtml.min.js")
	 (scale "100")
	 (align "center")
	 (font "TeX")
	 (linebreaks "false")
	 (autonumber "AMS")
	 (indent "0em")
	 (multlinewidth "85%")
	 (tagindent ".8em")
	 (tagside "right"))))

(define-minor-mode +auto-save-and-publish-site-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if +auto-save-and-publish-site-file-mode
      (progn ;; mode is enabled
        (add-hook 'after-save-hook #'+save-and-publish-site-file :append :local))
    ;; mode is disabled
    (remove-hook 'after-save-hook #'+save-and-publish-site-file :local)))

(use-package +auto-save-and-publish-site-file-mode
  :hook org-mode
  :custom
  (+auto-save-and-publish-site-file-mode 1))

(provide 'init-site)
;;; init-site.el ends here
