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
(require 'ox-publish)


(defvar ran--site-org-dir  (expand-file-name "~/haoran/no/org/site"))
(defvar ran--site-html-dir (expand-file-name "~/haoran/gr/haoran-mc.github.io"))
(defvar ran--wiki-org-dir  (expand-file-name "~/haoran/no/org/wiki"))
(defvar ran--my-export-dir (expand-file-name "~/haoran/no/org/export/org-preview"))

;; see init-ox.el for more
(setq org-html-postamble nil
      ;; 默认的 html-head，也就是会在 mydir 中使用
      org-html-head "<link rel='shortcut icon' href='assets/favicon.ico' type='image/x-icon' />
                     <link rel='stylesheet' href='css/org.css' type='text/css' />
                     <script type='module' src='js/main.js' defer></script>"
      org-publish-project-alist
      `(("wiki"
         :base-directory ,ran--wiki-org-dir
         ;; :publishing-directory "/ssh:jack@192.168.0.112:~/site/public/"
         ;; .gitignore will ignore .html files, you may not find images after changing it
         :publishing-directory ,ran--wiki-org-dir
         :base-extension "org"
         :exclude "^_[[:word:]-]*.org" ;; regexp exclude files like "_draft-demo-1.org"
         :recursive t
         :publishing-function org-html-publish-to-html ;; Publishing action
	     :author "Haoran Liu"
	     :email "haoran.mc@outlook.com"
         ;; :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
         :html-head
         "<link rel='shortcut icon' href='assets/favicon.ico' type='image/x-icon' />
          <link rel='stylesheet' href='css/org.css' type='text/css' />
          <script type='module' src='js/main.js' defer></script>"
         )
        ("site"
         :base-directory ,ran--site-org-dir
         :publishing-directory ,ran--site-html-dir
         :base-extension "org"
         :exclude "^_[[:word:]-]*.org"     ;; regexp exclude files like "_draft-demo-1.org"
         :recursive t
         :publishing-function org-html-publish-to-html ;; Publishing action
	     :author "Haoran Liu"
	     :email "haoran.mc@outlook.com"
         :html-head
         "<link rel='shortcut icon' href='assets/favicon.ico' type='image/x-icon' />
          <link rel='stylesheet' href='css/org.css' type='text/css' />
          <script type='module' src='js/main.js' defer></script>"
         )
        ))

(defun +org-preview-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (cond
     ;; my site
     ((string-prefix-p ran--site-org-dir file-path)
      (+org-export-and-preview 'site ran--site-html-dir))

     ;; wiki dir: too many images, no cp images
     ((string-prefix-p ran--wiki-org-dir file-path)
      (+org-export-and-preview 'wiki ran--wiki-org-dir))

     (t
      (+org-export-to-mydir-and-preview)))))

(defun +httpd-start-server (port root-dir)
  "Start the HTTPD server on PORT with ROOT-DIR as the root directory."
  (httpd-stop)
  (unless (httpd-running-p)
    (setq httpd-port port)
    (setq httpd-root root-dir)
    (httpd-start)))

(defun +org-export-and-preview (project root-dir)
  "Open current buffer as html in browser.
PROJECT specifies whether it's for 'wiki' or 'site'.
ROOT-DIR specifies the root directory for HTTPD server."
  (interactive)
  (save-buffer t)
  (org-publish-current-file project)
  ;; Start HTTPD server and open the buffer in browser
  (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
    (+httpd-start-server 9517 root-dir)
    (browse-url fileurl)))

(defun +org-export-to-mydir-and-preview()
  "Export org to my dir and preview. NOTE that org files in
other org folders should use absolute paths to define images. Those html files
use export/org-preview/org.css render style."
  (interactive)
  (save-buffer)
  (delete-directory (concat ran--my-export-dir "/images") t)
  (shell-command (format "cp -r %s/images %s/images" default-directory ran--my-export-dir))
  (shell-command
   (format "mv -v %s %s"
           (shell-quote-argument (org-html-export-to-html))
           ran--my-export-dir))
  (let ((fileurl (concat "http://127.0.0.1:9517/" (file-name-base (buffer-name)) ".html")))
    (+httpd-start-server 9517 ran--my-export-dir)
    (browse-url fileurl)))


(defun +delete-site-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")
    (let ((fileurl (concat ran--site-html-dir "/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))



;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert buffer text and decorations to HTML.
(require 'htmlize)
(setq org-html-htmlize-output-type 'inline-css)


;; simple-httpd ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A great Emacs package which can host your files as a website on
;; your local machine so that you can pull it up in your browser.
(require 'simple-httpd)
(setq org-html-mathjax-options
      '((path "MathJax/cdn.bootcdn.net/ajax/libs/mathjax/3.1.2/es5/tex-mml-chtml.min.js")
	    (scale "100")
	    (align "center")
	    (font "TeX")
	    (linebreaks "false")
	    (autonumber "AMS")
	    (indent "0em")
	    (multlinewidth "85%")
	    (tagindent ".8em")
	    (tagside "right")))


;; define-minor-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generate-sitemap (base-dir sitemap-file)
  "Generate a sitemap file for the Org files in BASE-DIR."
  (interactive "DDirectory: \nFOutput Sitemap File: ")
  (let ((files (sort (directory-files-recursively base-dir "\.org$") #'string>)))
    (with-temp-buffer
      (insert "#+TITLE: Blog Articles\n\n")
      (dolist (file files)
        (unless (member (file-name-nondirectory file) '("index.org" "sitemap.org" "404.org"))
          (insert (format "- [[file:%s][%s]]\n" file (file-name-nondirectory file)))))
      (write-region (point-min) (point-max) sitemap-file))))

(defun +org-publish-site-file ()
  "Save current buffer and publish if it's in the `ran--site-org-dir' directory."
  (when (and (buffer-file-name) ;; 缓冲区有文件才继续
             (string-prefix-p (expand-file-name ran--site-org-dir)
                              (expand-file-name (buffer-file-name))))
    (org-publish-current-file 'site)
    (let ((sitemap-file (concat ran--site-org-dir "/sitemap.org")))
      (generate-sitemap ran--site-org-dir sitemap-file)
      (org-publish-file sitemap-file))))

(define-minor-mode +auto-save-and-publish-site-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if +auto-save-and-publish-site-file-mode
      (add-hook 'after-save-hook #'+org-publish-site-file :append :local)
    (remove-hook 'after-save-hook #'+org-publish-site-file :local)))

;; (setq +auto-save-and-publish-site-file-mode 1)
(add-hook 'org-mode-hook '+auto-save-and-publish-site-file-mode)

(provide 'init-site)
;;; init-site.el ends here
