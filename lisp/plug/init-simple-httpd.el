;;; init-simple-httpd.el --- create an emacs web server  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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


(defun +httpd-start-server (port root-dir)
  "Start the HTTPD server on PORT with ROOT-DIR as the root directory."
  (httpd-stop)
  (unless (httpd-running-p)
    (setq httpd-port port)
    (setq httpd-root root-dir)
    (httpd-start)))


(defun +httpd-start-currfile ()
  "Start the HTTPD server on PORT:9517 with CURRENT-DIR as the root directory."
  (interactive)
  (+httpd-start-server 9517 default-directory)
  (let ((filename (buffer-name)))
    (if (and (buffer-file-name)
             (string-equal (file-name-extension filename) "html"))
        (browse-url (concat "http://127.0.0.1:9517/" (file-name-nondirectory filename)))
      (message "Unsupported file type or no file."))))


(provide 'init-simple-httpd)
;;; init-simple-httpd.el ends here
