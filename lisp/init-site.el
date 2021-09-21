;;; init-site.el --- Exports org to site. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package htmlize :ensure t)

(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))

(use-package ox-publish
  :config
  (setq org-publish-project-alist
        '(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/haoran/Notes/Org/Programming/org/" ;; local dir
           :publishing-directory "~/haoran/Notes/Org/Programming/public/"
           :base-extension "org"
           :exclude "^_[[:word:]-]*.org"
           :recursive t

           ;; ; Publishing action
           :publishing-function org-html-publish-to-html

           ;; ; Generic properties
           :headline-levels 6    ;; org-export-headline-levels
           :language "zh"        ;; org-export-default-language
           :section-numbers nil  ;; org-export-with-section-numbers
           :with-planning t      ;; org-export-with-planning
           :with-priority t      ;; org-export-with-priority
           ;;  :with-tags not-in-toc ;; org-export-with-tags
           :with-toc t           ;; org-export-with-toc

           :html-doctype "html5" ;; org-html-doctype
           ;;  :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
           :html-head-include-default-style nil ;; org-html-head-include-default-style
           :html-head-include-scripts nil ;; org-html-head-include-scripts
           :html-head
           "<link rel=\"shortcut icon\" href=\"images/favicon.ico\" type=\"image/x-icon\" />
            <link rel=\"stylesheet\" href=\"css/style.css\" type=\"text/css\"  />
            <script type=\"text/javascript\" src=\"https://cdn.jsdelivr.net/npm/valine@1.4.14/dist/Valine.min.js\"></script>
            <script type=\"module\" src=\"js/main.js\" defer></script>" ;; org-html-head
           :html-checkbox-type unicode  ;; org-html-checkbox-type
           )
          ;; ;; static assets
          ;; ("confs"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/org/"
          ;;  :base-extension "js"
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/public/"
          ;;  :recursive nil
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ("images"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/images/"
          ;;  :base-extension any
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/public/images/"
          ;;  :recursive t
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ("themes"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/themes/"
          ;;  :base-extension any
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/public/themes/"
          ;;  :recursive t
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ;; reverse static assets
          ;; ("rconfs"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/public/"
          ;;  :base-extension "js"
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/org"
          ;;  :recursive nil
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ("rimages"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/public/images/"
          ;;  :base-extension any
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/images/"
          ;;  :recursive t
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ("rthemes"
          ;;  :base-directory "~/haoran/Notes/Org/Programming/public/themes/"
          ;;  :base-extension any
          ;;  :publishing-directory "~/haoran/Notes/Org/Programming/themes/"
          ;;  :recursive t
          ;;  :publishing-function org-publish-attachment
          ;;  )
          ;; ("website" :components ("orgfiles" "confs" "images" "themes"))
          ;; ("statics" :components ("confs" "images" "themes"))
          ;; ("rstatics" :components ("rconfs" "rimages" "rthemes"))
          ))
  (setq org-export-with-sub-superscripts nil))

(defun save-and-publish-website()
  "Save all buffers and publish."
  (interactive)
  (when (yes-or-no-p "Really save and publish current project?")
    (save-some-buffers t)
    (org-publish-project "website" t)
    (message "Site published done.")))

(defun save-and-publish-statics ()
  "Just copy statics like js, css, and image file .etc."
  (interactive)
  (org-publish-project "statics" t)
  (message "Copy statics done."))

(defun save-and-publish-rstatics ()
  "Just copy statics like js, css, and image file .etc.
Which is a reverse operation of `save-and-publish-statics'."
  (interactive)
  (org-publish-project "rstatics" t)
  (message "Copy rstatics done."))

(defun ogmc/save-and-publish-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))

(defun delete-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")
    (let ((fileurl (concat "~/haoran/Notes/Org/Programming/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun just-delete-relative-html ()
  "Just delete the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete the relative html?")
    (let ((fileurl (concat "~/haoran/Notes/Org/Programming/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (progn
            (delete-file fileurl)
            (message "Delete the relative html done."))
        (message "None relative html.")))))

(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'save-and-publish-file :local)))

;; (use-package auto-save-and-publish-file-mode :hook (org-mode))

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/haoran/Notes/Org/Programming/public"))

(defun ogmc/preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-base (buffer-name)) ".html")))
    (ogmc/save-and-publish-file)
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))

(provide 'init-site)
;;; init-site.el ends here
