;;; init-text.el --- Writing -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `org-mode' is too huge to place here.

;;; Code:

;; Pixel alignment for org/markdown tables
(use-package valign
  :ensure t
  :hook ((markdown-mode org-mode) . valign-mode))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :bind (:map markdown-mode-map
              ("C-c m r" . +markdown-insert-ruby-tag)
              ("C-c m d" . +markdown-insert-details)
              ("C-c m p" . +markdown-preview-as-html)
              ("C-c m k" . +markdown-kill-grip))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t)
  :config
  (with-no-warnings
    ;; Use `which-key' instead
    (advice-add #'markdown--command-map-prompt :override #'ignore)
    (advice-add #'markdown--style-map-prompt   :override #'ignore))

  (defun +markdown-insert-ruby-tag (text ruby)
    "Insert ruby tag with `TEXT' and `RUBY' quickly."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

  (defun +markdown-insert-details (title)
    "Insert details tag (collapsible) quickly."
    (interactive "sTitle: ")
    (insert (format "<details><summary>%s</summary>\n\n</details>" title)))

  ;; sudo pip install grip
  (defun +markdown-preview-as-html ()
    "Preview markdown as html."
    (interactive)
    (start-process "grip" "*gfm-to-html*" "grip"
                   (buffer-file-name) "5000")
    (browse-url (format "http://localhost:5000/%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name)))))

  ;; kill grip process, M-x list-processes to list processes
  (defun +markdown-kill-grip ()
    "Preview is avaiable after kill grip."
    (interactive)
    (kill-process "grip"))

  ;; Table of contents
  (use-package markdown-toc))

(provide 'init-text)
;;; init-text.el ends here
