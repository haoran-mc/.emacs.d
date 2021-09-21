;;; init-webs.el --- Configurations of web develop. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.*tml\\'" "\\.*xml\\'" "\\.ejs\\'" "\\.vue\\'")
  :config
  (setq web-mode-markup-indent-offset 2  ;; web-mode, html tag in html file
        web-mode-css-indent-offset 2     ;; web-mode, css in html file
        web-mode-code-indent-offset 2    ;; web-mode, js code in html file
        web-mode-enable-current-element-highlight t 
        web-mode-enable-current-column-highlight t))

(use-package emmet-mode
  :ensure t
  :hook (web-mode css-mode html-mode)
  :config
  (setq emmet-self-closing-tag-style " /") ;; default "/" ;; only " /", "/" and "" are valid.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil))))

(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))


(provide 'init-webs)
;;; init-webs.el ends here
