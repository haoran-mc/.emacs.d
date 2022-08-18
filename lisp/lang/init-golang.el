;;; init-golang.el --- Golang -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind(("C-c C-c" . go-run-buffer)
        ("C-c C-d" . godoc) ;; TODO
        ("C-c C-a" . go-import-add))
  :config
  (defun go-run-buffer()
    (interactive)
    (shell-command (concat "go run " (buffer-name))))
  (progn
    (exec-path-from-shell-initialize)
    (setq exec-path (append exec-path '("/root/go/bin")))
    (exec-path-from-shell-copy-env "GOPATH"))
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

;; go get -u -v golang.org/x/tools/cmd/...
;; go get -u -v github.com/rogpeppe/godef TODO
;; go get -u -v golang.org/x/tools/cmd/goimports
;; go get -u -v golang.org/x/tools/gopls
;; go get -u -v github.com/mdempsky/gocode

(provide 'init-golang)
;;; init-golang.el ends here
