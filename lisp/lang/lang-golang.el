;;; lang-golang.el --- Golang -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
         ("C-c C-c" . +go-run-buffer)
         ("C-c C-u" . go-remove-unused-imports))
  :init
  (progn ;; env vars
    (exec-path-from-shell-initialize)
    (setq exec-path (append exec-path '("/root/go/bin")))
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (progn ;; command args
    (setq gofmt-command "goimports"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)

  (defun +go-run-buffer()
    "Run go code in buffer."
    (interactive)
    (shell-command (concat "go run " (buffer-name))))

  (use-package go-tag
    :bind (:map go-mode-map
           ("C-c t a" . go-tag-add)
           ("C-c t r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "snakecase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
           ("C-c t g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
           ("C-c t f" . go-test-current-file)
           ("C-c t t" . go-test-current-test)
           ("C-c t j" . go-test-current-project)
           ("C-c t b" . go-test-current-benchmark)
           ("C-c t c" . go-test-current-coverage)
           ("C-c t x" . go-run))))

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode))


;; failure maybe cause by lsp_bridge
;; (use-package go-eldoc
;;   :defer t
;;   :hook (go-mode . go-eldoc-setup))

;; Install the tools manually in the current GOPATH
;; go install golang.org/x/tools/gopls
;; go install golang.org/x/tools/cmd/...
;; go install github.com/nsf/gocode
;; go install github.com/rogpeppe/godef
;; go install github.com/lukehoban/go-outline
;; go install golang.org/x/tools/cmd/goimports
;; go install sourcegraph.com/sqs/goreturns
;; go install golang.org/x/tools/cmd/gorename
;; go install github.com/tpng/gopkgs
;; go install github.com/newhook/go-symbols
;; go install github.com/cweill/gotests/...
;; go install golang.org/x/tools/cmd/guru
;; go install github.com/golang/lint/golint
;; go install github.com/zmb3/gogetdoc
;; go install github.com/fatih/gomodifytags@latest
;; Go come with godoc, no download required
;; Go come with gofmt, no download required

;; gopls  Officially maintained, Implementation of LSPS
;; godef  Quickly prompt messages and define hops
;; gocode Automatic code completion
;; godoc  Shows documentation for the specified code package
;; go-outline Outline of the file
;; gofmt  Formatting code

(provide 'lang-golang)
;;; lang-golang.el ends here
