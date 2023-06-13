;;; lang-golang.el --- Golang -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("C-c C-c" . +go-run-buffer))
  :init
  (exec-path-from-shell-copy-envs '("GOROOT"
                                    "GOPATH"
                                    "GOBIN"
                                    "GO111MODULE"
                                    "GOPROXY"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)

  (defun +go-run-buffer()
    "Run go code in buffer."
    (interactive)
    (shell-command (concat "go run " (buffer-name))))
  :custom
  (gofmt-command "goimports"))


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
