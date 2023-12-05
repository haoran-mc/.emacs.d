;;; lang-golang.el --- Golang -*- lexical-binding: t -*-

;; Copyright (C) 2023  Haoran Liu

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

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ("C-c C-c" . +go-run-buffer)
              ("C-c C-d" . godoc))
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
;; go install golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/...
;; go install github.com/nsf/gocode@latest
;; go install github.com/rogpeppe/godef@latest
;; go install github.com/lukehoban/go-outline@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install sourcegraph.com/sqs/goreturns@latest
;; go install golang.org/x/tools/cmd/gorename@latest
;; go install github.com/tpng/gopkgs@latest
;; go install github.com/newhook/go-symbols@latest
;; go install github.com/cweill/gotests/...
;; go install golang.org/x/tools/cmd/guru@latest
;; go install github.com/golang/lint/golint@latest
;; go install github.com/zmb3/gogetdoc@latest
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
