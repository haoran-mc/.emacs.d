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

;; Mode association (autoload go-mode for *.go files)
(require 'go-mode)

;; Custom function
(defun +go-run-buffer ()
  "Run go code in buffer."
  (interactive)
  (executable-interpret
   (read-shell-command "Run: " (concat "go run " (buffer-file-name)))))

;; Key bindings
(define-key go-mode-map (kbd "C-c C-c") '+go-run-buffer)
;; (define-key go-mode-map (kbd "C-c C-a") 'go-import-add)
;; (define-key go-mode-map (kbd "C-.") 'gofmt) ;; format by emacs-reformatter
;; (define-key go-mode-map (kbd "C-c C-d") 'godoc)

;; Configuration
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; Customization
(setq gofmt-command "goimports")


;; Install the tools manually in the current GOPATH
;; go install golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/cmd/gorename@latest
;; go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.56.2

;; go install golang.org/x/tools/cmd/...
;; go install github.com/nsf/gocode@latest
;; go install github.com/rogpeppe/godef@latest
;; go install github.com/lukehoban/go-outline@latest
;; go install sourcegraph.com/sqs/goreturns@latest
;; go install github.com/tpng/gopkgs@latest
;; go install github.com/newhook/go-symbols@latest
;; go install github.com/cweill/gotests/...
;; go install golang.org/x/tools/cmd/guru@latest
;; go install golang.org/x/lint/golint@latest
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
