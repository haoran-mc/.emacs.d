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
(require 'go-mode)

(defun my/go-run-buffer ()
  (interactive)
  (executable-interpret
   (read-shell-command "Run: " (concat "go run " (buffer-file-name)))))


;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (setq gofmt-command "goimports")


(define-key go-mode-map (kbd "C-c C-c") 'my/go-run-buffer)
;; (define-key go-mode-map (kbd "C-c C-a") 'go-import-add)
;; (define-key go-mode-map (kbd "C-c C-d") 'godoc)



;; go install golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install golang.org/x/tools/cmd/gorename@latest

;; gopls      Officially maintained, Implementation of LSPS
;; goimports  Formatting code
;; gorename   Extra refactoring commands


;; go install golang.org/x/tools/cmd/goimports@latest
;; go install github.com/cweill/gotests/gotests@latest
;; go install github.com/golangci/golangci-lint/v2/cmd/golangci-lint@latest
;; go install github.com/tpng/gopkgs@latest
;; go install github.com/newhook/go-symbols@latest
;; go install github.com/lukehoban/go-outline@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/mvdan/gofumpt@latest
;; Go come with godoc, no download required
;; Go come with gofmt, no download required

(provide 'lang-golang)
;;; lang-golang.el ends here
