;;; init-formatter.el --- define commands which run reformatters on the current Emacs buffer  -*- lexical-binding: t; -*-

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
(require 'reformatter)

(reformatter-define reformatter-gofmt
  :program "goimports")
;; → then this code generate two commands: reformatter-gofmt-buffer、reformatter-gofmt-region
;;                 and a local minor mode: reformatter-gofmt-on-save-mode
;; (add-hook 'go-mode-hook 'evgeni-gofmt-on-save-mode)

;; https://github.com/scop/emacs-ruff-format/blob/main/ruff-format.el
(reformatter-define reformatter-pyfmt
  :program "ruff"
  :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
  :lighter " RuffFmt")

(reformatter-define reformatter-rustfmt
  :program "rustfmt")

(reformatter-define reformatter-shfmt
  ;; go install mvdan.cc/sh/v3/cmd/shfmt@latest
  :program "shfmt"
  :args (list "-i" "2" "--filename" (or (buffer-file-name) input-file))
  :lighter " ShFmt")

(reformatter-define reformatter-tomlfmt
  ;; brew install taplo
  :program "taplo"
  :args (list "fmt" "-"))

(defun ran/format-code-dwim()
  "Format the current buffer based on the file type."
  (interactive)
  (let ((file-extension (file-name-extension (buffer-file-name))))
    (cond
     ;;================= frontend
     ((eq major-mode 'web-mode)
      (web-mode-buffer-indent))

     ((or (eq major-mode 'js-mode)
          (eq major-mode 'typescript-mode)
          (eq major-mode 'css-mode))
      (require 'prettier-js) ;; npm install -g prettier
      (prettier-js))

     ;;=================
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'org-mode)
          (eq major-mode 'nxml-mode))
      (my/indent-buffer))

     ;; c/c++
     ((or (eq major-mode 'c++-mode)
          (eq major-mode 'c-mode))
      (require 'clang-format) ;; brew install clang-format
      (clang-format-buffer))

     ((eq major-mode 'json-mode)
      (json-mode-beautify (point-min) (point-max)))

     ((eq major-mode 'go-mode)
      (reformatter-gofmt-buffer))

     ((eq major-mode 'python-mode)
      (reformatter-pyfmt-buffer))

     ((eq major-mode 'rust-mode)
      (reformatter-rustfmt-buffer))

     ((eq major-mode 'sh-mode)
      (reformatter-shfmt-buffer))

     ((eq major-mode 'toml-mode)
      (reformatter-tomlfmt-buffer))

     (t
      (message "Unsupported file type or no file.")))))


(provide 'init-formatter)
;;; init-formatter.el ends here
