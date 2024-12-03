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
(require 'clang-format)


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


(defun ran/format-code-dwim()
  "Format the current buffer based on the file type."
  (interactive)
  (let ((file-extension (file-name-extension (buffer-file-name))))
    (cond
     ((or (string-equal file-extension "el")
          (string-equal file-extension "org"))
      (my/indent-buffer))

     ((or (string-equal file-extension "c")
          (string-equal file-extension "cpp")
          (string-equal file-extension ".h"))
      (clang-format-buffer))

     ((string-equal file-extension "json")
      (json-mode-beautify (point-min) (point-max)))

     ((string-equal file-extension "sql")
      (sqlformat-buffer))

     ((string-equal file-extension "go")
      (reformatter-gofmt-buffer))

     ((string-equal file-extension "py")
      (reformatter-pyfmt-buffer))

     ((string-equal file-extension "rs")
      (reformatter-rustfmt-buffer))

     ((string-equal file-extension "sh")
      (reformatter-shfmt-buffer))

     ((string-equal file-extension "css")
      (my/indent-buffer))

     (t
      (message "Unsupported file type or no file.")))))


(provide 'init-formatter)
;;; init-formatter.el ends here
