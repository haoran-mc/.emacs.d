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

(reformatter-define reformatter-pyfmt
  :program "black"
  :args '("-"))

(reformatter-define reformatter-rustfmt
  :program "rustfmt")


(defun +format-code-dwim()
  "Format the current buffer or region based on the file type and selection state."
  (interactive)
  (let ((file-extension (file-name-extension (buffer-file-name)))
        (in-selection (use-region-p)))
    (cond
     ((or (string-equal file-extension "el")
          (string-equal file-extension "org"))
      (my/indent-buffer))

     ((string-equal file-extension "json")
      (if in-selection
          (json-mode-beautify (region-beginning) (region-end))
        (json-mode-beautify (point-min) (point-max))))

     ((string-equal file-extension "sql")
      (if in-selection
          (sqlformat-region (region-beginning) (region-end))
        (sqlformat-buffer)))

     ((string-equal file-extension "go")
      ;; golang 有强制的格式规定，不单独 format region
      (reformatter-gofmt-buffer))

     ((string-equal file-extension "py")
      (if in-selection
          (reformatter-pyfmt-region (region-beginning) (region-end))
        (reformatter-pyfmt-buffer)))

     ((or (string-equal file-extension "c")
          (string-equal file-extension "cpp")
          (string-equal file-extension ".h"))
      (clang-format-buffer))

     ((string-equal file-extension "rs")
      (reformatter-rustfmt-buffer))

     (t
      (message "Unsupported file type or no file.")))))


(provide 'init-formatter)
;;; init-formatter.el ends here
