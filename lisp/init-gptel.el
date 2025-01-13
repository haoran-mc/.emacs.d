;;; init-gptel.el --- A simple LLM client for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Haoran Liu

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

(require 'init-markdown)
(require 'gptel)
(require 'gptel-curl)

(defun get-deepseek-key ()
  "Read the DeepSeek API key from a file."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/Users/haoran/haoran/no/org/sync-notes/misc/secret/emacs-deepseek")
    (string-trim (buffer-string))))

(setq gptel-backend (gptel-make-openai "DeepSeek"
                      :host "api.deepseek.com"
                      :endpoint "/chat/completions"
                      :stream t
                      :key #'get-deepseek-key
                      :models '(deepseek-chat deepseek-coder)))

(provide 'init-gptel)
;;; init-gptel.el ends here
