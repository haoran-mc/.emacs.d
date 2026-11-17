;;; init-ai.el --- A simple LLM client for Emacs     -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Haoran Liu

;; Author: Haoran Liu <haoran.mc@qq.com>
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


;; gptel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-markdown)
(require 'gptel)
;; (require 'gptel-curl)

(defun get-api-key (file-path)
  "Read the DeepSeek API key from a specified file."
  (interactive "fSelect the file containing the DeepSeek API key: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

;; (setq gptel-api-key (get-api-key "/Users/haoran/haoran/note/org/sync-notes/misc/secret/emacs-chatgpt"))


(require 'gptel-openai-oauth)
(gptel-make-openai-oauth "OpenAI-sub")
(setq gptel-backend
      (gptel-make-openai-oauth
          "OpenAI-sub"
        :models '(gpt-6-sol gpt-5.6-sol gpt-5.6-terra gpt-5.6-luna
                            gpt-6-astra gpt-6-luna gpt-6.1-sol gpt-6.1-terra)
        :stream t)
      gptel-model 'gpt-6-sol) ;; then M-x gptel-openai-oauth-login

(provide 'init-ai)
;;; init-ai.el ends here
