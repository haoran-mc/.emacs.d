;;; init-ai.el --- A simple LLM client for Emacs  -*- lexical-binding: t; -*-

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
;; gptel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-markdown)
(require 'gptel)
(require 'gptel-curl)

(defun get-api-key (file-path)
  "Read the DeepSeek API key from a specified file."
  (interactive "fSelect the file containing the DeepSeek API key: ")
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

(setq gptel-api-key (get-api-key "/Users/haoran/haoran/no/org/sync-notes/misc/secret/emacs-chatgpt"))


;; aidermacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/Documents/emacs/local-packages/aidermacs"))
(require 'aidermacs)
(setenv "DEEPSEEK_API_KEY" (get-api-key "/Users/haoran/haoran/no/org/sync-notes/misc/secret/emacs-deepseek"))
(setq aidermacs-default-model "deepseek/deepseek-chat"
      aidermacs-show-diff-after-change t
      aidermacs-use-architect-mode nil
      aidermacs-architect-model "deepseek/deepseek-chat"
      aidermacs-editor-model "deepseek/deepseek-chat")



(provide 'init-ai)
;;; init-ai.el ends here
