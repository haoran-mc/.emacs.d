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


;; aider ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'aider)
(setq aider-args '("--no-auto-commits" "--deepseek" "--cache-prompts" "--read-only"))
(with-eval-after-load 'aider
  (setenv "DEEPSEEK_API_KEY" (get-api-key "/Users/haoran/haoran/no/org/sync-notes/misc/secret/emacs-deepseek"))

  ;; Add keybinding to apply changes after review
  (define-key aider-mode-map (kbd "C-c C-a") 'aider-apply-changes)

  ;; Add confirmation prompt before applying changes
  (defun aider-confirm-apply-changes ()
    "Ask for confirmation before applying aider changes."
    (interactive)
    (when (yes-or-no-p "Apply aider's suggested changes?")
      (aider-apply-changes)))

  (advice-add 'aider-apply-changes :before #'aider-confirm-apply-changes))

(provide 'init-ai)
;;; init-ai.el ends here
