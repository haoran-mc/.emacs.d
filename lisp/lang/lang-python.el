;;; lang-python.el --- Python -*- lexical-binding: t -*-

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

;;; Require:
;; remember-init
(require 'basic-tookit)

;; Mode association (autoload python-mode for *.py files)
(require 'python)


;;; Code:

;; Initialization
(defun +python-mode-delete-trailing-whitespace ()
  "Delete trailing whitespace before saving file."
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(defun lazycat/jump-to-import()
  (interactive)
  ;; Rember position before jump.
  (lazycat/remember-init)
  ;; Jump to `import ...` position.
  (goto-char (point-min))
  (search-forward-regexp "\\(^import\\|^from\\)" nil t))

(defun python/run-current-file (&optional directory)
  "Execute the current python file."
  (interactive
   (list (or (and current-prefix-arg
                  (read-directory-name "Run in directory: " nil nil t))
             default-directory)))
  (when (buffer-file-name)
    (let* ((command (or (and (boundp 'executable-command) executable-command)
                        (concat "python3 " (buffer-file-name))))
           (default-directory directory)
           (compilation-ask-about-save nil))
      (executable-interpret (read-shell-command "Run: " command)))))


;; binding keys
(define-key python-mode-map (kbd "C-S-j") 'lazycat/jump-to-import)
(define-key python-mode-map (kbd "C-c C-c") 'python/run-current-file)

;; error when open org
;; (exec-path-from-shell-copy-envs '("PYTHONPATH"))


;; Configuration
;; Default to Python 3. Prefer the versioned Python binaries since some
;; systems stupidly make the unversioned one point at Python 2.
(when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
  (setq python-shell-interpreter "python3"))

(setenv "PYTHONIOENCODING" "utf-8") ;; run-python print chinese


;; Customization
(setq python-indent-guess-indent-offset-verbose nil)


;; Hook
;; (add-hook 'python-mode-hook 'python-mode-delete-trailing-whitespace)


(provide 'lang-python)
;;; lang-python.el ends here
