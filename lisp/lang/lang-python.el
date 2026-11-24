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
(require 'func-lazy)

;; Mode association (autoload python-mode for *.py files)
(require 'python)


;;; Code:
(defun python-mode-delete-trailing-whitespace ()
  "Delete trailing whitespace before saving file."
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
;; (add-hook 'python-mode-hook 'python-mode-delete-trailing-whitespace)


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
(define-key python-mode-map (kbd "C-c C-c") 'python/run-current-file)


;; Two ways to make pyright work with installed package.
;;
;; 1. 用 M-x pyvenv-activate 选择虚拟环境，再用 M-x eglot-reconnect 重连
;;
;; 2. Use pdm.
;; Packages installed with pdm under __pypackages__/<version>/lib/,
;; update pyproject.toml to make pyright work with it, for example:
;; [tool.pyright]
;; extraPaths = ["__pypackages__/3.8/lib/", "src/]
;; https://pdm-project.org/en/latest/usage/pep582/#emacs
;;
;; (also check basedpyright and delance)
(require 'pyvenv)
;; 1. python -m venv .
;; 2. source bin/activate
;; 3. pip install -r requirements.txt
;; 4. pyvenv-mode
;; 5. pyvenv-activate /Users/haoran/haoran/gr/action-send-mail
;; 6. eglot-reconnect


;; lint ruff, formatter.el ruff
(require 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)
(add-hook 'eglot-managed-mode-hook 'flymake-ruff-load)


;; Default to Python 3. Prefer the versioned Python binaries since some
;; systems stupidly make the unversioned one point at Python 2.
(when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
  (setq python-shell-interpreter "python3"))

(setenv "PYTHONIOENCODING" "utf-8") ;; run-python print chinese

;; Disable readline based native completion
(setq python-shell-dedicated 'project
      python-indent-guess-indent-offset nil)


;; 因为 pip 不允许全局安装，所以使用 brew/node
;; brew install ruff
;; brew install basedpyright
;; brew install pyright ←basedpyright没配置成功，所以实际上使用的pyright


(provide 'lang-python)
;;; lang-python.el ends here
