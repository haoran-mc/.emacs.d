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

;;; Code:


;; Mode association (autoload python-mode for *.py files)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(with-eval-after-load 'python
  '(progn
     ;; Initialization
     (defun python-mode-delete-trailing-whitespace ()
       "Delete trailing whitespace before saving file."
       (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

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
     (add-hook 'python-mode-hook 'python-mode-delete-trailing-whitespace)))


;; (use-package jupyter
;;   :ensure t)
;;
;; ;; Live Coding in Python
;; (use-package live-py-mode
;;   :ensure t
;;   :defer t)
;;
;; ;; python -m venv ENV_DIR
;; (use-package pyvenv
;;   :ensure t
;;   :commands pyvenv-deactivate pyvenv-deactivate)


(provide 'lang-python)
;;; lang-python.el ends here
