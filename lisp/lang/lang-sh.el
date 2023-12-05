;;; lang-sh.el --- Shell scripts -*- lexical-binding: t -*-

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

;; Edit shell scripts
;;
;; awesoome. sh-mode provides `sh-while-getopts' to automate getopts.
;;
;; Emacs master enables `sh-mode' when editing archlinux PKGBUILD.
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46660
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'"     . sh-mode)
         ("/PKGBUILD\\'" . sh-mode))
  :hook (sh-mode . sh-mode-setup)
  :bind (:map sh-mode-map
         ("C-c C-e" . sh-execute-region))
  :config
  (defun sh-mode-setup ()
    (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(provide 'lang-sh)
;;; lang-sh.el ends here
