;;; init-completion.el --- code completion           -*- lexical-binding: t; -*-

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

(require 'company)

(defun words-completion-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; Only use ispell as backend
  (setq company-backends '(company-ispell))

  ;; Accelerating completion
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)

  ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
  ;;  but I prefer hard code the dictionary path. That's more portable.
  (setq company-ispell-dictionary
        (file-truename
         (format "%s%s%s" "~/.emacs.d/resources/word-completion/" major-mode "-words.txt"))))


(dolist (mode-hook '(sql-mode-hook))
  (add-hook mode-hook 'company-mode)
  (add-hook mode-hook 'words-completion-hook-setup))


(provide 'init-completion)
;;; init-completion.el ends here
