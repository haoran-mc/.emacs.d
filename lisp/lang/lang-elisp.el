;;; lang-elisp.el --- elisp -*- lexical-binding: t -*-

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
(defun elisp-mode-delete-trailing-whitespace ()
  "Delete trailing whitespace before saving file."
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
;; (add-hook 'emacs-lisp-mode-hook #'elisp-mode-delete-trailing-whitespace)

(defconst eval-as-comment-prefix ";;=> ")
;; from https://github.com/dakra/dmacs
(defun eval-to-comment (&optional arg)
  (interactive "P")
  (let ((start (point)))
    (eval-print-last-sexp arg)
    (save-excursion
      (goto-char start)
      (save-match-data
        (re-search-forward "[[:space:]\n]+" nil t)
        (insert eval-as-comment-prefix)))))

;; eval-print-last-sexp -> eval-to-comment
(define-key emacs-lisp-mode-map (kbd "M-<return>") 'eval-to-comment)
(define-key lisp-interaction-mode-map (kbd "M-<return>") 'eval-to-comment)


(provide 'lang-elisp)
;;; lang-elisp.el ends here
