;;; init-autoinsert.el --- built in autoinsert       -*- lexical-binding: t; -*-

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


;; Don't want to be prompted before insertion:
(setq auto-insert-query nil)
(setq auto-insert-directory (locate-user-emacs-file "templates"))
(add-hook 'find-file-hook 'auto-insert)
(auto-insert-mode 1)


(defun autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert "\\.org$"
  ["~/.emacs.d/templates/default-org.org" autoinsert-yas-expand])
(define-auto-insert "\\.html$"
  ["~/.emacs.d/templates/default-html.html" autoinsert-yas-expand])
;; (define-auto-insert "\\.go$"
;;   ["~/.emacs.d/templates/default-go.go" autoinsert-yas-expand])
;; (define-auto-insert "\\.py$"
;;   ["~/.emacs.d/templates/default-py.py" autoinsert-yas-expand])
;; (define-auto-insert "\\.c++$"
;;   ["~/.emacs.d/templates/default-cpp.cpp" autoinsert-yas-expand])


(provide 'init-autoinsert)
;;; init-autoinsert.el ends here
