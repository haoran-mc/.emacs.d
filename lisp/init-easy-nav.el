;;; init-easy-nav.el --- config for view source code  -*- lexical-binding: t; -*-

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

;;; Require

;;; Code:

(defvar easy-nav-map nil
  "Keymap used when popup is shown.")

(setq easy-nav-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "h") #'backward-char)
        (define-key map (kbd "l") #'forward-char)
        (define-key map (kbd "j") #'next-line)
        (define-key map (kbd "k") #'previous-line)
        (define-key map (kbd "g") #'beginning-of-buffer)
        (define-key map (kbd "G") #'end-of-buffer)
        (define-key map (kbd "q") #'easy-nav-exist)
        map))

(define-minor-mode easy-nav-mode
  "Easy navigator."
  :keymap easy-nav-map
  :init-value nil)

(defun easy-nav-enter ()
  (interactive)
  (read-only-mode 1)
  (easy-nav-mode 1)
  (message "Enter easy navigator."))

(defun easy-nav-exist ()
  (interactive)
  (read-only-mode -1)
  (easy-nav-mode -1)
  (message "Exit easy navigator."))

(defun easy-nav-jump ()
  (interactive)
  (lsp-bridge-jump)
  (easy-nav-enter))

(provide 'init-easy-nav)
;;; init-easy-nav.el ends here
