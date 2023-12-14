;;; init-eshell.el --- emacs shell                   -*- lexical-binding: t; -*-

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

;;  (with-eval-after-load 'eshell
;;    (custom-set-faces
;;     '(epe-dir-face ((t (:inherit bold :foreground "gray"))))
;;     '(epe-git-face ((t (:foreground "skyblue"))))
;;     '(epe-pipeline-host-face ((t (:foreground "skyblue"))))
;;     '(epe-pipeline-user-face ((t (:foreground "darkcyan"))))
;;     '(epe-pipeline-time-face ((t (:foreground "darkorange"))))))
;;

;;; Code:

;; emacs command shell

(require 'aweshell)

(define-key eshell-mode-map (kbd "C-l") 'eshell/clear)

(add-hook 'eshell-mode-hook (lambda() (setq truncate-lines t)))

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(add-hook 'eshell-mode #'(bind-key "C-l" 'eshell/clear eshell-mode-map))
(add-hook 'eshell-mode #'(eshell/alias "l" "ls -lah"))
(add-hook 'eshell-mode #'(eshell/alias "ll" "ls -l"))
(add-hook 'eshell-mode #'(eshell/alias "la" "ls -lAFh"))
(add-hook 'eshell-mode #'(eshell/alias "lr" "ls -tRFh"))
(add-hook 'eshell-mode #'(eshell/alias "gm" "go run main.go"))
(add-hook 'eshell-mode #'(eshell/alias "pm" "python main.py"))
(defalias 'eshell/e #'eshell/emacs)
(defalias 'eshell/q #'eshell/exit)
(defalias 'eshell/c #'eshell/clear)
(defalias 'eshell/imgcat #'eshell/imgcat)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/emacs (&rest args)
  "Open a file (ARGS) in Emacs.  Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))

(defun eshell/imgcat (&rest args)
  "Display image files."
  (unless args (error "Usage: imgcat FILE ..."))
  (dolist (img (eshell-flatten-list args))
    (eshell-printn
     (propertize " " 'display (create-image img)))))

(setq comint-prompt-read-only t
      eshell-banner-message "")

(provide 'init-eshell)
;;; init-eshell.el ends here
