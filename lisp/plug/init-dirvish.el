;;; init-dirvish.el ---improved version of the emacs inbuilt package dired  -*- lexical-binding: t; -*-

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

(require 'dirvish)
(dirvish-override-dired-mode)
(setq delete-by-moving-to-trash t)

(eval-after-load 'dirvish
  '(progn
     (setq dirvish-mode-map (make-sparse-keymap))
     ;; cursor move
     (define-key dirvish-mode-map (kbd "n") #'dired-next-line)
     (define-key dirvish-mode-map (kbd "h") #'dired-up-directory)
     (define-key dirvish-mode-map (kbd "j") #'dired-next-line)
     (define-key dirvish-mode-map (kbd "k") #'dired-previous-line)
     (define-key dirvish-mode-map (kbd "l") #'dired-find-file)
     ;; mark
     (define-key dirvish-mode-map (kbd "d") #'dired-flag-file-deletion)
     (define-key dirvish-mode-map (kbd "x") #'dired-do-flagged-delete)
     (define-key dirvish-mode-map (kbd "u") #'dired-unmark)
     (define-key dirvish-mode-map (kbd "U") #'dired-unmark-all-marks)
     ;; file copy, move, create
     (define-key dirvish-mode-map (kbd "+") #'dired-create-directory)
     (define-key dirvish-mode-map (kbd "R") #'dired-do-rename)
     (define-key dirvish-mode-map (kbd "C") #'dired-do-copy)
     ;; file access
     (define-key dirvish-mode-map (kbd "v") #'dired-view-file)
     (define-key dirvish-mode-map (kbd "o") #'dired-find-file-other-window)
     (define-key dirvish-mode-map (kbd "C-o") #'dired-display-file)
     ;; refresh
     (define-key dirvish-mode-map (kbd "C-/") #'dired-undo)
     (define-key dirvish-mode-map (kbd "g") #'revert-buffer)
     ;; quit
     (define-key dirvish-mode-map (kbd "q") #'dirvish-quit)
     ;; other
     (define-key dirvish-mode-map (kbd "w") #'dired-copy-filename-as-kill)
     (define-key dirvish-mode-map (kbd "Z") #'dired-do-compress)
     (define-key dirvish-mode-map (kbd "X") #'dired-do-shell-command)
     (define-key dirvish-mode-map (kbd "B") #'dired-do-byte-compile)
     (define-key dirvish-mode-map (kbd "L") #'dired-do-load)
     ))


(provide 'init-dirvish)
;;; init-dirvish.el ends here
