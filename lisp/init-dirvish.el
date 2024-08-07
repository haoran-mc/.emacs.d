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

;;; Require:
;;
(require 'dirvish)
(require 'dirvish-icons)
(require 'dirvish-subtree)
(require 'all-the-icons)

;;; Code:
(dirvish-override-dired-mode)

(setq mouse-1-click-follows-link nil
      dired-kill-when-opening-new-dired-buffer t ;; added in Emacs 28
      dired-mouse-drag-files t                   ;; added in Emacs 29
      mouse-drag-and-drop-region-cross-program t ;; added in Emacs 29
      dired-dwim-target t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-do-revert-buffer t
      dired-auto-revert-buffer #'dired-directory-changed-p
      dired-hide-details-hide-symlink-targets nil
      ;; --almost-all exclude `.' and `..'
      dired-listing-switches "-l  --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"
      delete-by-moving-to-trash t)

(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

(setq dirvish-attributes '(subtree-statesubtree-state all-the-icons file-size)
      dirvish-all-the-icons-height 0.9
      dirvish-all-the-icons-offset 0.1
      dirvish-subtree-prefix "  ")


(setq dirvish-mode-map (make-sparse-keymap)) ;; dirvish-mode 键置空
;; cursor move
(define-key dirvish-mode-map (kbd "h") 'dired-up-directory)
(define-key dirvish-mode-map (kbd "j") 'dired-next-line)
(define-key dirvish-mode-map (kbd "k") 'dired-previous-line)
(define-key dirvish-mode-map (kbd "l") 'dired-find-file)
(define-key dirvish-mode-map (kbd "<return>") #'dired-find-file)
;; file copy, move, create, delete
(define-key dirvish-mode-map (kbd "+") 'dired-create-directory)
(define-key dirvish-mode-map (kbd "R") 'dired-do-rename)
(define-key dirvish-mode-map (kbd "c") 'dired-do-copy)
(define-key dirvish-mode-map (kbd "X") 'dired-do-delete)
;; mark
(define-key dirvish-mode-map (kbd "d") 'dired-flag-file-deletion)
(define-key dirvish-mode-map (kbd "x") 'dired-do-flagged-delete)
(define-key dirvish-mode-map (kbd "u") 'dired-unmark)
(define-key dirvish-mode-map (kbd "U") 'dired-unmark-all-marks)
;; file access
(define-key dirvish-mode-map (kbd "o") 'dired-find-file-other-window)
(define-key dirvish-mode-map (kbd "C-v") 'dired-view-file)
(define-key dirvish-mode-map (kbd "C-o") 'dired-display-file) ;; underused function
;; refresh
(define-key dirvish-mode-map (kbd "C-/") 'dired-undo)
(define-key dirvish-mode-map (kbd "g") 'revert-buffer)
;; quit
(define-key dirvish-mode-map (kbd "q") 'dirvish-quit)
;; other
(define-key dirvish-mode-map (kbd "/") 'dired-goto-file)
(define-key dirvish-mode-map (kbd "I") 'dired-insert-subdir)
(define-key dirvish-mode-map (kbd "w") 'dired-copy-filename-as-kill)
(define-key dirvish-mode-map (kbd "Z") 'dired-do-compress)
(define-key dirvish-mode-map (kbd "B") 'dired-do-byte-compile)
(define-key dirvish-mode-map (kbd "L") 'dired-do-load)
;; (define-key dirvish-mode-map (kbd "X") #'dired-do-shell-command)

(define-key dirvish-mode-map (kbd "TAB") 'dirvish-subtree-toggle)


(provide 'init-dirvish)
;;; init-dirvish.el ends here
