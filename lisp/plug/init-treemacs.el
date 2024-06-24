;;; init-treemacs.el --- file tree -*- lexical-binding: t; -*-

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
;; A tree layout file explorer
(require 'treemacs)
(require 'treemacs-mouse-interface)
(require 'treemacs-file-management)
(require 'treemacs-hydras)

(require 'init-ace-window)
(require 'init-hydra)

;;; Code:

(setq treemacs-filewatch-mode t
      treemacs-git-mode nil
      treemacs-follow-mode nil
      treemacs-show-cursor t
      treemacs-sorting 'alphabetic-asc
      treemacs-missing-project-action 'remove
      treemacs-width 35)

;; (treemacs-define-RET-action 'file-node-closed
;;                             #'(lambda(&optional arg)
;;                                 (treemacs-visit-node-in-most-recently-used-window)
;;                                 (delete-window (treemacs-get-local-window))))

;; (defun treemacs-visit-node-first-window(&optional arg)
;;   (interactive "P")
;;   (treemacs-visit-node-no-split)
;;   (delete-window (treemacs-get-local-window)))

(setq treemacs-mode-map (make-sparse-keymap))
;; navigation
(define-key treemacs-mode-map (kbd "h") #'treemacs-goto-parent-node)
(define-key treemacs-mode-map (kbd "j") #'treemacs-next-line)
(define-key treemacs-mode-map (kbd "k") #'treemacs-previous-line)
;; (define-key treemacs-mode-map (kbd "H") #'treemacs-collapse-parent-node)
(define-key treemacs-mode-map (kbd "M-H") #'treemacs-root-up)
(define-key treemacs-mode-map (kbd "M-L") #'treemacs-root-down)
;; open
(define-key treemacs-mode-map (kbd "<tab>") #'treemacs-TAB-action)
(define-key treemacs-mode-map (kbd "l") #'treemacs-visit-node-ace)
(define-key treemacs-mode-map (kbd "<return>") #'treemacs-visit-node-ace)
;; create, copy, move
(define-key treemacs-mode-map (kbd "a") #'treemacs-create-file) ;; C-c f x
(define-key treemacs-mode-map (kbd "+") #'treemacs-create-dir)
(define-key treemacs-mode-map (kbd "m") #'treemacs-move-file)
(define-key treemacs-mode-map (kbd "yf") #'treemacs-copy-file)
(define-key treemacs-mode-map (kbd "yp") #'treemacs-copy-project-path-at-point)  ;; root path
(define-key treemacs-mode-map (kbd "ya") #'treemacs-copy-absolute-path-at-point) ;; absolute
(define-key treemacs-mode-map (kbd "yr") #'treemacs-copy-relative-path-at-point) ;; relative
;; workspace
(define-key treemacs-mode-map (kbd "we") #'treemacs-edit-workspaces)
(define-key treemacs-mode-map (kbd "wa") #'treemacs-create-workspace)
(define-key treemacs-mode-map (kbd "wd") #'treemacs-remove-workspace)
(define-key treemacs-mode-map (kbd "wr") #'treemacs-rename-workspace)
(define-key treemacs-mode-map (kbd "ws") #'treemacs-switch-workspace)
(define-key treemacs-mode-map (kbd "wn") #'treemacs-next-workspace)
(define-key treemacs-mode-map (kbd "wf") #'treemacs-set-fallback-workspace)
;; project
(define-key treemacs-mode-map (kbd "pa") #'treemacs-add-project-to-workspace)
(define-key treemacs-mode-map (kbd "pd") #'treemacs-remove-project-from-workspace)
;; width
(define-key treemacs-mode-map (kbd "g") #'treemacs-refresh)
(define-key treemacs-mode-map (kbd "W") #'treemacs-set-width)
(define-key treemacs-mode-map (kbd "/") #'treemacs-common-helpful-hydra)
(define-key treemacs-mode-map (kbd "?") #'treemacs-advanced-helpful-hydra)

(with-eval-after-load 'project
  (defun +treemacs-add-current-project-workspace ()
    "Add the current project to the Treemacs workspace."
    (interactive)
    (let ((project (project-current t)))
      (if project
          (let ((project-path (project-root project)))
            (if (and project-path (file-directory-p project-path))
                (treemacs-add-project-to-workspace project-path)
              (message "Invalid project path: %s" project-path)))
        (message "No project found for current buffer"))))
  (global-set-key (kbd "C-c p t") #'+treemacs-add-current-project-workspace))

;; keeping my fringe settings
(advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)


(provide 'init-treemacs)
;;; init-treemacs.el ends here
