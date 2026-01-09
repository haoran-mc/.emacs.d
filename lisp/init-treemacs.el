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

(require 'cfrs)
;; ace-window, hydra


;;; Code:
(setq treemacs-filewatch-mode t
      treemacs-git-mode nil
      treemacs-show-git-status nil
      treemacs-follow-mode nil
      treemacs-show-cursor t
      treemacs-sorting 'alphabetic-asc
      treemacs-missing-project-action 'remove
      treemacs-width 35)


(setq treemacs-mode-map (make-sparse-keymap))
;; navigation
(define-key treemacs-mode-map (kbd "h") #'backward-char)
(define-key treemacs-mode-map (kbd "j") #'treemacs-next-line)
(define-key treemacs-mode-map (kbd "k") #'treemacs-previous-line)
(define-key treemacs-mode-map (kbd "l") #'forward-char)
(define-key treemacs-mode-map (kbd "H") #'treemacs-goto-parent-node) ;; treemacs-collapse-parent-node
(define-key treemacs-mode-map (kbd "M-p") #'treemacs-collapse-parent-node)
(define-key treemacs-mode-map (kbd "rd") #'treemacs-root-down)
(define-key treemacs-mode-map (kbd "ru") #'treemacs-root-up)
;; (define-key treemacs-mode-map (kbd "M-H") #'treemacs-root-up)
;; (define-key treemacs-mode-map (kbd "M-L") #'treemacs-root-down)
;; mouse
(define-key treemacs-mode-map (kbd "<mouse-3>") #'treemacs-rightclick-menu)
(define-key treemacs-mode-map (kbd "<double-mouse-1>") #'treemacs-doubleclick-action)
(define-key treemacs-mode-map (kbd "<drag-mouse-1>") #'treemacs-dragleftclick-action)
(define-key treemacs-mode-map (kbd "<down-mouse-1>") #'treemacs-leftclick-action)
;; open
(define-key treemacs-mode-map (kbd "o") #'treemacs-visit-node-ace)
(define-key treemacs-mode-map (kbd "<tab>") #'treemacs-TAB-action)
(define-key treemacs-mode-map (kbd "<return>") #'treemacs-RET-action)
;; create, copy, move
;; (define-key treemacs-mode-map (kbd "a") #'treemacs-create-file) ;; conflict with ace-window, just use C-c f x
(define-key treemacs-mode-map (kbd "+") #'treemacs-create-dir)
(define-key treemacs-mode-map (kbd "R") #'treemacs-move-file) ;; rename
(define-key treemacs-mode-map (kbd "d") #'treemacs-delete-file)
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


;; keeping my fringe settings
(advice-remove #'treemacs-select-window #'doom-themes-hide-fringes-maybe)


(defun my/treemacs-add-current-project-workspace-exclusively ()
  "Add the current project to the Treemacs workspace."
  (interactive)
  (require 'project)
  (let ((project (project-current t)))
    (if project
        (let ((project-path (project-root project)))
          (if (and project-path (file-directory-p project-path))
              (treemacs-add-and-display-current-project-exclusively)
            (message "Invalid project path: %s" project-path)))
      (message "No project found for current buffer"))))


;; set treemacs faces
(defvar my/theme-fg (face-foreground 'default))
(set-face-attribute 'treemacs-directory-face nil :weight 'normal :family ran--font-family :foreground my/theme-fg)
(set-face-attribute 'treemacs-file-face      nil :weight 'normal :family ran--font-family :foreground my/theme-fg)
(set-face-attribute 'treemacs-root-face      nil :weight 'normal :family ran--font-family)

;; Git 状态相关，统一继承自 treemacs-file-face
(set-face-attribute 'treemacs-git-added-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-modified-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-unmodified-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-ignored-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-conflict-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-renamed-face nil :inherit 'treemacs-file-face)
(set-face-attribute 'treemacs-git-untracked-face nil :inherit 'treemacs-file-face)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
