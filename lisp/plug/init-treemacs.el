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

;;; Code:


;; A tree layout file explorer
(require 'treemacs)

(with-eval-after-load 'treemacs
  (custom-set-faces
   '(treemacs-directory-face      ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
   '(treemacs-file-face           ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
   '(treemacs-git-added-face      ((t (:inherit treemacs-file-face))))
   '(treemacs-git-conflict-face   ((t (:inherit treemacs-file-face))))
   '(treemacs-git-deleted-face    ((t (:inherit treemacs-file-face))))
   '(treemacs-git-ignored-face    ((t (:inherit treemacs-file-face))))
   '(treemacs-git-modified-face   ((t (:inherit treemacs-file-face))))
   '(treemacs-git-renamed-face    ((t (:inherit treemacs-file-face))))
   '(treemacs-git-unmodified-face ((t (:inherit treemacs-file-face))))
   '(treemacs-git-untracked-face  ((t (:inherit treemacs-file-face))))
   '(treemacs-root-face           ((t (:height 1 :blod nil :family "JetBrainsMono Nerd Font"))))
   '(variable-pitch               ((t nil)))))
(require 'treemacs-hydras)
(require 'hydra)


;; ([mouse-1]   . treemacs-single-click-expand-action)
;; ("o 1"       . treemacs-visit-node-first-window))

;; (treemacs-define-RET-action 'file-node-closed
;;                             #'(lambda(&optional arg)
;;                                 (treemacs-visit-node-in-most-recently-used-window)
;;                                 (delete-window (treemacs-get-local-window))))

(defun treemacs-visit-node-first-window(&optional arg)
  (interactive "P")
  (treemacs-visit-node-no-split)
  (delete-window (treemacs-get-local-window)))

(setq treemacs-filewatch-mode t
      treemacs-git-mode nil
      treemacs-follow-mode nil
      treemacs-show-cursor t
      treemacs-sorting 'alphabetic-asc
      treemacs-missing-project-action 'remove
      treemacs-width 30)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
