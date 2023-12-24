;;; init-mini.el --- The minimal configuration       -*- lexical-binding: t; -*-

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


;; (setq debug-on-error t)
;; (setq-default lexical-binding t)


;; (global-unset-key (kbd "C-w")) ;; s-w kill-region

;; ;; w for window, unify keys with vim, M-w instead of C-w
;; (global-set-key (kbd "C-w h") 'windmove-left)
;; (global-set-key (kbd "C-w j") 'windmove-down)
;; (global-set-key (kbd "C-w n") 'windmove-down)
;; (global-set-key (kbd "C-w p") 'windmove-up)
;; (global-set-key (kbd "C-w k") 'windmove-up)
;; (global-set-key (kbd "C-w l") 'windmove-right)
;; (global-set-key (kbd "C-w s") 'split-window-vertically)
;; (global-set-key (kbd "C-w v") 'split-window-horizontally)
;; (global-set-key (kbd "C-w m") 'delete-other-windows)
;; (global-set-key (kbd "C-w c") 'delete-window)
;; (global-set-key (kbd "C-w =") 'balance-windows)

(global-set-key (kbd "C-c o f r") #'(lambda() (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c o i") #'(lambda() (interactive) (find-file "~/haoran/no/org/wiki/index.org")))
(global-set-key (kbd "C-c o s") #'(lambda() (interactive) (find-file "~/haoran/no/org/site/index.org")))

;; (provide 'init-mini)
;;; init-mini.el ends here
