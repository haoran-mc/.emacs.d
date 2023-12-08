;;; init-keybindings.el --- keys -*- lexical-binding: t; -*-

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

(global-unset-key (kbd "C-w"))


;; here is csM-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-:") 'execute-extended-command)
;; M-e -> expand-region ?
(global-set-key (kbd "C-<return>") 'bookmark-jump)
(global-set-key (kbd "C-<tab>") 'spacemacs/alternate-buffer)
;; (global-set-key (kbd "C-[") 'keyboard-quit)

;; b for buffer, bookmark
(global-set-key (kbd "C-c b m") 'bookmark-set)
(global-set-key (kbd "C-c b r") 'bookmark-rename)
(global-set-key (kbd "C-c b d") 'bookmark-delete)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c b s") 'bookmark-save)

;; h for help
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; j for jump
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-j") #'(lambda() (interactive)
                                           (org-open-at-point)))
  (define-key org-mode-map (kbd "C-k") #'(lambda() (interactive)
                                           (org-mark-ring-goto)))
  (define-key org-mode-map (kbd "C-<return>") 'bookmark-jump)
  (define-key org-mode-map (kbd "s-<return>") 'org-insert-heading-respect-content)
)


(global-set-key (kbd "C-o") 'open-newline-above)
(global-set-key (kbd "C-l") 'open-newline-below)


;; w for window, unify keys with vim, M-w instead of C-w
(global-set-key (kbd "s-w") 'kill-region)
(global-set-key (kbd "C-w h") 'windmove-left)
(global-set-key (kbd "C-w j") 'windmove-down)
(global-set-key (kbd "C-w k") 'windmove-up)
(global-set-key (kbd "C-w l") 'windmove-right)
(global-set-key (kbd "C-w s") 'split-window-vertically)
(global-set-key (kbd "C-w v") 'split-window-horizontally)
(global-set-key (kbd "C-w o") 'delete-other-windows)
(global-set-key (kbd "C-w c") 'delete-window)
;; (global-set-key (kbd "C-w ="))

;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(global-set-key (kbd "C-c c b") 'literate-calc-eval-buffer)
(global-set-key (kbd "C-c c i") 'literate-calc-insert-results)
(global-set-key (kbd "C-c c m") 'literate-calc-minor-mode)
(global-set-key (kbd "C-c c l") 'literate-calc-eval-line)
(global-set-key (kbd "C-c c c") 'literate-calc-clear-overlays)


;; f for file
(global-set-key (kbd "C-c f r") 'recentf-open-files)
(global-set-key (kbd "C-c f f") 'find-file)


;; i for insert
(global-set-key (kbd "C-c i t") 'hl-todo-insert)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c i l") #'+dwim-create-link-with-datetime)
  (define-key org-mode-map (kbd "C-c i i") #'+org-insert-image)
  (define-key org-mode-map (kbd "C-c i y") #'yas-insert-snippet)
  (define-key org-mode-map (kbd "C-c i ,") #'org-insert-structure-template)
  (define-key org-mode-map (kbd "C-c i !") #'(lambda () (interactive) (org-time-stamp-inactive '(16)))))


;; n for narrow
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c n s") #'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-c n w") #'widen))

;; o for open
(global-set-key (kbd "C-c o i") #'(lambda() (interactive) (find-file "~/haoran/no/org/wiki/index.org")))
(global-set-key (kbd "C-c o s") #'(lambda() (interactive) (find-file "~/haoran/no/org/site/index.org")))

;; s for switch
(global-set-key (kbd "C-c s") 'tab-bar-switch-to-tab)

;; t for tab
(global-set-key (kbd "C-c t n") '+create-new-tab-bar)
(global-set-key (kbd "C-c t c") 'tab-bar-close-tab)
(global-set-key (kbd "C-c t r") 'tab-bar-rename-tab)

;; u for user
(global-set-key (kbd "C-c u f") '+unfill-paragraph)
(global-set-key (kbd "C-c u r") '+open-file-init)


;; here is C-x ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-keybindings)
;;; init-keybindings.el ends here
