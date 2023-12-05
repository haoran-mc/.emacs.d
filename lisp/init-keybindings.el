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

(global-set-key (kbd "M-:") 'execute-extended-command)

;; h for help
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)


;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(global-set-key (kbd "C-c c b") 'literate-calc-eval-buffer)
(global-set-key (kbd "C-c c i") 'literate-calc-insert-results)
(global-set-key (kbd "C-c c m") 'literate-calc-minor-mode)
(global-set-key (kbd "C-c c l") 'literate-calc-eval-line)
(global-set-key (kbd "C-c c c") 'literate-calc-clear-overlays)


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


;; s for switch
(global-set-key (kbd "C-c s") 'tab-bar-switch-to-tab)

;; u for user
(global-set-key (kbd "C-c u f") '+unfill-paragraph)


(provide 'init-keybindings)
;;; init-keybindings.el ends here
