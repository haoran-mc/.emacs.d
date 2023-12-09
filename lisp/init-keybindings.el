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

;; (global-unset-key (kbd "C-w")) ;; s-w kill-region
(lazy-load-unset-keys '("C-x C-f" "C-z" "C-q" "s-T" "s-W" "s-z" "M-h" "C-\\" "s-c" "s-x" "s-v" "C-6" "M-." "M-,"))

;; here is C-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (lazy-load-global-keys '(("C-<return>" . bookmark-jump)) "bookmark")
(lazy-load-set-keys '(("C-<return>" . bookmark-jump)))
(lazy-load-set-keys '(("C-<return>" . bookmark-jump)) org-mode-map)
(lazy-load-set-keys '(("C-<tab>" . spacemacs/alternate-buffer))) ;; init-funs has be required by init.el

;; b for buffer, bookmark
(lazy-load-set-keys '(("C-c b m" . bookmark-set)
                      ("C-c b r" . bookmark-rename)
                      ("C-c b d" . bookmark-delete)
                      ("C-c b l" . bookmark-bmenu-list)
                      ("C-c b s" . bookmark-save)))

;; h for help
(lazy-load-set-keys '(("C-h C-f" . find-function)
                      ("C-h C-v" . find-variable)
                      ("C-h C-k" . find-function-on-key)))

(lazy-load-global-keys '(("C-o" . open-newline-above)
                         ("C-l" . open-newline-below))
                       "open-newline")

;; w for window, unify keys with vim, M-w instead of C-w
(global-set-key (kbd "C-w h") 'windmove-left)
(global-set-key (kbd "C-w j") 'windmove-down)
(global-set-key (kbd "C-w n") 'windmove-down)
(global-set-key (kbd "C-w p") 'windmove-up)
(global-set-key (kbd "C-w k") 'windmove-up)
(global-set-key (kbd "C-w l") 'windmove-right)
(global-set-key (kbd "C-w s") 'split-window-vertically)
(global-set-key (kbd "C-w v") 'split-window-horizontally)
(global-set-key (kbd "C-w m") 'delete-other-windows)
(global-set-key (kbd "C-w c") 'delete-window)
(global-set-key (kbd "C-w =") 'balance-windows)

;; here is M-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-set-keys '(("M-:" . execute-extended-command)))
;; M-e -> expand-region ?


;; here is s-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-set-keys '(("s-w" . kill-region)))
(lazy-load-set-keys '(("s-<return>" . org-insert-heading-respect-content)) org-mode-map) ;; origin C-RET


;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(lazy-load-global-keys '(("C-c c b" . literate-calc-eval-buffer)
                         ("C-c c i" . literate-calc-insert-results)
                         ("C-c c m" . literate-calc-minor-mode)
                         ("C-c c l" . literate-calc-eval-line)
                         ("C-c c c" . literate-calc-clear-overlays))
                       "literate-calc-mode")

;; f for file
(lazy-load-set-keys '(("C-c f r" . recentf-open-files)
                      ("C-c f f" . find-file)))

;; i for insert
(lazy-load-global-keys '(("C-c i t" . hl-todo-insert)) "hl-todo")
(lazy-load-global-keys '(("C-c i y" . yas-insert-snippet)) "yasnippet")
;; org-mode-map
(lazy-load-set-keys '(("C-c i l" . +dwim-create-link-with-datetime)
                      ("C-c i i" . +org-insert-image)
                      ("C-c i !" . (lambda () (interactive) (org-time-stamp-inactive '(16)))))
                    org-mode-map)

;; jk for jump and jump back
(lazy-load-set-keys '(("C-c j" . (lambda() (interactive) (org-open-at-point)))
                      ("C-c k" . (lambda() (interactive) (org-mark-ring-goto))))
                    org-mode-map) ;; and lsp-bridge

;; n for narrow
(lazy-load-set-keys '(("C-c n s" . org-narrow-to-subtree)
                      ("C-c n w" . widen))
                    org-mode-map)

;; o for open
(lazy-load-set-keys '(("C-c o i" . (lambda() (interactive) (find-file "~/haoran/no/org/wiki/index.org")))
                      ("C-c o s" . (lambda() (interactive) (find-file "~/haoran/no/org/site/index.org")))
                      ("C-c o f r" . (lambda() (interactive) (find-file "~/.emacs.d/init.el")))))

;; s for switch
(lazy-load-set-keys '(("C-c s" . tab-bar-switch-to-tab)))

;; t for tab
(lazy-load-set-keys '(("C-c t n" . +create-new-tab-bar)
                      ("C-c t c" . tab-bar-close-tab)
                      ("C-c t r" . tab-bar-rename-tab)))

;; u for user
(lazy-load-set-keys '(("C-c u f" . +unfill-paragraph)
                      ("C-c u i" . +indent-buffer)))

;; here is C-x ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-keybindings)
;;; init-keybindings.el ends here
