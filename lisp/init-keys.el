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

(lazy-load-unset-keys '("C-x C-f"
                        "C-z"
                        "C-q"
                        "s-T"
                        "s-W"
                        "s-z"
                        "M-h" "C-\\" "s-c" "s-x" "s-v" "C-6" "M-." "M-,"
                        "M-x"
                        "M-z" ;; zap-to-char like vim df?
                        ))


;; here is C-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys '(("C-<return>" . consult-bookmark)) "consult")
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-<return>" . bookmark-jump)) org-mode-map))
(lazy-load-set-keys '(("C-<tab>" . spacemacs/alternate-buffer))) ;; init-funs has be required by init.el

(lazy-load-global-keys '(("C-," . goto-last-change)) "goto-last-change")
(lazy-load-global-keys '(("C-;" . avy-goto-char-2)) "init-avy")

(lazy-load-global-keys '(("C-a" . mwim-beginning-of-line-or-code)
                         ("C-e" . mwim-end-of-line-or-code))
                       "mwim")

;; h for help
(lazy-load-set-keys '(("C-h C-f" . find-function)
                      ("C-h C-v" . find-variable)
                      ("C-h C-k" . find-function-on-key)))

(lazy-load-global-keys '(("C-o" . open-newline-above)
                         ("C-l" . open-newline-below))
                       "open-newline")

(lazy-load-set-keys '(("C-q" . quoted-insert)))
(lazy-load-global-keys '(("C-s" . consult-line)) "consult")

;; w for window, unify keys with vim, s-w instead of C-w
(lazy-load-set-keys '(("C-\\ h" . windmove-left)
                      ("C-\\ j" . windmove-down)
                      ("C-\\ k" . windmove-up)
                      ("C-\\ l" . windmove-right)
                      ("C-\\ =" . balance-windows)
                      ("C-\\ m" . delete-other-windows)))
(lazy-load-global-keys '(("C-\\ H" . vanilla/split-window-left-with-balance)
                         ("C-\\ J" . vanilla/split-window-below-with-balance)
                         ("C-\\ K" . vanilla/split-window-up-with-balance)
                         ("C-\\ L" . vanilla/split-window-right-with-balance)
                         ("C-\\ c" . vanilla/delete-window-with-balance)
                         ("C-\\ x" . vanilla/exchange-split-window-position-structure)
                         ("C-\\ |" . split-window-horizontally-instead)
                         ("C-\\ _" . split-window-vertically-instead))
                       "windowop")



;; here is M-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-set-keys '(("M-:" . execute-extended-command)))
(lazy-load-set-keys '(("M-;" . comment-dwim)))
(lazy-load-global-keys '(("M-<backspace>" . delete-block-backward)
                         ("M-d" . delete-block-forward))
                       "delete-block")
;; M-e -> expand-region ?
(lazy-load-set-keys '(("M-h" . mark-paragraph)))

(lazy-load-global-keys '(("M-n" . vanilla/move-cursor-8-lines-down)
                         ("M-p" . vanilla/move-cursor-8-lines-up))
                       "cursormove")

(lazy-load-global-keys '(("M-s" . symbol-overlay-put)) "init-symbol-overlay")

(lazy-load-global-keys '(("M-y" . consult-yank-pop)) "consult") ;; yank-pop

(lazy-load-global-keys '(("M-z t" . vanilla/move-to-window-top)
                         ("M-z z" . vanilla/move-to-window-middle)
                         ("M-z b" . vanilla/move-to-window-bottom))
                       "cursormove")



;; here is s-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys '(("s-h" . vanilla/scroll-right-half-page)
                         ("s-l" . vanilla/scroll-left-half-page))
                       "cursormove")
(lazy-load-global-keys '(("s-N" . move-text-down)
                         ("s-P" . move-text-up))
                       "move-text")
(with-eval-after-load 'org
  (lazy-load-set-keys '(("s-<return>" . org-insert-heading-respect-content)) org-mode-map)) ;; origin C-RET






;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; a for alone apps
(lazy-load-set-keys '(("C-c x" . org-capture)
                      ("C-c a a" . org-agenda)))

(lazy-load-global-keys '(("C-c a f" . fanyi-dwim)) "init-fanyi")

;; b for buffer, bookmark
(lazy-load-set-keys '(("C-c b m" . bookmark-set)
                      ("C-c b r" . bookmark-rename)
                      ("C-c b d" . bookmark-delete)
                      ("C-c b l" . bookmark-bmenu-list)
                      ("C-c b s" . bookmark-save)))
(lazy-load-global-keys '(("C-c b x" . vanilla/create-scratch-buffer)) "basic-tookit")
(lazy-load-global-keys '(("C-c b b" . consult-buffer)) "consult")


;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(lazy-load-global-keys '(("C-c c b" . literate-calc-eval-buffer)
                         ("C-c c i" . literate-calc-insert-results)
                         ("C-c c m" . literate-calc-minor-mode)
                         ("C-c c l" . literate-calc-eval-line)
                         ("C-c c c" . literate-calc-clear-overlays))
                       "literate-calc-mode")

;; e for eshell
(lazy-load-global-keys '(("C-c e n" . aweshell-new)
                         ("C-c e t" . aweshell-toggle)
                         ("C-c e d" . aweshell-dedicated-toggle)
                         ("C-c e C-n" . aweshell-next)
                         ("C-c e C-p" . aweshell-prev))
                       "init-eshell")

(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c e p" . +preview-current-buffer-in-browser)) org-mode-map))

;; f for find
(lazy-load-set-keys '(("C-c f f" . find-file)
                      ("C-c f p" . project-find-file)))

(lazy-load-global-keys '(("C-c f r" . consult-recent-file)
                         ("C-c f g" . consult-ripgrep)
                         ("C-c f m" . consult-mark))
                       "consult")

(lazy-load-global-keys '(("C-c f t" . treemacs)) "init-treemacs")


;; h for hideshow unify with vim
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(lazy-load-set-keys '(("C-c h m" . hs-toggle-hiding)
                      ("C-c h r" . hs-show-all)
                      ("C-c h a" . hs-hide-all)
                      ("C-c h o" . hs-show-block)
                      ("C-c h c" . hs-hide-block)))

;; i for insert
(lazy-load-global-keys '(("C-c i t" . hl-todo-insert)) "hl-todo")
(lazy-load-global-keys '(("C-c i y" . yas-insert-snippet)) "yasnippet")
;; org-mode-map
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c i l" . +dwim-create-link-with-datetime)
                        ("C-c i i" . +org-insert-image)
                        ("C-c i !" . (lambda () (interactive) (org-time-stamp-inactive '(16)))))
                      org-mode-map))

;; jk for jump and jump back
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c j" . (lambda() (interactive) (org-open-at-point)))
                        ("C-c k" . (lambda() (interactive) (org-mark-ring-goto))))
                      org-mode-map)) ;; and lsp-bridge

;; n for narrow
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c n s" . org-narrow-to-subtree)
                        ("C-c n w" . widen))
                      org-mode-map))

;; o for open
(lazy-load-set-keys '(("C-c o i" . (lambda() (interactive) (find-file (concat haoran/home-directory "/haoran/no/org/wiki/index.org"))))
                      ("C-c o s" . (lambda() (interactive) (find-file (concat haoran/home-directory "/haoran/no/org/site/index.org"))))
                      ("C-c o f r" . (lambda() (interactive) (find-file (concat haoran/home-directory "/.emacs.d/init.el"))))))

;; s for switch
(lazy-load-set-keys '(("C-c s" . tab-bar-switch-to-tab)))

;; t for tab
(lazy-load-global-keys '(("C-c t n" . +create-new-tab-bar)
                         ("C-c t c" . tab-bar-close-tab)
                         ("C-c t r" . tab-bar-rename-tab))
                       "init-tab-bar")

;; u for user
(lazy-load-set-keys '(("C-c u f" . +unfill-paragraph)
                      ("C-c u i" . +indent-buffer)))

;; here is C-x ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-keys)
;;; init-keys.el ends here
