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
                        ;; "C-z" ;; suspend-frame
                        "C-q"
                        "s-T"
                        "s-W"
                        "s-z"
                        "M-h" "C-\\" "s-c" "s-x" "s-v" "C-6" "M-." "M-,"
                        "M-x"
                        "M-z" ;; zap-to-char like vim df?
                        "ESC ESC ESC"
                        "C-t" ;; transpose-chars
                        ))

;; kill keyboard-escape-quit a few usage scenarios
;; (lazy-load-set-keys '(("ESC ESC ESC" . (lambda () (interactive) (message "ESC!")))))


;; here is C-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-funs has be required by init.el
(lazy-load-global-keys '(("C-<return>" . consult-bookmark)) "consult")
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-<return>" . bookmark-jump)) org-mode-map))

(lazy-load-global-keys '(("C-<tab>" . spacemacs/alternate-buffer)
                         ("C-<backspace>" . crux-kill-line-backwards))
                       "crux")

(lazy-load-global-keys '(("C-," . goto-last-change)) "goto-last-change")
;; C-. format code
(lazy-load-set-keys '(("C-;" . avy-goto-char)))
(lazy-load-global-keys '(("C-?" . vundo)) "init-vundo") ;; keep C-/ undo, use C-? vundo instead undo-redo

(lazy-load-global-keys '(("C-a" . mwim-beginning-of-line-or-code)
                         ("C-e" . mwim-end-of-line-or-code))
                       "mwim")

;; h for help
(lazy-load-set-keys '(("C-h C-f" . find-function)
                      ("C-h C-v" . find-variable)
                      ("C-h C-k" . find-function-on-key)))

(lazy-load-global-keys '(("C-j" . vanilla/merge-line-down) ;; electric-newline-and-maybe-indent
                         ("C-k" . vanilla/smart-kill-line))
                       "basic-tookit")

(lazy-load-global-keys '(("C-o" . open-newline-above)  ;; open-line
                         ("C-l" . open-newline-below)) ;; recenter-top-bottom
                       "open-newline")

(lazy-load-set-keys '(("C-q" . quoted-insert)))
(lazy-load-global-keys '(("C-s" . consult-line)) "consult")

;; C-t transpose-chars
;; (cond (haoran--os-linux (lazy-load-set-keys '(("C-T" . fullscreen-toggle))))
;;       (haoran--os-mac (lazy-load-set-keys '(("C-T" . toggle-frame-fullscreen)))))

(with-eval-after-load 'org
  ;; only full paths are supported
  (lazy-load-global-keys '(("C-v" . vanilla/preview-file-link)) "org-funcs")) ;; scroll-up-command

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
                         ("C-\\ |" . split-window-horizontally-instead)
                         ("C-\\ _" . split-window-vertically-instead))
                       "windowop")

(lazy-load-global-keys '(("C-\\ x" . ace-swap-window)
                         ("C-\\ d" . ace-delete-window))
                       "init-ace-window")


;; ----------------------------------
;; I combined vim's hjkl and emacs' npbf movement direction
;;
;; M-?:
;;                  ^  vanilla/scroll-half-page-down
;; mark-paragraph   k
;;            < h       l >  vanilla/downcase-word
;;                  j
;;                  v  vanilla/scroll-half-page-up
;;
;;
;; M-?:
;;                 ^  vanilla/move-cursor-8-lines-up
;; backward-word   p
;;           < b       f >  forward-word
;;                 n
;;                 v  vanilla/move-cursor-8-lines-down
;;
;;
;; M-?:
;;                                   ^  duplicate-line-above-comment
;; duplicate-line-or-region-below    K
;;                             < H       L >  duplicate-line-or-region-above
;;                                   J
;;                                   v  duplicate-line-below-comment
;;
;;
;;
;; s-?:
;;                                   ^  vanilla/scroll-down-one-line
;; vanilla/scroll-right-half-page    k
;;                             < h       l >  vanilla/scroll-left-half-page
;;                                   j
;;                                   v  vanilla/scroll-up-one-line
;;
;;
;; s-?:
;;              ^  move-text-up
;;              K
;;        < H       L >
;;              J
;;              v  move-text-down
;;
;; ----------------------------------



;; here is M-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-? -> cursor move and mark
(lazy-load-set-keys '(("M-:" . execute-extended-command)
                      ("M-;" . comment-dwim)))

(lazy-load-global-keys '(("M-<backspace>" . delete-block-backward)
                         ("M-d" . delete-block-forward))
                       "delete-block")

(with-eval-after-load 'org
  (lazy-load-set-keys '(("M-." . org-open-at-point)
                        ("M-," . org-mark-ring-goto))
                      org-mode-map))

;; you may use M-e (expand-region) more
(lazy-load-global-keys '(("M-@" . vanilla/mark-whole-word)) "basic-tookit")

(lazy-load-global-keys '(("ESC ESC ESC" . easy-nav-enter)) "init-easy-nav")

;; TODO
(lazy-load-global-keys '(("M-0" . treemacs-select-window)) "init-treemacs")

(lazy-load-global-keys '(("M-e" . er/expand-region) ;; like M-a confusing sentence ending
                         ("M--" . er/contract-region))
                       "init-expand-region")

;; only support en_word
;; see C-M-. for vimlike-semicolon
(lazy-load-global-keys '(("M-f" . vimlike-f) ;; forward-word
                         ("M-b" . vimlike-F)) ;; backward-word
                       "vim-like")

(lazy-load-global-keys '(("M-g" . goto-line-preview)) "goto-line-preview") ;; goto-line

(lazy-load-set-keys '(("M-h" . mark-paragraph))) ;; mark-paragraph

(lazy-load-global-keys '(("M-j" . vanilla/scroll-half-page-up) ;; default-indent-new-line
                         ("M-k" . vanilla/scroll-half-page-down)) ;; kill-sentence
                       "cursormove")

(lazy-load-global-keys '(("M-l" . vanilla/downcase-word) ;; downcase-word
                         ("M-u" . vanilla/upcase-word)) ;; upcase-word
                       "basic-tookit")

(lazy-load-global-keys '(("M-n" . vanilla/move-cursor-8-lines-down)
                         ("M-p" . vanilla/move-cursor-8-lines-up))
                       "cursormove")

(lazy-load-global-keys '(("M-o" . ace-window)) "init-ace-window")

;; highlight-phrase
;; highlight-regexp
;; unhighlight-regexp
(lazy-load-global-keys '(("M-s" . symbol-overlay-put)) "init-symbol-overlay")

(lazy-load-global-keys '(("M-y" . consult-yank-pop)) "consult") ;; yank-pop

(lazy-load-global-keys '(("M-z t" . vanilla/move-to-window-top)
                         ("M-z z" . vanilla/move-to-window-middle)
                         ("M-z b" . vanilla/move-to-window-bottom))
                       "cursormove")

;; capital letters
(lazy-load-global-keys '(("M-L" . duplicate-line-or-region-above)
                         ("M-H" . duplicate-line-or-region-below)
                         ("M-J" . duplicate-line-below-comment)
                         ("M-K" . duplicate-line-above-comment))
                       "duplicate-line")



;; here is s-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s-? -> text operation
;; here are some hyper keys used by yabai on macos

(with-eval-after-load 'org
  (lazy-load-set-keys '(("s-<return>" . org-insert-heading-respect-content)) org-mode-map)) ;; origin C-RET

(lazy-load-global-keys '(("s-." . lazycat/remember-init)
                         ("s-," . lazycat/remember-jump))
                       "basic-tookit")

(lazy-load-global-keys '(("s-\\" . vanilla/toggle-maximize-buffer)) "windowop")

(lazy-load-set-keys '(;; resize window
                      ("s-<up>"   . shrink-window)
                      ("s-<down>" . enlarge-window)
                      ("s-<left>" . shrink-window-horizontally)
                      ("s-<right>" . enlarge-window-horizontally)))

(lazy-load-global-keys '(("s-M-h" . vanilla/scroll-right-half-page)
                         ("s-M-l" . vanilla/scroll-left-half-page)
                         ("s-M-j" . vanilla/scroll-up-one-line)
                         ("s-M-k" . vanilla/scroll-down-one-line))
                       "cursormove")

(lazy-load-set-keys '(("s-z" . kill-this-buffer)))

(lazy-load-global-keys '(("s-J" . move-text-down)
                         ("s-K" . move-text-up))
                       "move-text")


;; here is C-c ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check ext-which-key.el for prompt

;; a for alone apps
(lazy-load-set-keys '(("C-c a a" . org-agenda)
                      ("C-c x" . org-capture)
                      ("C-c d" . (lambda () (interactive)
                                   (org-agenda nil "T")))))

(lazy-load-global-keys '(("C-c a f" . fanyi-dwim2)) "init-fanyi")


;; b for buffer, bookmark
(lazy-load-set-keys '(("C-c b m" . bookmark-set)
                      ("C-c b r" . bookmark-rename)
                      ("C-c b d" . bookmark-delete)
                      ("C-c b l" . bookmark-bmenu-list)
                      ("C-c b s" . bookmark-save)))
(lazy-load-global-keys '(("C-c b x" . vanilla/create-scratch-buffer)) "basic-tookit")
(lazy-load-global-keys '(("C-c b b" . consult-buffer)) "consult")

;; c for code

;; e for eshell
(lazy-load-global-keys '(("C-c e n" . eshell)) "eshell")

(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c e p" . +preview-current-buffer-in-browser)) org-mode-map))

;; f for find
(lazy-load-set-keys '(("C-c f x" . find-file)))

(lazy-load-global-keys '(("C-c f R" . vanilla/rename-current-file)
                         ("C-c f D" . vanilla/delete-current-file))
                       "fileop")

(lazy-load-global-keys '(("C-c f f" . consult-fd)
                         ("C-c f g" . consult-ripgrep)
                         ("C-c f m" . consult-mark)
                         ("C-c f r" . consult-recent-file))
                       "consult")

(lazy-load-global-keys '(("C-c f t" . treemacs)) "init-treemacs")

;; g for git
(lazy-load-set-keys '(("C-c g b" . git-messenger:popup-message)))

(lazy-load-global-keys '(("C-c g B" . magit-branch)) "magit")

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

;; git hunk, diff-hl has been required by init.el
;; C-c g ? for magit
;; C-c h ? for diff-hl
(lazy-load-set-keys '(("C-c h h" . +diff-hl-find-hunk)
                      ("C-c h n" . diff-hl-next-hunk)
                      ("C-c h p" . diff-hl-previous-hunk)
                      ;; ("C-c h s" . diff-hl-stage-current-hunk)
                      ("C-c h d" . diff-hl-diff-goto-hunk)))

;; i for insert
(lazy-load-global-keys '(("C-c i t" . hl-todo-insert)) "hl-todo")
(lazy-load-global-keys '(("C-c i y" . yas-insert-snippet)) "yasnippet")
;; org-mode-map
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c i !" . (lambda () (interactive) (org-time-stamp-inactive '(16)))))
                      org-mode-map)

  (lazy-load-local-keys '(("C-c i l" . vanilla/dwim-create-link-with-datetime)
                          ("C-c i i" . vanilla/org-insert-image)
                          ("C-c i s" . vanilla/org-insert-image-with-timestamp))
                        org-mode-map
                        "org-insert"))

(lazy-load-set-keys '(("C-c k" . avy-goto-line-above)
                      ("C-c j" . avy-goto-line-below)))

(lazy-load-global-keys '(("C-c K" . symbol-overlay-remove-all))
                       "init-symbol-overlay")

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(lazy-load-global-keys '(("C-c l b" . literate-calc-eval-buffer)
                         ("C-c l i" . literate-calc-insert-results)
                         ("C-c l m" . literate-calc-minor-mode)
                         ("C-c l l" . literate-calc-eval-line)
                         ("C-c l c" . literate-calc-clear-overlays))
                       "literate-calc-mode")

;; n for narrow
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c n s" . org-narrow-to-subtree)
                        ("C-c n w" . widen))
                      org-mode-map))

;; o for open
(lazy-load-set-keys '(("C-c o i" . (lambda () (interactive) (find-file haoran--private-notes)))
                      ("C-c o f r" . (lambda () (interactive) (find-file user-init-file)))))

(lazy-load-global-keys '(("C-c o o" . crux-open-with)) "crux")

(lazy-load-set-keys '(("C-c o d s" . (lambda () (interactive) (dired haoran--github-page)))))

;; p for project
(lazy-load-global-keys '(("C-c p f" . project-find-file)
                         ("C-c p p" . project-switch-project))
                       "init-project")

;; r C-c r instead C-x r as inaccessible

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

;; w for window
(lazy-load-set-keys '(("C-c w h" . windmove-left)
                      ("C-c w j" . windmove-down)
                      ("C-c w k" . windmove-up)
                      ("C-c w l" . windmove-right)
                      ("C-c w =" . balance-windows)
                      ("C-c w m" . delete-other-windows)))

(lazy-load-global-keys '(("C-c w H" . vanilla/split-window-left-with-balance)
                         ("C-c w J" . vanilla/split-window-below-with-balance)
                         ("C-c w K" . vanilla/split-window-up-with-balance)
                         ("C-c w L" . vanilla/split-window-right-with-balance)
                         ("C-c w c" . vanilla/delete-window-with-balance)
                         ("C-c w |" . split-window-horizontally-instead)
                         ("C-c w _" . split-window-vertically-instead))
                       "windowop")

(lazy-load-global-keys '(("C-c w x" . ace-swap-window))
                       "init-ace-window")

;; here is C-x ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys '(("C-x g" . magit-status)) "magit")



;; here is C-M-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO Incorrect capitalization
(lazy-load-global-keys '(("C-M-." . vimlike-semicolon)) ;; xref-find-apropos
                       "vim-like")


(provide 'init-keys)
;;; init-keys.el ends here
