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
;; 1. Use lazy-load only in init-keys.el
;; 2. Use only lazy-load in init-keys.el
;; 3. Elsewhere use the native define-key

;;; Require:
(require 'lazy-load)

;;; Code:
(lazy-load-unset-keys '("C-x C-f"
                        ;; "C-z" ;; suspend-frame
                        "C-q"
                        "s-T"
                        "s-W"
                        "s-z"
                        "C-\\" "s-c" "s-x" "s-v" "C-6"
                        "M-x"
                        "M-z" ;; zap-to-char like vim df?
                        "C-t" ;; transpose-chars
                        ))


;; here is C-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-funs has be required by init.el
(lazy-load-global-keys '(("C-<return>" . consult-bookmark)) "consult")
(with-eval-after-load 'org
  (lazy-load-local-keys '(("C-<return>" . bookmark-jump))
                        org-mode-map ""))

(lazy-load-global-keys '(("C-<tab>" . spacemacs/alternate-buffer)
                         ("C-<backspace>" . crux-kill-line-backwards))
                       "crux")

(lazy-load-global-keys '(("C-," . goto-last-change)) "goto-last-change")
(with-eval-after-load 'org
  (lazy-load-local-keys '(("C-," . goto-last-change))
                        org-mode-map "goto-last-change"))

(lazy-load-global-keys '(("C-." . +format-code-dwim)) "init-formatter")
(lazy-load-global-keys '(("C-;" . avy-goto-char)) "init-avy")
(lazy-load-global-keys '(("C-?" . vundo)) "init-vundo") ;; keep C-/ undo, use C-? vundo instead undo-redo

(lazy-load-global-keys '(("C-a" . mwim-beginning-of-line-or-code)
                         ("C-e" . mwim-end-of-line-or-code))
                       "mwim")

;; h for help
(lazy-load-set-keys '(("C-h C-f" . find-function)
                      ("C-h C-v" . find-variable)
                      ("C-h C-k" . find-function-on-key)))

(lazy-load-global-keys '(("C-j" . vanilla/merge-line-down)) ;; electric-newline-and-maybe-indent
                       "basic-tookit")
(with-eval-after-load 'org
  (lazy-load-local-keys '(("C-j" . vanilla/merge-line-down))
                        org-mode-map "basic-tookit"))

(lazy-load-global-keys '(("C-o" . open-newline-above)
                         ("C-l" . open-newline-below))
                       "open-newline")

(lazy-load-set-keys '(("C-q" . quoted-insert)))
(lazy-load-global-keys '(("C-s" . consult-line)) "consult")

(with-eval-after-load 'org
  ;; only full paths are supported
  (lazy-load-local-keys '(("C-v" . vanilla/preview-file-link))
                        org-mode-map "org-funcs"))

;; also navigate windows by ace-window(M-o)
(lazy-load-set-keys '(("C-\\ h" . windmove-left)
                      ("C-\\ j" . windmove-down)
                      ("C-\\ k" . windmove-up)
                      ("C-\\ l" . windmove-right)
                      ("C-\\ =" . balance-windows)))

(lazy-load-global-keys '(("C-\\ H" . vanilla/split-window-left-with-balance)
                         ("C-\\ J" . vanilla/split-window-below-with-balance)
                         ("C-\\ K" . vanilla/split-window-up-with-balance)
                         ("C-\\ L" . vanilla/split-window-right-with-balance)
                         ("C-\\ c" . vanilla/delete-window-with-balance)
                         ("C-\\ m" . vanilla/toggle-maximize-buffer)
                         ("C-\\ |" . split-window-horizontally-instead)
                         ("C-\\ _" . split-window-vertically-instead))
                       "windowop")

(lazy-load-set-keys '(("C-\\ x" . ace-swap-window)
                      ("C-\\ d" . ace-delete-window)))


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
;; duplicate-line-or-region-above    K
;;                             < H       L >  duplicate-line-or-region-below
;;                                   J
;;                                   v  duplicate-line-below-comment
;;
;;
;;
;; s-M-?:
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
                      ("M-;" . comment-dwim) ;; comment-dwim/eval-expression
                      ))

(lazy-load-global-keys '(("M-<backspace>" . delete-block-backward)
                         ("M-d" . delete-block-forward)) ;; kill-word
                       "delete-block")

(with-eval-after-load 'org
  (lazy-load-local-keys '(("M-." . org-open-at-point) ;; xref-find-dfinitions
                          ("M-," . org-mark-ring-goto)) ;; xref-pop-marker-stack
                        org-mode-map ""))

(lazy-load-global-keys '(("M-0" . treemacs-select-window)) "init-treemacs")

(lazy-load-global-keys '(("M-g" . goto-line-preview)) "goto-line-preview") ;; goto-line

(lazy-load-global-keys '(("M-j" . vanilla/scroll-half-page-up) ;; default-indent-new-line
                         ("M-k" . vanilla/scroll-half-page-down)) ;; kill-sentence
                       "cursormove")

(lazy-load-global-keys '(("M-l" . vanilla/downcase-word) ;; downcase-word
                         ("M-u" . vanilla/upcase-word)) ;; upcase-word
                       "basic-tookit")

(lazy-load-set-keys '(("M-o" . ace-window))) ;; undefined

(lazy-load-global-keys '(("M-s" . symbol-overlay-put)) "init-symbol-overlay") ;; tab-to-tab-stop

(lazy-load-global-keys '(("M-y" . consult-yank-pop)) "consult") ;; yank-pop

(lazy-load-global-keys '(("M-H" . duplicate-line-or-region-above)
                         ("M-L" . duplicate-line-or-region-below)
                         ("M-J" . duplicate-line-below-comment)
                         ("M-K" . duplicate-line-above-comment))
                       "duplicate-line")

;; window operation
;;   1. C-\
;;   2. s-hjkl   resize
;; cursor/screen move
;;   1. M-j      vanilla/scroll-half-page-up
;;   2. M-k      vanilla/scroll-half-page-down
;;   3. M-p      vanilla/move-cursor-8-lines-up
;;   4. M-n      vanilla/move-cursor-8-lines-down
;;   5. s-M-hjkl vanilla/scroll-...
;; text operation
;;   1. M-HJKL   duplicate-line
;;   2. s-JK     move-text
;;   3. C-j      vanilla/merge-line-down

;; here is s-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s-? -> text operation
;; here are some hyper keys used by yabai on macos
(lazy-load-set-keys '(("s-SPC" . just-one-space))) ;; origin M-SPC

(with-eval-after-load 'org
  (lazy-load-local-keys '(("s-<return>" . org-insert-heading-respect-content))
                        org-mode-map "")) ;; origin C-RET

(lazy-load-set-keys '(("s-." . lazycat/remember-init)
                      ("s-," . lazycat/remember-jump)))

(lazy-load-set-keys '(;; resize window
                      ("s-k" . shrink-window)
                      ("s-j" . enlarge-window)
                      ("s-h" . shrink-window-horizontally)
                      ("s-l" . enlarge-window-horizontally)))

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
;; a for alone apps
(lazy-load-global-keys '(("C-c a a" . org-agenda)
                         ("C-c x" . org-capture)
                         ("C-c d" . +org-agenda-T))
                       "init-org")

(lazy-load-global-keys '(("C-c a f" . fanyi-dwim2)) "fanyi")


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
  (lazy-load-set-keys '(("C-c e p" . +org-preview-in-browser)) org-mode-map))

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

(lazy-load-global-keys '(("C-c f t" . my/treemacs-add-current-project-workspace-exclusively))
                       "init-treemacs")

;; g for git
(pretty-hydra-define hydra-git (:title (format "%s Git Commands"
                                               (all-the-icons-alltheicon "git"))
                                       :body-pre (require 'magit)
                                       :color blue :quit-key ("q" "C-g"))
  ("dispatch"
   (("m" magit-dispatch "dispatch menu" :exit t)
    ("f" magit-file-dispatch "dispatch file" :exit t))))
(lazy-load-set-keys '(("C-c g" . hydra-git/body)))

;; h for highlight
(pretty-hydra-define hydra-highlight-hunk (:body-pre (require 'hi-lock)
                                                     :color blue :quit-key ("q" "C-g"))
  ("Highlight"
   (("l" highlight-phrase "phrase" :exit t)
    ("r" highlight-regexp "regexp" :exit t)
    ("u" unhighlight-regexp "unregexp" :exit t))
   "Hunk"
   (("h" +diff-hl-find-hunk "find hunk")
    ("n" diff-hl-next-hunk "next hunk")
    ("p" diff-hl-previous-hunk "prev hunk")
    ("s" diff-hl-stage-current-hunk "stage hunk")
    ("d" diff-hl-diff-goto-hunk "diff hunk"))))
(lazy-load-set-keys '(("C-c h" . hydra-highlight-hunk/body)))

;; i for insert
(defhydra hydra-insert (:color blue)
  ("t" hl-todo-insert "todo insert" :column "insert"))
(lazy-load-set-keys '(("C-c i" . hydra-insert/body)))

(with-eval-after-load 'org
  (defhydra hydra-org-insert (:body-pre (require 'org-insert)
                                        :color blue)
    ("!" vanilla/org-insert-stamp-inactive "inactive time" :exit t :column "org-insert")
    ("l" vanilla/dwim-create-link-with-datetime "datetime link" :exit t)
    ("i" vanilla/org-insert-image "image with name" :exit t)
    ("s" vanilla/org-insert-image-with-timestamp "image with time" :exit t)
    ("t" hl-todo-insert "todo insert" :column "insert"))
  (lazy-load-local-keys '(("C-c i" . hydra-org-insert/body)) org-mode-map ""))

(lazy-load-global-keys '(("C-c j" . avy-goto-line-below)
                         ("C-c k" . avy-goto-line-above))
                       "init-avy")

(lazy-load-global-keys '(("C-c K" . symbol-overlay-remove-all))
                       "init-symbol-overlay")

;; n for narrow
(with-eval-after-load 'org
  (defhydra hydra-narrow (:color blue)
    ("s" org-narrow-to-subtree "narrow to subtree" :exit t :column "narrow")
    ("w" widen "widen" :exit t))
  (lazy-load-set-keys '(("C-c n" . hydra-narrow/body)) org-mode-map))

;; o for open
(lazy-load-global-keys '(("C-c o h" . +httpd-start-currfile)) "init-simple-httpd")
(lazy-load-set-keys '(("C-c o a" . my/algo-layout)))
(lazy-load-set-keys '(("C-c o i" . (lambda () (interactive) (find-file ran--private-notes)))
                      ("C-c o f r" . (lambda () (interactive) (find-file user-init-file)))))
(lazy-load-global-keys '(("C-c o o" . crux-open-with)) "crux")

;; p for project
(lazy-load-global-keys '(("C-c p f" . project-find-file)
                         ("C-c p p" . project-switch-project))
                       ;; C-c p t [project to treemacs] in treemacs
                       "project")

;; r C-c r instead C-x r as inaccessible

;; s for switch
(lazy-load-global-keys '(("C-c s" . vanilla/tab-bar-switch-to-tab))
                       "init-tab-bar")

;; t for tab
(lazy-load-global-keys '(("C-c t c" . tab-bar-close-tab)
                         ("C-c t r" . tab-bar-rename-tab)
                         ("C-c t h" . tab-bar-move-tab-backward)
                         ("C-c t l" . tab-bar-move-tab))
                       "init-tab-bar")

;; u for user
(lazy-load-set-keys '(("C-c u f" . my/unfill-paragraph)
                      ("C-c u i" . my/indent-buffer)
                      ("C-c u l" . my/copy-file-path-and-line-number)))

;; y for yasnippet
(lazy-load-set-keys '(("C-c y C-s" . yas-insert-snippet)
                      ("C-c y C-n" . yas-new-snippet)
                      ("C-c y C-v" . yas-visit-snippet-file)))

;; z for folding unify with vim
(defhydra hydra-yafolding (:body-pre (require 'yafolding)
                                     :color blue)
  "folding"
  ("e" yafolding-toggle-element "element")
  ("a" yafolding-show-all "show all")
  ("p" yafolding-go-parent-element "go parent"))
(lazy-load-set-keys '(("C-c z" . hydra-yafolding/body)))


(lazy-load-global-keys '(("C-c M-g" . magit-file-dispatch)) "magit")


;; here is C-x ? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys '(("C-x g" . magit-status)) "magit")


(provide 'init-keys)
;;; init-keys.el ends here
