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
                        "C-\\" "s-c" "s-x" "s-v" "C-6" "M-." "M-,"
                        "M-x"
                        "M-z" ;; zap-to-char like vim df?
                        "C-t" ;; transpose-chars
                        "C-SPC" ;; always use meow
                        ))


;; here is C-? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init-funs has be required by init.el
(lazy-load-global-keys '(("C-<return>" . consult-bookmark)) "consult")
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-<return>" . bookmark-jump)) org-mode-map))

(lazy-load-global-keys '(("C-<tab>" . spacemacs/alternate-buffer)
                         ("C-<backspace>" . crux-kill-line-backwards))
                       "crux")

(lazy-load-global-keys '(("C-," . goto-last-change)) "goto-last-change")
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

(lazy-load-global-keys '(("C-j" . vanilla/merge-line-down) ;; electric-newline-and-maybe-indent
                         ("C-k" . vanilla/smart-kill-line))
                       "basic-tookit")
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-j" . vanilla/merge-line-down)) org-mode-map))

(lazy-load-global-keys '(("C-o" . open-newline-above)
                         ("C-l" . open-newline-below))
                       "open-newline")

(lazy-load-set-keys '(("C-q" . quoted-insert)))
(lazy-load-global-keys '(("C-s" . consult-line)) "consult")

(with-eval-after-load 'org
  ;; only full paths are supported
  (lazy-load-global-keys '(("C-v" . vanilla/preview-file-link)) "org-funcs")) ;; scroll-up-command

;; also navigate windows by ace-window(M-o)
(lazy-load-set-keys '(("C-\\ h" . windmove-left)
                      ("C-\\ j" . windmove-down)
                      ("C-\\ k" . windmove-up)
                      ("C-\\ l" . windmove-right)
                      ("C-\\ =" . balance-windows)
                      ("C-\\ o" . delete-other-windows)))

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
                      ("M-;" . comment-dwim) ;; comment-dwim/eval-expression
                      ("M-[" . ESC-prefix))) ;; undefined

(lazy-load-global-keys '(("M-<backspace>" . delete-block-backward)
                         ("M-d" . delete-block-forward)) ;; kill-word
                       "delete-block")

(with-eval-after-load 'org
  (lazy-load-set-keys '(("M-." . org-open-at-point) ;; xref-find-dfinitions
                        ("M-," . org-mark-ring-goto)) ;; xref-pop-marker-stack
                      org-mode-map))

(lazy-load-global-keys '(("M-0" . treemacs-select-window)) "init-treemacs")

(lazy-load-global-keys '(("M-e" . er/expand-region) ;; like M-a confusing sentence ending
                         ("M--" . er/contract-region))
                       "init-expand-region")

(lazy-load-global-keys '(("M-g" . goto-line-preview)) "goto-line-preview") ;; goto-line

(lazy-load-global-keys '(("M-j" . vanilla/scroll-half-page-up) ;; default-indent-new-line
                         ("M-k" . vanilla/scroll-half-page-down)) ;; kill-sentence
                       "cursormove")

(lazy-load-global-keys '(("M-l" . vanilla/downcase-word) ;; downcase-word
                         ("M-u" . vanilla/upcase-word)) ;; upcase-word
                       "basic-tookit")

(lazy-load-global-keys '(("M-n" . vanilla/move-cursor-8-lines-down) ;; undefined
                         ("M-p" . vanilla/move-cursor-8-lines-up)) ;; undefined
                       "cursormove")

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
(lazy-load-set-keys '(("s-SPC" . just-one-space))) ;; origin M-SPC

(with-eval-after-load 'org
  (lazy-load-set-keys '(("s-<return>" . org-insert-heading-respect-content)) org-mode-map)) ;; origin C-RET

(lazy-load-global-keys '(("s-." . lazycat/remember-init)
                         ("s-," . lazycat/remember-jump))
                       "basic-tookit")

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
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c a" "standalone apps"))
(lazy-load-global-keys '(("C-c a a" . org-agenda)
                         ("C-c x" . org-capture)
                         ("C-c d" . +org-agenda-T))
                       "init-org")

(lazy-load-global-keys '(("C-c a f" . fanyi-dwim2)) "init-fanyi")


;; b for buffer, bookmark
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c b" "buffer/bookmark"))
(lazy-load-set-keys '(("C-c b m" . bookmark-set)
                      ("C-c b r" . bookmark-rename)
                      ("C-c b d" . bookmark-delete)
                      ("C-c b l" . bookmark-bmenu-list)
                      ("C-c b s" . bookmark-save)))
(lazy-load-global-keys '(("C-c b x" . vanilla/create-scratch-buffer)) "basic-tookit")
(lazy-load-global-keys '(("C-c b b" . consult-buffer)) "consult")

;; c for code
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c c" "code"))

;; e for eshell
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c e" "eshell"))
(lazy-load-global-keys '(("C-c e n" . eshell)) "eshell")

(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c e p" . +org-preview-in-browser)) org-mode-map))

;; f for find
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f" "find/file"))
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
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c g" "git"))
(pretty-hydra-define hydra-git (:title (format "%s Git Commands"
                                               (all-the-icons-alltheicon "git"))
                                       :body-pre (require 'magit)
                                       :color amaranth :quit-key ("q" "C-g"))
  ("Message"
   (("b" git-messenger:popup-message "popup-msg" :exit t)
    ("B" magit-branch "branch" :exit t))
   "Hunk"
   (("h" +diff-hl-find-hunk "find hunk")
    ("n" diff-hl-next-hunk "next hunk")
    ("p" diff-hl-previous-hunk "prev hunk")
    ("s" diff-hl-stage-current-hunk "stage hunk")
    ("d" diff-hl-diff-goto-hunk "diff hunk"))))
(lazy-load-set-keys '(("C-c g" . hydra-git/body)))

;; h for highlight
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c h" "highlight"))
(global-set-key
 (kbd "C-c h")
 (defhydra hydra-highlight (:body-pre (require 'hi-lock)
                                      :color blue :quit-key ("q" "C-g"))
   ("p" highlight-phrase "phrase" :column "highlight" :exit t)
   ("r" highlight-regexp "regexp" :exit t)
   ("u" unhighlight-regexp "unregexp" :exit t)))

;; i for insert
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c g" "git"))
(lazy-load-global-keys '(("C-c i t" . hl-todo-insert)) "hl-todo")
;; org-mode-map
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c i !" . (lambda () (interactive) (org-time-stamp-inactive '(16)))))
                      org-mode-map)

  (lazy-load-local-keys '(("C-c i l" . vanilla/dwim-create-link-with-datetime)
                          ("C-c i i" . vanilla/org-insert-image)
                          ("C-c i s" . vanilla/org-insert-image-with-timestamp))
                        org-mode-map
                        "org-insert"))

(lazy-load-global-keys '(("C-c j" . avy-goto-line-below)
                         ("C-c k" . avy-goto-line-above))
                       "init-avy")

(lazy-load-global-keys '(("C-c K" . symbol-overlay-remove-all))
                       "init-symbol-overlay")

;; literate-calc-mode literate-calc-set-radix literate-calc-remove-results
;; a = 140 * 12 => a: 1,680
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c l" "literate-calc"))
(defhydra hydra-literate-calc (:body-pre (require 'literate-calc-mode)
                                         :color blue)
  ("b" literate-calc-eval-buffer "eval buffer" :column "literate calc")
  ("i" literate-calc-insert-results "insert result")
  ("m" literate-calc-minor-mode "minor mode")
  ("l" literate-calc-eval-line "eval line")
  ("c" literate-calc-clear-overlays "clear overlays"))
(lazy-load-set-keys '(("C-c l" . hydra-literate-calc/body)))

;; n for narrow
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c n" "narrow"))
(with-eval-after-load 'org
  (lazy-load-set-keys '(("C-c n s" . org-narrow-to-subtree)
                        ("C-c n w" . widen))
                      org-mode-map))

;; o for open
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c o" "open"))
(lazy-load-global-keys '(("C-c o h" . +httpd-start-currfile)) "init-simple-httpd")
(lazy-load-set-keys '(("C-c o i" . (lambda () (interactive) (find-file ran--private-notes)))
                      ("C-c o f r" . (lambda () (interactive) (find-file user-init-file)))))
(lazy-load-global-keys '(("C-c o o" . crux-open-with)) "crux")

;; p for project
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c p" "project"))
(lazy-load-global-keys '(("C-c p f" . project-find-file)
                         ("C-c p p" . project-switch-project))
                       ;; C-c p t [project to treemacs] in treemacs
                       "init-project")

;; r C-c r instead C-x r as inaccessible

;; s for switch
(lazy-load-global-keys '(("C-c s" . tab-bar-switch-to-tab))
                       "init-tab-bar")

;; t for tab
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c t" "tab-bar"))
(lazy-load-global-keys '(("C-c t n" . vanilla/create-new-tab-bar)
                         ("C-c t c" . tab-bar-close-tab)
                         ("C-c t r" . tab-bar-rename-tab))
                       "init-tab-bar")

;; u for user
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c u" "user"))
(lazy-load-set-keys '(("C-c u f" . +unfill-paragraph)
                      ("C-c u i" . +indent-buffer)
                      ("C-c u l" . +copy-file-path-and-line-number)))

;; y for yasnippet
(lazy-load-set-keys '(("C-c y C-s" . yas-insert-snippet)
                      ("C-c y C-n" . yas-new-snippet)
                      ("C-c y C-v" . yas-visit-snippet-file)))

;; z for folding unify with vim
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
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
