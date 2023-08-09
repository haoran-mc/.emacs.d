;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-funcs)
(require 'init-macros)

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on `:q'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-normal-state-map
         ("Q" . kill-this-buffer))
  :config
  ;; Install `undo-fu' when necessary
  (when (< emacs-major-version 28)
    (use-package undo-fu
      :ensure t))

  (with-eval-after-load 'evil
    (setq evil-insert-state-cursor 'bar))

  ;; Silence line out of range error.
  (shut-up! #'evil-indent)
  :custom
  ;; undo will never freeze my Emacs
  (evil-undo-system (if (>= emacs-major-version 28) 'undo-redo 'undo-fu))
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-C-u-scroll t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t))

(use-package evil-surround
  :ensure t
  :hook (after-init . global-evil-surround-mode))

(use-package evil-collection
  :ensure t
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-setup-debugger-keys nil)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t))

;; evil leader map
(use-package evil
  :ensure nil
  :config
  (with-no-warnings
    ;; We use "SPC" as the leader key, "SPC m" as the localleader key. Due to the
    ;; limitation of `evil-set-leader', we can't easily set localleader key with
    ;;
    ;; ``` elisp
    ;; (evil-set-leader 'normal (kbd "SPC m") :localleader)
    ;; ```
    ;;
    ;; An error is prompted:
    ;;
    ;; ``` elisp
    ;; (error "Key sequence SPC m starts with non-prefix key SPC")
    ;; ```
    ;;
    ;; If you know how to fix that, let me know. Thanks.
    (evil-set-leader 'normal (kbd "SPC"))
    (evil-set-leader 'normal (kbd "<leader>m") :localleader)

    (defun define-leader-key (state map localleader &rest bindings)
      "Define leader key in MAP when STATE, a wrapper for
`evil-define-key*'. All BINDINGS are prefixed with \"<leader>\"
if LOCALLEADER is nil, otherwise \"<localleader>\"."
      (cl-assert (cl-evenp (length bindings)))
      (let ((prefix (if localleader "<localleader>" "<leader>"))
            wk-replacements)
        (while bindings
          (let ((key (pop bindings))
                (def (pop bindings)))
            (when (symbolp def)
              (evil-define-key* state map (kbd (concat prefix key)) def))
            ;; Save which-key (key . replacement).
            (pcase def
              (`(:wk ,replacement)
               (push (cons (concat prefix key) replacement) wk-replacements)))))
        ;; which-key integration.
        ;; XXX: replacement for localleader NOT supported.
        (with-eval-after-load 'which-key
          (cl-loop for (key . replacement) in wk-replacements
                   unless localleader
                   do (which-key-add-key-based-replacements key replacement)))))

    (define-leader-key 'normal 'global nil
      "SPC" 'execute-extended-command
      "TAB" 'spacemacs/alternate-buffer
      "RET" 'bookmark-jump

      "0" '+func-start-writing-at-the-next-special-word

      ;; Resume
      "'" 'vertico-repeat

      ;; app
      "a" '(:wk "app")
      "aa" 'org-agenda
      "ac" 'org-capture
      "ad" 'calendar
      "af" 'fanyi-dwim2
      "ao" 'consult-org-agenda ;; outlines in agenda dir

      ;; buffer & bookmark
      "b" '(:wk "bufmark")
      "bY" '+copy-current-buffer-name       ;; copy current buffer name
      "bb" 'switch-to-buffer                ;; switch buffer
      "bx" '+create-scratch-buffer          ;; create scratch buffer
      "bz" 'bury-buffer                     ;; bury buffer
      ;; --------------
      "bm" 'bookmark-set                    ;; add a new bookmark
      "br" 'bookmark-rename                 ;; rename bookmark
      "bd" 'bookmark-delete                 ;; delete bookmark
      ;; "bj" 'bookmark-jump                   ;; jump bookmark
      "bl" 'bookmark-bmenu-list             ;; list bookmark
      "bs" 'bookmark-save                   ;; save bookmark

      ;; dired
      "d" '(:wk "dired")
      "dj" 'dired-jump
      "dp" 'dired-at-point

      ;; eshell & edit
      "e"  '(:wk "eshell/edit")
      "ed" 'aweshell-dedicated-toggle
      "en" 'aweshell-new
      "et" 'aweshell-toggle
      "ei" 'imenu

      ;; file
      "f"  '(:wk "files/find")
      "fC" '+copy-current-file
      "fD" '+delete-current-file
      "fY" '+copy-current-filename
      "fR" '+rename-current-file
      "fd" 'ediff-buffers
      "ff" 'find-file
      "fr" 'recentf-open-files
      "ft" 'treemacs
      "fj" 'evil-avy-goto-line-below
      "fk" 'evil-avy-goto-line-above
      "fw" 'evil-avy-goto-word-0
      "fc" 'evil-avy-goto-char

      ;; open
      "o"   '(:wk "open")
      "oO"  '+open-in-browser
      "oD"  '+open-current-directory  ;; depend on consult
      "ol"  'org-store-link
      "oi"  '+open-wiki-note
      "os"  '+open-site-note
      "oo"  '+open-centre-org
      "od"  '(:wk "open directory")
      "of"  '(:wk "open file")
      "ofr" '+open-file-init

      ;; project
      "p" 'projectile-command-map

      ;; tab
      "t"  '(:wk "tab")
      "tc" 'tab-bar-close-tab
      "tC" 'tab-bar-close-group-tabs
      "tg" 'tab-bar-change-tab-group
      "ti" 'tab-switcher
      "tn" '+create-new-tab-bar
      "to" 'tab-bar-close-other-tabs
      "tt" 'tab-bar-switch-to-tab
      "tp" 'tab-bar-switch-to-recent-tab
      "tr" 'tab-bar-rename-tab

      ;; user's
      "u" '(:wk "user funcs")
      "ui" '+indent-region-or-buffer

      ;; window
      "w"  'evil-window-map
      "wx" 'kill-buffer-and-window
      "wm" '+toggle-maximize-buffer
      )

    (with-eval-after-load 'org
      (define-leader-key 'normal org-mode-map :localleader
        "." 'org-goto
        "," 'org-priority
        "d" 'org-deadline
        "s" 'org-schedule
        "p" 'org-set-property
        "q" 'org-set-tags-command
        "t" 'org-todo

        ;; export
        "e"  '(:wk "export")
        "ep" '+preview-current-buffer-in-browser

        ;; insert
        "i"  '(:wk "insert")
        "id" 'org-insert-drawer
        "in" 'org-add-note
        "it" 'org-time-stamp-inactive ;; not recognised by org-mode
        "iT" 'org-time-stamp
        ))

    (with-eval-after-load 'elisp-mode
      (dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
        (define-leader-key 'normal keymap :localleader
          "i" 'info-lookup-symbol

          ;; eval
          "eb" 'eval-buffer
          "ed" 'eval-defun
          "ee" 'eval-last-sexp
          "el" 'load-library

          ;; goto
          "gf" 'find-function
          "gv" 'find-variable
          "gl" 'find-library)))))

(provide 'init-evil)
;;; init-evil.el ends here
