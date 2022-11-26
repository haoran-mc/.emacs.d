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
         ("Q" . kill-this-buffer)
         :map evil-motion-state-map
         ("F" . evil-avy-goto-char-in-line))
  :config
  ;; Install `undo-fu' when necessary
  (when (< emacs-major-version 28)
    (use-package undo-fu
      :ensure t))

  (with-eval-after-load 'evil
    (setq evil-insert-state-cursor 'box))

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
      ;; SPC, quit minibuffer.
      ;; "SPC" 'keyboard-escape-quit
      "SPC" 'execute-extended-command
      ":"   'execute-extended-command
      "TAB" 'spacemacs/alternate-buffer

      ;; Resume
      "'" 'vertico-repeat
      "." 'avy-menu
      "0" '+add-whitespace-around

      ;; app
      "a" '(:wk "app")
      "aa" 'org-agenda
      "ac" 'calendar
      "ag" 'gnus
      "ai" 'rcirc

      ;; buffer & bookmark
      "b" '(:wk "bufmark")
      "bb" 'switch-to-buffer                    ;; switch buffer
      "bB" 'switch-to-buffer-other-window       ;; switch buffer other window
      "bc" 'clone-indirect-buffer               ;; clone indirect buffer
      "bC" 'clone-indirect-buffer-other-window  ;; clone indirect buffer other window
      "by" '+copy-current-buffer-name           ;; copy current buffer name
      "bv" 'revert-buffer                       ;; revert buffer
      "bx" '+create-scratch-buffer              ;; create scratch buffer
      "bz" 'bury-buffer                         ;; bury buffer
      ;; --------------
      "bm" 'bookmark-set                        ;; add a new bookmark
      "bM" 'bookmark-set-no-overwrite           ;; add a new bookmark while no overwrite
      "bi" 'bookmark-insert                     ;; insert the selected bookmark content current postion
      "br" 'bookmark-rename                     ;; rename bookmark
      "bd" 'bookmark-delete                     ;; delete bookmark
      "bw" 'bookmark-write                      ;; write bookmark into a file
      "bj" 'bookmark-jump                       ;; jump bookmark
      "bJ" 'bookmark-jump-other-window          ;; jump bookmark other window
      "bl" 'bookmark-bmenu-list                 ;; list bookmark
      "bs" 'bookmark-save                       ;; save bookmark

      ;; code
      "c" '(:wk "code")
      "cd" 'rmsbolt-compile
      "cc" 'compile
      "cC" 'recompile
      "ck" 'kill-compilation
      "cw" 'delete-trailing-whitespace
      "cx" 'quickrun

      ;; dired
      "d" '(:wk "dired")
      "dj" 'dired-jump
      "dJ" 'dired-jump-other-window
      "dp" 'dired-at-point

      ;; eshell
      "e"  '(:wk "eshell")
      "ed" 'aweshell-dedicated-toggle
      "en" 'aweshell-new
      "et" 'aweshell-toggle

      ;; file
      "f"  '(:wk "files")
      "ff" 'find-file
      "fd" 'ediff-buffers
      "fF" 'find-file-other-window
      "f/" 'find-file-other-window
      "fC" '+copy-current-file
      "fD" '+delete-current-file
      "fy" '+copy-current-filename
      "fR" '+rename-current-file
      "fr" 'recentf-open-files
      "fl" 'find-file-literally
      "ft" 'treemacs

      ;; open
      "o" '(:wk "open")
      "oc" 'org-capture
      "ol" 'org-store-link
      "ot" 'ansi-term
      "oe" 'eshell
      "os" 'shell
      "oo" '+open-in-browser
      "oD"  '+open-current-directory  ;; depend on consult
      ;; open directory
      "od" '(:wk "open directory")
      "odm" '+open-directory-markdown
      "odh" '+open-directory-haoran
      ;; open file
      "of"  '(:wk "open file")
      "oi"  '+open-file-note
      "ofi" '+open-file-note
      "ofr" '+open-file-init
      "ofn" '+open-file-navigate
      "ofb" '+open-file-backup

      ;; project
      "p" 'projectile-command-map

      ;; search
      "s" '(:wk "search")
      "sj" 'evil-show-jumps
      "sm" 'evil-show-marks
      "sr" 'evil-show-registers
      "si" 'imenu

      ;; tab/tree
      "t" '(:wk "tab/tree")
      "t9" 'tab-bar-switch-to-last-tab
      "tc" 'tab-bar-close-tab
      "tC" 'tab-bar-close-group-tabs
      "tg" 'tab-bar-change-tab-group
      "ti" 'tab-switcher
      "tn" '+create-new-tab-bar
      "to" 'tab-bar-close-other-tabs
      "tt" 'tab-bar-switch-to-tab
      "tp" 'tab-bar-switch-to-recent-tab
      "tr" 'tab-bar-rename-tab

      ;; toggle
      "T" '(:wk "toggle")
      "Tf" 'follow-mode
      "Tt" 'awesome-tray-mode

      ;; user's
      "u" '(:wk "user")
      "ui" '+indent-region-or-buffer
      "uf" 'fanyi-dwim2

      ;; window
      "w" 'evil-window-map
      "wx" 'kill-buffer-and-window
      "wu" '+transient-tab-bar-history
      "w-" 'split-window-vertically
      "w/" 'split-window-horizontally
      "wm" 'spacemacs/toggle-maxmize-buffer
      )

    (with-eval-after-load 'org
      (define-leader-key 'normal org-mode-map :localleader
        "." 'org-goto
        "a" 'org-archive-subtree
        "d" 'org-deadline
        "e" 'org-set-effort
        "f" 'org-footnote-action
        "l" 'org-lint
        "o" 'org-toggle-ordered-property
        "p" 'org-set-property
        "q" 'org-set-tags-command
        "r" 'org-refile
        "s" 'org-schedule
        "t" 'org-todo
        "T" 'org-todo-list

        ;; babel
        "b"  '(:wk "babel")
        "bp" 'org-babel-previous-src-block
        "bn" 'org-babel-next-src-block
        "be" 'org-babel-expand-src-block
        "bg" 'org-babel-goto-named-src-block
        "bs" 'org-babel-execute-subtree
        "bb" 'org-babel-execute-buffer
        "bt" 'org-babel-tangle
        "bf" 'org-babel-tangle-file
        "bc" 'org-babel-check-src-block
        "bi" 'org-babel-insert-header-arg
        "bI" 'org-babel-view-src-block-info
        "bk" 'org-babel-remove-result-one-or-many

        ;; clock
        "c"  '(:wk "clock")
        "cc" 'org-clock-in
        "cC" 'org-clock-out
        "cd" 'org-clock-mark-default-task
        "ce" 'org-clock-modify-effort-estimate
        "cg" 'org-clock-goto
        "cl" 'org-clock-in-last
        "cr" 'org-clock-report
        "cs" 'org-clock-display
        "cx" 'org-clock-cancel
        "c=" 'org-clock-timestamps-up
        "c-" 'org-clock-timestamps-down

        ;; insert
        "i"  '(:wk "insert")
        "id" 'org-insert-drawer
        "in" 'org-add-note
        "it" 'org-time-stamp-inactive
        "iT" 'org-time-stamp

        ;; user
        "u"   '(:wk "user")
        "ub"  '(:wk "browser")
        "ubb" '+preview-current-buffer-in-browser
        "ue"  '(:wk "export")
        "ueh" '+org-export-html-to-my-dir
        "uep" '+org-preview-html-in-my-dir
        "ueH" '+org-export-html-to-my-dir-and-preview
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
