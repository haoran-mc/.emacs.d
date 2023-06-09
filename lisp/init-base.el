;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(setq user-full-name "Haoran Liu")
(setq user-mail-address "haoran.mc@outlook.com")

(setq-default default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq-default default-enable-multibyte-characters t)
(set-language-environment "English")

;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; screen pos and size on startup
(setq initial-frame-alist
      '((left . 445) ;; left 55
        (top . 50)
        (width . 141)
        (height . 40)))

(setq-default mode-line-format nil)

(setq-default cursor-type 'bar)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-underline-at-descent-line t)

;; With GPG 2.1+, this forces gpg-agent to use the Emacs minibuffer to prompt
;; for the key passphrase.
(setq epg-pinentry-mode 'loopback)

;; Optimize for very long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

;; The nano style for truncated long lines.
(setq auto-hscroll-mode 'current-line)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; Dont move points out of eyes
(setq mouse-yank-at-point t)

(setq-default fill-column 80)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-auto-revert-mode 1)

(setq system-time-locale "C")

(setq initial-scratch-message ";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
")

;; Font size
;; (set-face-attribute 'default nil :font "Fira Code" :weight 'semi-bold)
;; (set-face-attribute 'default nil :font "PingFang SC")
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :weight 'semi-bold)

;; Sane defaults
(setq use-short-answers t)
(unless (>= emacs-major-version 28)
  (fset 'yes-or-no-p 'y-or-n-p))

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Enable the disabled narrow commands
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Enable the disabled dired commands
(put 'dired-find-alternate-file 'disabled nil)

;; Enable the disabled `list-timers', `list-threads' commands
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; Use TeX as default IM
(setq default-input-method "TeX")

;; Keep clean but enable `menu-bar' in MacOS
(when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; FIXME
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file "~/haoran/no/org/bookmark-default.el"))

;; Back to the previous position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recently opened files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '(;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

;; Highlight current line in GUI
(use-package hl-line
  :ensure nil
  :when (display-graphic-p)
  :hook (after-init . global-hl-line-mode))

(use-package newcomment
  :ensure nil
  :bind ([remap comment-dwim] . comment-or-uncomment)
  :config
  (defun comment-or-uncomment ()
    "Comment or uncomment the current line or region.

If the region is active and `transient-mark-mode' is on, call
`comment-or-uncomment-region'.
Else, if the current line is empty, insert a comment and indent
it.
Else, call `comment-or-uncomment-region' on the current line."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (comment-dwim nil)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  ;; `auto-fill' inside comments.
  ;;
  ;; The quoted text in `message-mode' are identified as comments, so only
  ;; quoted text can be `auto-fill'ed.
  (comment-auto-fill-only-comments t))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box nil))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Workaround with minified source files
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;; Highlight parenthesises
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Auto complete parens
(use-package elec-pair
  :ensure nil
  :hook ((go-mode . electric-pair-local-mode)
         (emacs-lisp-mode . electric-pair-local-mode))
  :custom (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Server mode.
;; Use emacsclient to connect
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; window layout manager
;;
;; gt next-tab
;; gT prev-tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; Buffer index
(use-package imenu
  :hook (imenu-after-jump . recenter))

;; Buffer manager
;;
;; `sR': switch to saved filter groups
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-movement-cycle nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Default"
      ("Emacs" (or (name . "\\*scratch\\*")
                   (name . "\\*dashboard\\*")
                   (name . "\\*compilation\\*")
                   (name . "\\*Backtrace\\*")
                   (name . "\\*Packages\\*")
                   (name . "\\*Messages\\*")
                   (name . "\\*Customize\\*")))
      ("Browser" (or (mode . eww-mode)
                     (mode . xwidget-webkit-mode)))
      ("Help" (or (name . "\\*Help\\*")
                  (name . "\\*Apropos\\*")
                  (name . "\\*info\\*")
                  (mode . Man-mode)
                  (mode . woman-mode)))
      ("Repl" (or (mode . gnuplot-comint-mode)
                  (mode . inferior-emacs-lisp-mode)
                  (mode . inferior-python-mode)))
      ("Term" (or (mode . term-mode)
                  (mode . shell-mode)
                  (mode . eshell-mode)))
      ("Mail" (or (mode . mail-mode)
                  (mode . message-mode)
                  (derived-mode . gnus-mode)))
      ("Conf" (or (mode . yaml-mode)
                  (mode . conf-mode)))
      ("Dict" (or (mode . fanyi-mode)
                  (mode . dictionary-mode)))
      ("Text" (and (derived-mode . text-mode)
                   (not (starred-name))))
      ("Magit" (or (mode . magit-repolist-mode)
                   (mode . magit-submodule-list-mode)
                   (mode . git-rebase-mode)
                   (derived-mode . magit-section-mode)
                   (mode . vc-annotate-mode)))
      ("VC" (or (mode . diff-mode)
                (derived-mode . log-view-mode)))
      ("Prog" (and (derived-mode . prog-mode)
                   (not (starred-name))))
      ("Dired" (mode . dired-mode))
      ("IRC" (or (mode . rcirc-mode)
                 (mode . erc-mode)))))))

;; Show line/column number and more
(use-package simple
  :ensure nil
  :custom
  ;; show line/column/filesize in modeline
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode nil)
  ;; No visual feedback on copy/delete.
  (copy-region-blink-delay 0)
  (delete-pair-blink-delay 0)
  ;; confusing if no fringes (GUI only).
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  ;; don't save current clipboard text before replacing it
  (save-interprogram-paste-before-kill nil)
  ;; eliminate duplicates
  (kill-do-not-save-duplicates t)
  ;; include '\n' when point starts at the beginning-of-line
  (kill-whole-line t)
  ;; show cwd when `shell-command' and `async-shell-command'
  (shell-command-prompt-show-cwd t)
  ;; show the name of character in `what-cursor-position'
  (what-cursor-show-names t)
  ;; List only applicable commands.
  ;;
  ;; ``` elisp
  ;; (defun foo ()
  ;;   (interactive nil org-mode)
  ;;   (message "foo"))
  ;; ```
  ;;
  ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; The builtin incremental search
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o"                   . isearch-occur)
              ([escape]                    . isearch-cancel)
              ;; Edit the search string instead of jumping back
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  (define-advice isearch-occur (:after (_regexp &optional _nlines))
    (isearch-exit))
  :custom
  ;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
  (isearch-resume-in-command-history t)
  ;; One space can represent a sequence of whitespaces
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace t)
  (isearch-repeat-on-direction-change t)
  ;; M-< and M-> move to the first/last occurrence of the current search string.
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  ;; lazy isearch
  (isearch-lazy-count t)
  (isearch-lazy-highlight t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]")
  (lazy-highlight-buffer t)
  ;; Mimic Vim
  (lazy-highlight-cleanup nil))

;; Web search
(use-package webjump
  :ensure nil
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c /" . webjump)
  :config
  (defconst webjump-weather-default-cities '("杭州" "深圳" "北京" "上海"))
  (defconst webjump-weather-url-template "https://weathernew.pae.baidu.com/weathernew/pc?query=%s天气&srcid=4982")

  (defun webjump-weather (_name)
    (let ((city (completing-read "City: " webjump-weather-default-cities)))
      (format webjump-weather-url-template city)))
  :custom
  (webjump-sites '(;; Internet search engines.
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Ludwig Guru" .
                    [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                   ("Stack Overflow" .
                    [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                   ("Man Search" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                   ("Man Go" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])
                   ;; Code search
                   ("Code Search" .
                    [simple-query "sourcegraph.com" "sourcegraph.com/search?q=context:global+" "&patternType=literal"])
                   ;; Life
                   ("Weather" . webjump-weather)
                   ;; Language specific engines.
                   ("x86 Instructions Reference" .
                    [simple-query "www.felixcloutier.com"
                                  "www.felixcloutier.com/x86/" ""]))))

(provide 'init-base)
;;; init-base.el ends here
