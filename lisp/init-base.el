;;; init-base.el --- The necessary settings -*- lexical-binding: t -*-

;; Copyright (C) 2022  Haoran Liu

;; Author: HaoRan Liu <haoran.mc@outlook.com>
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

(setq user-full-name "Haoran Liu")
(setq user-mail-address "haoran.mc@outlook.com")

(setq-default default-enable-multibyte-characters t)
(set-language-environment "English")
(setq system-time-locale "C")

;; encoding
(prefer-coding-system 'utf-8)

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

;; The nano style for truncated long lines.
(setq auto-hscroll-mode t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(setq auto-save-default nil
      ;; No backup files
      make-backup-files nil
      ;; No lock files
      create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; Cutting and pasting use clipboard
(setq select-enable-clipboard t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; Help me find the cursor quickly
(blink-cursor-mode 1)
(setq blink-cursor-mode 0) ;; Flashes a million times.

;; Smooth scroll & friends
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

(setq-default fill-column 80)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")
            (modify-syntax-entry ?- "w")))

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-auto-revert-mode 1)

(setq initial-scratch-message ";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
")

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

;; Use TeX as default IM
;; (setq default-input-method "TeX")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Keep clean but enable `menu-bar' in MacOS
;; (when (and (fboundp 'menu-bar-mode) (not (eq system-type 'darwin)))
;;   (menu-bar-mode -1))
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (fboundp 'set-scroll-bar-mode)
;;   (set-scroll-bar-mode nil))

;; bookmark ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq bookmark-default-file (concat haoran/home-directory "/haoran/no/org/bookmark-default.el"))


;; saveplace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back to the previous position
(add-hook 'after-init-hook 'save-place-mode)


;; recentf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook 'recentf-mode)
(setq recentf-max-saved-items 300
      recentf-auto-cleanup 'never
      recentf-exclude '(;; Folders on MacOS start
                        "^/private/tmp/"
                        "^/var/folders/"
                        ;; Folders on MacOS end
                        "^/tmp/"
                        "/ssh\\(x\\)?:"
                        "/su\\(do\\)?:"
                        "^/usr/include/"
                        "/TAGS\\'"
                        "COMMIT_EDITMSG\\'"))


;; hl-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight current line in GUI
(when (display-graphic-p)
  (add-hook 'after-init-hook 'global-hl-line-mode))


;; newcomment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(global-set-key [remap comment-dwim] 'comment-or-uncomment)


;; hideshow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(setq hs-set-up-overlay #'hideshow-folded-overlay-fn)


;; whitespace ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show trailing whitespaces
(dolist (mode-hook '(prog-mode-hook markdown-mode-hook conf-mode-hook))
  (add-hook mode-hook 'whitespace-mode))

(setq whitespace-style '(face trailing))


;; so-long ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-enable-so-long-maybe ()
  "Enable so-long-mode if the file size is greater than 1MB."
  (when (> (buffer-size) 1000000)
    (so-long-mode 1)))

(add-hook 'find-file-hook 'my-enable-so-long-maybe)


;; server-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use emacsclient to connect
(add-hook 'after-init-hook 'server-mode)


;; help ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq help-window-select t)


;; imenu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'imenu-after-jump-hook 'recenter)


;; simple ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show line/column number and more
;; show line/column/filesize in modeline
(setq line-number-mode t
      column-number-mode t
      size-indication-mode nil
      ;; No visual feedback on copy/delete.
      copy-region-blink-delay 0
      delete-pair-blink-delay 0
      ;; confusing if no fringes (GUI only).
      visual-line-fringe-indicators '(nil right-curly-arrow)
      ;; don't save current clipboard text before replacing it
      save-interprogram-paste-before-kill nil
      ;; eliminate duplicates
      kill-do-not-save-duplicates t
      ;; include '\n' when point starts at the beginning-of-line
      kill-whole-line t
      ;; show cwd when `shell-command' and `async-shell-command'
      shell-command-prompt-show-cwd t
      ;; show the name of character in `what-cursor-position'
      what-cursor-show-names t
      ;; List only applicable commands.
      ;;
      ;; ``` elisp
      ;; (defun foo ()
      ;;   (interactive nil org-mode)
      ;;   (message "foo"))
      ;; ```
      ;;
      ;; M-x foo should only be available in `org-mode` or modes derived from `org-mode`.
      read-extended-command-predicate #'command-completion-default-include-p)

(provide 'init-base)
;;; init-base.el ends here
