;;; init-git.el --- Git is awesome -*- lexical-binding: t -*-

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
;;

;; The awesome git client
;;
;; Explicit binding makes it load lazily although it's the default.
;; See `magit-define-global-key-bindings' for more information.
;; magit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq magit-diff-refine-hunk t
      magit-diff-paint-whitespace nil
      magit-ediff-dwim-show-on-hunks t
      magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

(defun +magit-require ()
  (interactive)
  (require 'magit))

;; similar to vc-annotate, vc-region-history
(setq magit-blame-styles
      '((margin
         (margin-width . 32)
         (margin-format . ("%C %c %f"))
         (margin-face . magit-blame-margin)
         (margin-body-face . magit-blame-dimmed)
         (show-message . t))))


;; vc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: `diff-hl' depends on `vc'
(require 'vc)
(setq vc-follow-symlinks t
      vc-allow-async-revert t
      vc-handled-backends '(Git))


;; diff-hl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight uncommitted changes using VC
(require 'diff-hl)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook 'diff-hl-mode)

(require 'diff-hl-dired)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)

(defun +diff-hl-find-hunk ()
  "Go to the next hunk, loop to the beginning of the file."
  (interactive)
  (let ((next-hunk (diff-hl-search-next-hunk))
        (prev-hunk (diff-hl-search-next-hunk t)))
    (cond (next-hunk (goto-char (overlay-start next-hunk)))
          (prev-hunk (progn
                       (goto-line 1)
                       (goto-char (overlay-start (diff-hl-search-next-hunk)))))
          (t (message "No further hunks found")))))


;; ediff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUILT-IN: Visual diff interface
(require 'ediff)
(add-hook 'ediff-before-setup-hook 'ediff-save-window-conf)
(add-hook 'ediff-quit 'ediff-restore-window-conf)
(defvar local-ediff-saved-window-conf nil)

(defun ediff-save-window-conf ()
  (setq local-ediff-saved-window-conf (current-window-configuration)))

(defun ediff-restore-window-conf ()
  (when (window-configuration-p local-ediff-saved-window-conf)
    (set-window-configuration local-ediff-saved-window-conf)))

(setq ediff-highlight-all-diffs t
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-horizontally)


;; gitignore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup gitignore mode
(require 'conf-mode)
(add-to-list 'auto-mode-alist '("\\.gitignore\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'"     . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.gitattributes\\'" . conf-unix-mode))


(provide 'init-git)
;;; init-git.el ends here
