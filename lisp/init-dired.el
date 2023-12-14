;;; init-dired.el --- dired tweaks -*- lexical-binding: t -*-

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
;; dired-narrow is superseded by `consult-focus-lines'.

;;; Code:

;; dired-aux ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(lazy-load-global-keys '(("C-x C-j" . dired-jump)) "dired")

;; (add-hook 'dired-mode-hook (lambda ()
;;                              (setq truncate-lines t)))

;; see also dired+.el
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))


(setq  dired-dwim-target t
       dired-bind-vm nil
       dired-kill-when-opening-new-dired-buffer t
       dired-clean-confirm-killing-deleted-buffers nil
       dired-auto-revert-buffer #'dired-directory-changed-p
       dired-hide-details-hide-symlink-targets nil
       dired-listing-switches "-AFhlv")


;; dired-aux ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda ()
                             (require 'dired-aux)))
(with-no-warnings
  (defvar dired-dotfiles-show t)
  (defun dired-dotfiles-toggle (&rest _)
    "Show/hide dotfiles."
    (interactive)
    (if (not dired-dotfiles-show)
        (revert-buffer)
      (dired-mark-files-regexp "^\\.")
      (dired-do-kill-lines))
    (setq-local dired-dotfiles-show (not dired-dotfiles-show)))

  (advice-add 'dired-do-print :override #'dired-dotfiles-toggle))

(setq dired-vc-rename-file t
      dired-do-revert-buffer t
      dired-isearch-filenames 'dwim
      dired-create-destination-dirs 'ask)


;; dired-x ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq dired-omit-verbose nil
      dired-omit-files (rx string-start
                           (or ".DS_Store"
                               ".cache"
                               ".vscode"
                               ".ccls-cache" ".clangd")
                           string-end)
      dired-guess-shell-alist-user `((,(rx "."
                                           (or
                                            ;; Videos
                                            "mp4" "avi" "mkv" "flv" "ogv" "ogg" "mov"
                                            ;; Music
                                            "wav" "mp3" "flac"
                                            ;; Images
                                            "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                            ;; Docs
                                            "pdf" "md" "djvu" "ps" "eps" "doc" "docx" "xls" "xlsx" "ppt" "pptx")
                                           string-end)
                                      ,(pcase system-type
                                         ('gnu/linux "xdg-open")
                                         ('darwin "open")
                                         ('windows-nt "start")
                                         (_ "")))))
(add-hook 'dired-mode-hook 'dired-omit-mode)


;; dired-x ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make dired colorful
(with-eval-after-load 'dired
  (require 'diredfl)
  (add-hook 'dired-mode-hook #'diredfl-mode))


(provide 'init-dired)
;;; init-dired.el ends here
