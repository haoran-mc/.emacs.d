;;; func-org.el --- funcs to improve org            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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
;;
(require 'func-lazy)

(defun my/org-narrow-dwim ()
  "如果当前是 narrow 状态则 widen，否则 narrow to subtree 并展开"
  (interactive)
  (if (buffer-narrowed-p)
      (progn
        (widen)
        (recenter))
    (progn
      (org-narrow-to-subtree)
      (org-show-subtree))))

(defun my/preview-file-link ()
  "Preview the file links under the cursor in another window."
  (interactive)
  (let ((context (org-element-context)))
    (if (eq (org-element-type context) 'link)
        (progn
          (lazycat/remember-init)
          (if (one-window-p)
              (split-window-horizontally))
          (other-window 1)
          (lazycat/remember-jump)
          (org-open-at-point)
          (other-window -1))
      (message "No file link found under the cursor."))))


(defun my/open-and-play-gif-image (file &optional link)
  "Open and play GIF image `FILE' in Emacs buffer.

Optional for Org-mode file: `LINK'."
  (let ((gif-image (create-image file))
		(tmp-buf (get-buffer-create "*Org-mode GIF image animation*")))
	(switch-to-buffer tmp-buf)
	(erase-buffer)
	(insert-image gif-image)
	(image-animate gif-image nil t)
	(local-set-key (kbd "q") 'bury-buffer)))


(provide 'func-org)
;;; func-org.el ends here
