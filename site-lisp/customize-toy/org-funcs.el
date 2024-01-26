;;; org-funcs.el --- funcs to improve org            -*- lexical-binding: t; -*-

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

(require 'basic-tookit)

(defun vanilla/preview-file-link ()
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


(provide 'org-funcs)
;;; org-funcs.el ends here
