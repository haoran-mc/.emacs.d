;;; init-text.el ---                                 -*- lexical-binding: t; -*-

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

;; whole-line-or-region ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'whole-line-or-region)
;; (whole-line-or-region-global-mode)


;; valign ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pixel alignment for org/markdown tables
(require 'valign)
(defvar ran--valign-max-lines 500
  "Maximum line count for enabling `valign-mode'.")

(defun my/enable-valign-maybe ()
  "Enable `valign-mode' only when buffer line count is acceptable."
  (save-restriction
    (widen)
    (if (> (line-number-at-pos (point-max)) ran--valign-max-lines)
        (when (bound-and-true-p valign-mode)
          (valign-mode -1))
      (valign-mode 1))))

(dolist (mode-hook '(markdown-mode-hook org-mode-hook))
  (add-hook mode-hook #'my/enable-valign-maybe))
(setq valign-fancy-bar t)


(provide 'init-text)
;;; init-text.el ends here
