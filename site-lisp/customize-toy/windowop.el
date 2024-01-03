;;; windowop.el --- enhance poor emacs window operation  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defun vanilla/split-window-below-with-balance ()
  "Balance windows after split window, and move cursor to the new window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (balance-windows))

(defun vanilla/split-window-up-with-balance ()
  "Balance windows after split window, split window below but cursor stay up still."
  (interactive)
  (split-window-below)
  (balance-windows))

(defun vanilla/split-window-right-with-balance ()
  "Balance windows after split window, and move cursor to the new window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (balance-windows))

(defun vanilla/split-window-left-with-balance ()
  "Balance windows after split window, split window right but cursor stay left still."
  (interactive)
  (split-window-right)
  (balance-windows))

(defun vanilla/delete-window-with-balance ()
  "Balance windows after delete current window."
  (interactive)
  (delete-window)
  (balance-windows))

;;;###autoload
(defun vanilla/toggle-maximize-buffer()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun vanilla/exchange-split-window-position-structure ()
  "Exchange vertically to horizontally or vice versa."
  (interactive)
  (if (eq (window-width) (frame-width))
      (split-window-horizontally-instead)
    (split-window-vertically-instead)))


(provide 'windowop)
;;; windowop.el ends here
