;;; windowop.el --- cursor and text move  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun vanilla/scroll-half-page-down ()
  "Scroll down half a page."
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

;;;###autoload
(defun vanilla/scroll-half-page-up ()
  "Scroll up half a page."
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

;;;###autoload
(defun vanilla/move-to-window-top ()
  "Move the current line and cursor to the top third of the window."
  (interactive)
  (recenter-top-bottom 0))

;;;###autoload
(defun vanilla/move-to-window-middle ()
  "Move the current line and cursor to the middle of the window."
  (interactive)
  (recenter-top-bottom))

;;;###autoload
(defun vanilla/move-to-window-bottom ()
  "Move the current line and cursor to the bottom third of the window."
  (interactive)
  (recenter-top-bottom -1))

;;;###autoload
(defun vanilla/scroll-up-one-line ()
  "Scroll up one line and keep the cursor in its original position,
   unless the cursor is already at the top of the visible window,
   in which case, keep the cursor at the top of the visible window."
  (interactive)
  (let ((orig-line (line-number-at-pos))
        (scroll-margin-value (if (integerp scroll-margin) scroll-margin 0))
        (top-line (line-number-at-pos (window-start)))
        (orig-point (point)))
    (scroll-up 1)
    (if (> orig-line (+ top-line scroll-margin-value 1))
        (goto-char orig-point))))

;;;###autoload
(defun vanilla/scroll-down-one-line ()
  "Scroll down one line and keep the cursor in its original position,
   unless the cursor is already at the bottom of the visible window,
   in which case, keep the cursor at the bottom of the visible window."
  (interactive)
  (let ((orig-line (line-number-at-pos))
        (scroll-margin-value (if (integerp scroll-margin) scroll-margin 0))
        (bottom-line (line-number-at-pos (window-end)))
        (orig-point (point)))
    (scroll-down 1)
    (if (< orig-line (- bottom-line scroll-margin-value 1))
        (goto-char orig-point))))

;;;###autoload
(defun vanilla/scroll-left-half-page ()
  "Scroll the window left by half the page height."
  (interactive)
  (scroll-left (/ (window-body-height) 2)))

;;;###autoload
(defun vanilla/scroll-right-half-page ()
  "Scroll the window right by half the page height."
  (interactive)
  (scroll-right (/ (window-body-height) 2)))

;;;###autoload
(defun vanilla/move-cursor-8-lines-down ()
  "Move the cursor down by 8 lines."
  (interactive)
  (next-line 8))

;;;###autoload
(defun vanilla/move-cursor-8-lines-up ()
  "Move the cursor up by 8 lines."
  (interactive)
  (previous-line 8))

(provide 'cursormove)
;;; cursormove.el
