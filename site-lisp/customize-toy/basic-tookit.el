;;; windowop.el --- basic toolkit  -*- lexical-binding: t; -*-

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
(defun vanilla/upcase-word ()
  "Convert the entire word at point to uppercase."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (upcase-region (car bounds) (cdr bounds))))))

;;;###autoload
(defun vanilla/downcase-word ()
  "Convert the entire word at point to lowercase."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (downcase-region (car bounds) (cdr bounds))))))

;;;###autoload
(defun vanilla/merge-line-down ()
  "Merge the current line with the line below it."
  (interactive)
  (next-line)
  (delete-indentation))

;;;###autoload
(defun vanilla/clear-current-line ()
  "Delete the current line without removing the final newline."
  (interactive)
  (delete-region (point-at-bol) (point-at-eol)))

;;;###autoload
(defun vanilla/smart-kill-line ()
  "Kill to the end of the line and kill whole line on the next call.
1. xxx|xxx  kill-line
2. xxxxxx|  clear-current-line
3. |xxxxxx  clear-current-line
4. |        kill-line"
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (cond ((bolp) (kill-line))           ;; 空行
          ((= orig-point (point))        ;; 光标位于行尾
           (vanilla/clear-current-line))
          ((= orig-point (point-at-bol)) ;; 光标位于行首
           (vanilla/clear-current-line))
          (t (goto-char orig-point)      ;; 光标位于行中
             (kill-line)))))

;;;###autoload
(defun vanilla/mark-whole-word ()
  "Mark the whole word at point without moving the cursor."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (push-mark (car bounds) t t)
      (goto-char (cdr bounds)))))

;;;###autoload
(defun vanilla/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (vanilla/indent-buffer)
        (message "Indented buffer.")))))

;;;###autoload
(defun vanilla/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun vanilla/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (read-only-mode 0))

;;;###autoload
(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW.
If `spacemacs-layouts-restrict-spc-tab' is 't' then this only switches between
the current layouts buffers."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p spacemacs-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))





(provide 'basic-tookit)
;;; basic-tookit ends here
