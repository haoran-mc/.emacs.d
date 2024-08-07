;;; init-modeline.el --- modeline                    -*- lexical-binding: t; -*-

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

(defun +win-num ()
  (let ((n (window-numbering-get-number)))
    (alist-get
     n
     '((0 . "🄌")
       (1 . "❶")
       (2 . "❷")
       (3 . "❸")
       (4 . "❹")
       (5 . "❺")
       (6 . "❻")
       (7 . "❼")
       (8 . "❽")
       (9 . "❾")))))

(defvar +smart-file-name-cache nil)

(defun +shorten-long-path (path)
  (let ((paths (split-string path "/")))
    (if (< (length paths) 3)
        path
      (string-join (reverse (let ((rpaths (reverse paths)))
                              (-concat
                               (-take 2 rpaths)
                               (->> (-drop 2 rpaths)
                                    (--map (if (> (length it) 1)
                                               (substring it 0 1)
                                             it))))))
                   "/"))))

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (+shorten-long-path (file-relative-name bfn vc-dir)))
     (bfn bfn)
     (t (buffer-name)))))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

;; time...
(setq mode-line-misc-info (cdr mode-line-misc-info))

(defun +format-mode-line ()
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (arrow-color (if (eq bg-mode 'light) "red" "#FEDD38"))
         (vc-color (if (eq bg-mode 'light) "#859901" "#859901"))
         (mode-color (if (eq bg-mode 'light) "#258BD2" "#258BD2"))
         (bottom-color (if (eq bg-mode 'light) "#D33682" "#4FB3D8"))
         (lhs `((:eval (propertize " ❯ " 'face '(:foreground ,arrow-color)))
                (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                (:eval (when (bound-and-true-p meow-mode) (meow-indicator)))
                " "
                ;; TODO check the length of file-name
                (:eval (let ((file-name (+smart-file-name-cached)))
                         (if (and buffer-file-name (buffer-modified-p))
                             (propertize file-name 'face '(:foreground ,arrow-color))
			               file-name)))
                " "
                (:eval (when (> (window-width) 90)
                         (let ((vc-mode-value vc-mode))
                           (when vc-mode-value
                             (setq vc-mode-string (propertize
                                                   (string-trim vc-mode-value)
                                                   'face '(:foreground ,vc-color)))))))))
         (rhs `((:eval (when (> (window-width) 120)
                         mode-line-misc-info)) ;; with blank
                (:eval (when (and (> (window-width) 90) (stringp mode-name))
                         (propertize
                          (format "%s " (string-trim mode-name))
                          'face '(:foreground ,mode-color))))
                (:eval (propertize "[%l:%c] " 'face 'font-lock-constant-face))
                (:eval (propertize "%p" 'face '(:foreground ,bottom-color))))) ;; Top、Bottom
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))


(setq-default header-line-format '((:eval (+format-mode-line))))
(setq-default mode-line-format nil)


(provide 'init-modeline)
;;; init-modeline.el ends here
