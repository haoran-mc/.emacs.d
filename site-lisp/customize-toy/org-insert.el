;;; org-insert.el ---quickly insert in org           -*- lexical-binding: t; -*-

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


;;;###autoload
(defun vanilla/org-insert-stamp-inactive ()
  "insert an inactive stamp time."
  (interactive)
  (org-time-stamp-inactive '(16)))

;;;###autoload
(defun vanilla/org-insert-image ()
  "insert a image from clipboard"
  (interactive)
  (let* ((path (concat default-directory "images/"))
         (fname (read-string "Enter file name: "))
         (image-file (concat path fname)))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))
    ;; (shell-command (concat "pngpaste " image-file))
    (org-insert-link nil
                     (concat "file:" image-file)
                     "")
    (message image-file)))

;;;###autoload
(defun vanilla/org-insert-image-with-timestamp ()
  "paste image from clipboard, image name is automatically
assigned to current time timestamp, image only support png type"
  (interactive)
  (let* ((path (concat default-directory "images/"))
         (timestamp-str (number-to-string
                         (truncate (float-time (current-time))))) ;; timestamp
         (image-file (concat path timestamp-str ".png")))
    (if (not (file-exists-p path))
        (mkdir path))
    (do-applescript (concat
                     "set the_path to \"" image-file "\" \n"
                     "set png_data to the clipboard as «class PNGf» \n"
                     "set the_file to open for access (POSIX file the_path as string) with write permission \n"
                     "write png_data to the_file \n"
                     "close access the_file"))
    (org-insert-link nil (concat "file:" image-file) "")
    (message image-file)))

;;;###autoload
(defun vanilla/dwim-create-link-with-datetime ()
  "Create a link with current datetime and filename from the word at point.
The word at point is treated as a filename. Any consecutive hyphens or underscores
are treated as a single unit and preserved in the filename."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds))
         (filename (buffer-substring-no-properties start end))
         (clean-filename (replace-regexp-in-string "\\([-_]+\\)" " " filename))
         (clean-filename (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" clean-filename))
         (date (format-time-string "%y%m%d"))
         (link-filename (format "%s-%s.org" date filename))
         (link-description clean-filename))
    (delete-region start end)
    (org-insert-link nil (concat "./" link-filename) link-description)))


(provide 'org-insert)
;;; org-insert.el ends here
