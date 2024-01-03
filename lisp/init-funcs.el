;;; init-funcs.el --- core functions -*- lexical-binding: t -*-

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
;; functions that are not assigned keys and need to be called manually have the +func prefix

;;; Code:


;;;###autoload
(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line. Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 39 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'xah-show-formfeed-as-line))
(add-hook 'emacs-lisp-mode-hook #'xah-show-formfeed-as-line)

;;;###autoload
(defun +org-insert-image ()
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
    (message image-file))
  (org-display-inline-images))

;;;###autoload
(defun +dwim-create-link-with-datetime ()
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

;;;###autoload
(defun +func-make-emacs-opaque ()
  "Make Emacs window opaque."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;;;###autoload
(defun +func-make-emacs-transparent ()
  "Make Emacs window transparent."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 60))
  (add-to-list 'default-frame-alist '(alpha (85 60))))

;;;###autoload
(defun +func-set-frame-vertical-alignment ()
  "| Set the current frame's size(120x50) and position(445, 50)."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 120 50)
    (set-frame-position frame 445 50)))

;;;###autoload
(defun +func-set-frame-horizontal-alignment ()
  "- Set the current frame's size(141x40) and position(445, 50)."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 141 40)
    (set-frame-position frame 445 50)))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;;###autoload
(defun +unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun +func-open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list
                                             (concat buffer-file-name ":" line ":" column)
                                             "--goto"))))

;;;###autoload
(defun +func-spacemacs/sudo-edit (&optional arg)
  "Forcibly write to a file without permission after password `ARG'."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun +create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (read-only-mode 0))

;;;###autoload
(defun +func-remove-dos-eol()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;;###autoload
(defun +func-hidden-dos-eol()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW.
If `spacemacs-layouts-restrict-spc-tab' is 't' then this only switches between
the current layouts buffers.
Deprecated by crux-switch-to-previous-buffer."
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

;;;###autoload
(defun +indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (+indent-buffer)
        (message "Indented buffer.")))))

;;;###autoload
(defun +indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

;;;###autoload
(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

;;;###autoload
(defun +copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Copy file to: ")
           current-prefix-arg)))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) t)
    (copy-file old-path new-path (or overwrite-p 1))))

;;;###autoload
(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))

;;;###autoload
(defun +copy-current-buffer-name ()
  "Copy the name of current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "Copying '%s' to clipboard" (buffer-name)))

;;;###autoload
(defun +open-in-browser ()
  "Open in browser."
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


(provide 'init-funcs)
;;; init-funcs.el ends here
