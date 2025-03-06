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
;; 1. 需要预加载的函数放在这，不需要预加载的函数放在 basic-tookit.el
;; 2. 内部调用的函数使用 my/ 前缀，手动调用的函数使用 ran/ 前缀

;;; Code:


;;;###autoload
(defun my/reload-this-buffer ()
  "Kill the current buffer and reload it from the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (kill-this-buffer)
          (find-file filename))
      (message "This buffer is not associated with a file."))))

;;;###autoload
(defun my/center-layout ()
  "Load my center layout."
  (interactive)
  (let ((tab-name "cent"))
    (if (tab-bar--tab-index-by-name tab-name)
        (tab-bar-select-tab-by-name tab-name)
      (progn
        ;; 1. create algo tab
        (require 'init-tab-bar)
        (vanilla/tab-bar-switch-to-tab tab-name)
        ;; 2. open centre.org
        (find-file "~/haoran/no/org/sync-notes/centre.org")
        ;; 3. treemacs
        (require 'init-treemacs)
        (my/treemacs-add-current-project-workspace-exclusively)
        (switch-to-buffer "centre.org")
        ;; 4. split agenda
        (vanilla/split-window-right-with-balance)
        (org-agenda "TODO" "T")
        (windmove-left)))))

;;;###autoload
(defun my/algo-layout ()
  "Load my algo layout."
  (interactive)
  (let ((tab-name "algo"))
    (if (tab-bar--tab-index-by-name tab-name)
        (tab-bar-select-tab-by-name tab-name)
      (progn
        ;; 1. create algo tab
        (require 'init-tab-bar)
        (vanilla/tab-bar-switch-to-tab tab-name)
        ;; 2. open initial-algo-message.txt
        (find-file "~/haoran/code/algo/initial-algo-message.txt")
        ;; 3. treemacs
        (require 'init-treemacs)
        (my/treemacs-add-current-project-workspace-exclusively)
        (switch-to-buffer "initial-algo-message.txt")))))

;;;###autoload
(defun lazycat/remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

;;;###autoload
(defun lazycat/remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

;; 自动格式化 *.el 文件， 保存并自动加载，TODO 是否需要这个函数？
;;;###autoload
(defun my/refresh-file ()
  "Automatic reload current file."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (my/indent-buffer)
         (my/indent-comment-buffer)
         (save-buffer)
         (load-file (buffer-file-name)))
        ((member major-mode '(lisp-mode c-mode perl-mode))
         (my/indent-buffer)
         (my/indent-comment-buffer)
         (save-buffer))
        ((member major-mode '(haskell-mode sh-mode))
         (my/indent-comment-buffer)
         (save-buffer))
        ((derived-mode-p 'scss-mode)
         (require 'css-sort)
         (css-sort))
        (t (message "Current mode is not supported, so not reload"))))

;;;###autoload
(defun my/xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line. Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 39 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))
;; ───────────────────────────────────────
(add-hook 'emacs-lisp-mode-hook #'my/xah-show-formfeed-as-line)
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'my/xah-show-formfeed-as-line))

;;;###autoload
(defun my/copy-file-path-and-line-number ()
  "Copy the current buffer file name and line number to the clipboard."
  (interactive)
  (if buffer-file-name
      (let ((file-path (concat (file-truename buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
        (kill-new file-path)
        (message "Copied: %s" file-path))
    (message "Buffer is not visiting a file")))

;;;###autoload
(defun ran/make-emacs-opaque ()
  "Make Emacs window opaque."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

;;;###autoload
(defun ran/make-emacs-transparent ()
  "Make Emacs window transparent."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 60))
  (add-to-list 'default-frame-alist '(alpha (85 60))))

;;;###autoload
(defun ran/set-frame-vertical-alignment ()
  "| Set the current frame's size(120x50) and position(445, 50)."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 120 50)
    (set-frame-position frame 445 50)))

;;;###autoload
(defun ran/set-frame-horizontal-alignment ()
  "- Set the current frame's size(141x40) and position(445, 50)."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-size frame 141 40)
    (set-frame-position frame 445 50)))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;;###autoload
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun spacemacs/sudo-edit (&optional arg)
  "Forcibly write to a file without permission after password `ARG'."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun ran/remove-dos-eol()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;;###autoload
(defun ran/hidden-dos-eol()
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

(defun my/indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (my/indent-comment-region (point-min) (point-max)))

(defun my/indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))

;;;###autoload
(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (my/indent-buffer)
        (message "Indented buffer.")))))

;;;###autoload
(defun my/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun my/open-in-browser ()
  "Open in browser."
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


(provide 'init-funcs)
;;; init-funcs.el ends here
