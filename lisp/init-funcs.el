;;; init-funcs.el --- core functions -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:


;;;###autoload
(defun +set-frame-size-mac ()
  "Set the current frame's size to 120x50."
  (interactive)
  (set-frame-size (selected-frame) 120 50))

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
(defun +open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list
                                             (concat buffer-file-name ":" line ":" column)
                                             "--goto"))))

;;;###autoload
(defun spacemacs/sudo-edit (&optional arg)
  "Forcibly write to a file without permission after password `ARG'."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun +create-new-tab-bar ()
  "Create a new tab bar and switch dashboard."
  (interactive)
  (tab-bar-new-tab)
  ;; TODO use if instead
  ;; (pcase (treemacs-current-visibility)
  ;;   ('visible (delete-window (treemacs-get-local-window))))
  ;; (dashboard-refresh-buffer)
  (+create-scratch-buffer)
  (tab-bar-rename-tab "xxx"))

;;;###autoload
(defun +create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (read-only-mode 0))

;;;###autoload
(defun +remove-dos-eol()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;;###autoload
(defun +hidden-dos-eol()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun +toggle-maximize-buffer()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

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

;;;###autoload
(defun +open-file-init ()
  "Open file: init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el")
  (tab-bar-rename-tab "hack"))

;; FIXME
;;;###autoload
(defun +open-wiki-note ()
  "Open file: init.el."
  (interactive)
  (find-file "~/haoran/no/org/site/org/index.org")
  (tab-bar-rename-tab "note"))

;;;###autoload
(defun +open-daily-task ()
  "Open file: gtd-daily.el."
  (interactive)
  (find-file "~/haoran/no/org/org-directory/daily.org")
  (tab-bar-rename-tab "note"))

;; FIXME
;;;###autoload
(defun +open-directory-markdown ()
  "Open directory: markdown."
  (interactive)
  (dired "~/haoran/no/markdown/"))

;; FIXME
;;;###autoload
(defun +open-directory-haoran ()
  "Open directory: haoran."
  (interactive)
  (dired "~/haoran/"))

(provide 'init-funcs)
;;; init-funcs.el ends here
