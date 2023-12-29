;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-

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
;;; Code:

;; A simple benchmark of calls to Emacs require and load functions.
(add-to-list 'load-path "~/.emacs.d/site-lisp/benchmark-init-el")
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

;; A big contributor to startup times is garbage collection. We up the gc threshold to
;; temporarily prevent it from running, and then reset it by the `gcmh' package.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; read-process-output-max: the maximum bytes read from processs in a single chunk (default is 4kb).
;; This is too small for the LSP protocol that uses JSON communication
(setq read-process-output-max (* 4 1024 1024))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             (cl-remove-if ;; filter catalog
              #'(lambda (subdir)
                  (or
                   (not (file-directory-p (concat dir subdir))) ;; remove non-directory files
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; paths with .el .so .dll files in the directory are added to `load-path' only.
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll: dynamic library
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))
          ;; `add-to-list' The third argument must be t
          ;; which means add to the end of the list. BFS
          (add-to-list 'load-path subdir-path t))
        ;; recursive search
        (add-subdirs-to-load-path subdir-path)))))

(let ((userdir  (locate-user-emacs-file "lisp"))
      (extdir   (locate-user-emacs-file "site-lisp"))
      (themedir (locate-user-emacs-file "theme")))
  (add-subdirs-to-load-path (file-name-directory userdir))
  (add-subdirs-to-load-path (file-name-directory extdir))
  (add-subdirs-to-load-path (file-name-directory themedir)))
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-variables)
(require 'init-font)
(require 'no-littering)
(require 'init-base)
(require 'lazy-load)
(require 'init-funcs)

(require 'init-ui)
(require 'init-mode)
(require 'init-modeline)
(require 'init-yasnippet)
(require 'init-lsp)
(require 'init-git)
(require 'init-dev)
(require 'init-dirvish)
(require 'init-minibuffer)
(require 'init-windows)
(require 'init-time)
(require 'init-indent)
(require 'init-keys)

;; standalone apps
(require 'init-reader)
(require 'init-gc)
(require 'init-autosave)
(require 'init-autoinsert)
(require 'init-keyfreq)

;; load later
(run-with-idle-timer
 1 nil
 #'(lambda ()
     (require 'init-utils)
     (require 'init-idle)
     (require 'init-parens)
     (require 'init-isearch)
     (require 'init-cursor-chg)
     (require 'init-hl-todo)
     (require 'init-which-key)
     (require 'init-highlight-thing)
     (require 'init-vundo)
     (require 'init-whole-line-or-region)
     (require 'init-project)
     (require 'init-valign)
     ))

(if (display-graphic-p)
    (if (eq 'light (frame-parameter nil 'background-mode))
        (require 'init-theme-light) ;; light theme
      (require 'init-theme-dark)) ;; dark theme
  (message "Emacs running in non-GUI mode"))

;; os specific
(cond ((eq system-type 'gnu/linux) (require 'init-linux))
      ((eq system-type 'darwin) (require 'init-osx)))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
