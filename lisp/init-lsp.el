;;; init-lsp.el --- The lsp client -*- lexical-binding: t -*-

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


;; Jump to definition, I keep it anyway though it doesn't load most of the time
(require 'dumb-jump)
(setq dumb-jump-quiet t
      dumb-jump-aggressive t
      dumb-jump-selector 'completing-read)

(require 'posframe)


;; lsp-bridge ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar +lsp-bridge-jump-stack nil
  "Stack to store jump locations for +lsp-bridge-jump-back.")

;;;###autoload
(defun +lsp-bridge-jump ()
  "Fuses LSP-bridge find-function and dumb-jump intelligent jumps."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (let ((symb (function-called-at-point)))
      (when symb
        (push (point-marker) +lsp-bridge-jump-stack)
        (find-function symb))))
   (lsp-bridge-mode
    (lsp-bridge-find-def))
   (t
    (require 'dumb-jump)
    (dumb-jump-go))))

;;;###autoload
(defun +lsp-bridge-jump-back ()
  "Jump back to the previous location."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (if (null +lsp-bridge-jump-stack)
        (message "No previous location to jump back to.")
      (let ((marker (pop +lsp-bridge-jump-stack)))
        (if (marker-buffer marker)
            (progn
              (switch-to-buffer (marker-buffer marker))
              (goto-char (marker-position marker)))
          (message "Jump location is no longer available.")))))
   (lsp-bridge-mode
    (lsp-bridge-find-def-return))
   (t
    (require 'dumb-jump)
    (dumb-jump-back))))

;;;###autoload
(defun my/yas-expand()
  (interactive)
  (if (use-region-p) ; indent region instead if it's active
      (indent-region (region-beginning) (region-end))
    (when (and (let ((indent (current-indentation)))
                 (funcall indent-line-function)
                 (= indent (current-indentation)))
               (looking-at "[[:space:]\n]")) ;; TODO or "[^[:word:]]"
      (setq this-command 'yas-expand)
      (call-interactively #'yas-expand))))

(setq lsp-bridge-enable-hover-diagnostic t
      acm-enable-quick-access nil
      lsp-bridge-enable-mode-line nil)


(defvar my/lsp-bridge-loaded nil
  "Flag to track whether lsp-bridge has been loaded.")

(defun my/load-lsp-bridge ()
  "Load lsp-bridge if it hasn't been loaded yet."
  (unless my/lsp-bridge-loaded
    (require 'lsp-bridge)
    (setq my/lsp-bridge-loaded t)
    (message "lsp-bridge loaded")
    (define-key lsp-bridge-mode-map (kbd "M-.") '+lsp-bridge-jump)
    (define-key lsp-bridge-mode-map (kbd "M-,") '+lsp-bridge-jump-back)
    (define-key lsp-bridge-mode-map (kbd "M-?") 'lsp-bridge-find-references)
    (define-key lsp-bridge-mode-map (kbd "<tab>") 'my/yas-expand)
    (define-key lsp-bridge-mode-map (kbd "C-c c r") 'lsp-bridge-rename) ;; code rename
    (define-key lsp-bridge-mode-map (kbd "C-c c q") 'lsp-bridge-ref-quit))
  (message "major mode is: %s" major-mode)
  (lsp-bridge-mode))

(dolist (mode-hook '(python-mode-hook
                     emacs-lisp-mode-hook
                     go-mode-hook))
  (add-hook mode-hook #'my/load-lsp-bridge))


(provide 'init-lsp)
;;; init-lsp.el ends here
