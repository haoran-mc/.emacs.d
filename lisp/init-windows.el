;;; init-windows.el ---noisy emacs' window           -*- lexical-binding: t; -*-

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

;; ace-window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 依赖 avy，所以要提前导入 init-avy
;; 否则如果使用 ace-window 先于 avy，init-avy 中的配置失效
(require 'init-avy)
(require 'ace-window)
(require 'ace-window-posframe)

(setq aw-keys '(?h ?j ?l ?k ?a ?s ?d ?f ?g)
      aw-background nil)

(ace-window-posframe-enable)
(set-face-attribute 'aw-leading-char-face nil :weight 'bold :height 3.0 :foreground "red")


;; popper ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enforce rules for popups
(require 'popper)
(popper-mode +1)
;; (setq popper-group-function #'popper-group-by-directory)

;; (require 'popper-echo)
;; (add-hook 'emacs-startup-hook 'popper-echo-mode)
;; (setq popper-echo-dispatch-actions t)

(global-set-key (kbd "M-p") 'popper-toggle)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)

(setq popper-reference-buffers
      '("\\*Messages\\*$"
        "Output\\*$"
        "\\*Async Shell Command\\*$"
        compilation-mode "\\*Compile-Log\\*$" "\\*Completions\\*$"
        "\\*Warnings\\*$"
        help-mode helpful-mode "\\*Help\\*$"

        "^\\*.*eshell.*\\*.*$"
        "^\\*.*shell.*\\*.*$"
        "^\\*.*vterm[inal]*.*\\*.*$"))

(with-no-warnings
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))

  (setq popper-window-height #'my-popper-fit-window-height)

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))

  (advice-add #'keyboard-quit :before #'popper-close-window-hack))


(setq display-buffer-alist
      '(("\\*Org Src" ;; Adjust this pattern based on the actual buffer name
         (display-buffer-in-side-window)
         (window-width . 0.4)  ;; Adjust the width as needed
         (side . right))
        ("\\*vc-.*\\*"
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right))
        ("\\*Annotate.*\\*"
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right))))

(provide 'init-windows)
;;; init-windows.el ends here
