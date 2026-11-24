;;; init-hydra.el --- tie related commands into a family of short bindings with a common prefix  -*- lexical-binding: t; -*-

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

;;; Require:
(require 'hydra)
(require 'pretty-hydra)
(require 'all-the-icons)

;;; Code:
;; hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hydra-hint-display-type 'posframe)
;; 所以不使用 pretty-hydra-define 的方式定义也可以 posframe

(defun hydra-set-posframe-show-params (&optional _theme)
  "Set hydra-posframe style."
  (setq hydra-posframe-show-params ;; cl-defun posframe-show
        `(:left-fringe 8
                       :right-fringe 8
                       :internal-border-width 2
                       :internal-border-color "red"
                       :background-color ,(face-background 'tooltip nil t)
                       :foreground-color ,(face-foreground 'tooltip nil t)
                       :lines-truncate t
                       :poshandler posframe-poshandler-window-top-right-corner)))
(hydra-set-posframe-show-params)
(add-hook 'enable-theme-functions #'hydra-set-posframe-show-params)
(add-hook 'disable-theme-functions #'hydra-set-posframe-show-params)



(defun my/toggle-indent-tabs-mode ()
  "Toggle between using tabs and spaces for indentation."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indentation now uses %s." (if indent-tabs-mode "tabs" "spaces")))

(with-no-warnings
  (pretty-hydra-define hydra-main (:title (format "%s Emacs-Lisp Commands"
                                                  (all-the-icons-fileicon "emacs"))
                                          :color amaranth :quit-key ("q" "C-g"))
    ("Basic"
     (("f" toggle-frame-fullscreen "fullscreen" :exit t)
      ("m" toggle-frame-maximized "maximized" :exit t)
      ("k" kill-this-buffer "kill this buffer" :exit t)
      ("r" my/reload-this-buffer "refresh file" :exit t)
      ("+" ran/larger-font "zoom in") ;; text-scale-increase 只更改当前 buffer
      ("-" ran/smaller-font "zoom out"))
     "Toggle"
     (("t i" my/toggle-indent-tabs-mode "tab indent" :exit t)
      ("t r" rainbow-mode "rainbow" :exit t)))))

(global-set-key (kbd "M-h") #'hydra-main/body)
(with-eval-after-load 'org (define-key org-mode-map (kbd "M-h") #'hydra-main/body))

(provide 'init-hydra)
;;; init-hydra.el ends here
