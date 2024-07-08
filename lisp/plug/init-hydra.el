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
(require 'major-mode-hydra)
(require 'posframe)

;;; Code:
;; hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hydra-hint-display-type 'posframe)
;; 所以不使用 pretty-hydra-define 的方式定义也可以 posframe

(defun hydra-set-posframe-show-params ()
  "Set hydra-posframe style."
  (setq hydra-posframe-show-params ;; cl-defun posframe-show
        `(:left-fringe 8
                       :right-fringe 8
                       :internal-border-width 2
                       :internal-border-color "red"
                       :background-color ,(face-background 'tooltip nil t)
                       :foreground-color ,(face-foreground 'tooltip nil t)
                       :lines-truncate t
                       :poshandler posframe-poshandler-point-bottom-left-corner-upward)))
(hydra-set-posframe-show-params)
(add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t)

(require 'rect)
(global-set-key
 (kbd "C-x SPC")
 (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                      :color pink
                                      :post (deactivate-mark))
   "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _x_kill
"
   ("h" rectangle-backward-char nil)
   ("l" rectangle-forward-char nil)
   ("k" rectangle-previous-line nil)
   ("j" rectangle-next-line nil)
   ("e" hydra-ex-point-mark nil)
   ("n" copy-rectangle-as-kill nil)
   ("d" delete-rectangle nil)
   ("r" (if (region-active-p)
            (deactivate-mark)
          (rectangle-mark-mode 1)) nil)
   ("y" yank-rectangle nil)
   ("u" undo nil)
   ("s" string-rectangle nil)
   ("x" kill-rectangle nil)
   ("o" nil nil)))


;; major-mode-hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/toggle-rainbow-mode ()
  "Toggle rainbow mode."
  (interactive)
  (if (featurep 'rainbow-mode)
      (if (bound-and-true-p rainbow-mode)
          (rainbow-mode -1)
        (rainbow-mode 1))
    (require 'rainbow-mode)
    (rainbow-mode 1)))

(with-no-warnings
  (pretty-hydra-define hydra-main (:title (format "%s Emacs-Lisp Commands"
                                                  (all-the-icons-fileicon "emacs"))
                                          :color amaranth :quit-key ("q" "C-g"))
    ("Basic"
     (("f" toggle-frame-fullscreen "fullscreen" :exit t)
      ("+" text-scale-increase "zoom in")
      ("-" text-scale-decrease "zoom out"))
     "Toggle"
     (("t r" my/toggle-rainbow-mode "rainbow" :exit t)))))

(global-set-key (kbd "M-h") #'hydra-main/body)
(with-eval-after-load 'org (define-key org-mode-map (kbd "M-h") #'hydra-main/body))

(provide 'init-hydra)
;;; init-hydra.el ends here
