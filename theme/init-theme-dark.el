;;; init-theme-dark.el --- custom set theme for dark  -*- lexical-binding: t; -*-

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

;; hl-line
;; (set-face-attribute 'hl-line nil
                    ;; :background "#424868")

;; region
(set-face-attribute 'region nil
                    :background "grey"
                    :foreground "black")

;; org-mode
(with-eval-after-load 'org
  (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-5 nil :inherit 'outline-5 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-6 nil :inherit 'outline-6 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-7 nil :inherit 'outline-7 :weight 'normal :height 1.0)
  (set-face-attribute 'org-level-8 nil :inherit 'outline-8 :weight 'normal :height 1.0)

  (set-face-attribute 'org-document-title nil
                      :weight 'normal
                      :height 1.0)

  (set-face-attribute 'org-link nil
                      :inherit 'link
                      :foreground "#2AA1AE"
                      :weight 'normal))

;; treemacs
(with-eval-after-load 'treemacs
  (set-face-attribute 'treemacs-directory-face nil
                      :height 1
                      :weight 'normal
                      :family "JetBrainsMono Nerd Font")

  (set-face-attribute 'treemacs-file-face nil
                      :height 1
                      :weight 'normal
                      :family "JetBrainsMono Nerd Font")

  ;; Git 状态相关，统一继承自 treemacs-file-face
  (set-face-attribute 'treemacs-git-added-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-conflict-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-ignored-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-modified-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-renamed-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-unmodified-face nil :inherit 'treemacs-file-face)
  (set-face-attribute 'treemacs-git-untracked-face nil :inherit 'treemacs-file-face)

  (set-face-attribute 'treemacs-root-face nil
                      :height 1
                      :weight 'normal
                      :family "JetBrainsMono Nerd Font")

  (set-face-attribute 'variable-pitch nil
                      :family nil))

;; highlight-thing
(with-eval-after-load 'highlight-thing
  (set-face-attribute 'highlight-thing nil
                      :background "#606689"
                      :foreground "white"))

;; symbol-overlay
(with-eval-after-load 'symbol-overlay
  (set-face-attribute 'symbol-overlay-face-1 nil :foreground "black" :background "#A4E57E")
  (set-face-attribute 'symbol-overlay-face-2 nil :foreground "black" :background "#8CCBEA")
  (set-face-attribute 'symbol-overlay-face-3 nil :foreground "black" :background "#FFDB72")
  (set-face-attribute 'symbol-overlay-face-4 nil :foreground "black" :background "#FF7272")
  (set-face-attribute 'symbol-overlay-face-5 nil :foreground "black" :background "#FFB3FF")
  (set-face-attribute 'symbol-overlay-face-6 nil :foreground "black" :background "#9999FF")
  (set-face-attribute 'symbol-overlay-face-7 nil :foreground "black" :background "#1E90FF")
  (set-face-attribute 'symbol-overlay-face-8 nil :foreground "black" :background "#40E0D0"))

;; vertico
(with-eval-after-load 'vertico
  (set-face-attribute 'vertico-current nil
                      :background "#424868"))


(provide 'init-theme-dark)
;;; init-theme-dark.el ends here
