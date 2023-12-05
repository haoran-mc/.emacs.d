;;; custom.el --- custom configuration -*- lexical-binding: t -*-

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
;; keep custom.el clean.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-catch-invisible-edits 'smart nil nil "Customized with use-package org")
 '(package-selected-packages
   '(jupyter imenu-list vertico-prescient prescient toml-mode json-mode literate-calc-mode yasnippet-snippets yaml-mode writeroom-mode with-proxy which-key vertico valign use-package try treemacs-evil sqlformat simple-httpd shackle rg rainbow-mode rainbow-identifiers quelpa pyvenv projectile plantuml-mode org-superstar org-download org-contrib org-appear orderless ob-go no-littering markdown-toc marginalia magit lua-mode live-py-mode hungry-delete htmlize hl-todo grip-mode go-mode gcmh fanyi exec-path-from-shell evil-surround evil-collection embark-consult dumb-jump doom-themes diredfl diff-hl auctex-latexmk all-the-icons ace-pinyin)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
