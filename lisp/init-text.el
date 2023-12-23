;;; init-text.el --- Writing -*- lexical-binding: t -*-

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
;; `org-mode' is too huge to place here.
;; `txt'

;;; Code:

;; valign ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pixel alignment for org/markdown tables
(require 'valign)
(dolist (mode-hook '(markdown-mode org-mode))
  (add-hook mode-hook 'valign-mode))
(setq valign-fancy-bar t)


;; valign ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type text
(require 'text-mode)
;; better word wrapping for CJK characters
(setq word-wrap-by-category t
      ;; paragraphs
      sentence-end-double-space nil)


;; writeroom-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; another nice writing environment
;; (require 'writeroom-mode)


;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The markdown mode is awesome! unbeatable
(require 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))


(setq markdown-enable-wiki-links t
      markdown-italic-underscore t
      markdown-asymmetric-header t
      markdown-make-gfm-checkboxes-buttons t
      markdown-gfm-uppercase-checkbox t
      markdown-fontify-code-blocks-natively t)

(with-no-warnings
  ;; Use `which-key' instead
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore))

(defun +func-markdown-insert-ruby-tag (text ruby)
  "Insert ruby tag with `TEXT' and `RUBY' quickly."
  (interactive "sText: \nsRuby: \n")
  (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

(defun +func-markdown-insert-details (title)
  "Insert details tag (collapsible) quickly."
  (interactive "sTitle: ")
  (insert (format "<details><summary>%s</summary>\n\n</details>" title)))


;; markdown-toc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of contents
;; (require 'markdown-toc)


;; grip-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sudo pip install grip or pip install grip
;; use grip-mode in markdown-mode and org-mode
(require 'grip-mode)
(setq grip-update-after-change nil)


(provide 'init-text)
;;; init-text.el ends here
