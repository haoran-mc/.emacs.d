;;; init-which-key.el --- prompt keys                 -*- lexical-binding: t; -*-

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
;; Tips for next keystroke
;; (add-hook 'after-init-hook 'which-key-mode)

;;; Require:
(require 'which-key)

;;; Code:
(which-key-mode)

(which-key-add-major-mode-key-based-replacements 'markdown-mode
  "C-c m" "markdown")

(setq which-key-idle-delay 100000 ;; 0.05 设置一个足够大的值，意味着不自动弹出 which-key buffer
      which-key-show-early-on-C-h t ;; 使用 C-h 手动调用 which-key buffer
      which-key-idle-secondary-delay 0.03
      which-key-add-column-padding 1)

(provide 'init-which-key)
;;; init-which-key.el ends here
