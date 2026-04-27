;;; init-org-appear.el --- make invisible parts of Org elements appear visible  -*- lexical-binding: t; -*-

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
(require 'org-expose-emphasis-markers)

;; 1. make sure `org-hide-emphasis-markers' is true
(setq org-hide-emphasis-markers t)

;; 2. only expose when in evil insert mode
(setq org-expose-emphasis-markers-inhibit-determine-function
      (lambda () (not (evil-insert-state-p))))

;; 3. scope: expose only the emphasis element under cursor
(setq org-expose-emphasis-markers-type 'item)

;; 4. turn on the mode
(add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t)))


(provide 'init-org-appear)
;;; init-org-appear.el ends here
