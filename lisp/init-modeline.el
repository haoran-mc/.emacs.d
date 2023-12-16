;;; init-modeline.el --- modeline                    -*- lexical-binding: t; -*-

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

;; (setq-default mode-line-format nil)

(add-to-list 'load-path "~/Documents/emacs/local-packages/awesome-tray")
(require 'awesome-tray)

(setq awesome-tray-active-modules '("location" "buffer-name")
      awesome-tray-update-interval 0.5)

(awesome-tray-mode 1)

(provide 'init-modeline)
;;; init-modeline.el ends here
