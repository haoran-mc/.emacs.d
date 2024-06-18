;;; init-tab-bar.el --- vanilla tab bar              -*- lexical-binding: t; -*-

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

(require 'tab-bar)

(setq tab-bar-show t
      tab-bar-close-button nil
      tab-bar-close-button-show nil
      tab-bar-tab-hints nil ;; absolute numbers on tabs
      tab-bar-auto-width nil
      tab-bar-tab-name-function 'tab-bar-tab-name-all
      tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
      tab-bar-new-tab-to 'rightmost)

(require 'basic-tookit)


(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
