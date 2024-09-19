;;; init-linux.el --- specific config for linux      -*- lexical-binding: t; -*-

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

;; Linux specific
(setq x-gtk-use-system-tooltips nil
      x-gtk-use-native-input t
      x-underline-at-descent-line t)


;; fullscreen
(require 'fullscreen)
(fullscreen)

;; (require 'init-eaf)


(run-with-idle-timer
 2 nil
 #'(lambda ()
     (require 'cache-path-from-shell)
     (dolist (var '("LANG" "LC_CTYPE"))
       (add-to-list 'exec-path-from-shell-variables var))
     (exec-path-from-shell-initialize)))

(provide 'init-linux)
;;; init-linux.el ends here
