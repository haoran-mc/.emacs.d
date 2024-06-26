;;; init-gc.el --- GC optimization                 -*- lexical-binding: t; -*-

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

(require 'gcmh)

(add-hook 'after-init-hook 'gcmh-mode)

(setq gcmh-idle-delay 10
      ;; gcmh-verbose t
      gcmh-high-cons-threshold #x12c00000) ;; 100MB

;; (+ (* 500 1024 1024) 0)
;;=> 524288000


(provide 'init-gc)
;;; init-gc.el ends here
