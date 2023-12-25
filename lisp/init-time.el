;;; init-time.el --- time in emacs                   -*- lexical-binding: t; -*-

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

(setq system-time-locale "C")

(setq display-time-day-and-date t)    ;; 打开日期显示
(display-time-mode 1)                 ;; 打开时间显示
(display-time)                        ;; 显示时间
(setq display-time-format "%H:%M")    ;; 设定时间显示格式
(setq display-time-24hr-format t)     ;; 打开24小时显示模式

(provide 'init-time)
;;; init-time.el ends here
