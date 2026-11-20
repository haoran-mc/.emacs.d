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


;; 控制 Emacs 在 GC 时是否压缩字体缓存，使用字体缓存可以避免卡顿
(setq inhibit-compacting-font-caches t)


(require 'gcmh)

(setq gcmh-idle-delay 10 ;; 空闲 10 秒后触发垃圾回收
      gcmh-high-cons-threshold (* 300 1024 1024)) ;; 编辑时的 GC 分配阈值为 300 MiB

;; 初始化完成后启用 GCMH，由它接管 GC 阈值。
(add-hook 'after-init-hook #'gcmh-mode)

(provide 'init-gc)
;;; init-gc.el ends here
