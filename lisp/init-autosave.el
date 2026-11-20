;;; init-autosave.el --- auto save                   -*- lexical-binding: t; -*-

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


(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t) ;; quietly save
(setq auto-save-delete-trailing-whitespace t) ;; automatically delete spaces at the end of the line when saving

;; 按实际文件后缀跳过 .pgp 和 .gpg 文件的自动保存
(setq auto-save-disable-predicates
      '((lambda ()
          (when buffer-file-name
            (member (downcase (or (file-name-extension buffer-file-name) ""))
                    '("pgp" "gpg"))))))


(provide 'init-autosave)
;;; init-autosave.el ends here
