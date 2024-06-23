;;; init-vundo.el ---visualize undo                  -*- lexical-binding: t; -*-

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

;;; Require:
;;
(require 'vundo)

;;; Code:
(defhydra hydra-vundo (:body-pre (require 'vundo)
                                         :color blue)
  ("h" vundo-backward "backward" :column "vundo")
  ("j" vundo-stem-root "stem root")
  ("k" vundo-stem-end "stem end")
  ("l" vundo-forward "forward")
  ("n" vundo-next "next")
  ("p" vundo-previous "previous")
  ("," vundo-goto-last-saved "last save")
  ("C-m" vundo-confirm "confirm")
  ("i" vundo--inspect "inspect")
  ("d" vundo--debug "debug"))
  ;; ("q" vundo-quit "quit")
  ;; ("C-g" vundo-quit "quit"))
(define-key vundo-mode-map (kbd "?") #'hydra-vundo/body)


(provide 'init-vundo)
;;; init-vundo.el ends here
