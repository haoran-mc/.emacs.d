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
(require 'org-appear)

(setq org-appear-trigger 'manual
      org-appear-autoemphasis t ;; emphasis(bold, verbatim, code, italic, underline, strike-through)
      org-appear-autolinks t ;; link
      org-appear-autosubmarkers t)

(add-hook 'org-mode-hook (lambda ()
                           (org-appear-mode +1)
                           (add-hook 'evil-insert-state-entry-hook
                                     #'org-appear-manual-start
                                     nil
                                     t)
                           (add-hook 'evil-insert-state-exit-hook
                                     #'org-appear-manual-stop
                                     nil
                                     t)))


(provide 'init-org-appear)
;;; init-org-appear.el ends here
