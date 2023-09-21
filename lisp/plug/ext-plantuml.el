;;; ext-plantuml.el --- PlantUML mode for emacs      -*- lexical-binding: t; -*-

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

(use-package plantuml-mode
  :ensure t
  ;; :config
  ;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  :custom
  ;; EXTERNAL-TOOLS
  (plantuml-jar-path (expand-file-name "~/Documents/emacs/org/private/plantuml.jar"))
  (plantuml-default-exec-mode 'jar)
  (org-plantuml-jar-path (expand-file-name "~/Documents/emacs/org/private/plantuml.jar"))
  (plantuml-jar-args '("-charset" "UTF-8"))
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))


(provide 'ext-plantuml)
;;; ext-plantuml.el ends here
