;;; ext-projectile.el --- project search -*- lexical-binding: t; -*-

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






;; Project management
;; sudo apt-get install silversearcher-ag
;; sudo apt-get install ripgrep
;; ? Commander help buffer.
;; D Open project root in dired.
;; R Regenerate the project’s etags/gtags.
;; T Find test file in project.
;; V Browse dirty projects
;; a Run ag on project.
;; b Switch to project buffer.
;; d Find directory in project.
;; e Find recently visited file in project.
;; f Find file in project.
;; g Run grep on project.
;; j Find tag in project.
;; k Kill all project buffers.
;; o Run multi-occur on project buffers.
;; r Replace a string in the project.
;; s Switch project.
;; v Open project root in vc-dir or magit.
;; projectile-add-know-project
;; projectile-project-root-
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (dolist (dir '("bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  :custom
  (projectile-use-git-grep t)
  (projectile-indexing-method 'alien)
  (projectile-kill-buffers-filter 'kill-only-files)
  ;; Ignore uninteresting files. It has no effect when using alien mode.
  (projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so" ".a"))
  (projectile-ignored-projects `("~/"
                                 "/tmp/"
                                 "/private/tmp/"
                                 ,package-user-dir)))


(provide 'ext-projectile)
;;; ext-projectile.el ends here
