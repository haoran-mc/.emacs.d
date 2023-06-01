;;; ext-projectile.el --- project search -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Haoran Liu

;; Author: Haoran Liu <haoran@me-2.local>
;; Keywords:

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
