;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

;; Highlight TODO
(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t i" . hl-todo-insert)
              ("C-c t o" . hl-todo-occur)
              ("C-c t s" . hl-todo-rgrep)))

;; Show trailing whitespaces
(use-package whitespace
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

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

;; xref
(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))

;; Hiding structured data
;;
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))

  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  :custom
  (hs-set-up-overlay #'hideshow-folded-overlay-fn))

;; XML
(use-package nxml-mode
  :ensure nil
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

;; (require 'init-cpp)
;; (require 'init-rust)
;; (require 'init-ocaml)
;; (require 'init-bazel)
;; (require 'init-haskell)
(require 'init-python)
(require 'init-elisp)
(require 'init-sh)
(require 'init-golang)

(provide 'init-dev)

;;; init-dev.el ends here
