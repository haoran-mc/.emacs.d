;;; init-straight.el --- straight.el bootstrap and package list -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>

;;; Commentary:

;;; Code:

(setq straight-vc-git-default-clone-depth 1
  straight-use-package-by-default nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(org :type built-in))

(defconst my/straight-submodule-packages
  '((go-mode :type git :host github :repo "dominikh/go-mode.el")
    (yaml-mode :type git :host github :repo "yoshiki/yaml-mode")
    (toml-mode :type git :host github :repo "dryman/toml-mode.el")
    (json-mode :type git :host github :repo "json-emacs/json-mode")
    (symbol-overlay :type git :host github :repo "wolray/symbol-overlay")
    (rg :type git :host github :repo "dajva/rg.el")
    (yasnippet :type git :host github :repo "joaotavora/yasnippet")
    (diredfl :type git :host github :repo "purcell/diredfl")
    (marginalia :type git :host github :repo "minad/marginalia")
    (vertico :type git :host github :repo "minad/vertico")
    (consult :type git :host github :repo "minad/consult")
    (orderless :type git :host github :repo "oantolin/orderless")
    (org-superstar :type git :host github :repo "integral-dw/org-superstar-mode")
    (org-appear :type git :host github :repo "awth13/org-appear")
    (rainbow-mode :type git :host github :repo "emacsmirror/rainbow-mode")
    (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
    (auto-save :type git :host github :repo "manateelazycat/auto-save")
    (avy :type git :host github :repo "abo-abo/avy")
    (vundo :type git :host github :repo "casouri/vundo")
    (treemacs :type git :host github :repo "Alexander-Miller/treemacs")
    (dirvish :type git :host github :repo "alexluigit/dirvish")
    (diff-hl :type git :host github :repo "dgutov/diff-hl")
    (valign :type git :host github :repo "casouri/valign")
    (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
    (grip-mode :type git :host github :repo "seagle0128/grip-mode")
    (benchmark-init :type git :host github :repo "dholm/benchmark-init-el")
    (fanyi :type git :host github :repo "condy0919/fanyi.el")
    (plantuml-mode :type git :host github :repo "skuro/plantuml-mode")
    (no-littering :type git :host github :repo "emacscollective/no-littering")
    compat
    (dash :type git :host github :repo "magnars/dash.el")
    (s :type git :host github :repo "magnars/s.el")
    (f :type git :host github :repo "rejeep/f.el")
    (popup :type git :host github :repo "auto-complete/popup-el")
    (reformatter :type git :host github :repo "purcell/emacs-reformatter")
    (lua-mode :type git :host github :repo "immerrr/lua-mode")
    (hl-todo :type git :host github :repo "tarsius/hl-todo")
    (magit :type git :host github :repo "magit/magit")
    (with-editor :type git :host github :repo "magit/with-editor")
    (keyfreq :type git :host github :repo "dacap/keyfreq")
    (ht :type git :host github :repo "Wilfred/ht.el")
    (hydra :type git :host github :repo "abo-abo/hydra")
    (yafolding :type git :host github :repo "emacsorphanage/yafolding")
    (web-mode :type git :host github :repo "fxbois/web-mode")
    (major-mode-hydra :type git :host github :repo "jerrypnz/major-mode-hydra.el")
    (posframe :type git :host github :repo "tumashu/posframe")
    (rust-mode :type git :host github :repo "rust-lang/rust-mode")
    (dockerfile-mode :type git :host github :repo "spotify/dockerfile-mode")
    (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")
    (pyvenv :type git :host github :repo "jorgenschaefer/pyvenv"))
  "Packages that were previously managed as git submodules under site-lisp.")

(dolist (package my/straight-submodule-packages)
  (straight-use-package package))

(provide 'init-straight)
;;; init-straight.el ends here