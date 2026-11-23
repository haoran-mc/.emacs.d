;;; init-straight.el --- straight.el bootstrap and package list -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Haoran Liu

;; Author: Haoran Liu <haoran.mc@outlook.com>

;;; Commentary:

;;; Code:

(setq straight-vc-git-default-clone-depth 1  ;; git 仓库默认深度为 1 的浅克隆
      straight-use-package-by-default nil)  ;; use-package 用 :straight 显式启用 Straight 安装

;; straight 默认在启动时扫描插件源码的修改时间，发现改动就重新构建，更新编译产物和自动加载文件
;; 但是会增加启动时间，所以这里配置跳过启动扫描。只有手动执行 M-x straight-check-all 才会构建
(setq straight-check-for-modifications '(find-when-checking only-once))

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
  '((no-littering :type git :host github :repo "emacscollective/no-littering")
    (auto-save :type git :host github :repo "manateelazycat/auto-save")
    (vundo :type git :host github :repo "casouri/vundo")
    (evil :type git :host github :repo "emacs-evil/evil")

    ;; minibuffer
    (vertico :type git :host github :repo "minad/vertico")
    (orderless :type git :host github :repo "oantolin/orderless")
    (marginalia :type git :host github :repo "minad/marginalia")
    (consult :type git :host github :repo "minad/consult")
    (rg :type git :host github :repo "dajva/rg.el")

    ;; ui
    (painting-theme :type git :host github :repo "haoran-mc/painting-theme")
    (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
    (rainbow-mode :type git :host github :repo "emacsmirror/rainbow-mode")
    (hl-todo :type git :host github :repo "tarsius/hl-todo")

    ;; git
    (magit :type git :host github :repo "magit/magit")
    (with-editor :type git :host github :repo "magit/with-editor")
    (diff-hl :type git :host github :repo "dgutov/diff-hl")

    ;; file
    (treemacs :type git :host github :repo "Alexander-Miller/treemacs")
    (dirvish :type git :host github :repo "alexluigit/dirvish")
    (diredfl :type git :host github :repo "purcell/diredfl")

    ;; lang
    (markdown-mode :type git :host github :repo "jrblevin/markdown-mode")
    (grip-mode :type git :host github :repo "seagle0128/grip-mode")
    (go-mode :type git :host github :repo "dominikh/go-mode.el")
    (rust-mode :type git :host github :repo "rust-lang/rust-mode")
    (lua-mode :type git :host github :repo "immerrr/lua-mode")
    (yaml-mode :type git :host github :repo "yoshiki/yaml-mode")
    (toml-mode :type git :host github :repo "dryman/toml-mode.el")
    (json-mode :type git :host github :repo "json-emacs/json-mode")
    (plantuml-mode :type git :host github :repo "skuro/plantuml-mode")
    (web-mode :type git :host github :repo "fxbois/web-mode")
    (dockerfile-mode :type git :host github :repo "spotify/dockerfile-mode")
    (flymake-ruff :type git :host github :repo "erickgnavar/flymake-ruff")

    (corfu :type git :host github :repo "minad/corfu")
    (external-completion :type git :host github :repo "emacs-straight/external-completion")
    (eglot :type git :host github :repo "joaotavora/eglot")

    (pyvenv :type git :host github :repo "jorgenschaefer/pyvenv")
    (reformatter :type git :host github :repo "purcell/emacs-reformatter")
    (yafolding :type git :host github :repo "emacsorphanage/yafolding")
    (yasnippet :type git :host github :repo "joaotavora/yasnippet")

    ;; org
    (org-superstar :type git :host github :repo "integral-dw/org-superstar-mode")
    (org-expose-emphasis-markers :type git :host github :repo "lorniu/org-expose-emphasis-markers")

    (hydra :type git :host github :repo "abo-abo/hydra")
    (avy :type git :host github :repo "abo-abo/avy")
    (major-mode-hydra :type git :host github :repo "jerrypnz/major-mode-hydra.el")
    (posframe :type git :host github :repo "tumashu/posframe")
    (valign :type git :host github :repo "casouri/valign")
    (symbol-overlay :type git :host github :repo "wolray/symbol-overlay")
    (fanyi :type git :host github :repo "condy0919/fanyi.el")
    (popup :type git :host github :repo "auto-complete/popup-el")
    (keyfreq :type git :host github :repo "dacap/keyfreq") ;; 跟踪 emacs 命令使用频率
    ;; (benchmark-init :type git :host github :repo "dholm/benchmark-init-el") ;; 对 emacs 初始化进行基准测试

    compat
    (dash :type git :host github :repo "magnars/dash.el")
    (s :type git :host github :repo "magnars/s.el")
    (f :type git :host github :repo "rejeep/f.el")
    (ht :type git :host github :repo "Wilfred/ht.el")

    ;; ai
    (gptel :type git :host github :repo "karthink/gptel")
    )
  "Packages that were previously managed as git submodules under site-lisp.")

(dolist (package my/straight-submodule-packages)
  (straight-use-package package))

(provide 'init-straight)
;;; init-straight.el ends here
