;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'init-macros)

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

;; (require 'lang-cpp)
;; (require 'lang-rust)
;; (require 'lang-ocaml)
;; (require 'lang-bazel)
;; (require 'lang-haskell)
;; (require 'lang-sh)
;; (require 'lang-golang)
(require 'lang-python)
(require 'lang-elisp)
(require 'lang-latex)

(provide 'init-dev)

;;; init-dev.el ends here
