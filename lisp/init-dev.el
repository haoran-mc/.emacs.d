;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;; Copyright (C) 2022  Haoran Liu

;; Author: HaoRan Liu <haoran.mc@outlook.com>
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
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :custom
  (setq yaml-indent-offset 2))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :ensure nil
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))

(use-package json-mode
  :ensure t
  :hook (json-mode . (lambda() (make-local-variable 'js-indent-level)
                       ;; https://stackoverflow.com/a/24668842/14093697
                       (setq js-indent-level 2))))

(use-package toml-mode
  :ensure t)

(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . (lambda() (setq truncate-lines t)))))

(require 'lang-cpp)
;; (require 'lang-rust)
;; (require 'lang-ocaml)
;; (require 'lang-bazel)
;; (require 'lang-haskell)
;; (require 'lang-sh)
(require 'lang-golang)
(require 'lang-sql)
(require 'lang-python)
(require 'lang-elisp)
(require 'lang-latex)
(require 'lang-lua) ;; just syntax for org-mode src
(require 'lang-js)

(provide 'init-dev)

;;; init-dev.el ends here
