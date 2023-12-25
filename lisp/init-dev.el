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


;; conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for systemd files
;; (add-to-list 'auto-mode-alist ((rx "."
;;                                    (or "automount" "busname" "link" "mount" "netdev" "network"
;;                                        "path" "service" "slice" "socket" "swap" "target" "timer")
;;                                    string-end) . conf-mode))



;; prog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; imenu-list?


;; TODO org-mode src
;; TODO wrapped into a function?
;; TODO or idle?

;; (require 'lang-cpp)
;; (require 'lang-rust)
;; (require 'lang-ocaml)
;; (require 'lang-bazel)
;; (require 'lang-haskell)
;; (require 'lang-sh)
;; (require 'lang-latex)
;; (require 'lang-lua) ;; just syntax for org-mode src
;; (require 'lang-golang)
;; (require 'lang-sql)
;; (require 'lang-python)
;; (require 'lang-elisp)
(require 'lang-js)

(provide 'init-dev)
;;; init-dev.el ends here
