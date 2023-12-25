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

;; xml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss\\'" . nxml-mode))
(with-eval-after-load 'nxml-mode
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))


;; yaml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . (lambda () (require 'yaml-mode)
                                                  (yaml-mode))))
(with-eval-after-load 'yaml-mode
  (setq yaml-indent-offset 2))


;; conf ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for systemd files
;; (add-to-list 'auto-mode-alist ((rx "."
;;                                    (or "automount" "busname" "link" "mount" "netdev" "network"
;;                                        "path" "service" "slice" "socket" "swap" "target" "timer")
;;                                    string-end) . conf-mode))


;; json ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.json\\'" . (lambda () (require 'json-mode)
                                                 (json-mode))))
(with-eval-after-load 'json-mode
  (make-local-variable 'js-indent-level)
  ;; https://stackoverflow.com/a/24668842/14093697
  (setq js-indent-level 2))


;; toml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.toml\\'" . (lambda () (require 'toml-mode)
                                                 (toml-mode))))


;; prog ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))


;; imenu-list?


;; TODO org-mode src
(defun +load-lang-config ()
  "Load lang-golang.el configuration when visiting a .go file."
  (cond ((and buffer-file-name
              (string= (file-name-extension buffer-file-name) "go"))
         (require 'lang-golang))
        ((and buffer-file-name
              (string= (file-name-extension buffer-file-name) "py"))
         (require 'lang-python))
        ((and buffer-file-name
              (string= (file-name-extension buffer-file-name) "el"))
         (require 'lang-elisp))
        ((and buffer-file-name
              (string= (file-name-extension buffer-file-name) "sql"))
         (require 'lang-sql))
        ((and buffer-file-name
              (string= (file-name-extension buffer-file-name) "lua"))
         (require 'lang-lua))))


(add-hook 'find-file-hook '+load-lang-config)


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
