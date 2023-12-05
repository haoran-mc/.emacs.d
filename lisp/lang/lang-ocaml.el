;;; lang-ocaml.el --- ocaml -*- lexical-binding: t -*-

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

;; Ocaml mode
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :custom
  (tuareg-match-patterns-aligned t)
  (tuareg-indent-align-with-first-arg t))

;; Context sensitive completion
;; Bundled with the aur package `merlin'
(use-package merlin
  :ensure nil
  :hook (tuareg-mode . merlin-mode)
  :custom
  (merlin-command "ocamlmerlin"))

;; Indentation tool for OCaml
;; Bundled with the system package `ocaml-ocp-indent'
(use-package ocp-indent
  :ensure nil
  :when (executable-find "ocp-indent")
  :commands ocp-indent-region ocp-indent-buffer
  :hook (tuareg-mode . ocp-setup-indent))

;; The dune build system
;; Bundled with system package `dune'
(use-package dune
  :ensure nil
  :when (executable-find "dune")
  :mode ("dune\\(?:\\.inc\\)?\\'" . dune-mode))

(provide 'lang-ocaml)
;;; lang-ocaml.el ends here
