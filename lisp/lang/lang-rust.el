;;; lang-rust.el --- Rust -*- lexical-binding: t -*-

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

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (with-no-warnings
    (with-eval-after-load 'lsp-mode
      (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-extern-crate"])))
  :custom
  (rust-format-show-buffer nil)
  (rust-format-on-save (executable-find "rustfmt")))

;; Cargo integration
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide 'lang-rust)
;;; lang-rust.el ends here
