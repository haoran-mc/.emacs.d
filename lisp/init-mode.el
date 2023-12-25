;;; init-mode.el --- config emacs major mode         -*- lexical-binding: t; -*-

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


(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))




(dolist (elt-cons '(
                    ("\\.org\\'" . org-mode)
                    ("\\.go$" . go-mode)
                    ("\\.py$" . python-mode)
                    ("\\.el$" . emacs-lisp-mode)
                    ("\\.sql$" . sql-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.json$" . json-mode)
                    ("\\.toml$" . toml-mode)
                    ("\\.xml$" . xml-mode)
                    ("\\.rss$" . xml-mode)
                    ("\\.yaml$" . yaml-mode)
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))


(autoload 'org-mode "init-org")
(autoload 'go-mode "lang-golang")
(autoload 'python-mode "lang-python")
(autoload 'emacs-lisp-mode "lang-elisp")
(autoload 'sql-mode "lang-sql")
(autoload 'lua-mode "lang-lua")
(autoload 'json-mode "lang-json")
(autoload 'toml-mode "lang-toml")
(autoload 'xml-mode "lang-xml")
(autoload 'yaml-mode "lang-yaml")


(provide 'init-mode)
;;; init-mode.el ends here
