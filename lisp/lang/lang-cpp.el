;;; lang-cpp.el --- Cpp -*- lexical-binding: t -*-

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


;; Mode association (autoload cc-mode for *.cpp files)
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . cc-mode))

;; BUILT-IN
(with-eval-after-load 'cc-mode
  '(progn
     (defun +compile-file ()
       (interactive)
       (compile
        (format "g++ -o %s %s -g -lm -Wall"
                (file-name-sans-extension (buffer-name))
                (buffer-name))))

     (defun +run-file ()
       (interactive)
       (if (with-no-warnings (eshell-command
                              (format "g++ -o a %s -g -lm -Wall"
                                      (buffer-name))))
           (aweshell-dedicated-toggle)))

     (add-hook 'c-mode-common (lambda () (c-set-style "stroustrup")))
     (define-key c-mode-base-map (kbd "C-c C-c") '+compile-file)
     (define-key c-mode-base-map (kbd "<f9>") '+run-file)
     (define-key c-mode-base-map (kbd "<f10>") 'gud-gdb)
     )
  )


(provide 'lang-cpp)
;;; lang-cpp.el ends here
