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
                                 (buffer-name))))))

;; 直到遇到 hook 才加载
(dolist (hook '(c-mode-common-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'(lambda ()
                     (require 'cc-mode) ;; BUILT-IN c++-mode 定义在 cc-mode 中
                     ;; (require 'modern-cpp-font-lock)

                     (defun c-mode-style-setup ()
                       (interactive)
                       ;; base-style
                       (c-set-style "stroustrup"))

                     (c-mode-style-setup)

                     ;; 先只配置 c++
                     (define-key c++-mode-map (kbd "C-c C-c") '+compile-file)
                     (define-key c++-mode-map (kbd "<f9>") '+run-file)
                     (define-key c++-mode-map (kbd "<f10>") 'gud-gdb))))

(provide 'lang-cpp)
;;; lang-cpp.el ends here
