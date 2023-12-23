;;; init-isearch.el --- enhance isearch              -*- lexical-binding: t; -*-

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

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

(define-advice isearch-occur (:after (_regexp &optional _nlines))
  (isearch-exit))

;; Record isearch in minibuffer history, so C-x ESC ESC can repeat it.
(setq isearch-resume-in-command-history t
      ;; One space can represent a sequence of whitespaces
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace t
      isearch-repeat-on-direction-change t
      ;; M-< and M-> move to the first/last occurrence of the current search string.
      isearch-allow-motion t
      isearch-motion-changes-direction t
      ;; lazy isearch
      isearch-lazy-count t
      isearch-lazy-highlight t
      lazy-count-prefix-format nil
      lazy-count-suffix-format " [%s/%s]"
      lazy-highlight-buffer t
      ;; Mimic Vim
      lazy-highlight-cleanup nil)


(provide 'init-isearch)
;;; init-isearch.el ends here
