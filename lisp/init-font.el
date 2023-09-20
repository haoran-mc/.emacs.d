;;; init-font.el --- font for emacs                  -*- lexical-binding: t; -*-

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

;; install Fira Code and Fira Code Symbol Font
;; https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip

;;; Code:


;; NOTE: change org-mode custom-face when change font-weight
;; (set-face-attribute 'default nil :font "Fira Code" :weight 'normal)
;; (set-face-attribute 'default nil :font "Roboto Mono" :weight 'normal)
;; (set-face-attribute 'default nil :font "PingFang SC")
(set-face-attribute 'default nil
                    :font (font-spec :family
                                     "JetBrainsMono Nerd Font"
                                     :weight 'semi-bold)) ;; set height in arch

(defun my-correct-symbol-bounds (pretty-alist)
  "Prepend a TAB character to each symbol in this alist,
  this way compose-region called by prettify-symbols-mode
  will use the correct width of the symbols
  instead of the width measured by char-width."
  (mapcar (lambda (el)
            (setcdr el (string ?\t (cdr el)))
            el)
          pretty-alist))


(defun my-ligature-list (ligatures codepoint-start)
  "Create an alist of strings to replace with
  codepoints starting from codepoint-start."
  (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
    (-zip-pair ligatures codepoints)))


(setq my-fira-code-ligatures
      (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                     "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                     "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                     "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                     ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                     "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                     "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                     "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                     ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                     "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                     "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                     "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                     "x" ":" "+" "+" "*")))
        (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))


(defun my-set-fira-code-ligatures ()
  "Add fira code ligatures for use with prettify-symbols-mode."
  (setq prettify-symbols-alist
        (append my-fira-code-ligatures prettify-symbols-alist))
  (prettify-symbols-mode))


;; (add-hook 'prog-mode-hook 'my-set-fira-code-ligatures)
;; (add-hook 'org-mode-hook 'my-set-fira-code-ligatures)


(provide 'init-font)
;;; init-font.el ends here
