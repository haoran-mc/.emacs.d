;;; init-avy.el --- quick move -*- lexical-binding: t; -*-

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

;;; Code:



;; :config
;; (when (>= emacs-major-version 28)
;;   (use-package transient
;;     :ensure nil
;;     :config
;;     (transient-define-prefix avy-menu ()
;;       "Avy quick menu."
;;       :transient-suffix     'transient--do-stay
;;       :transient-non-suffix 'transient--do-warn
;;       [["Move"
;;         ("j" "avy-next" avy-next)
;;         ("k" "avy-prev" avy-prev)
;;         ("p" "avy-pop-mark" avy-pop-mark)]
;;        ["Resume"
;;         ("r" "avy-resume" avy-resume)]
;;        ["Exit"
;;         ("q" "quit" transient-quit-one)]])))

(require 'avy)

(setq avy-background t
      avy-all-windows nil
      ;; 单页字符数量一般不会需要超过三个字符定位
      ;; 两个字符定位时，会优先使用倒叙的字符作为开头，建议左手按第一个字符，右手按第二个字符
      ;; ; 是 macvim 中 easymotion 的首字母，这里沿用
      ;; 单字符选择时，把右手下面的几个按键排在前面
      avy-keys '(?j ?i ?o ?k ?l ?c ?x ?z ?n ?m ?h ?p
                    ?a ?w ?e ?s ?d ?f ?;
                    )
      ;; overlay is used during isearch, `pre' style makes avy keys evident.
      avy-styles-alist '((avy-isearch . pre)))



(require 'ace-pinyin)
(ace-pinyin-global-mode +1)


(provide 'init-avy)
;;; init-avy.el ends here
