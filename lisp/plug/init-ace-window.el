;;; init-ace-window.el ---use ace-window for a better treemacs experience  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Haoran Liu

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

;;; Require:
(require 'init-avy) ;; 依赖 avy，所以要提前导入 init-avy
                    ;; 否则如果使用 ace-window 先于 avy，init-avy 中的配置失效
(require 'ace-window)
(require 'ace-window-posframe)


;;; Code:
(setq aw-background nil)
(ace-window-posframe-enable)


(set-face-attribute 'aw-leading-char-face nil
                    :inherit 'font-lock-keyword-face
                    :foreground 'unspecified
                    :weight 'bold
                    :height 3.0)

(set-face-attribute 'aw-minibuffer-leading-char-face nil
                    :inherit 'font-lock-keyword-face
                    :weight 'bold
                    :height 1.0)

(set-face-attribute 'aw-mode-line-face nil
                    :inherit 'mode-line-emphasis
                    :weight 'bold)


(provide 'init-ace-window)
;;; init-ace-window.el ends here
