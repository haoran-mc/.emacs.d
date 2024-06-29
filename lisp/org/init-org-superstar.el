;;; init-org-superstar.el --- prettify headings and plain lists in org mode.  -*- lexical-binding: t; -*-

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


(require 'org-superstar)


;; ● ☯ ○ ❂ ❉ ✸ ⊕ ◉ ◎ ☉ ☰ ☷ ☲ ☵ ❖ ■ ◆ ▲ ▼ ▶ ✦ ✧ ✶ ▸ ▪ ▫ ◇ ◆ ☀ ☁ ☂ ☃ ☎ ☑ ☢ ☣ ☪ ☮ ☸ ☹ ☺ ☻ ☼
;; ⚀ ⚁ ⚂ ⚃ ⚄ ⚅
(setq org-superstar-headline-bullets-list '("☯" "❅" "❂" "✸" "▸") ;; 五级标题已经够多了
      org-superstar-special-todo-items t ;; 用于定义在标题行中特殊的待办事项标记的显示样式
      org-superstar-prettify-item-bullets t ;; 使用列表的美化
      org-superstar-item-bullet-alist '((?- . ?◦) (?* . ?–) (?+ . ?•)))

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(provide 'init-org-superstar)
;;; init-org-superstar.el ends here
