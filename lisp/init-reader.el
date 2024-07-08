;;; init-reader.el --- rss reader                    -*- lexical-binding: t; -*-

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

;; ;; Another Atom/RSS reader
;; (use-package newsticker
;;   :ensure nil
;;   :bind ("C-x w" . newsticker-show-news)
;;   ;; :hook (newsticker-treeview-item-mode . centaur-read-mode)
;;   :init (setq newsticker-url-list
;;               '(("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
;;                 ("Mastering Emacs" "http://www.masteringemacs.org/feed/")
;;                 ("Oremacs" "https://oremacs.com/atom.xml")
;;                 ("EmacsCast" "https://pinecast.com/feed/emacscast")
;;                 ("Emacs TIL" "https://emacstil.com/feed.xml")
;;                 ;; ("Emacs Reddit" "https://www.reddit.com/r/emacs.rss")
;;                 )))
;;
;; (require 'init-elfeed)
;;
;; (use-package nov
;;   :ensure t
;;   :mode ("\\.epub\\'" . nov-mode))
;;
;; ;; (use-package nov-xwidget
;; ;;   :demand t
;; ;;   :after nov
;; ;;   :config
;; ;;   (define-key nov-mode-map (kbd "o") 'nov-xwidget-view)
;; ;;   (add-hook 'nov-mode-hook 'nov-xwidget-inject-all-files))


(provide 'init-reader)
;;; init-reader.el ends here
